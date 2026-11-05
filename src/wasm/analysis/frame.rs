use std::collections::BTreeMap;

use walrus::{FunctionId, GlobalId, Module, ir::Instr};

use super::{
    address::{AddressFact, find_stack_pointer},
    calls::{CallResolver, PointerBehavior},
    forward::ForwardResults,
    index::{InstructionId, ProgramIndex},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FrameObjectId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FrameAccessKind {
    Load,
    Store,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FrameOffset {
    Fixed(i64),
    Range { minimum: i64, maximum: i64 },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrameAccess {
    pub instruction: InstructionId,
    pub kind: FrameAccessKind,
    pub offset: FrameOffset,
    pub width: u32,
    pub alignment: u32,
    pub object: Option<FrameObjectId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrameObject {
    pub id: FrameObjectId,
    pub start: i64,
    pub size: u64,
    pub alignment: u32,
    pub accesses: Vec<InstructionId>,
    pub address_escapes: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FunctionFrame {
    /// A lower bound inferred from constant stack-pointer adjustments.
    pub size_lower_bound: u64,
    pub objects: Vec<FrameObject>,
    pub dynamic_accesses: Vec<InstructionId>,
    pub address_escapes: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FrameResults {
    pub stack_pointer: Option<GlobalId>,
    pub functions: BTreeMap<FunctionId, FunctionFrame>,
    pub accesses: BTreeMap<InstructionId, FrameAccess>,
}

pub fn recover_frames<R: CallResolver>(
    module: &Module,
    index: &ProgramIndex,
    calls: &R,
    addresses: &ForwardResults<AddressFact>,
) -> FrameResults {
    let stack_pointer = find_stack_pointer(module);
    let mut results = FrameResults {
        stack_pointer,
        ..FrameResults::default()
    };

    for (&function, info) in &index.functions {
        let address_escapes = addresses.function_returns[&function]
            .iter()
            .any(|fact| fact.stack_range(function).is_some());
        let mut frame = FunctionFrame {
            address_escapes,
            ..FunctionFrame::default()
        };
        let mut accesses = Vec::new();
        let mut frame_address_is_borrowed = false;

        for &instruction_id in &info.instructions {
            let instruction = index.instruction(module, instruction_id);
            let inputs = addresses.instruction_inputs[instruction_id.0]
                .as_deref()
                .unwrap_or(&[]);

            update_frame_size(
                instruction,
                inputs,
                function,
                stack_pointer,
                &mut frame.size_lower_bound,
            );
            frame.address_escapes |=
                instruction_escapes(module, calls, instruction, inputs, function, stack_pointer);
            frame_address_is_borrowed |=
                instruction_borrows_frame(module, calls, instruction, inputs, function);

            if let Some(access) = memory_access(instruction_id, instruction, inputs, function) {
                if matches!(access.offset, FrameOffset::Range { .. }) {
                    frame.dynamic_accesses.push(instruction_id);
                }
                accesses.push(access);
            }
        }

        frame.objects = build_objects(
            &mut accesses,
            frame.address_escapes,
            frame.size_lower_bound,
            frame_address_is_borrowed,
        );
        for access in accesses {
            results.accesses.insert(access.instruction, access);
        }
        results.functions.insert(function, frame);
    }

    results
}

fn memory_access(
    instruction_id: InstructionId,
    instruction: &Instr,
    inputs: &[AddressFact],
    function: FunctionId,
) -> Option<FrameAccess> {
    let (kind, argument, width) = match instruction {
        Instr::Load(load) => (FrameAccessKind::Load, load.arg, load.kind.width()),
        Instr::Store(store) => (FrameAccessKind::Store, store.arg, store.kind.width()),
        _ => return None,
    };
    let (minimum, maximum) = inputs.first()?.stack_range(function)?;
    let offset = i64::from(argument.offset);
    let minimum = minimum.saturating_add(offset);
    let maximum = maximum.saturating_add(offset);

    Some(FrameAccess {
        instruction: instruction_id,
        kind,
        offset: if minimum == maximum {
            FrameOffset::Fixed(minimum)
        } else {
            FrameOffset::Range { minimum, maximum }
        },
        width,
        alignment: argument.align,
        object: None,
    })
}

fn build_objects(
    accesses: &mut [FrameAccess],
    address_escapes: bool,
    frame_size: u64,
    frame_address_is_borrowed: bool,
) -> Vec<FrameObject> {
    if frame_address_is_borrowed && frame_size > 0 {
        let start = -(frame_size.min(i64::MAX as u64) as i64);
        let mut object = FrameObject {
            id: FrameObjectId(0),
            start,
            size: frame_size,
            alignment: 1,
            accesses: Vec::new(),
            address_escapes,
        };
        for access in accesses {
            object.alignment = object.alignment.max(access.alignment);
            object.accesses.push(access.instruction);
            access.object = Some(object.id);
        }
        return vec![object];
    }

    let mut fixed: Vec<_> = accesses
        .iter()
        .enumerate()
        .filter_map(|(index, access)| match access.offset {
            FrameOffset::Fixed(start) => {
                Some((index, start, start.saturating_add(i64::from(access.width))))
            }
            FrameOffset::Range { .. } => None,
        })
        .collect();
    fixed.sort_by_key(|(_, start, end)| (*start, *end));

    let mut objects: Vec<FrameObject> = Vec::new();
    for (access_index, start, end) in fixed {
        let existing = objects.last_mut().filter(|object| {
            let object_end = object.start.saturating_add(object.size as i64);
            // Keep adjacent accesses together so an address into the region
            // retains their relative layout after abstract storage allocation.
            start <= object_end && end > object.start
        });
        let object = match existing {
            Some(object) => {
                let new_start = object.start.min(start);
                let new_end = object.start.saturating_add(object.size as i64).max(end);
                object.start = new_start;
                object.size = new_end.saturating_sub(new_start) as u64;
                object
            }
            None => {
                let id = FrameObjectId(objects.len());
                objects.push(FrameObject {
                    id,
                    start,
                    size: end.saturating_sub(start) as u64,
                    alignment: 1,
                    accesses: Vec::new(),
                    address_escapes,
                });
                objects.last_mut().unwrap()
            }
        };
        object.alignment = object.alignment.max(accesses[access_index].alignment);
        object.accesses.push(accesses[access_index].instruction);
        accesses[access_index].object = Some(object.id);
    }
    objects
}

fn instruction_borrows_frame<R: CallResolver>(
    module: &Module,
    calls: &R,
    instruction: &Instr,
    inputs: &[AddressFact],
    function: FunctionId,
) -> bool {
    if matches!(instruction, Instr::MemoryFill(_)) {
        return inputs
            .first()
            .is_some_and(|input| input.stack_range(function).is_some());
    }

    let callee = match instruction {
        Instr::Call(call) => call.func,
        Instr::ReturnCall(call) => call.func,
        _ => return false,
    };
    let summary = calls.resolve(module, callee);
    inputs
        .iter()
        .zip(&summary.arguments)
        .any(|(input, argument)| {
            input.stack_range(function).is_some()
                && matches!(
                    argument.pointer,
                    PointerBehavior::ReadOnly | PointerBehavior::ReadWrite
                )
        })
}

fn update_frame_size(
    instruction: &Instr,
    inputs: &[AddressFact],
    function: FunctionId,
    stack_pointer: Option<GlobalId>,
    size: &mut u64,
) {
    let Instr::GlobalSet(set) = instruction else {
        return;
    };
    if Some(set.global) != stack_pointer {
        return;
    }
    let Some((minimum, _)) = inputs.first().and_then(|fact| fact.stack_range(function)) else {
        return;
    };
    if minimum < 0 {
        *size = (*size).max(minimum.unsigned_abs());
    }
}

fn instruction_escapes<R: CallResolver>(
    module: &Module,
    calls: &R,
    instruction: &Instr,
    inputs: &[AddressFact],
    function: FunctionId,
    stack_pointer: Option<GlobalId>,
) -> bool {
    let is_stack_address = |index: usize| {
        inputs
            .get(index)
            .is_some_and(|fact| fact.stack_range(function).is_some())
    };

    match instruction {
        Instr::Store(_) => is_stack_address(1),
        Instr::GlobalSet(set) => Some(set.global) != stack_pointer && is_stack_address(0),
        Instr::Call(call) => call_escapes(module, calls, call.func, &is_stack_address),
        Instr::ReturnCall(call) => call_escapes(module, calls, call.func, &is_stack_address),
        Instr::CallIndirect(_) | Instr::ReturnCallIndirect(_) => inputs
            .iter()
            .any(|fact| fact.stack_range(function).is_some()),
        Instr::Return(_) => inputs
            .iter()
            .any(|fact| fact.stack_range(function).is_some()),
        _ => false,
    }
}

fn call_escapes<R: CallResolver>(
    module: &Module,
    calls: &R,
    function: FunctionId,
    is_stack_address: &impl Fn(usize) -> bool,
) -> bool {
    let summary = calls.resolve(module, function);
    let parameter_count = module.types.params(module.funcs.get(function).ty()).len();
    (0..parameter_count).any(|index| {
        is_stack_address(index)
            && summary
                .arguments
                .get(index)
                .is_none_or(|argument| argument.pointer == PointerBehavior::Escapes)
    })
}
