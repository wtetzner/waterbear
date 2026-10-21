use std::collections::{BTreeMap, BTreeSet};

use walrus::{
    FunctionId, FunctionKind, GlobalId, Module, ValType,
    ir::{Instr, InstrLocId, InstrSeqId, LocalId},
};

use super::shape::instruction_shape;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstructionId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstructionKey {
    pub function: FunctionId,
    pub sequence: InstrSeqId,
    pub index: usize,
}

#[derive(Clone, Debug)]
pub enum ValueProducer {
    Instruction {
        instruction: InstructionId,
        result: usize,
    },
    FunctionParameter {
        function: FunctionId,
        parameter: usize,
        local: LocalId,
    },
    LocalInitial {
        function: FunctionId,
        local: LocalId,
    },
    GlobalInitial(GlobalId),
}

#[derive(Clone, Debug)]
pub struct ValueInfo {
    pub id: ValueId,
    pub ty: Option<ValType>,
    pub producer: ValueProducer,
}

#[derive(Clone, Debug)]
pub struct InstructionInfo {
    pub id: InstructionId,
    pub key: InstructionKey,
    pub location: InstrLocId,
    pub outputs: Vec<ValueId>,
}

#[derive(Clone, Debug)]
pub struct FunctionInfo {
    pub function: FunctionId,
    pub entry: InstrSeqId,
    pub parameters: Vec<ValueId>,
    pub locals: BTreeMap<LocalId, ValueId>,
    pub instructions: Vec<InstructionId>,
}

#[derive(Clone, Debug, Default)]
pub struct ProgramIndex {
    pub instructions: Vec<InstructionInfo>,
    pub values: Vec<ValueInfo>,
    pub functions: BTreeMap<FunctionId, FunctionInfo>,
    pub globals: BTreeMap<GlobalId, ValueId>,
    instruction_ids: BTreeMap<InstructionKey, InstructionId>,
}

impl ProgramIndex {
    pub fn build(module: &Module) -> Self {
        let mut index = Self::default();

        for global in module.globals.iter() {
            let value =
                index.push_value(Some(global.ty), ValueProducer::GlobalInitial(global.id()));
            index.globals.insert(global.id(), value);
        }

        for function in module.funcs.iter() {
            let FunctionKind::Local(body) = &function.kind else {
                continue;
            };

            let function_id = function.id();
            let mut parameters = Vec::with_capacity(body.args.len());
            let mut locals = BTreeMap::new();

            for (parameter, local) in body.args.iter().copied().enumerate() {
                let value = index.push_value(
                    Some(module.locals.get(local).ty()),
                    ValueProducer::FunctionParameter {
                        function: function_id,
                        parameter,
                        local,
                    },
                );
                parameters.push(value);
                locals.insert(local, value);
            }

            let mut instructions = Vec::new();
            let mut seen_sequences = BTreeSet::new();
            let mut pending_sequences = vec![body.entry_block()];

            while let Some(sequence) = pending_sequences.pop() {
                if !seen_sequences.insert(sequence) {
                    continue;
                }

                for (instruction_index, (instruction, location)) in
                    body.block(sequence).instrs.iter().enumerate()
                {
                    collect_nested_sequences(instruction, &mut pending_sequences);
                    collect_locals(instruction, &mut locals, function_id, module, &mut index);

                    let key = InstructionKey {
                        function: function_id,
                        sequence,
                        index: instruction_index,
                    };
                    let shape = instruction_shape(module, body, instruction);
                    let instruction_id = InstructionId(index.instructions.len());
                    let outputs = shape
                        .outputs
                        .into_iter()
                        .enumerate()
                        .map(|(result, ty)| {
                            index.push_value(
                                ty,
                                ValueProducer::Instruction {
                                    instruction: instruction_id,
                                    result,
                                },
                            )
                        })
                        .collect();

                    index.instructions.push(InstructionInfo {
                        id: instruction_id,
                        key,
                        location: *location,
                        outputs,
                    });
                    index.instruction_ids.insert(key, instruction_id);
                    instructions.push(instruction_id);
                }
            }

            index.functions.insert(
                function_id,
                FunctionInfo {
                    function: function_id,
                    entry: body.entry_block(),
                    parameters,
                    locals,
                    instructions,
                },
            );
        }

        index
    }

    pub fn instruction_id(&self, key: InstructionKey) -> InstructionId {
        self.instruction_ids[&key]
    }

    pub fn instruction<'a>(&self, module: &'a Module, id: InstructionId) -> &'a Instr {
        let key = self.instructions[id.0].key;
        let function = module.funcs.get(key.function).kind.unwrap_local();
        &function.block(key.sequence).instrs[key.index].0
    }

    fn push_value(&mut self, ty: Option<ValType>, producer: ValueProducer) -> ValueId {
        let id = ValueId(self.values.len());
        self.values.push(ValueInfo { id, ty, producer });
        id
    }
}

fn collect_nested_sequences(instruction: &Instr, pending: &mut Vec<InstrSeqId>) {
    match instruction {
        Instr::Block(op) => pending.push(op.seq),
        Instr::Loop(op) => pending.push(op.seq),
        Instr::IfElse(op) => {
            pending.push(op.alternative);
            pending.push(op.consequent);
        }
        _ => {}
    }
}

fn collect_locals(
    instruction: &Instr,
    locals: &mut BTreeMap<LocalId, ValueId>,
    function: FunctionId,
    module: &Module,
    index: &mut ProgramIndex,
) {
    let local = match instruction {
        Instr::LocalGet(op) => Some(op.local),
        Instr::LocalSet(op) => Some(op.local),
        Instr::LocalTee(op) => Some(op.local),
        _ => None,
    };

    if let Some(local) = local
        && let std::collections::btree_map::Entry::Vacant(entry) = locals.entry(local)
    {
        let value = index.push_value(
            Some(module.locals.get(local).ty()),
            ValueProducer::LocalInitial { function, local },
        );
        entry.insert(value);
    }
}
