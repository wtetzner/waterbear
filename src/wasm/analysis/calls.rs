use std::collections::{BTreeMap, BTreeSet};

use walrus::{FunctionId, FunctionKind, Module, ValType, ir::Instr};

use super::{forward::ForwardResults, index::ProgramIndex, pointer::PointerOrigins};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PointerBehavior {
    NotAPointer,
    ReadOnly,
    ReadWrite,
    Escapes,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryEffects {
    None,
    Read,
    Write,
    ReadWrite,
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArgumentSummary {
    pub observed_bits: Option<u128>,
    pub pointer: PointerBehavior,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ResultPointerSummary {
    /// Parameter indexes whose pointer identity can reach this result.
    pub parameters: BTreeSet<usize>,
    pub static_data: bool,
    pub shadow_stack: bool,
    pub unknown: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallSummary {
    pub parameters: Vec<ValType>,
    pub results: Vec<ValType>,
    pub arguments: Vec<ArgumentSummary>,
    pub result_pointers: Vec<ResultPointerSummary>,
    pub memory: MemoryEffects,
}

impl CallSummary {
    pub fn conservative(module: &Module, function: FunctionId) -> Self {
        let ty = module.funcs.get(function).ty();
        let parameters = module.types.params(ty).to_vec();
        let results = module.types.results(ty).to_vec();
        let arguments = parameters
            .iter()
            .copied()
            .map(|ty| ArgumentSummary {
                observed_bits: integer_mask(ty),
                pointer: PointerBehavior::Escapes,
            })
            .collect();
        let result_pointers = results
            .iter()
            .map(|_| ResultPointerSummary {
                unknown: true,
                ..ResultPointerSummary::default()
            })
            .collect();

        Self {
            parameters,
            results,
            arguments,
            result_pointers,
            memory: MemoryEffects::Unknown,
        }
    }

    fn local(module: &Module, function: FunctionId) -> Self {
        let ty = module.funcs.get(function).ty();
        let parameters = module.types.params(ty).to_vec();
        let results = module.types.results(ty).to_vec();
        let arguments = parameters
            .iter()
            .copied()
            .map(|ty| ArgumentSummary {
                // Local argument observations are inferred from the body.
                // Starting at no observations lets the fixed point distinguish
                // an unused parameter from one whose integer bits are read.
                observed_bits: integer_mask(ty).map(|_| 0),
                pointer: PointerBehavior::NotAPointer,
            })
            .collect();
        let result_pointers = results
            .iter()
            .map(|_| ResultPointerSummary::default())
            .collect();

        Self {
            parameters,
            results,
            arguments,
            result_pointers,
            memory: MemoryEffects::Unknown,
        }
    }
}

/// Supplies semantic summaries for internal and platform-specific calls.
pub trait CallResolver {
    fn resolve(&self, module: &Module, function: FunctionId) -> CallSummary;
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ConservativeCallResolver;

impl CallResolver for ConservativeCallResolver {
    fn resolve(&self, module: &Module, function: FunctionId) -> CallSummary {
        CallSummary::conservative(module, function)
    }
}

/// Call summaries for a whole module after propagating local pointer behavior
/// through the direct-call graph.
#[derive(Clone, Debug)]
pub struct InferredCallResolver {
    summaries: BTreeMap<FunctionId, CallSummary>,
}

impl InferredCallResolver {
    pub fn infer<R: CallResolver>(
        module: &Module,
        index: &ProgramIndex,
        origins: &ForwardResults<PointerOrigins>,
        imports: &R,
    ) -> Self {
        let mut summaries = module
            .funcs
            .iter()
            .map(|function| {
                let summary = match function.kind {
                    FunctionKind::Local(_) => CallSummary::local(module, function.id()),
                    _ => imports.resolve(module, function.id()),
                };
                (function.id(), summary)
            })
            .collect::<BTreeMap<_, _>>();

        loop {
            let previous = summaries.clone();
            for (&function, info) in &index.functions {
                let parameters = &info.parameters;
                let mut behaviors = vec![PointerBehavior::NotAPointer; parameters.len()];

                for &instruction in &info.instructions {
                    let inputs = origins.instruction_inputs[instruction.0]
                        .as_deref()
                        .unwrap_or(&[]);
                    let effects = pointer_effects(
                        index.instruction(module, instruction),
                        &previous,
                        inputs.len(),
                    );
                    for (input, effect) in inputs.iter().zip(effects) {
                        if effect == PointerBehavior::NotAPointer {
                            continue;
                        }
                        for (parameter, behavior) in parameters.iter().zip(&mut behaviors) {
                            if input.unknown || input.parameters.contains(parameter) {
                                *behavior = (*behavior).max(effect);
                            }
                        }
                    }
                }

                let summary = summaries
                    .get_mut(&function)
                    .expect("local function summary");
                for (argument, behavior) in summary.arguments.iter_mut().zip(behaviors) {
                    argument.pointer = behavior;
                    if behavior != PointerBehavior::NotAPointer {
                        argument.observed_bits = None;
                    }
                }
                let pointer_parameters = summary
                    .arguments
                    .iter()
                    .enumerate()
                    .filter_map(|(index, argument)| {
                        (argument.pointer != PointerBehavior::NotAPointer).then_some(index)
                    })
                    .collect::<BTreeSet<_>>();
                summary.result_pointers = origins.function_returns[&function]
                    .iter()
                    .map(|result| ResultPointerSummary {
                        parameters: result
                            .parameters
                            .iter()
                            .filter_map(|origin| {
                                parameters.iter().position(|parameter| parameter == origin)
                            })
                            .filter(|index| pointer_parameters.contains(index))
                            .collect(),
                        static_data: result.static_data,
                        shadow_stack: result.shadow_stack,
                        unknown: result.unknown,
                    })
                    .collect();
            }

            if summaries == previous {
                return Self { summaries };
            }
        }
    }

    pub fn summaries(&self) -> &BTreeMap<FunctionId, CallSummary> {
        &self.summaries
    }

    /// Adds integer-bit observations discovered in a local function body.
    ///
    /// Pointer parameters use `None` because their accesses are described by
    /// [`PointerBehavior`]. Non-pointer observations grow monotonically as
    /// demands propagate through the direct-call graph.
    pub(crate) fn join_observed_bits(
        &mut self,
        function: FunctionId,
        observed_bits: impl IntoIterator<Item = u128>,
    ) -> bool {
        let Some(summary) = self.summaries.get_mut(&function) else {
            return false;
        };
        let mut changed = false;
        for (argument, observed_bits) in summary.arguments.iter_mut().zip(observed_bits) {
            if argument.pointer != PointerBehavior::NotAPointer {
                continue;
            }
            let Some(current) = argument.observed_bits.as_mut() else {
                continue;
            };
            let joined = *current | observed_bits;
            changed |= joined != *current;
            *current = joined;
        }
        changed
    }
}

impl CallResolver for InferredCallResolver {
    fn resolve(&self, module: &Module, function: FunctionId) -> CallSummary {
        self.summaries
            .get(&function)
            .cloned()
            .unwrap_or_else(|| CallSummary::conservative(module, function))
    }
}

fn pointer_effects(
    instruction: &Instr,
    summaries: &BTreeMap<FunctionId, CallSummary>,
    input_count: usize,
) -> Vec<PointerBehavior> {
    use PointerBehavior::{Escapes, NotAPointer, ReadOnly, ReadWrite};

    match instruction {
        Instr::Load(_) => vec![ReadOnly],
        Instr::Store(_) => vec![ReadWrite, Escapes],
        Instr::MemoryCopy(_) => vec![ReadWrite, ReadOnly, NotAPointer],
        Instr::MemoryFill(_) => vec![ReadWrite, NotAPointer, NotAPointer],
        Instr::MemoryInit(_) => vec![ReadWrite, NotAPointer, NotAPointer],
        Instr::AtomicRmw(_) => vec![ReadWrite, NotAPointer],
        Instr::Cmpxchg(_) => vec![ReadWrite, NotAPointer, NotAPointer],
        Instr::AtomicNotify(_) | Instr::AtomicWait(_) => vec![ReadWrite, NotAPointer],
        Instr::Call(call) => summaries
            .get(&call.func)
            .map(|summary| {
                summary
                    .arguments
                    .iter()
                    .map(|argument| argument.pointer)
                    .collect()
            })
            .unwrap_or_default(),
        Instr::ReturnCall(call) => summaries
            .get(&call.func)
            .map(|summary| {
                summary
                    .arguments
                    .iter()
                    .map(|argument| argument.pointer)
                    .collect()
            })
            .unwrap_or_default(),
        Instr::CallIndirect(_) | Instr::ReturnCallIndirect(_) | Instr::Return(_) => {
            vec![Escapes; input_count]
        }
        Instr::GlobalSet(_) => vec![Escapes],
        _ => vec![NotAPointer; input_count],
    }
}

fn integer_mask(ty: ValType) -> Option<u128> {
    match ty {
        ValType::I32 | ValType::F32 => Some(u32::MAX as u128),
        ValType::I64 | ValType::F64 => Some(u64::MAX as u128),
        ValType::V128 => Some(u128::MAX),
        ValType::Ref(_) => None,
    }
}
