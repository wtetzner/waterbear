use walrus::{FunctionId, Module, ValType};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
pub struct ArgumentSummary {
    pub observed_bits: Option<u128>,
    pub pointer: PointerBehavior,
}

#[derive(Clone, Debug)]
pub struct CallSummary {
    pub parameters: Vec<ValType>,
    pub results: Vec<ValType>,
    pub arguments: Vec<ArgumentSummary>,
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

        Self {
            parameters,
            results,
            arguments,
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

fn integer_mask(ty: ValType) -> Option<u128> {
    match ty {
        ValType::I32 | ValType::F32 => Some(u32::MAX as u128),
        ValType::I64 | ValType::F64 => Some(u64::MAX as u128),
        ValType::V128 => Some(u128::MAX),
        ValType::Ref(_) => None,
    }
}
