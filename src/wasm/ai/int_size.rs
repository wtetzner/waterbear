use crate::wasm::ai::Analyzer;


#[derive(Debug, Clone, Copy)]
pub struct IntSizeAnalysisError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntSize {
    Bits1,
    Bits8,
    Bits16,
    Bits32,
    Bits64,
    Bits128,
}

impl Default for IntSize {
    fn default() -> Self {
        IntSize::Bits1
    }
}

#[derive(Debug, Clone)]
pub struct IntSizeAnalyzer;

// impl Analyzer<IntSize> for IntSizeAnalyzer {
//     type Error = IntSizeAnalysisError;
//
//     fn const_op(&self, cnst: &walrus::ir::Const) -> Result<IntSize, Self::Error> {
//         match cnst.value {
//             walrus::ir::Value::I32(0 | 1) | walrus::ir::Value::I64(0 | 1) | walrus::ir::Value::V128(0 | 1) => Ok(IntSize::Bits1),
//             walrus::ir::Value::F32(_) => Ok(IntSize::Bits32),
//             walrus::ir::Value::F64(_) => Ok(IntSize::Bits64),
//             walrus::ir::Value::I32(_) => 
//             walrus::ir::Value::I64(_)
//             walrus::ir::Value::V128(_)
//         }
//     }
//
//     fn local_set(&self, function: &walrus::Function, local: &walrus::Local, current: Option<&IntSize>, set: super::ValueState<IntSize>) -> Result<IntSize, Self::Error> {
//         todo!()
//     }
//
//     fn local_get(&self, function: &walrus::Function, local: &walrus::Local, value: Option<&IntSize>) -> Result<IntSize, Self::Error> {
//         todo!()
//     }
//
//     fn global_get(&self, global: &walrus::Global, value: Option<&IntSize>) -> Result<IntSize, Self::Error> {
//         todo!()
//     }
//
//     fn global_set(&self, global: &walrus::Global, current: Option<&IntSize>, set: &IntSize) -> Result<IntSize, Self::Error> {
//         todo!()
//     }
//
//     fn binop(&self, binop: &walrus::ir::Binop, left: super::ValueState<IntSize>, right: super::ValueState<IntSize>) -> Result<IntSize, Self::Error> {
//         todo!()
//     }
// }

fn compute_min_size(value: u128) -> IntSize {
    if value <= 1 {
        IntSize::Bits1
    } else if value <= u8::MAX as u128 {
        IntSize::Bits8
    } else if value <= u16::MAX as u128 {
        IntSize::Bits16
    } else if value <= u32::MAX as u128 {
        IntSize::Bits32
    } else if value <= u64::MAX as u128 {
        IntSize::Bits64
    } else {
        IntSize::Bits128
    }
}

