use crate::wasm::ai::{AnalysisError, Analyzer, BinopValues, Lattice};

#[derive(Debug, Clone, Copy)]
pub struct SignednessAnalysisError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sign {
    Signed,
    Unsigned,
    Unknown,
    Both,
}

impl Lattice for Sign {
    fn bottom() -> Self {
        Sign::Unknown
    }

    fn join(&self, other: &Self) -> Self {
        match (self, other) {
            (Sign::Signed, Sign::Signed) => Sign::Signed,
            (Sign::Signed, Sign::Unsigned) => Sign::Both,
            (Sign::Signed, Sign::Unknown) => Sign::Signed,
            (Sign::Signed, Sign::Both) => Sign::Both,
            (Sign::Unsigned, Sign::Signed) => Sign::Both,
            (Sign::Unsigned, Sign::Unsigned) => Sign::Unsigned,
            (Sign::Unsigned, Sign::Unknown) => Sign::Unsigned,
            (Sign::Unsigned, Sign::Both) => Sign::Both,
            (Sign::Unknown, Sign::Signed) => Sign::Signed,
            (Sign::Unknown, Sign::Unsigned) => Sign::Unsigned,
            (Sign::Unknown, Sign::Unknown) => Sign::Unknown,
            (Sign::Unknown, Sign::Both) => Sign::Both,
            (Sign::Both, Sign::Signed) => Sign::Both,
            (Sign::Both, Sign::Unsigned) => Sign::Both,
            (Sign::Both, Sign::Unknown) => Sign::Both,
            (Sign::Both, Sign::Both) => Sign::Both,
        }
    }
}

impl Default for Sign {
    fn default() -> Self {
        Sign::Unknown
    }
}

#[derive(Debug, Clone)]
pub struct SignAnalyzer;

impl Analyzer<Sign> for SignAnalyzer {
    type Error = SignednessAnalysisError;

    fn const_op(&self, cnst: &walrus::ir::Const) -> Result<Sign, Self::Error> {
        Ok(Sign::Unknown)
    }

    fn binop(
        &self,
        binop: &walrus::ir::Binop,
        left: Sign,
        right: Sign,
    ) -> Result<BinopValues<Sign>, Self::Error> {
        match binop.op {
            walrus::ir::BinaryOp::I32Eq
            | walrus::ir::BinaryOp::I32Ne
            | walrus::ir::BinaryOp::I64Eq
            | walrus::ir::BinaryOp::I64Ne
            | walrus::ir::BinaryOp::F32Eq
            | walrus::ir::BinaryOp::F32Ne
            | walrus::ir::BinaryOp::F64Eq
            | walrus::ir::BinaryOp::F64Ne
            | walrus::ir::BinaryOp::I8x16Eq
            | walrus::ir::BinaryOp::I8x16Ne
            | walrus::ir::BinaryOp::I16x8Eq
            | walrus::ir::BinaryOp::I16x8Ne
            | walrus::ir::BinaryOp::I32x4Eq
            | walrus::ir::BinaryOp::I32x4Ne
            | walrus::ir::BinaryOp::I64x2Eq
            | walrus::ir::BinaryOp::I64x2Ne
            | walrus::ir::BinaryOp::F32x4Eq
            | walrus::ir::BinaryOp::F32x4Ne
            | walrus::ir::BinaryOp::F64x2Eq
            | walrus::ir::BinaryOp::F32Lt
            | walrus::ir::BinaryOp::F32Gt
            | walrus::ir::BinaryOp::F32Le
            | walrus::ir::BinaryOp::F32Ge
            | walrus::ir::BinaryOp::F64Lt
            | walrus::ir::BinaryOp::F64Gt
            | walrus::ir::BinaryOp::F64Le
            | walrus::ir::BinaryOp::F64Ge
            | walrus::ir::BinaryOp::I32Add
            | walrus::ir::BinaryOp::I32Sub
            | walrus::ir::BinaryOp::I32Mul
            | walrus::ir::BinaryOp::I32And
            | walrus::ir::BinaryOp::I32Or
            | walrus::ir::BinaryOp::I32Xor
            | walrus::ir::BinaryOp::I32Shl
            | walrus::ir::BinaryOp::I32Rotl
            | walrus::ir::BinaryOp::I32Rotr
            | walrus::ir::BinaryOp::I64Add
            | walrus::ir::BinaryOp::I64Sub
            | walrus::ir::BinaryOp::I64Mul
            | walrus::ir::BinaryOp::I64And
            | walrus::ir::BinaryOp::I64Or
            | walrus::ir::BinaryOp::I64Xor
            | walrus::ir::BinaryOp::I64Shl
            | walrus::ir::BinaryOp::I64Rotl
            | walrus::ir::BinaryOp::I64Rotr
            | walrus::ir::BinaryOp::F32Add
            | walrus::ir::BinaryOp::F32Sub
            | walrus::ir::BinaryOp::F32Mul
            | walrus::ir::BinaryOp::F32Div
            | walrus::ir::BinaryOp::F32Min
            | walrus::ir::BinaryOp::F32Max
            | walrus::ir::BinaryOp::F32Copysign
            | walrus::ir::BinaryOp::F64Add
            | walrus::ir::BinaryOp::F64Sub
            | walrus::ir::BinaryOp::F64Mul
            | walrus::ir::BinaryOp::F64Div
            | walrus::ir::BinaryOp::F64Min
            | walrus::ir::BinaryOp::F64Max
            | walrus::ir::BinaryOp::F64Copysign
            | walrus::ir::BinaryOp::I8x16ReplaceLane { .. }
            | walrus::ir::BinaryOp::I16x8ReplaceLane { .. }
            | walrus::ir::BinaryOp::I32x4ReplaceLane { .. }
            | walrus::ir::BinaryOp::I64x2ReplaceLane { .. }
            | walrus::ir::BinaryOp::F32x4ReplaceLane { .. }
            | walrus::ir::BinaryOp::F64x2ReplaceLane { .. }
            | walrus::ir::BinaryOp::F32x4Lt
            | walrus::ir::BinaryOp::F32x4Gt
            | walrus::ir::BinaryOp::F32x4Le
            | walrus::ir::BinaryOp::F32x4Ge
            | walrus::ir::BinaryOp::F64x2Lt
            | walrus::ir::BinaryOp::F64x2Gt
            | walrus::ir::BinaryOp::F64x2Le
            | walrus::ir::BinaryOp::F64x2Ge
            | walrus::ir::BinaryOp::V128And
            | walrus::ir::BinaryOp::V128Or
            | walrus::ir::BinaryOp::V128Xor
            | walrus::ir::BinaryOp::V128AndNot
            | walrus::ir::BinaryOp::I8x16Shl
            | walrus::ir::BinaryOp::I8x16Add
            | walrus::ir::BinaryOp::I8x16Sub
            | walrus::ir::BinaryOp::I16x8Shl
            | walrus::ir::BinaryOp::I16x8Add
            | walrus::ir::BinaryOp::I16x8Sub
            | walrus::ir::BinaryOp::I16x8Mul
            | walrus::ir::BinaryOp::I32x4Shl
            | walrus::ir::BinaryOp::I32x4Add
            | walrus::ir::BinaryOp::I32x4Sub
            | walrus::ir::BinaryOp::I32x4Mul
            | walrus::ir::BinaryOp::I64x2Shl
            | walrus::ir::BinaryOp::I64x2Add
            | walrus::ir::BinaryOp::I64x2Sub
            | walrus::ir::BinaryOp::I64x2Mul
            | walrus::ir::BinaryOp::F32x4Add
            | walrus::ir::BinaryOp::F32x4Sub
            | walrus::ir::BinaryOp::F32x4Mul
            | walrus::ir::BinaryOp::F32x4Div
            | walrus::ir::BinaryOp::F32x4Min
            | walrus::ir::BinaryOp::F32x4Max
            | walrus::ir::BinaryOp::F32x4PMin
            | walrus::ir::BinaryOp::F32x4PMax
            | walrus::ir::BinaryOp::F64x2Add
            | walrus::ir::BinaryOp::F64x2Sub
            | walrus::ir::BinaryOp::F64x2Mul
            | walrus::ir::BinaryOp::F64x2Div
            | walrus::ir::BinaryOp::F64x2Min
            | walrus::ir::BinaryOp::F64x2Max
            | walrus::ir::BinaryOp::F64x2PMin
            | walrus::ir::BinaryOp::F64x2PMax
            | walrus::ir::BinaryOp::F64x2Ne => Ok(BinopValues {
                return_value: Sign::bottom(),
                left_input: Sign::bottom(),
                right_input: Sign::bottom(),
            }),

            | walrus::ir::BinaryOp::I32LtU
            | walrus::ir::BinaryOp::I32GtU
            | walrus::ir::BinaryOp::I32LeU
            | walrus::ir::BinaryOp::I32GeU
            | walrus::ir::BinaryOp::I64LtU
            | walrus::ir::BinaryOp::I64LeU
            | walrus::ir::BinaryOp::I64GtU
            | walrus::ir::BinaryOp::I32DivU
            | walrus::ir::BinaryOp::I64GeU
            | walrus::ir::BinaryOp::I32RemU
            | walrus::ir::BinaryOp::I64DivU
            | walrus::ir::BinaryOp::I64RemU
            | walrus::ir::BinaryOp::I8x16LtU
            | walrus::ir::BinaryOp::I8x16GtU
            | walrus::ir::BinaryOp::I8x16LeU
            | walrus::ir::BinaryOp::I8x16GeU
            | walrus::ir::BinaryOp::I16x8LtU
            | walrus::ir::BinaryOp::I16x8GtU
            | walrus::ir::BinaryOp::I16x8LeU
            | walrus::ir::BinaryOp::I16x8GeU
            | walrus::ir::BinaryOp::I32x4LtU
            | walrus::ir::BinaryOp::I32x4GtU
            | walrus::ir::BinaryOp::I32x4LeU
            | walrus::ir::BinaryOp::I32x4GeU
            | walrus::ir::BinaryOp::I8x16AddSatU
            | walrus::ir::BinaryOp::I8x16SubSatU
            | walrus::ir::BinaryOp::I16x8AddSatU
            | walrus::ir::BinaryOp::I16x8SubSatU
            | walrus::ir::BinaryOp::I8x16NarrowI16x8U
            | walrus::ir::BinaryOp::I16x8NarrowI32x4U
            | walrus::ir::BinaryOp::I8x16AvgrU
            | walrus::ir::BinaryOp::I16x8AvgrU
            | walrus::ir::BinaryOp::I8x16MinU
            | walrus::ir::BinaryOp::I8x16MaxU
            | walrus::ir::BinaryOp::I16x8MinU
            | walrus::ir::BinaryOp::I16x8MaxU
            | walrus::ir::BinaryOp::I32x4MinU
            | walrus::ir::BinaryOp::I32x4MaxU
            | walrus::ir::BinaryOp::I16x8ExtMulLowI8x16U
            | walrus::ir::BinaryOp::I16x8ExtMulHighI8x16U
            | walrus::ir::BinaryOp::I32x4ExtMulLowI16x8U
            | walrus::ir::BinaryOp::I32x4ExtMulHighI16x8U
            | walrus::ir::BinaryOp::I64x2ExtMulLowI32x4U
            | walrus::ir::BinaryOp::I64x2ExtMulHighI32x4U => Ok(BinopValues {
                return_value: Sign::Unknown,
                left_input: Sign::Unsigned,
                right_input: Sign::Unsigned,
            }),


            | walrus::ir::BinaryOp::I64ShrU
            | walrus::ir::BinaryOp::I32ShrU
            | walrus::ir::BinaryOp::I8x16ShrU
            | walrus::ir::BinaryOp::I16x8ShrU
            | walrus::ir::BinaryOp::I32x4ShrU
            | walrus::ir::BinaryOp::I64x2ShrU => Ok(BinopValues {
                return_value: Sign::bottom(),
                left_input: Sign::Unsigned,
                right_input: Sign::bottom(),
            }),

            | walrus::ir::BinaryOp::I64x2ShrS
            | walrus::ir::BinaryOp::I32x4ShrS
            | walrus::ir::BinaryOp::I16x8ShrS
            | walrus::ir::BinaryOp::I32ShrS
            | walrus::ir::BinaryOp::I64ShrS
            | walrus::ir::BinaryOp::I8x16ShrS

            | walrus::ir::BinaryOp::I32LtS
            | walrus::ir::BinaryOp::I64x2ExtMulLowI32x4S
            | walrus::ir::BinaryOp::I64x2ExtMulHighI32x4S
            | walrus::ir::BinaryOp::I32x4ExtMulLowI16x8S
            | walrus::ir::BinaryOp::I32x4ExtMulHighI16x8S
            | walrus::ir::BinaryOp::I32x4DotI16x8S
            | walrus::ir::BinaryOp::I16x8Q15MulrSatS
            | walrus::ir::BinaryOp::I16x8ExtMulLowI8x16S
            | walrus::ir::BinaryOp::I16x8ExtMulHighI8x16S
            | walrus::ir::BinaryOp::I32x4MaxS
            | walrus::ir::BinaryOp::I32x4MinS
            | walrus::ir::BinaryOp::I16x8MaxS
            | walrus::ir::BinaryOp::I16x8MinS
            | walrus::ir::BinaryOp::I8x16MaxS
            | walrus::ir::BinaryOp::I8x16MinS
            | walrus::ir::BinaryOp::I16x8NarrowI32x4S
            | walrus::ir::BinaryOp::I8x16NarrowI16x8S
            | walrus::ir::BinaryOp::I16x8SubSatS
            | walrus::ir::BinaryOp::I16x8AddSatS
            | walrus::ir::BinaryOp::I8x16SubSatS
            | walrus::ir::BinaryOp::I8x16AddSatS
            | walrus::ir::BinaryOp::I32GtS
            | walrus::ir::BinaryOp::I32LeS
            | walrus::ir::BinaryOp::I32GeS
            | walrus::ir::BinaryOp::I64LtS
            | walrus::ir::BinaryOp::I64GtS
            | walrus::ir::BinaryOp::I64LeS
            | walrus::ir::BinaryOp::I64GeS
            | walrus::ir::BinaryOp::I32DivS
            | walrus::ir::BinaryOp::I32RemS
            | walrus::ir::BinaryOp::I64DivS
            | walrus::ir::BinaryOp::I64RemS
            | walrus::ir::BinaryOp::I8x16LtS
            | walrus::ir::BinaryOp::I8x16GtS
            | walrus::ir::BinaryOp::I8x16LeS
            | walrus::ir::BinaryOp::I8x16GeS
            | walrus::ir::BinaryOp::I16x8GtS
            | walrus::ir::BinaryOp::I16x8LtS
            | walrus::ir::BinaryOp::I16x8LeS
            | walrus::ir::BinaryOp::I16x8GeS
            | walrus::ir::BinaryOp::I32x4LtS
            | walrus::ir::BinaryOp::I32x4GtS
            | walrus::ir::BinaryOp::I32x4LeS
            | walrus::ir::BinaryOp::I32x4GeS
            | walrus::ir::BinaryOp::I64x2LtS
            | walrus::ir::BinaryOp::I64x2GtS
            | walrus::ir::BinaryOp::I64x2LeS
            | walrus::ir::BinaryOp::I64x2GeS => Ok(BinopValues {
                return_value: Sign::Unknown,
                left_input: Sign::Signed,
                right_input: Sign::Signed,
            }),
        }
    }
}

impl From<SignednessAnalysisError> for AnalysisError<SignednessAnalysisError> {
    fn from(value: SignednessAnalysisError) -> Self {
        AnalysisError::AnalyzerError(value)
    }
}
