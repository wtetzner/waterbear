use walrus::{
    LocalFunction, Module, RefType, ValType,
    ir::{AtomicWidth, BinaryOp, Instr, InstrSeqId, InstrSeqType, LoadKind, LoadSimdKind, UnaryOp},
};

#[derive(Clone, Debug)]
pub struct InstructionShape {
    pub inputs: usize,
    pub outputs: Vec<Option<ValType>>,
}

impl InstructionShape {
    fn new(inputs: usize, outputs: impl IntoIterator<Item = ValType>) -> Self {
        Self {
            inputs,
            outputs: outputs.into_iter().map(Some).collect(),
        }
    }

    fn unknown_output(inputs: usize) -> Self {
        Self {
            inputs,
            outputs: vec![None],
        }
    }

    fn none(inputs: usize) -> Self {
        Self {
            inputs,
            outputs: Vec::new(),
        }
    }
}

pub fn sequence_signature(
    module: &Module,
    function: &LocalFunction,
    sequence: InstrSeqId,
) -> (Vec<ValType>, Vec<ValType>) {
    match function.block(sequence).ty {
        InstrSeqType::Simple(result) => (Vec::new(), result.into_iter().collect()),
        InstrSeqType::MultiValue(ty) => {
            let (parameters, results) = module.types.params_results(ty);
            (parameters.to_vec(), results.to_vec())
        }
    }
}

pub fn instruction_shape(
    module: &Module,
    function: &LocalFunction,
    instruction: &Instr,
) -> InstructionShape {
    use Instr::*;

    match instruction {
        Block(op) => {
            let (parameters, results) = sequence_signature(module, function, op.seq);
            InstructionShape::new(parameters.len(), results)
        }
        Loop(op) => {
            let (parameters, results) = sequence_signature(module, function, op.seq);
            InstructionShape::new(parameters.len(), results)
        }
        IfElse(op) => {
            let (parameters, results) = sequence_signature(module, function, op.consequent);
            InstructionShape::new(parameters.len() + 1, results)
        }
        Call(op) => {
            let ty = module.funcs.get(op.func).ty();
            InstructionShape::new(
                module.types.params(ty).len(),
                module.types.results(ty).to_vec(),
            )
        }
        CallIndirect(op) => InstructionShape::new(
            module.types.params(op.ty).len() + 1,
            module.types.results(op.ty).to_vec(),
        ),
        LocalGet(op) => InstructionShape::new(0, [module.locals.get(op.local).ty()]),
        LocalSet(_) => InstructionShape::none(1),
        LocalTee(op) => InstructionShape::new(1, [module.locals.get(op.local).ty()]),
        GlobalGet(op) => InstructionShape::new(0, [module.globals.get(op.global).ty]),
        GlobalSet(_) => InstructionShape::none(1),
        Const(op) => InstructionShape::new(0, [value_type(op.value)]),
        Binop(op) => InstructionShape::new(2, [binary_result_type(op.op)]),
        Unop(op) => InstructionShape::new(1, [unary_result_type(op.op)]),
        Select(op) => match op.ty {
            Some(ty) => InstructionShape::new(3, [ty]),
            None => InstructionShape::unknown_output(3),
        },
        Unreachable(_) | Br(_) | BrTable(_) | Return(_) => InstructionShape::none(0),
        BrIf(_) => InstructionShape::none(1),
        Drop(_) => InstructionShape::none(1),
        MemorySize(op) => InstructionShape::new(0, [memory_index_type(module, op.memory)]),
        MemoryGrow(op) => InstructionShape::new(1, [memory_index_type(module, op.memory)]),
        MemoryInit(_) | MemoryCopy(_) | MemoryFill(_) => InstructionShape::none(3),
        DataDrop(_) | AtomicFence(_) | ElemDrop(_) => InstructionShape::none(0),
        Load(op) => InstructionShape::new(1, [load_result_type(op.kind)]),
        Store(_) => InstructionShape::none(2),
        AtomicRmw(op) => InstructionShape::new(2, [atomic_result_type(op.width)]),
        Cmpxchg(op) => InstructionShape::new(3, [atomic_result_type(op.width)]),
        AtomicNotify(_) => InstructionShape::new(2, [ValType::I32]),
        AtomicWait(_) => InstructionShape::new(3, [ValType::I32]),
        TableGet(op) => {
            InstructionShape::new(1, [ValType::Ref(module.tables.get(op.table).element_ty)])
        }
        TableSet(_) => InstructionShape::none(2),
        TableGrow(_) => InstructionShape::new(2, [ValType::I32]),
        TableSize(_) => InstructionShape::new(0, [ValType::I32]),
        TableFill(_) | TableInit(_) | TableCopy(_) => InstructionShape::none(3),
        RefNull(op) => InstructionShape::new(0, [ValType::Ref(op.ty)]),
        RefIsNull(_) => InstructionShape::new(1, [ValType::I32]),
        RefFunc(_) => InstructionShape::new(0, [ValType::Ref(RefType::Funcref)]),
        V128Bitselect(_) => InstructionShape::new(3, [ValType::V128]),
        I8x16Swizzle(_) | I8x16Shuffle(_) => InstructionShape::new(2, [ValType::V128]),
        LoadSimd(op) => match op.kind {
            LoadSimdKind::V128Load8Lane(_)
            | LoadSimdKind::V128Load16Lane(_)
            | LoadSimdKind::V128Load32Lane(_)
            | LoadSimdKind::V128Load64Lane(_) => InstructionShape::new(2, [ValType::V128]),
            LoadSimdKind::V128Store8Lane(_)
            | LoadSimdKind::V128Store16Lane(_)
            | LoadSimdKind::V128Store32Lane(_)
            | LoadSimdKind::V128Store64Lane(_) => InstructionShape::none(2),
            _ => InstructionShape::new(1, [ValType::V128]),
        },
        ReturnCall(op) => {
            let ty = module.funcs.get(op.func).ty();
            InstructionShape::none(module.types.params(ty).len())
        }
        ReturnCallIndirect(op) => InstructionShape::none(module.types.params(op.ty).len() + 1),
    }
}

pub fn value_type(value: walrus::ir::Value) -> ValType {
    match value {
        walrus::ir::Value::I32(_) => ValType::I32,
        walrus::ir::Value::I64(_) => ValType::I64,
        walrus::ir::Value::F32(_) => ValType::F32,
        walrus::ir::Value::F64(_) => ValType::F64,
        walrus::ir::Value::V128(_) => ValType::V128,
    }
}

pub fn zero_value(ty: ValType) -> Option<walrus::ir::Value> {
    match ty {
        ValType::I32 => Some(walrus::ir::Value::I32(0)),
        ValType::I64 => Some(walrus::ir::Value::I64(0)),
        ValType::F32 => Some(walrus::ir::Value::F32(0.0)),
        ValType::F64 => Some(walrus::ir::Value::F64(0.0)),
        ValType::V128 => Some(walrus::ir::Value::V128(0)),
        ValType::Ref(_) => None,
    }
}

pub fn load_result_type(kind: LoadKind) -> ValType {
    use LoadKind::*;
    match kind {
        I32 { .. } | I32_8 { .. } | I32_16 { .. } => ValType::I32,
        I64 { .. } | I64_8 { .. } | I64_16 { .. } | I64_32 { .. } => ValType::I64,
        F32 => ValType::F32,
        F64 => ValType::F64,
        V128 => ValType::V128,
    }
}

fn memory_index_type(module: &Module, memory: walrus::MemoryId) -> ValType {
    if module.memories.get(memory).memory64 {
        ValType::I64
    } else {
        ValType::I32
    }
}

fn atomic_result_type(width: AtomicWidth) -> ValType {
    match width {
        AtomicWidth::I32 | AtomicWidth::I32_8 | AtomicWidth::I32_16 => ValType::I32,
        AtomicWidth::I64 | AtomicWidth::I64_8 | AtomicWidth::I64_16 | AtomicWidth::I64_32 => {
            ValType::I64
        }
    }
}

fn binary_result_type(op: BinaryOp) -> ValType {
    use BinaryOp::*;
    match op {
        I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU
        | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS | I64GeU
        | F32Eq | F32Ne | F32Lt | F32Gt | F32Le | F32Ge | F64Eq | F64Ne | F64Lt | F64Gt | F64Le
        | F64Ge => ValType::I32,

        I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or
        | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Rotl | I32Rotr => ValType::I32,

        I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or
        | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotl | I64Rotr => ValType::I64,

        F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32Copysign => ValType::F32,
        F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64Copysign => ValType::F64,
        _ => ValType::V128,
    }
}

fn unary_result_type(op: UnaryOp) -> ValType {
    use UnaryOp::*;
    match op {
        I32Eqz | I64Eqz | V128AnyTrue | I8x16AllTrue | I8x16Bitmask | I16x8AllTrue
        | I16x8Bitmask | I32x4AllTrue | I32x4Bitmask | I64x2AllTrue | I64x2Bitmask => ValType::I32,

        I32Clz
        | I32Ctz
        | I32Popcnt
        | I32WrapI64
        | I32TruncSF32
        | I32TruncUF32
        | I32TruncSF64
        | I32TruncUF64
        | I32ReinterpretF32
        | I32Extend8S
        | I32Extend16S
        | I32TruncSSatF32
        | I32TruncUSatF32
        | I32TruncSSatF64
        | I32TruncUSatF64
        | I8x16ExtractLaneS { .. }
        | I8x16ExtractLaneU { .. }
        | I16x8ExtractLaneS { .. }
        | I16x8ExtractLaneU { .. }
        | I32x4ExtractLane { .. } => ValType::I32,

        I64Clz
        | I64Ctz
        | I64Popcnt
        | I64ExtendSI32
        | I64ExtendUI32
        | I64TruncSF32
        | I64TruncUF32
        | I64TruncSF64
        | I64TruncUF64
        | I64ReinterpretF64
        | I64Extend8S
        | I64Extend16S
        | I64Extend32S
        | I64TruncSSatF32
        | I64TruncUSatF32
        | I64TruncSSatF64
        | I64TruncUSatF64
        | I64x2ExtractLane { .. } => ValType::I64,

        F32Abs
        | F32Neg
        | F32Ceil
        | F32Floor
        | F32Trunc
        | F32Nearest
        | F32Sqrt
        | F32ConvertSI32
        | F32ConvertUI32
        | F32ConvertSI64
        | F32ConvertUI64
        | F32DemoteF64
        | F32ReinterpretI32
        | F32x4ExtractLane { .. } => ValType::F32,

        F64Abs
        | F64Neg
        | F64Ceil
        | F64Floor
        | F64Trunc
        | F64Nearest
        | F64Sqrt
        | F64ConvertSI32
        | F64ConvertUI32
        | F64ConvertSI64
        | F64ConvertUI64
        | F64PromoteF32
        | F64ReinterpretI64
        | F64x2ExtractLane { .. } => ValType::F64,

        _ => ValType::V128,
    }
}
