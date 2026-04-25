use std::{collections::BTreeMap, ops::Index};

use walrus::{ir::{BinaryOp, Binop, Const, Value}, Function, FunctionId, FunctionKind, Global, GlobalId, InstrLocId, Local, LocalId, Module, TypeId, ValType};
use core::fmt::Debug;
use core::clone::Clone;
use core::cmp::PartialEq;
use core::cmp::Eq;

pub mod int_size;
pub mod signedness;
pub mod ir0;

#[derive(Debug)]
pub enum AnalysisError<T> {
    AnalyzerError(T),
    EmptyStack,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueState<T: Lattice> {
    id: Option<InstrLocId>,
    value_type: ValType,
    state: T,
}

impl<T: Lattice> ValueState<T> {
    pub fn new(id: InstrLocId, value_type: ValType, state: T) -> ValueState<T> {
        ValueState {
            id: Some(id),
            value_type,
            state,
        }
    }

    pub fn of(value_type: ValType, state: T) -> ValueState<T> {
        ValueState {
            id: None,
            value_type,
            state,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FunctionData<T: Lattice> {
    args: Vec<T>,
    return_values: Vec<T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InstructionData<T: Lattice> {
    args: Vec<T>,
    return_values: Vec<T>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AnalysisState<T: Lattice> {
    stack: Vec<ValueState<T>>,
    globals: BTreeMap<GlobalId, T>,
    locals: BTreeMap<LocalId, T>,
    functions: BTreeMap<FunctionId, FunctionData<T>>,
    instructions: BTreeMap<InstrLocId, InstructionData<T>>,
}

impl<T: Lattice> AnalysisState<T> {
    pub fn new() -> AnalysisState<T> {
        AnalysisState {
            stack: vec![],
            globals: BTreeMap::new(),
            locals: BTreeMap::new(),
            functions: BTreeMap::new(),
            instructions: BTreeMap::new(),
        }
    }

    pub fn pop<E>(&mut self) -> Result<ValueState<T>, AnalysisError<E>> {
        match self.stack.pop() {
            Some(value) => Ok(value),
            None => Err(AnalysisError::EmptyStack),
        }
    }

    pub fn set_local(&mut self, id: LocalId, value: T) {
        self.locals.insert(id, value);
    }

    pub fn get_local(&self, id: LocalId) -> T {
        if let Some(value) = self.locals.get(&id) {
            value.clone()
        } else {
            T::bottom()
        }
    }

    pub fn set_global(&mut self, id: GlobalId, value: T) {
        self.globals.insert(id, value);
    }

    pub fn get_global(&self, id: GlobalId) -> T {
        if let Some(value) = self.globals.get(&id) {
            value.clone()
        } else {
            T::bottom()
        }
    }
}

impl<T: Lattice> Index<LocalId> for AnalysisState<T> {
    type Output = T;

    fn index(&self, index: LocalId) -> &Self::Output {
        &self.locals[&index]
    }
}

pub fn analyze<T: Lattice, A: Analyzer<T>>(
    module: &Module,
    analyzer: &mut A,
) -> Result<AnalysisState<T>, AnalysisError<A::Error>>
where AnalysisError<A::Error>: From<A::Error> {
    let mut state: AnalysisState<T> = AnalysisState::new();

    for global in module.globals.iter() {
        let global_id = global.id();
        match global.kind {
            walrus::GlobalKind::Import(id) => {
                // TODO: Capture info from "known" external globals.
                state.globals.insert(global_id, T::bottom());
            },
            walrus::GlobalKind::Local(const_expr) => match const_expr {
                walrus::ConstExpr::Value(value) => {
                    let computed = analyzer.const_op(&Const { value: value.clone() })?;
                    state.globals.insert(global_id, computed);
                },
                _ => {
                    state.globals.insert(global_id, T::bottom());
                },
            },
        }
    }

    for function in module.functions() {
        let function_id = function.id();
        let mut args = vec![];
        let mut return_values = vec![];
        match &function.kind {
            FunctionKind::Import(imported_function) => {
                // TODO: Take a table of supported import functions, and use the type info from
                // that.
                let func_type = module.types.get(imported_function.ty);
                for _ in func_type.params() {
                    args.push(T::bottom());
                }
                for _ in func_type.results() {
                    return_values.push(T::bottom());
                }
            },
            FunctionKind::Local(local_function) => {
                let func_type = module.types.get(local_function.ty());
                for _ in func_type.params() {
                    args.push(T::bottom());
                }
                for _ in func_type.results() {
                    return_values.push(T::bottom());
                }
                for arg in &local_function.args {
                    state.locals.insert(*arg, T::bottom());
                }
            },
            FunctionKind::Uninitialized(id) => {
                let func_type = module.types.get(*id);
                for _ in func_type.params() {
                    args.push(T::bottom());
                }
                for _ in func_type.results() {
                    return_values.push(T::bottom());
                }
            },
        }
        state.functions.insert(function_id, FunctionData { args, return_values });
    }

    for function in module.functions() {
        if let FunctionKind::Local(body) = &function.kind {
            let entry_id = body.entry_block();
            for (instr, id) in body.block(entry_id).iter() {
                match instr {
                    walrus::ir::Instr::Block(_) => {},
                    walrus::ir::Instr::Loop(_) => {},
                    walrus::ir::Instr::Call(_) => {},
                    walrus::ir::Instr::CallIndirect(_) => {},
                    walrus::ir::Instr::LocalGet(local_get) => {
                        let local_id = local_get.local;
                        let local = module.locals.get(local_id);
                        let local_value = state.get_local(local_id);
                        state.stack.push(ValueState::new(
                            *id,
                            local.ty(),
                            local_value,
                        ));
                    },
                    walrus::ir::Instr::LocalSet(local_set) => {
                        let local_id = local_set.local;
                        let new_value = state.pop()?;
                        let local_value = state.get_local(local_id);
                        let computed = local_value.join(&new_value.state);
                        state.set_local(local_id, computed);
                    },
                    walrus::ir::Instr::LocalTee(local_id) => {},
                    walrus::ir::Instr::GlobalGet(global_get) => {
                        let global = module.globals.get(global_get.global);
                        let global_value = state.get_global(global_get.global);
                        state.stack.push(ValueState::new(
                            *id,
                            global.ty,
                            global_value,
                        ));
                    },
                    walrus::ir::Instr::GlobalSet(global_set) => {
                        let global_id = global_set.global;
                        let new_value = state.pop()?;
                        let global_value = state.get_global(global_id);
                        let computed = global_value.join(&new_value.state);
                        state.set_global(global_id, computed);
                    },
                    walrus::ir::Instr::Const(value) => {
                        let value = ValueState::new(
                            *id,
                            type_of(value.value),
                            analyzer.const_op(&value)?,
                        );
                        state.stack.push(value);
                    },
                    walrus::ir::Instr::Binop(op) => {
                        let right = state.pop()?;
                        let left = state.pop()?;
                        let computed = analyzer.binop(op, left.state, right.state)?;
                        state.stack.push(ValueState::new(
                            *id,
                            return_type(&op.op),
                            computed.return_value,
                        ));
                    },
                    walrus::ir::Instr::Unop(_) => {},
                    walrus::ir::Instr::Select(_) => {},
                    walrus::ir::Instr::Unreachable(_) => {},
                    walrus::ir::Instr::Br(_) => {},
                    walrus::ir::Instr::BrIf(_) => {},
                    walrus::ir::Instr::IfElse(_) => {},
                    walrus::ir::Instr::BrTable(_) => {},
                    walrus::ir::Instr::Drop(_) => {},
                    walrus::ir::Instr::Return(_) => {},
                    walrus::ir::Instr::MemorySize(_) => {},
                    walrus::ir::Instr::MemoryGrow(_) => {},
                    walrus::ir::Instr::MemoryInit(_) => {},
                    walrus::ir::Instr::DataDrop(_) => {},
                    walrus::ir::Instr::MemoryCopy(_) => {},
                    walrus::ir::Instr::MemoryFill(_) => {},
                    walrus::ir::Instr::Load(_) => {},
                    walrus::ir::Instr::Store(_) => {},
                    walrus::ir::Instr::AtomicRmw(_) => {},
                    walrus::ir::Instr::Cmpxchg(_) => {},
                    walrus::ir::Instr::AtomicNotify(_) => {},
                    walrus::ir::Instr::AtomicWait(_) => {},
                    walrus::ir::Instr::AtomicFence(_) => {},
                    walrus::ir::Instr::TableGet(_) => {},
                    walrus::ir::Instr::TableSet(_) => {},
                    walrus::ir::Instr::TableGrow(_) => {},
                    walrus::ir::Instr::TableSize(_) => {},
                    walrus::ir::Instr::TableFill(_) => {},
                    walrus::ir::Instr::RefNull(_) => {},
                    walrus::ir::Instr::RefIsNull(_) => {},
                    walrus::ir::Instr::RefFunc(_) => {},
                    walrus::ir::Instr::V128Bitselect(_) => {},
                    walrus::ir::Instr::I8x16Swizzle(_) => {},
                    walrus::ir::Instr::I8x16Shuffle(_) => {},
                    walrus::ir::Instr::LoadSimd(_) => {},
                    walrus::ir::Instr::TableInit(_) => {},
                    walrus::ir::Instr::ElemDrop(_) => {},
                    walrus::ir::Instr::TableCopy(_) => {},
                    walrus::ir::Instr::ReturnCall(_) => {},
                    walrus::ir::Instr::ReturnCallIndirect(_) => {},
                }
            }
        }
    }

    Ok(state)
}

fn return_type(binop: &BinaryOp) -> ValType {
    match binop {
        BinaryOp::I32Eq => ValType::I32,
        BinaryOp::I32Ne => ValType::I32,
        BinaryOp::I32LtS => ValType::I32,
        BinaryOp::I32LtU => ValType::I32,
        BinaryOp::I32GtS => ValType::I32,
        BinaryOp::I32GtU => ValType::I32,
        BinaryOp::I32LeS => ValType::I32,
        BinaryOp::I32LeU => ValType::I32,
        BinaryOp::I32GeS => ValType::I32,
        BinaryOp::I32GeU => ValType::I32,
        BinaryOp::I64Eq => ValType::I32,
        BinaryOp::I64Ne => ValType::I32,
        BinaryOp::I64LtS => ValType::I32,
        BinaryOp::I64LtU => ValType::I32,
        BinaryOp::I64GtS => ValType::I32,
        BinaryOp::I64GtU => ValType::I32,
        BinaryOp::I64LeS => ValType::I32,
        BinaryOp::I64LeU => ValType::I32,
        BinaryOp::I64GeS => ValType::I32,
        BinaryOp::I64GeU => ValType::I32,
        BinaryOp::F32Eq => ValType::I32,
        BinaryOp::F32Ne => ValType::I32,
        BinaryOp::F32Lt => ValType::I32,
        BinaryOp::F32Gt => ValType::I32,
        BinaryOp::F32Le => ValType::I32,
        BinaryOp::F32Ge => ValType::I32,
        BinaryOp::F64Eq => ValType::I32,
        BinaryOp::F64Ne => ValType::I32,
        BinaryOp::F64Lt => ValType::I32,
        BinaryOp::F64Gt => ValType::I32,
        BinaryOp::F64Le => ValType::I32,
        BinaryOp::F64Ge => ValType::I32,
        BinaryOp::I32Add => ValType::I32,
        BinaryOp::I32Sub => ValType::I32,
        BinaryOp::I32Mul => ValType::I32,
        BinaryOp::I32DivS => ValType::I32,
        BinaryOp::I32DivU => ValType::I32,
        BinaryOp::I32RemS => ValType::I32,
        BinaryOp::I32RemU => ValType::I32,
        BinaryOp::I32And => ValType::I32,
        BinaryOp::I32Or => ValType::I32,
        BinaryOp::I32Xor => ValType::I32,
        BinaryOp::I32Shl => ValType::I32,
        BinaryOp::I32ShrS => ValType::I32,
        BinaryOp::I32ShrU => ValType::I32,
        BinaryOp::I32Rotl => ValType::I32,
        BinaryOp::I32Rotr => ValType::I32,
        BinaryOp::I64Add => ValType::I64,
        BinaryOp::I64Sub => ValType::I64,
        BinaryOp::I64Mul => ValType::I64,
        BinaryOp::I64DivS => ValType::I64,
        BinaryOp::I64DivU => ValType::I64,
        BinaryOp::I64RemS => ValType::I64,
        BinaryOp::I64RemU => ValType::I64,
        BinaryOp::I64And => ValType::I64,
        BinaryOp::I64Or => ValType::I64,
        BinaryOp::I64Xor => ValType::I64,
        BinaryOp::I64Shl => ValType::I64,
        BinaryOp::I64ShrS => ValType::I64,
        BinaryOp::I64ShrU => ValType::I64,
        BinaryOp::I64Rotl => ValType::I64,
        BinaryOp::I64Rotr => ValType::I64,
        BinaryOp::F32Add => ValType::F32,
        BinaryOp::F32Sub => ValType::F32,
        BinaryOp::F32Mul => ValType::F32,
        BinaryOp::F32Div => ValType::F32,
        BinaryOp::F32Min => ValType::F32,
        BinaryOp::F32Max => ValType::F32,
        BinaryOp::F32Copysign => ValType::F32,
        BinaryOp::F64Add => ValType::I64,
        BinaryOp::F64Sub => ValType::I64,
        BinaryOp::F64Mul => ValType::I64,
        BinaryOp::F64Div => ValType::I64,
        BinaryOp::F64Min => ValType::I64,
        BinaryOp::F64Max => ValType::I64,
        BinaryOp::F64Copysign => ValType::I64,
        BinaryOp::I8x16ReplaceLane { .. } => ValType::V128,
        BinaryOp::I16x8ReplaceLane { .. } => ValType::V128,
        BinaryOp::I32x4ReplaceLane { .. } => ValType::V128,
        BinaryOp::I64x2ReplaceLane { .. } => ValType::V128,
        BinaryOp::F32x4ReplaceLane { .. } => ValType::V128,
        BinaryOp::F64x2ReplaceLane { .. } => ValType::V128,
        BinaryOp::I8x16Eq => ValType::V128,
        BinaryOp::I8x16Ne => ValType::V128,
        BinaryOp::I8x16LtS => ValType::V128,
        BinaryOp::I8x16LtU => ValType::V128,
        BinaryOp::I8x16GtS => ValType::V128,
        BinaryOp::I8x16GtU => ValType::V128,
        BinaryOp::I8x16LeS => ValType::V128,
        BinaryOp::I8x16LeU => ValType::V128,
        BinaryOp::I8x16GeS => ValType::V128,
        BinaryOp::I8x16GeU => ValType::V128,
        BinaryOp::I16x8Eq => ValType::V128,
        BinaryOp::I16x8Ne => ValType::V128,
        BinaryOp::I16x8LtS => ValType::V128,
        BinaryOp::I16x8LtU => ValType::V128,
        BinaryOp::I16x8GtS => ValType::V128,
        BinaryOp::I16x8GtU => ValType::V128,
        BinaryOp::I16x8LeS => ValType::V128,
        BinaryOp::I16x8LeU => ValType::V128,
        BinaryOp::I16x8GeS => ValType::V128,
        BinaryOp::I16x8GeU => ValType::V128,
        BinaryOp::I32x4Eq => ValType::V128,
        BinaryOp::I32x4Ne => ValType::V128,
        BinaryOp::I32x4LtS => ValType::V128,
        BinaryOp::I32x4LtU => ValType::V128,
        BinaryOp::I32x4GtS => ValType::V128,
        BinaryOp::I32x4GtU => ValType::V128,
        BinaryOp::I32x4LeS => ValType::V128,
        BinaryOp::I32x4LeU => ValType::V128,
        BinaryOp::I32x4GeS => ValType::V128,
        BinaryOp::I32x4GeU => ValType::V128,
        BinaryOp::I64x2Eq => ValType::V128,
        BinaryOp::I64x2Ne => ValType::V128,
        BinaryOp::I64x2LtS => ValType::V128,
        BinaryOp::I64x2GtS => ValType::V128,
        BinaryOp::I64x2LeS => ValType::V128,
        BinaryOp::I64x2GeS => ValType::V128,
        BinaryOp::F32x4Eq => ValType::V128,
        BinaryOp::F32x4Ne => ValType::V128,
        BinaryOp::F32x4Lt => ValType::V128,
        BinaryOp::F32x4Gt => ValType::V128,
        BinaryOp::F32x4Le => ValType::V128,
        BinaryOp::F32x4Ge => ValType::V128,
        BinaryOp::F64x2Eq => ValType::V128,
        BinaryOp::F64x2Ne => ValType::V128,
        BinaryOp::F64x2Lt => ValType::V128,
        BinaryOp::F64x2Gt => ValType::V128,
        BinaryOp::F64x2Le => ValType::V128,
        BinaryOp::F64x2Ge => ValType::V128,
        BinaryOp::V128And => ValType::V128,
        BinaryOp::V128Or => ValType::V128,
        BinaryOp::V128Xor => ValType::V128,
        BinaryOp::V128AndNot => ValType::V128,
        BinaryOp::I8x16Shl => ValType::V128,
        BinaryOp::I8x16ShrS => ValType::V128,
        BinaryOp::I8x16ShrU => ValType::V128,
        BinaryOp::I8x16Add => ValType::V128,
        BinaryOp::I8x16AddSatS => ValType::V128,
        BinaryOp::I8x16AddSatU => ValType::V128,
        BinaryOp::I8x16Sub => ValType::V128,
        BinaryOp::I8x16SubSatS => ValType::V128,
        BinaryOp::I8x16SubSatU => ValType::V128,
        BinaryOp::I16x8Shl => ValType::V128,
        BinaryOp::I16x8ShrS => ValType::V128,
        BinaryOp::I16x8ShrU => ValType::V128,
        BinaryOp::I16x8Add => ValType::V128,
        BinaryOp::I16x8AddSatS => ValType::V128,
        BinaryOp::I16x8AddSatU => ValType::V128,
        BinaryOp::I16x8Sub => ValType::V128,
        BinaryOp::I16x8SubSatS => ValType::V128,
        BinaryOp::I16x8SubSatU => ValType::V128,
        BinaryOp::I16x8Mul => ValType::V128,
        BinaryOp::I32x4Shl => ValType::V128,
        BinaryOp::I32x4ShrS => ValType::V128,
        BinaryOp::I32x4ShrU => ValType::V128,
        BinaryOp::I32x4Add => ValType::V128,
        BinaryOp::I32x4Sub => ValType::V128,
        BinaryOp::I32x4Mul => ValType::V128,
        BinaryOp::I64x2Shl => ValType::V128,
        BinaryOp::I64x2ShrS => ValType::V128,
        BinaryOp::I64x2ShrU => ValType::V128,
        BinaryOp::I64x2Add => ValType::V128,
        BinaryOp::I64x2Sub => ValType::V128,
        BinaryOp::I64x2Mul => ValType::V128,
        BinaryOp::F32x4Add => ValType::V128,
        BinaryOp::F32x4Sub => ValType::V128,
        BinaryOp::F32x4Mul => ValType::V128,
        BinaryOp::F32x4Div => ValType::V128,
        BinaryOp::F32x4Min => ValType::V128,
        BinaryOp::F32x4Max => ValType::V128,
        BinaryOp::F32x4PMin => ValType::V128,
        BinaryOp::F32x4PMax => ValType::V128,
        BinaryOp::F64x2Add => ValType::V128,
        BinaryOp::F64x2Sub => ValType::V128,
        BinaryOp::F64x2Mul => ValType::V128,
        BinaryOp::F64x2Div => ValType::V128,
        BinaryOp::F64x2Min => ValType::V128,
        BinaryOp::F64x2Max => ValType::V128,
        BinaryOp::F64x2PMin => ValType::V128,
        BinaryOp::F64x2PMax => ValType::V128,
        BinaryOp::I8x16NarrowI16x8S => ValType::V128,
        BinaryOp::I8x16NarrowI16x8U => ValType::V128,
        BinaryOp::I16x8NarrowI32x4S => ValType::V128,
        BinaryOp::I16x8NarrowI32x4U => ValType::V128,
        BinaryOp::I8x16AvgrU => ValType::V128,
        BinaryOp::I16x8AvgrU => ValType::V128,
        BinaryOp::I8x16MinS => ValType::V128,
        BinaryOp::I8x16MinU => ValType::V128,
        BinaryOp::I8x16MaxS => ValType::V128,
        BinaryOp::I8x16MaxU => ValType::V128,
        BinaryOp::I16x8MinS => ValType::V128,
        BinaryOp::I16x8MinU => ValType::V128,
        BinaryOp::I16x8MaxS => ValType::V128,
        BinaryOp::I16x8MaxU => ValType::V128,
        BinaryOp::I32x4MinS => ValType::V128,
        BinaryOp::I32x4MinU => ValType::V128,
        BinaryOp::I32x4MaxS => ValType::V128,
        BinaryOp::I32x4MaxU => ValType::V128,
        BinaryOp::I32x4DotI16x8S => ValType::V128,
        BinaryOp::I16x8Q15MulrSatS => ValType::V128,
        BinaryOp::I16x8ExtMulLowI8x16S => ValType::V128,
        BinaryOp::I16x8ExtMulHighI8x16S => ValType::V128,
        BinaryOp::I16x8ExtMulLowI8x16U => ValType::V128,
        BinaryOp::I16x8ExtMulHighI8x16U => ValType::V128,
        BinaryOp::I32x4ExtMulLowI16x8S => ValType::V128,
        BinaryOp::I32x4ExtMulHighI16x8S => ValType::V128,
        BinaryOp::I32x4ExtMulLowI16x8U => ValType::V128,
        BinaryOp::I32x4ExtMulHighI16x8U => ValType::V128,
        BinaryOp::I64x2ExtMulLowI32x4S => ValType::V128,
        BinaryOp::I64x2ExtMulHighI32x4S => ValType::V128,
        BinaryOp::I64x2ExtMulLowI32x4U => ValType::V128,
        BinaryOp::I64x2ExtMulHighI32x4U => ValType::V128,
    }
}

fn type_of(value: Value) -> ValType {
    match value {
        Value::I32(_) => ValType::I32,
        Value::I64(_) => ValType::I64,
        Value::F32(_) => ValType::F32,
        Value::F64(_) => ValType::F64,
        Value::V128(_) => ValType::V128,
    }
}

pub trait Lattice: Clone + Debug {
    fn bottom() -> Self;
    fn join(&self, other: &Self) -> Self;
}

// fn analyze_block<A: Analyzer>() -> Result<(), A::Error> {
// }

pub struct BinopValues<T: Lattice> {
    return_value: T,
    left_input: T,
    right_input: T,
}

pub trait Analyzer<T: Lattice> {
    type Error;

    fn const_op(&self, cnst: &Const) -> Result<T, Self::Error>;
    fn binop(&self, binop: &Binop, left: T, right: T) -> Result<BinopValues<T>, Self::Error>;
}
