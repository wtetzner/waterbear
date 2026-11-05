use std::{collections::BTreeSet, convert::Infallible};

use walrus::{
    ConstExpr, DataKind, ValType,
    ir::{BinaryOp, Instr},
};

use super::{
    domain::AbstractDomain,
    forward::{ForwardAnalysis, RootKind, TransferContext},
    index::ValueId,
};

/// Function parameters whose pointer identity can reach a value.
///
/// This is deliberately provenance, not general data dependence. Reading
/// through a pointer consumes its provenance; the loaded data does not inherit
/// it. That distinction lets call summaries tell a returned pointer from a
/// returned value computed from pointed-to data.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PointerOrigins {
    pub parameters: BTreeSet<ValueId>,
    /// The value is derived from an address inside an active data segment.
    pub static_data: bool,
    /// The value is derived from the compiler-generated shadow-stack pointer.
    pub shadow_stack: bool,
    pub unknown: bool,
}

impl PointerOrigins {
    fn parameter(value: ValueId) -> Self {
        Self {
            parameters: [value].into_iter().collect(),
            static_data: false,
            shadow_stack: false,
            unknown: false,
        }
    }
}

impl AbstractDomain for PointerOrigins {
    fn bottom(_ty: Option<ValType>) -> Self {
        Self::default()
    }

    fn top(_ty: ValType) -> Self {
        Self {
            parameters: BTreeSet::new(),
            static_data: false,
            shadow_stack: false,
            unknown: true,
        }
    }

    fn is_bottom(&self) -> bool {
        !self.unknown && !self.static_data && !self.shadow_stack && self.parameters.is_empty()
    }

    fn join_assign(&mut self, other: &Self) -> bool {
        let old_len = self.parameters.len();
        let old_static_data = self.static_data;
        let old_shadow_stack = self.shadow_stack;
        let old_unknown = self.unknown;
        self.parameters.extend(other.parameters.iter().copied());
        self.static_data |= other.static_data;
        self.shadow_stack |= other.shadow_stack;
        self.unknown |= other.unknown;
        old_len != self.parameters.len()
            || old_static_data != self.static_data
            || old_shadow_stack != self.shadow_stack
            || old_unknown != self.unknown
    }
}

/// Tracks pointer identity through copies, control joins, and pointer offsets.
#[derive(Clone, Copy, Debug, Default)]
pub struct PointerOriginAnalysis;

impl ForwardAnalysis for PointerOriginAnalysis {
    type Fact = PointerOrigins;
    type Error = Infallible;

    fn root(
        &mut self,
        cx: &TransferContext<'_>,
        kind: RootKind,
        value: ValueId,
        _default: Self::Fact,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(match kind {
            RootKind::FunctionParameter => PointerOrigins::parameter(value),
            RootKind::GlobalInitial
                if matches!(
                    cx.index.values[value.0].producer,
                    super::index::ValueProducer::GlobalInitial(global)
                        if Some(global) == super::address::find_stack_pointer(cx.module)
                ) =>
            {
                PointerOrigins {
                    shadow_stack: true,
                    ..PointerOrigins::default()
                }
            }
            RootKind::LocalInitial | RootKind::GlobalInitial => PointerOrigins::default(),
        })
    }

    fn constant(
        &mut self,
        cx: &TransferContext<'_>,
        value: walrus::ir::Value,
    ) -> Result<Self::Fact, Self::Error> {
        let address = match value {
            walrus::ir::Value::I32(value) => u64::from(value as u32),
            walrus::ir::Value::I64(value) => value as u64,
            _ => return Ok(PointerOrigins::default()),
        };
        Ok(PointerOrigins {
            static_data: is_active_data_address(cx.module, address),
            ..PointerOrigins::default()
        })
    }

    fn instruction(
        &mut self,
        cx: &TransferContext<'_>,
        instruction: &Instr,
        inputs: &[Self::Fact],
        output_types: &[Option<ValType>],
        call: Option<&super::calls::CallSummary>,
    ) -> Result<Vec<Self::Fact>, Self::Error> {
        if matches!(instruction, Instr::Call(_) | Instr::CallIndirect(_)) {
            return self.call(cx, call.expect("call summary"), inputs);
        }
        let result = match instruction {
            Instr::Binop(operation)
                if matches!(operation.op, BinaryOp::I32Add | BinaryOp::I64Add) =>
            {
                if inputs[0].is_bottom() {
                    inputs[1].clone()
                } else {
                    inputs[0].clone()
                }
            }
            Instr::Binop(operation)
                if matches!(operation.op, BinaryOp::I32Sub | BinaryOp::I64Sub) =>
            {
                inputs[0].clone()
            }
            _ => PointerOrigins::default(),
        };

        Ok(output_types.iter().map(|_| result.clone()).collect())
    }

    fn call(
        &mut self,
        _cx: &TransferContext<'_>,
        summary: &super::calls::CallSummary,
        arguments: &[Self::Fact],
    ) -> Result<Vec<Self::Fact>, Self::Error> {
        Ok(summary
            .result_pointers
            .iter()
            .map(|result| {
                let mut origins = PointerOrigins {
                    static_data: result.static_data,
                    shadow_stack: result.shadow_stack,
                    unknown: result.unknown,
                    ..PointerOrigins::default()
                };
                for &parameter in &result.parameters {
                    if let Some(argument) = arguments.get(parameter) {
                        origins.join_assign(argument);
                    }
                }
                origins
            })
            .collect())
    }
}

fn is_active_data_address(module: &walrus::Module, address: u64) -> bool {
    module.data.iter().any(|data| {
        let DataKind::Active { offset, .. } = data.kind else {
            return false;
        };
        let Some(start) = const_address(offset) else {
            return false;
        };
        let end = start.saturating_add(data.value.len() as u64);
        start <= address && address < end
    })
}

fn const_address(expression: ConstExpr) -> Option<u64> {
    match expression {
        ConstExpr::Value(walrus::ir::Value::I32(value)) => Some(u64::from(value as u32)),
        ConstExpr::Value(walrus::ir::Value::I64(value)) => Some(value as u64),
        _ => None,
    }
}
