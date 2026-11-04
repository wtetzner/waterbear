use std::{collections::BTreeSet, convert::Infallible};

use walrus::{
    ValType,
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
    pub unknown: bool,
}

impl PointerOrigins {
    fn parameter(value: ValueId) -> Self {
        Self {
            parameters: [value].into_iter().collect(),
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
            unknown: true,
        }
    }

    fn is_bottom(&self) -> bool {
        !self.unknown && self.parameters.is_empty()
    }

    fn join_assign(&mut self, other: &Self) -> bool {
        let old_len = self.parameters.len();
        let old_unknown = self.unknown;
        self.parameters.extend(other.parameters.iter().copied());
        self.unknown |= other.unknown;
        old_len != self.parameters.len() || old_unknown != self.unknown
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
        _cx: &TransferContext<'_>,
        kind: RootKind,
        value: ValueId,
        _default: Self::Fact,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(match kind {
            RootKind::FunctionParameter => PointerOrigins::parameter(value),
            RootKind::LocalInitial | RootKind::GlobalInitial => PointerOrigins::default(),
        })
    }

    fn instruction(
        &mut self,
        _cx: &TransferContext<'_>,
        instruction: &Instr,
        inputs: &[Self::Fact],
        output_types: &[Option<ValType>],
        _call: Option<&super::calls::CallSummary>,
    ) -> Result<Vec<Self::Fact>, Self::Error> {
        let result = match instruction {
            Instr::Binop(operation)
                if matches!(operation.op, BinaryOp::I32Add | BinaryOp::I64Add) =>
            {
                let mut origins = inputs[0].clone();
                origins.join_assign(&inputs[1]);
                origins
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
}
