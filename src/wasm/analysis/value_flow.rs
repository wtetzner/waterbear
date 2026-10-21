use std::{collections::BTreeSet, convert::Infallible};

use walrus::{ValType, ir::Instr};

use super::{
    domain::AbstractDomain,
    forward::{ForwardAnalysis, RootKind, TransferContext},
    index::ValueId,
};

/// The static value-production sites that can reach a program point.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ValueSet {
    pub values: BTreeSet<ValueId>,
    pub unknown: bool,
}

impl ValueSet {
    pub fn singleton(value: ValueId) -> Self {
        Self {
            values: [value].into_iter().collect(),
            unknown: false,
        }
    }
}

impl AbstractDomain for ValueSet {
    fn bottom(_ty: Option<ValType>) -> Self {
        Self::default()
    }

    fn top(_ty: ValType) -> Self {
        Self {
            values: BTreeSet::new(),
            unknown: true,
        }
    }

    fn is_bottom(&self) -> bool {
        !self.unknown && self.values.is_empty()
    }

    fn join_assign(&mut self, other: &Self) -> bool {
        let old_len = self.values.len();
        let old_unknown = self.unknown;
        self.values.extend(other.values.iter().copied());
        self.unknown |= other.unknown;
        old_len != self.values.len() || old_unknown != self.unknown
    }
}

/// Records value flow without assigning any target-level meaning to values.
#[derive(Clone, Copy, Debug, Default)]
pub struct ValueFlowAnalysis;

impl ForwardAnalysis for ValueFlowAnalysis {
    type Fact = ValueSet;
    type Error = Infallible;

    fn root(
        &mut self,
        _cx: &TransferContext<'_>,
        _kind: RootKind,
        value: ValueId,
        _default: Self::Fact,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(ValueSet::singleton(value))
    }

    fn instruction(
        &mut self,
        _cx: &TransferContext<'_>,
        _instruction: &Instr,
        _inputs: &[Self::Fact],
        output_types: &[Option<ValType>],
        _call: Option<&super::calls::CallSummary>,
    ) -> Result<Vec<Self::Fact>, Self::Error> {
        Ok(output_types.iter().map(|_| ValueSet::default()).collect())
    }

    fn materialize(
        &mut self,
        _cx: &TransferContext<'_>,
        value: ValueId,
        _fact: Self::Fact,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(ValueSet::singleton(value))
    }
}
