use std::{collections::BTreeSet, convert::Infallible};

use walrus::{ValType, ir::Instr};

use super::{
    domain::AbstractDomain,
    forward::{ForwardAnalysis, ForwardResults, RootKind, TransferContext},
    index::{ProgramIndex, ValueId},
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

/// Recovers the WebAssembly type of a value whose instruction does not carry
/// one explicitly, such as the result of an untyped `select`.
pub(crate) fn value_type(
    index: &ProgramIndex,
    flow: &ForwardResults<ValueSet>,
    value: ValueId,
) -> Option<ValType> {
    index.values[value.0]
        .ty
        .or_else(|| source_type(index, flow, &flow.values[value.0]))
}

/// Finds the common WebAssembly type of the values represented by `origins`.
/// A well-typed module cannot merge different types, but returning `None` for
/// an inconsistent set keeps malformed programmatic Walrus modules harmless.
pub(crate) fn source_type(
    index: &ProgramIndex,
    flow: &ForwardResults<ValueSet>,
    origins: &ValueSet,
) -> Option<ValType> {
    let mut pending = origins.values.iter().copied().collect::<Vec<_>>();
    let mut visited = BTreeSet::new();
    let mut common = None;

    while let Some(value) = pending.pop() {
        if !visited.insert(value) {
            continue;
        }
        if let Some(candidate) = index.values[value.0].ty {
            match common {
                None => common = Some(candidate),
                Some(existing) if existing == candidate => {}
                Some(_) => return None,
            }
        } else {
            pending.extend(flow.values[value.0].values.iter().copied());
        }
    }

    common
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
