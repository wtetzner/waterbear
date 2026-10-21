use walrus::ValType;

/// A finite-height abstraction of a set of WebAssembly values.
///
/// `bottom` represents no possible value, while `top` represents every value
/// of the requested WebAssembly type. Analyses are expected to move upward in
/// the lattice as control-flow paths are joined.
pub trait AbstractDomain: Clone + Eq {
    fn bottom(ty: Option<ValType>) -> Self;
    fn top(ty: ValType) -> Self;
    fn is_bottom(&self) -> bool;

    /// Joins `other` into this value and reports whether anything changed.
    fn join_assign(&mut self, other: &Self) -> bool;

    /// Accelerates convergence for domains with infinite ascending chains.
    fn widen_assign(&mut self, other: &Self) -> bool {
        self.join_assign(other)
    }
}
