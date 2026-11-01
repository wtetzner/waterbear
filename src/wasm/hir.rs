//! Tree-shaped high-level IR for the VMU backend.
//!
//! Calls and unrecovered memory accesses are represented by statements.
//! Expressions compute values and may read recovered places; evaluating an
//! expression can still trap.

use std::num::{NonZeroU8, NonZeroU32};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub main: Function,
    pub interrupt_handlers: InterruptHandlers,
    pub functions: Vec<Function>,
    pub statics: Vec<Static>,
}

/// The functions installed in the VMU's fixed interrupt-vector slots.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InterruptHandlers {
    pub external_0: Option<Function>,
    pub external_1: Option<Function>,
    pub external_2_or_timer_0_low: Option<Function>,
    pub external_3_or_base_timer: Option<Function>,
    pub timer_0_high: Option<Function>,
    pub timer_1: Option<Function>,
    pub serial_0: Option<Function>,
    pub serial_1: Option<Function>,
    pub dreamcast_interface: Option<Function>,
    pub port_3: Option<Function>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub result: Type,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    /// Statements execute in source order.
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    /// Introduces a variable in the current lexical scope without deciding
    /// where, or whether, it will be stored.
    Declare {
        name: String,
        ty: Type,
        mutable: bool,
        initializer: Option<Expression>,
    },
    Assign {
        /// The destination is evaluated before the value.
        destination: Place,
        value: Expression,
    },
    /// Reads from an address that could not be recovered as a `Place`.
    ///
    /// The address is evaluated and read before the destination is evaluated
    /// and assigned. Lowering uses a temporary when the source program
    /// requires a different order.
    Load {
        destination: Option<Place>,
        address: Expression,
        ty: Type,
        access: MemoryAccess,
    },
    /// Writes through an address that could not be recovered as a `Place`.
    /// The address is evaluated before the value, then the write occurs.
    Store {
        address: Expression,
        value: Expression,
        access: MemoryAccess,
    },
    /// Evaluates arguments from left to right, performs the call, then
    /// evaluates and assigns the destination.
    Call {
        destination: Option<Place>,
        function: String,
        arguments: Vec<Expression>,
    },
    /// Evaluates arguments from left to right, performs the intrinsic, then
    /// evaluates and assigns the destination.
    Intrinsic {
        destination: Option<Place>,
        operation: Intrinsic,
        arguments: Vec<Expression>,
    },
    Block {
        label: Option<String>,
        body: Block,
    },
    If {
        condition: Expression,
        then_body: Block,
        else_body: Option<Block>,
    },
    Loop {
        label: Option<String>,
        body: Block,
    },
    Switch {
        value: Expression,
        cases: Vec<SwitchCase>,
        default: Block,
    },
    Break {
        label: Option<String>,
    },
    Continue {
        label: Option<String>,
    },
    Return(Option<Expression>),
    Trap,
    Unreachable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SwitchCase {
    pub value: IntegerConstant,
    pub body: Block,
}

/// A value expression together with the smallest type proven by analysis.
///
/// Subexpressions are evaluated eagerly in the order their fields appear, and
/// vector elements are evaluated from left to right. The variants are arranged
/// so that this is also WebAssembly's stack-machine evaluation order.
/// Expressions contain no calls or unrecovered memory accesses, but evaluating
/// one can still trap.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    pub ty: Type,
    pub kind: ExpressionKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    Constant(Constant),
    /// Reads a recovered local, static, or subobject place.
    Read(Place),
    IntegerUnary {
        op: IntegerUnaryOp,
        operand: Box<Expression>,
    },
    IntegerBinary {
        op: IntegerBinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    FloatUnary {
        op: FloatUnaryOp,
        operand: Box<Expression>,
    },
    FloatBinary {
        op: FloatBinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Compare {
        op: CompareOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Convert {
        op: ConversionOp,
        operand: Box<Expression>,
    },
    Select {
        when_true: Box<Expression>,
        when_false: Box<Expression>,
        condition: Box<Expression>,
    },
    AddressOf(Place),
    PointerOffset {
        base: Box<Expression>,
        offset: Box<Expression>,
    },
    Aggregate(Vec<Expression>),
    ExtractElement {
        aggregate: Box<Expression>,
        index: usize,
    },
    InsertElement {
        aggregate: Box<Expression>,
        index: usize,
        value: Box<Expression>,
    },
}

/// The name of an operation supplied and validated by the VMU compiler.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Intrinsic(pub String);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerUnaryOp {
    Negate,
    Not,
    CountLeadingZeros,
    CountTrailingZeros,
    PopulationCount,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide(Signedness),
    Remainder(Signedness),
    And,
    Or,
    Xor,
    ShiftLeft,
    ShiftRight(Signedness),
    RotateLeft,
    RotateRight,
    SaturatingAdd(Signedness),
    SaturatingSubtract(Signedness),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatUnaryOp {
    AbsoluteValue,
    Negate,
    SquareRoot,
    Ceiling,
    Floor,
    Truncate,
    Nearest,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Minimum,
    Maximum,
    CopySign,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompareOp {
    Equal,
    NotEqual,
    LessThan(Signedness),
    LessThanOrEqual(Signedness),
    GreaterThan(Signedness),
    GreaterThanOrEqual(Signedness),
    FloatEqual,
    FloatNotEqual,
    FloatLessThan,
    FloatLessThanOrEqual,
    FloatGreaterThan,
    FloatGreaterThanOrEqual,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConversionOp {
    Truncate,
    ZeroExtend,
    SignExtend,
    IntegerToFloat(Signedness),
    FloatToInteger {
        signedness: Signedness,
        saturating: bool,
    },
    PromoteFloat,
    DemoteFloat,
    Reinterpret,
    BooleanToInteger,
    IntegerToBoolean,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Static {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
    pub initializer: StaticInitializer,
    pub alignment: Alignment,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StaticInitializer {
    Zeroed,
    Constant(Constant),
    Bytes(Vec<u8>),
    Aggregate(Vec<StaticInitializer>),
}

/// A recovered storage location.
///
/// A place's base is evaluated before its element or byte-offset expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Place {
    Variable(String),
    Static(String),
    Field {
        base: Box<Place>,
        index: usize,
    },
    Element {
        base: Box<Place>,
        index: Box<Expression>,
    },
    ByteOffset {
        base: Box<Place>,
        offset: Box<Expression>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Alignment(NonZeroU32);

impl Alignment {
    pub fn new(bytes: u32) -> Option<Self> {
        NonZeroU32::new(bytes)
            .filter(|bytes| bytes.get().is_power_of_two())
            .map(Self)
    }

    pub fn get(self) -> u32 {
        self.0.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MemoryAccess {
    pub alignment: Alignment,
    pub volatile: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Bits(BitWidth),
    Float(FloatWidth),
    /// An address whose physical VMU region has not yet been selected.
    Pointer,
    Array {
        element: Box<Type>,
        length: usize,
    },
    Struct(Vec<Type>),
    Tuple(Vec<Type>),
    OpaqueBytes(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BitWidth(NonZeroU8);

impl BitWidth {
    pub const MAX: u8 = 128;

    pub fn new(bits: u8) -> Option<Self> {
        if bits <= Self::MAX {
            NonZeroU8::new(bits).map(Self)
        } else {
            None
        }
    }

    pub fn get(self) -> u8 {
        self.0.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatWidth {
    Bits32,
    Bits64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constant {
    Bool(bool),
    Integer(IntegerConstant),
    Float32(u32),
    Float64(u64),
    NullPointer,
    StaticAddress { name: String, offset: usize },
    FunctionAddress(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IntegerConstant {
    width: BitWidth,
    bits: u128,
}

impl IntegerConstant {
    pub fn new(width: BitWidth, bits: u128) -> Option<Self> {
        let fits = width.get() == BitWidth::MAX || bits < (1_u128 << width.get());
        fits.then_some(Self { width, bits })
    }

    pub fn width(self) -> BitWidth {
        self.width
    }

    pub fn bits(self) -> u128 {
        self.bits
    }
}

#[cfg(test)]
mod tests {
    use super::{Alignment, BitWidth, IntegerConstant};

    #[test]
    fn alignment_accepts_only_nonzero_powers_of_two() {
        assert_eq!(Alignment::new(0), None);
        assert_eq!(Alignment::new(1).map(Alignment::get), Some(1));
        assert_eq!(Alignment::new(8).map(Alignment::get), Some(8));
        assert_eq!(Alignment::new(12), None);
    }

    #[test]
    fn bit_width_accepts_only_representable_integer_widths() {
        assert_eq!(BitWidth::new(0), None);
        assert_eq!(BitWidth::new(1).map(BitWidth::get), Some(1));
        assert_eq!(BitWidth::new(128).map(BitWidth::get), Some(128));
        assert_eq!(BitWidth::new(129), None);
    }

    #[test]
    fn integer_constant_must_fit_its_width() {
        let bits_8 = BitWidth::new(8).unwrap();
        assert_eq!(
            IntegerConstant::new(bits_8, 255).map(IntegerConstant::bits),
            Some(255)
        );
        assert_eq!(IntegerConstant::new(bits_8, 256), None);

        let bits_128 = BitWidth::new(128).unwrap();
        assert_eq!(
            IntegerConstant::new(bits_128, u128::MAX).map(IntegerConstant::bits),
            Some(u128::MAX)
        );
    }
}
