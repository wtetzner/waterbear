//! Byte- and bit-oriented VMU IR used for allocation and code generation.
//!
//! Wide HIR values have been decomposed before entering this IR. Instructions
//! refer to abstract storage IDs; the same program shape can carry virtual
//! placement constraints before allocation and concrete VMU placements after
//! allocation.

use std::num::{NonZeroU32, NonZeroUsize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StorageId(pub usize);

pub type VirtualProgram = Program<VirtualPlacement>;
pub type AllocatedProgram = Program<PhysicalPlacement>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program<P> {
    pub main: Function,
    pub interrupt_handlers: InterruptHandlers,
    pub functions: Vec<Function>,
    pub statics: Vec<Static>,
    pub calling_conventions: Vec<CallingConvention>,
    pub storages: Vec<Storage<P>>,
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
    pub kind: FunctionKind,
    pub calling_convention: String,
    pub blocks: Vec<Block>,
    pub entry: BlockId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunctionKind {
    Normal,
    /// Entered by a VMU interrupt vector and returned from with `reti`.
    Interrupt,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub id: BlockId,
    pub name: Option<String>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    pub kind: InstructionKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InstructionKind {
    CopyByte {
        destination: StorageId,
        source: ByteOperand,
    },
    CopyBit {
        destination: StorageId,
        source: BitOperand,
    },
    SetBit(StorageId),
    ClearBit(StorageId),
    ToggleBit(StorageId),
    ByteUnary {
        op: ByteUnaryOp,
        destination: StorageId,
        input: ByteOperand,
    },
    ByteBinary {
        op: ByteBinaryOp,
        destination: StorageId,
        left: ByteOperand,
        right: ByteOperand,
    },
    Add {
        destination: StorageId,
        left: ByteOperand,
        right: ByteOperand,
        carry_in: Option<StorageId>,
        carry_out: Option<StorageId>,
    },
    Subtract {
        destination: StorageId,
        left: ByteOperand,
        right: ByteOperand,
        borrow_in: Option<StorageId>,
        borrow_out: Option<StorageId>,
    },
    Multiply {
        left_high: ByteOperand,
        left_low: ByteOperand,
        right: ByteOperand,
        result_high: StorageId,
        result_middle: StorageId,
        result_low: StorageId,
    },
    Divide {
        dividend_high: ByteOperand,
        dividend_low: ByteOperand,
        divisor: ByteOperand,
        quotient_high: StorageId,
        quotient_low: StorageId,
        remainder: StorageId,
    },
    Rotate {
        direction: RotateDirection,
        through_carry: bool,
        destination: StorageId,
        input: ByteOperand,
        carry_in: Option<StorageId>,
        carry_out: Option<StorageId>,
    },
    LoadIndirect {
        destination: StorageId,
        address: Address,
    },
    StoreIndirect {
        address: Address,
        value: ByteOperand,
    },
    Push(ByteOperand),
    Pop(StorageId),
    Call {
        function: String,
        calling_convention: String,
    },
    Intrinsic {
        operation: Intrinsic,
        inputs: Vec<StorageId>,
        outputs: Vec<StorageId>,
    },
}

/// The name of an operation supplied and validated by the VMU compiler.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Intrinsic(pub String);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteUnaryOp {
    Not,
    Negate,
    Increment,
    Decrement,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteBinaryOp {
    And,
    Or,
    Xor,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RotateDirection {
    Left,
    Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteOperand {
    Immediate(u8),
    Storage(StorageId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitOperand {
    Constant(bool),
    Storage(StorageId),
}

/// A target-space address decomposed from least to most significant byte.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Address {
    space: AddressSpace,
    bytes: Vec<ByteOperand>,
}

impl Address {
    pub fn new(space: AddressSpace, bytes: Vec<ByteOperand>) -> Option<Self> {
        (!bytes.is_empty()).then_some(Self { space, bytes })
    }

    pub fn space(&self) -> AddressSpace {
        self.space
    }

    pub fn bytes(&self) -> &[ByteOperand] {
        &self.bytes
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TerminatorKind {
    Jump(BlockId),
    Branch {
        condition: BranchCondition,
        when_true: BlockId,
        when_false: BlockId,
    },
    /// Emits `ret` for a normal function and `reti` for an interrupt function.
    Return,
    Trap,
    Unreachable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BranchCondition {
    BitSet(StorageId),
    BitClear(StorageId),
    ByteZero(ByteOperand),
    ByteNonZero(ByteOperand),
    CarrySet(StorageId),
    CarryClear(StorageId),
    Equal {
        left: ByteOperand,
        right: ByteOperand,
    },
    NotEqual {
        left: ByteOperand,
        right: ByteOperand,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Storage<P> {
    pub id: StorageId,
    pub name: Option<String>,
    pub owner: StorageOwner,
    pub kind: StorageKind,
    pub placement: P,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StorageOwner {
    Program,
    Function(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageKind {
    Bit,
    Byte,
    /// An addressable object made up of multiple bytes, rather than a scalar
    /// value that has not yet been decomposed.
    Bytes {
        length: NonZeroUsize,
        alignment: Alignment,
    },
    Carry,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BitIndex(u8);

impl BitIndex {
    pub fn new(index: u8) -> Option<Self> {
        (index < 8).then_some(Self(index))
    }

    pub fn get(self) -> u8 {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VirtualPlacement {
    pub allowed_regions: Vec<StorageRegion>,
    pub must_be_direct: bool,
    pub fixed: Option<PhysicalPlacement>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageRegion {
    Accumulator,
    CarryFlag,
    DirectRam,
    SpecialRegister,
    IndirectRegister,
    StackFrame,
    WorkRam,
    Flash,
    XRam,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PhysicalPlacement {
    Accumulator,
    CarryFlag,
    DirectRam {
        bank: RamBank,
        offset: u8,
    },
    DirectBit {
        bank: RamBank,
        offset: u8,
        bit: BitIndex,
    },
    /// A byte at an offset from the base of the current function's frame.
    StackSlot {
        frame_offset: u8,
    },
    /// A packed bit in a byte belonging to the current function's frame.
    StackBit {
        frame_offset: u8,
        bit: BitIndex,
    },
    WorkRamBit {
        offset: u16,
        bit: BitIndex,
    },
    SpecialRegister(SpecialRegister),
    IndirectRegister {
        bank: IndirectRegisterBank,
        register: IndirectRegister,
    },
    WorkRam {
        offset: u16,
    },
    Flash {
        address: u16,
    },
    XRam {
        bank: u8,
        offset: u8,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RamBank {
    System,
    Application,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpecialRegister {
    ProgramStatus,
    B,
    C,
    TableReferenceLow,
    TableReferenceHigh,
    StackPointer,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndirectRegister {
    R0,
    R1,
    R2,
    R3,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndirectRegisterBank {
    Bank0,
    Bank1,
    Bank2,
    Bank3,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AddressSpace {
    MainRam(RamBank),
    StackFrame,
    WorkRam,
    Flash,
    XRam,
    SpecialFunctionRegisters,
    Code,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallingConvention {
    pub name: String,
    pub parameters: Vec<AbiValue>,
    pub results: Vec<AbiValue>,
    pub preserved: Vec<StorageId>,
    pub clobbered: Vec<StorageId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AbiValue {
    pub logical_index: usize,
    /// Pieces are ordered from least to most significant. A boolean has one
    /// bit-sized piece.
    pub pieces: Vec<StorageId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Static {
    pub name: String,
    pub storage: StorageId,
    pub initializer: Vec<u8>,
    pub mutable: bool,
}

#[cfg(test)]
mod tests {
    use super::{Address, AddressSpace, Alignment, BitIndex, ByteOperand, RamBank};

    #[test]
    fn alignment_accepts_only_nonzero_powers_of_two() {
        assert_eq!(Alignment::new(0), None);
        assert_eq!(Alignment::new(1).map(Alignment::get), Some(1));
        assert_eq!(Alignment::new(8).map(Alignment::get), Some(8));
        assert_eq!(Alignment::new(12), None);
    }

    #[test]
    fn bit_index_accepts_only_positions_within_a_byte() {
        assert_eq!(BitIndex::new(0).map(BitIndex::get), Some(0));
        assert_eq!(BitIndex::new(7).map(BitIndex::get), Some(7));
        assert_eq!(BitIndex::new(8), None);
    }

    #[test]
    fn address_requires_at_least_one_byte() {
        let space = AddressSpace::MainRam(RamBank::Application);
        assert_eq!(Address::new(space, Vec::new()), None);

        let address = Address::new(space, vec![ByteOperand::Immediate(42)]).unwrap();
        assert_eq!(address.space(), space);
        assert_eq!(address.bytes(), [ByteOperand::Immediate(42)]);
    }
}
