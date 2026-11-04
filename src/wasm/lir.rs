//! Byte- and bit-oriented VMU IR used for allocation and code generation.
//!
//! Wide HIR values have been decomposed before entering this IR. Instructions
//! refer to abstract storage IDs; the same program shape can carry virtual
//! placement constraints before allocation and concrete VMU placements after
//! allocation.

use std::num::NonZeroUsize;

use super::intrinsics::Intrinsic;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StorageId(pub usize);

/// A byte within an abstract storage object.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByteStorage {
    pub storage: StorageId,
    pub offset: usize,
}

pub type VirtualProgram = Program<VirtualPlacement>;
pub type AllocatedProgram = Program<PhysicalPlacement>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program<P> {
    pub main: Function,
    pub interrupt_handlers: InterruptHandlers,
    pub functions: Vec<Function>,
    pub statics: Vec<Static>,
    pub storages: Vec<Storage<P>>,
}

impl<P> Program<P> {
    /// Every function definition in deterministic emission order.
    pub fn all_functions(&self) -> impl Iterator<Item = &Function> {
        std::iter::once(&self.main)
            .chain(self.interrupt_handlers.iter())
            .chain(self.functions.iter())
    }

    /// Every mutable function definition in deterministic emission order.
    pub fn all_functions_mut(&mut self) -> impl Iterator<Item = &mut Function> {
        std::iter::once(&mut self.main)
            .chain(self.interrupt_handlers.iter_mut())
            .chain(self.functions.iter_mut())
    }
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

impl InterruptHandlers {
    pub fn iter(&self) -> impl Iterator<Item = &Function> {
        [
            self.external_0.as_ref(),
            self.external_1.as_ref(),
            self.external_2_or_timer_0_low.as_ref(),
            self.external_3_or_base_timer.as_ref(),
            self.timer_0_high.as_ref(),
            self.timer_1.as_ref(),
            self.serial_0.as_ref(),
            self.serial_1.as_ref(),
            self.dreamcast_interface.as_ref(),
            self.port_3.as_ref(),
        ]
        .into_iter()
        .flatten()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Function> {
        [
            self.external_0.as_mut(),
            self.external_1.as_mut(),
            self.external_2_or_timer_0_low.as_mut(),
            self.external_3_or_base_timer.as_mut(),
            self.timer_0_high.as_mut(),
            self.timer_1.as_mut(),
            self.serial_0.as_mut(),
            self.serial_1.as_mut(),
            self.dreamcast_interface.as_mut(),
            self.port_3.as_mut(),
        ]
        .into_iter()
        .flatten()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub kind: FunctionKind,
    /// The concrete storage assignments used at this function's boundary.
    pub abi: FunctionAbi,
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
        destination: ByteStorage,
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
        destination: ByteStorage,
        input: ByteOperand,
    },
    ByteBinary {
        op: ByteBinaryOp,
        destination: ByteStorage,
        left: ByteOperand,
        right: ByteOperand,
    },
    Add {
        destination: ByteStorage,
        left: ByteOperand,
        right: ByteOperand,
        carry_in: Option<StorageId>,
        carry_out: Option<StorageId>,
    },
    Subtract {
        destination: ByteStorage,
        left: ByteOperand,
        right: ByteOperand,
        borrow_in: Option<StorageId>,
        borrow_out: Option<StorageId>,
    },
    Multiply {
        left_high: ByteOperand,
        left_low: ByteOperand,
        right: ByteOperand,
        result_high: ByteStorage,
        result_middle: ByteStorage,
        result_low: ByteStorage,
    },
    Divide {
        dividend_high: ByteOperand,
        dividend_low: ByteOperand,
        divisor: ByteOperand,
        quotient_high: ByteStorage,
        quotient_low: ByteStorage,
        remainder: ByteStorage,
    },
    Rotate {
        direction: RotateDirection,
        through_carry: bool,
        destination: ByteStorage,
        input: ByteOperand,
        carry_in: Option<StorageId>,
        carry_out: Option<StorageId>,
    },
    LoadIndirect {
        destination: ByteStorage,
        address: Address,
    },
    StoreIndirect {
        address: Address,
        value: ByteOperand,
    },
    /// Pushes a byte onto the VMU's small hardware stack in system RAM.
    Push(ByteOperand),
    /// Pops a byte from the VMU's small hardware stack in system RAM.
    Pop(ByteStorage),
    /// Materializes the allocated address of an abstract storage object.
    AddressOf {
        destination_low: ByteStorage,
        destination_high: ByteStorage,
        storage: StorageId,
        offset: usize,
    },
    /// Materializes a symbolic function label as a 16-bit code address.
    FunctionAddress {
        destination_low: ByteStorage,
        destination_high: ByteStorage,
        function: String,
    },
    /// Computes a multi-byte shift. Bytes are ordered least-significant first.
    ShiftBytes {
        op: ShiftOp,
        destinations: Vec<ByteStorage>,
        value: Vec<ByteOperand>,
        amount: Vec<ByteOperand>,
        width: u8,
    },
    /// Compares equally-sized little-endian integers and writes one abstract bit.
    CompareBytes {
        op: CompareOp,
        destination: StorageId,
        left: Vec<ByteOperand>,
        right: Vec<ByteOperand>,
        width: u8,
    },
    SelectByte {
        destination: ByteStorage,
        condition: BitOperand,
        when_true: ByteOperand,
        when_false: ByteOperand,
    },
    SelectBit {
        destination: StorageId,
        condition: BitOperand,
        when_true: BitOperand,
        when_false: BitOperand,
    },
    BitToByte {
        destination: ByteStorage,
        source: BitOperand,
    },
    ByteToBit {
        destination: StorageId,
        source: ByteOperand,
    },
    SignExtend {
        destinations: Vec<ByteStorage>,
        source: Vec<ByteOperand>,
        source_width: u8,
    },
    IntegerUnaryBytes {
        op: IntegerUnaryBytesOp,
        destinations: Vec<ByteStorage>,
        input: Vec<ByteOperand>,
        width: u8,
    },
    IntegerBinaryBytes {
        op: IntegerBinaryBytesOp,
        destinations: Vec<ByteStorage>,
        left: Vec<ByteOperand>,
        right: Vec<ByteOperand>,
        width: u8,
    },
    /// Calls a function using the ABI assignments stored on the callee.
    Call {
        function: String,
    },
    /// Calls a function through a code address. Every possible target uses
    /// the concrete boundary assignments in `abi`.
    CallIndirect {
        target: CodeAddress,
        abi: FunctionAbi,
    },
    Intrinsic {
        operation: Intrinsic,
        inputs: Vec<ValueOperand>,
        outputs: Vec<ValueStorage>,
    },
}

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
pub enum ShiftOp {
    Left,
    RightUnsigned,
    RightSigned,
    RotateLeft,
    RotateRight,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompareOp {
    Equal,
    NotEqual,
    LessThanUnsigned,
    LessThanSigned,
    LessThanOrEqualUnsigned,
    LessThanOrEqualSigned,
    GreaterThanUnsigned,
    GreaterThanSigned,
    GreaterThanOrEqualUnsigned,
    GreaterThanOrEqualSigned,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerUnaryBytesOp {
    CountLeadingZeros,
    CountTrailingZeros,
    PopulationCount,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntegerBinaryBytesOp {
    Multiply,
    DivideUnsigned,
    DivideSigned,
    RemainderUnsigned,
    RemainderSigned,
    SaturatingAddUnsigned,
    SaturatingAddSigned,
    SaturatingSubtractUnsigned,
    SaturatingSubtractSigned,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ByteOperand {
    Immediate(u8),
    Storage(ByteStorage),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueOperand {
    Bit(BitOperand),
    Byte(ByteOperand),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueStorage {
    Bit(StorageId),
    Byte(ByteStorage),
}

/// A 16-bit address in VMU code space.
///
/// Function labels remain symbolic through assembly emission. Code generation
/// emits waterbear's `<label` and `>label` expressions for their low and high
/// bytes. A computed address is already decomposed in the order consumed by
/// the synthesized push/push/ret call sequence.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CodeAddress {
    Function(String),
    Computed { low: ByteOperand, high: ByteOperand },
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
    offset: u16,
}

impl Address {
    pub fn new(space: AddressSpace, bytes: Vec<ByteOperand>) -> Option<Self> {
        (!bytes.is_empty()).then_some(Self {
            space,
            bytes,
            offset: 0,
        })
    }

    pub fn with_offset(mut self, offset: u16) -> Self {
        self.offset = offset;
        self
    }

    pub fn space(&self) -> AddressSpace {
        self.space
    }

    pub fn bytes(&self) -> &[ByteOperand] {
        &self.bytes
    }

    pub fn offset(&self) -> u16 {
        self.offset
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
    Switch {
        value: Vec<ByteOperand>,
        width: u8,
        cases: Vec<(u128, BlockId)>,
        default: BlockId,
    },
    /// Calls `function` and returns directly to this function's caller.
    /// Code generation may use a jump or an equivalent call/return sequence.
    TailCall {
        function: String,
    },
    /// Returns through an indirectly addressed function. Code generation
    /// pushes the low byte, pushes the high byte, then emits `ret`.
    TailCallIndirect {
        target: CodeAddress,
        abi: FunctionAbi,
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
    /// value that has not yet been decomposed. Its bytes may begin at any
    /// address and are kept contiguous only so the object remains addressable.
    Bytes {
        length: NonZeroUsize,
    },
    Carry,
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

/// A byte offset into the VMU's 512-byte work RAM.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WorkRamOffset(u16);

impl WorkRamOffset {
    pub const MAX: u16 = 0x01ff;

    pub fn new(offset: u16) -> Option<Self> {
        (offset <= Self::MAX).then_some(Self(offset))
    }

    pub fn get(self) -> u16 {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum XRamBank {
    DisplayTop,
    DisplayBottom,
    Icons,
}

/// An addressable hardware byte offset within one XRAM bank.
///
/// Bitmap banks expose twelve bytes in each sixteen-address row. Bank 2 is
/// smaller still; validation applies its additional limit when the bank is
/// known.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct XRamOffset(u8);

impl XRamOffset {
    pub const MAX: u8 = super::target::XRAM_BANK_LAST_OFFSET;

    pub fn new(offset: u8) -> Option<Self> {
        super::target::xram_offset_is_addressable(offset).then_some(Self(offset))
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum StorageRegion {
    Accumulator,
    CarryFlag,
    ApplicationRam,
    SpecialRegister,
    IndirectRegister,
    /// Function-owned storage in compiler-managed work RAM.
    ///
    /// Allocation is currently static rather than frame-relative. LIR
    /// validation rejects call graphs that could reactivate a live function.
    ShadowStack,
    /// Statically allocated compiler-owned work RAM.
    WorkRam,
    Flash,
    XRam,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PhysicalPlacement {
    Accumulator,
    CarryFlag,
    /// A byte in application RAM (main RAM bank 1).
    ApplicationRam {
        offset: u8,
    },
    /// A packed bit in application RAM (main RAM bank 1).
    ApplicationBit {
        offset: u8,
        bit: BitIndex,
    },
    /// Function-owned work-RAM storage.
    ///
    /// `frame_offset` is currently an absolute WRAM offset. It is named for
    /// the intended dynamic-frame representation, which is not implemented.
    ShadowStack {
        frame_offset: WorkRamOffset,
    },
    /// A packed bit in the current function's work-RAM frame.
    ShadowStackBit {
        frame_offset: WorkRamOffset,
        bit: BitIndex,
    },
    WorkRamBit {
        offset: WorkRamOffset,
        bit: BitIndex,
    },
    SpecialRegister(SpecialRegister),
    IndirectRegister {
        bank: IndirectRegisterBank,
        register: IndirectRegister,
    },
    WorkRam {
        offset: WorkRamOffset,
    },
    /// Read-only bytes emitted alongside the program in code flash.
    Flash,
    XRam {
        bank: XRamBank,
        offset: XRamOffset,
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
    /// Lifted WebAssembly memory. Immutable objects may reside in flash while
    /// mutable objects and stack frames reside in work RAM.
    MainMemory,
    MainRam(RamBank),
    /// Addressing into compiler-managed function storage in work RAM.
    ShadowStack,
    WorkRam,
    Flash,
    XRam,
    SpecialFunctionRegisters,
    Code,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FunctionAbi {
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
    pub pieces: Vec<ValueStorage>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Static {
    pub name: String,
    pub storage: StorageId,
    pub initializer: Vec<StaticByte>,
    pub mutable: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AddressByte {
    Low,
    High,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StaticByte {
    Value(u8),
    StorageAddress {
        storage: StorageId,
        offset: usize,
        byte: AddressByte,
    },
    FunctionAddress {
        function: String,
        byte: AddressByte,
    },
}

#[cfg(test)]
mod tests {
    use super::{Address, AddressSpace, BitIndex, ByteOperand, RamBank, WorkRamOffset, XRamOffset};

    #[test]
    fn bit_index_accepts_only_positions_within_a_byte() {
        assert_eq!(BitIndex::new(0).map(BitIndex::get), Some(0));
        assert_eq!(BitIndex::new(7).map(BitIndex::get), Some(7));
        assert_eq!(BitIndex::new(8), None);
    }

    #[test]
    fn work_ram_offset_accepts_only_physical_work_ram() {
        assert_eq!(WorkRamOffset::new(0).map(WorkRamOffset::get), Some(0));
        assert_eq!(
            WorkRamOffset::new(WorkRamOffset::MAX).map(WorkRamOffset::get),
            Some(0x01ff)
        );
        assert_eq!(WorkRamOffset::new(0x0200), None);
    }

    #[test]
    fn xram_offset_accepts_only_the_addressable_bank_window() {
        assert_eq!(XRamOffset::new(0).map(XRamOffset::get), Some(0));
        assert_eq!(
            XRamOffset::new(XRamOffset::MAX).map(XRamOffset::get),
            Some(0x7b)
        );
        assert_eq!(XRamOffset::new(0x7c), None);
        assert_eq!(XRamOffset::new(0x0b).map(XRamOffset::get), Some(0x0b));
        assert_eq!(XRamOffset::new(0x0c), None);
        assert_eq!(XRamOffset::new(0x10).map(XRamOffset::get), Some(0x10));
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
