//! Instruction selection from allocated VMU LIR to Waterbear assembly.

use std::{collections::BTreeMap, error::Error, fmt};

use crate::{
    ast::{ByteValue, Statement, Statements},
    expression::{Arg, Expr, IndirectionMode},
    instruction::Instr,
    location::Span,
};

use super::{
    intrinsics::{Intrinsic, Sfr},
    lir::{
        Address, AddressByte, AddressSpace, AllocatedProgram, BitOperand, BlockId, BranchCondition,
        ByteBinaryOp, ByteOperand, ByteStorage, ByteUnaryOp, CodeAddress, CompareOp, Function,
        FunctionKind, InstructionKind, IntegerBinaryBytesOp, IntegerUnaryBytesOp,
        PhysicalPlacement, RamBank, RotateDirection, ShiftOp, StaticByte, Storage, StorageId,
        StorageKind, StorageOwner, TerminatorKind, ValueOperand, ValueStorage,
    },
};

const ACC: u16 = 0x100;
const PSW: u16 = 0x101;
const B: u16 = 0x102;
const C: u16 = 0x103;
const TRL: u16 = 0x104;
const TRH: u16 = 0x105;
const XBNK: u16 = 0x125;
const VSEL: u16 = 0x163;
const VRMAD1: u16 = 0x164;
const VRMAD2: u16 = 0x165;
const VTRBF: u16 = 0x166;
const XRAM_BASE: u16 = 0x180;

const R2: u16 = 0x02;
const SCRATCH0: u16 = 0x10;
const SCRATCH1: u16 = 0x11;
const SCRATCH2: u16 = 0x12;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LowerError {
    MissingStorage(StorageId),
    InvalidByteOffset(ByteStorage),
    InvalidBitStorage(StorageId),
    InvalidWorkRamAddress(usize),
    UnsupportedPlacement(PhysicalPlacement),
    UnsupportedAddressSpace(AddressSpace),
    UnsupportedInstruction(&'static str),
    MalformedIntrinsic(Intrinsic),
    UnknownFunction(String),
}

impl fmt::Display for LowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingStorage(storage) => write!(f, "missing allocated storage %s{}", storage.0),
            Self::InvalidByteOffset(byte) => write!(
                f,
                "byte offset {} is outside allocated storage %s{}",
                byte.offset, byte.storage.0
            ),
            Self::InvalidBitStorage(storage) => {
                write!(f, "storage %s{} is not allocated as a bit", storage.0)
            }
            Self::InvalidWorkRamAddress(address) => {
                write!(
                    f,
                    "work RAM address {address:#x} is outside its 512-byte range"
                )
            }
            Self::UnsupportedPlacement(placement) => {
                write!(
                    f,
                    "assembly lowering does not support placement {placement:?}"
                )
            }
            Self::UnsupportedAddressSpace(space) => {
                write!(
                    f,
                    "assembly lowering does not support address space {space:?}"
                )
            }
            Self::UnsupportedInstruction(instruction) => {
                write!(f, "assembly lowering does not yet support {instruction}")
            }
            Self::MalformedIntrinsic(intrinsic) => {
                write!(f, "malformed LIR operands for intrinsic {intrinsic:?}")
            }
            Self::UnknownFunction(function) => {
                write!(f, "LIR refers to unknown function {function:?}")
            }
        }
    }
}

impl Error for LowerError {}

pub fn lower(program: &AllocatedProgram) -> Result<Statements, LowerError> {
    Lowerer::new(program).lower()
}

struct Lowerer<'a> {
    program: &'a AllocatedProgram,
    output: Statements,
    function_labels: BTreeMap<String, String>,
    label_counter: usize,
    accumulator: Option<AccumulatorValue>,
    selected_work_address: Option<u16>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ByteLocation {
    Direct(u16),
    WorkRam(u16),
    Flash { storage: StorageId, offset: usize },
    XRam { bank: u8, offset: u8 },
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BitLocation {
    Direct { address: u16, bit: u8 },
    WorkRam { address: u16, bit: u8 },
    Carry,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AccumulatorValue {
    Immediate(u8),
    Location(ByteLocation),
}

impl<'a> Lowerer<'a> {
    fn new(program: &'a AllocatedProgram) -> Self {
        Self {
            program,
            output: Statements::empty(),
            function_labels: function_labels(program),
            label_counter: 0,
            accumulator: None,
            selected_work_address: None,
        }
    }

    fn lower(mut self) -> Result<Statements, LowerError> {
        self.emit_register_aliases();
        self.emit_flash_statics()?;
        self.emit_function(&self.program.main, true)?;
        for function in interrupt_functions(self.program) {
            self.emit_function(function, false)?;
        }
        for function in &self.program.functions {
            self.emit_function(function, false)?;
        }
        Ok(self.output)
    }

    fn emit_flash_statics(&mut self) -> Result<(), LowerError> {
        let statics = self
            .program
            .statics
            .iter()
            .filter(|static_| self.storage_is_flash(static_.storage))
            .collect::<Vec<_>>();
        if statics.is_empty() {
            return Ok(());
        }

        self.output.push_comment("read-only data in code flash");
        for static_ in statics {
            self.output.push_label(&self.flash_label(static_.storage)?);
            for bytes in static_.initializer.chunks(16) {
                let bytes = bytes
                    .iter()
                    .map(|byte| self.static_byte_expression(byte).map(ByteValue::Expr))
                    .collect::<Result<Vec<_>, _>>()?;
                self.output.push(Statement::byte(bytes));
            }
        }
        self.output.push_comment("");
        Ok(())
    }

    fn emit_register_aliases(&mut self) {
        self.output
            .push_comment("VMU registers used by generated code");
        for (name, address) in [
            ("ACC", ACC),
            ("PSW", PSW),
            ("B", B),
            ("C", C),
            ("TRL", TRL),
            ("TRH", TRH),
            ("XBNK", XBNK),
            ("VSEL", VSEL),
            ("VRMAD1", VRMAD1),
            ("VRMAD2", VRMAD2),
            ("VTRBF", VTRBF),
        ] {
            self.output.push_var(name, direct(address));
        }
        self.output.push_comment("");
        self.emit_storage_aliases();
    }

    fn emit_storage_aliases(&mut self) {
        let aliases = self
            .program
            .storages
            .iter()
            .flat_map(storage_aliases)
            .collect::<Vec<_>>();
        if aliases.is_empty() {
            return;
        }
        self.output
            .push_comment("named compiler storage in application RAM");
        for (name, address) in aliases {
            self.output.push_var(&name, direct(address));
        }
        self.output.push_comment("");
    }

    fn emit_function(&mut self, function: &Function, main: bool) -> Result<(), LowerError> {
        self.reset_machine_state();
        let label = self.function_label(&function.name)?.to_owned();
        self.emit_label(&label);
        if main {
            self.output
                .push_comment("select application RAM and indirect-register bank zero");
            self.instr(Instr::Set1(direct(PSW), Expr::num(1)));
            self.instr(Instr::Clr1(direct(PSW), Expr::num(3)));
            self.instr(Instr::Clr1(direct(PSW), Expr::num(4)));
            self.instr(Instr::Clr1(direct(VSEL), Expr::num(4)));
            self.emit_static_initializers()?;
        }
        self.jmpf(self.block_label(function.entry));

        for block in &function.blocks {
            self.emit_label(&self.block_label(block.id));
            self.reset_machine_state();
            if let Some(name) = &block.name {
                self.output.push_comment(name);
            }
            for instruction in &block.instructions {
                self.emit_instruction(&instruction.kind)?;
            }
            self.emit_terminator(function, &block.terminator.kind)?;
        }
        self.output.push_comment("");
        Ok(())
    }

    fn emit_static_initializers(&mut self) -> Result<(), LowerError> {
        let statics = self
            .program
            .statics
            .iter()
            .filter(|static_| !self.storage_is_flash(static_.storage))
            .collect::<Vec<_>>();
        if statics.is_empty() {
            return Ok(());
        }
        self.output
            .push_comment("initialize compiler-owned work RAM");
        for static_ in statics {
            for (index, byte) in static_.initializer.iter().enumerate() {
                let destination = ByteStorage {
                    storage: static_.storage,
                    offset: index,
                };
                match byte {
                    StaticByte::Value(value) => self.load_immediate(*value),
                    StaticByte::StorageAddress {
                        storage,
                        offset,
                        byte,
                    } => {
                        let address = self.storage_address_expression(*storage, *offset)?;
                        self.load_expression(match byte {
                            AddressByte::Low => lower_byte(address),
                            AddressByte::High => upper_byte(address),
                        });
                    }
                    StaticByte::FunctionAddress { function, byte } => {
                        let label = self.function_label(function)?.to_owned();
                        let expression = match byte {
                            AddressByte::Low => lower_byte(Expr::name(&label)),
                            AddressByte::High => upper_byte(Expr::name(&label)),
                        };
                        self.load_expression(expression);
                    }
                }
                self.store_accumulator(destination)?;
            }
        }
        Ok(())
    }

    fn emit_instruction(&mut self, instruction: &InstructionKind) -> Result<(), LowerError> {
        match instruction {
            InstructionKind::CopyByte {
                destination,
                source,
            } => self.copy_byte(*destination, *source),
            InstructionKind::CopyBit {
                destination,
                source,
            } => self.copy_bit(*destination, *source),
            InstructionKind::SetBit(destination) => self.set_bit(*destination, true),
            InstructionKind::ClearBit(destination) => self.set_bit(*destination, false),
            InstructionKind::ToggleBit(destination) => self.toggle_bit(*destination),
            InstructionKind::ByteUnary {
                op,
                destination,
                input,
            } => self.byte_unary(*op, *destination, *input),
            InstructionKind::ByteBinary {
                op,
                destination,
                left,
                right,
            } => self.byte_binary(*op, *destination, *left, *right),
            InstructionKind::Add {
                destination,
                left,
                right,
                carry_in,
                carry_out,
            } => self.add_subtract(false, *destination, *left, *right, *carry_in, *carry_out),
            InstructionKind::Subtract {
                destination,
                left,
                right,
                borrow_in,
                borrow_out,
            } => self.add_subtract(true, *destination, *left, *right, *borrow_in, *borrow_out),
            InstructionKind::Multiply {
                left_high,
                left_low,
                right,
                result_high,
                result_middle,
                result_low,
            } => self.multiply(
                *left_high,
                *left_low,
                *right,
                *result_high,
                *result_middle,
                *result_low,
            ),
            InstructionKind::Divide {
                dividend_high,
                dividend_low,
                divisor,
                quotient_high,
                quotient_low,
                remainder,
            } => self.divide(
                *dividend_high,
                *dividend_low,
                *divisor,
                *quotient_high,
                *quotient_low,
                *remainder,
            ),
            InstructionKind::Rotate {
                direction,
                through_carry,
                destination,
                input,
                carry_in,
                carry_out,
            } => self.rotate(
                *direction,
                *through_carry,
                *destination,
                *input,
                *carry_in,
                *carry_out,
            ),
            InstructionKind::LoadIndirect {
                destination,
                address,
            } => self.load_indirect(*destination, address),
            InstructionKind::StoreIndirect { address, value } => {
                self.store_indirect(address, *value)
            }
            InstructionKind::Push(value) => self.push_operand(*value),
            InstructionKind::Pop(destination) => self.pop_storage(*destination),
            InstructionKind::AddressOf {
                destination_low,
                destination_high,
                storage,
                offset,
            } => self.address_of(*destination_low, *destination_high, *storage, *offset),
            InstructionKind::FunctionAddress {
                destination_low,
                destination_high,
                function,
            } => self.function_address(*destination_low, *destination_high, function),
            InstructionKind::ShiftBytes {
                op,
                destinations,
                value,
                amount,
                width,
            } => self.shift_bytes(*op, destinations, value, amount, *width),
            InstructionKind::CompareBytes {
                op,
                destination,
                left,
                right,
                width,
            } => self.compare_bytes(*op, *destination, left, right, *width),
            InstructionKind::SelectByte {
                destination,
                condition,
                when_true,
                when_false,
            } => self.select_byte(*destination, *condition, *when_true, *when_false),
            InstructionKind::SelectBit {
                destination,
                condition,
                when_true,
                when_false,
            } => self.select_bit(*destination, *condition, *when_true, *when_false),
            InstructionKind::BitToByte {
                destination,
                source,
            } => self.bit_to_byte(*destination, *source),
            InstructionKind::ByteToBit {
                destination,
                source,
            } => self.byte_to_bit(*destination, *source),
            InstructionKind::SignExtend {
                destinations,
                source,
                source_width,
            } => self.sign_extend(destinations, source, *source_width),
            InstructionKind::IntegerUnaryBytes {
                op,
                destinations,
                input,
                width,
            } => self.integer_unary(*op, destinations, input, *width),
            InstructionKind::IntegerBinaryBytes {
                op,
                destinations,
                left,
                right,
                width,
            } => self.integer_binary(*op, destinations, left, right, *width),
            InstructionKind::Call { function } => {
                let label = self.function_label(function)?.to_owned();
                self.instr(Instr::Callf(Expr::name(&label)));
                self.reset_machine_state();
                Ok(())
            }
            InstructionKind::CallIndirect { target, .. } => {
                let result = self.call_indirect(target);
                self.reset_machine_state();
                result
            }
            InstructionKind::Intrinsic {
                operation,
                inputs,
                outputs,
            } => {
                let result = self.intrinsic(*operation, inputs, outputs);
                self.reset_machine_state();
                result
            }
        }
    }

    fn emit_terminator(
        &mut self,
        function: &Function,
        terminator: &TerminatorKind,
    ) -> Result<(), LowerError> {
        match terminator {
            TerminatorKind::Jump(target) => self.jmpf(self.block_label(*target)),
            TerminatorKind::Branch {
                condition,
                when_true,
                when_false,
            } => self.branch(condition, *when_true, *when_false)?,
            TerminatorKind::Switch {
                value,
                width,
                cases,
                default,
            } => self.switch(value, *width, cases, *default)?,
            TerminatorKind::TailCall { function } => {
                let label = self.function_label(function)?.to_owned();
                self.jmpf(label);
            }
            TerminatorKind::TailCallIndirect { target, .. } => {
                self.tail_call_indirect(target)?;
            }
            TerminatorKind::Return => self.instr(match function.kind {
                FunctionKind::Normal => Instr::Ret,
                FunctionKind::Interrupt => Instr::Reti,
            }),
            TerminatorKind::Trap | TerminatorKind::Unreachable => {
                let trap = self.fresh_label("trap");
                self.emit_label(&trap);
                self.jmpf(trap);
            }
        }
        Ok(())
    }

    fn instr(&mut self, instruction: Instr<Expr, IndirectionMode>) {
        self.output.push_instr(instruction);
        self.accumulator = None;
    }

    fn emit_label(&mut self, label: impl AsRef<str>) {
        self.output.push_label(label.as_ref());
        self.reset_machine_state();
    }

    fn reset_machine_state(&mut self) {
        self.accumulator = None;
        self.selected_work_address = None;
    }

    fn jmpf(&mut self, label: impl AsRef<str>) {
        self.instr(Instr::Jmpf(Expr::name(label.as_ref())));
    }

    fn function_label(&self, function: &str) -> Result<&str, LowerError> {
        self.function_labels
            .get(function)
            .map(String::as_str)
            .ok_or_else(|| LowerError::UnknownFunction(function.into()))
    }

    fn block_label(&self, block: BlockId) -> String {
        format!(".block{}", block.0)
    }

    fn fresh_label(&mut self, purpose: &str) -> String {
        let label = format!(".{}_{}", purpose, self.label_counter);
        self.label_counter += 1;
        label
    }

    fn storage(&self, id: StorageId) -> Result<&Storage<PhysicalPlacement>, LowerError> {
        self.program
            .storages
            .iter()
            .find(|storage| storage.id == id)
            .ok_or(LowerError::MissingStorage(id))
    }

    fn byte_location(&self, byte: ByteStorage) -> Result<ByteLocation, LowerError> {
        let storage = self.storage(byte.storage)?;
        let length = match storage.kind {
            StorageKind::Bit | StorageKind::Carry => 0,
            StorageKind::Byte => 1,
            StorageKind::Bytes { length } => length.get(),
        };
        if byte.offset >= length {
            return Err(LowerError::InvalidByteOffset(byte));
        }
        match storage.placement {
            PhysicalPlacement::Accumulator => Ok(ByteLocation::Direct(ACC)),
            PhysicalPlacement::ApplicationRam { offset } => {
                Ok(ByteLocation::Direct(offset as u16 + byte.offset as u16))
            }
            PhysicalPlacement::ShadowStack { frame_offset } => self
                .checked_work_address(frame_offset.get() as usize + byte.offset)
                .map(ByteLocation::WorkRam),
            PhysicalPlacement::WorkRam { offset } => self
                .checked_work_address(offset.get() as usize + byte.offset)
                .map(ByteLocation::WorkRam),
            PhysicalPlacement::SpecialRegister(register) => {
                Ok(ByteLocation::Direct(special_register_address(register)))
            }
            PhysicalPlacement::Flash => Ok(ByteLocation::Flash {
                storage: byte.storage,
                offset: byte.offset,
            }),
            PhysicalPlacement::XRam { bank, offset } => Ok(ByteLocation::XRam {
                bank: xram_bank(bank),
                offset: offset.get().wrapping_add(byte.offset as u8),
            }),
            placement => Err(LowerError::UnsupportedPlacement(placement)),
        }
    }

    fn bit_location(&self, storage: StorageId) -> Result<BitLocation, LowerError> {
        match self.storage(storage)?.placement {
            PhysicalPlacement::ApplicationBit { offset, bit } => Ok(BitLocation::Direct {
                address: offset as u16,
                bit: bit.get(),
            }),
            PhysicalPlacement::ShadowStackBit { frame_offset, bit } => Ok(BitLocation::WorkRam {
                address: frame_offset.get(),
                bit: bit.get(),
            }),
            PhysicalPlacement::WorkRamBit { offset, bit } => Ok(BitLocation::WorkRam {
                address: offset.get(),
                bit: bit.get(),
            }),
            PhysicalPlacement::CarryFlag => Ok(BitLocation::Carry),
            _ => Err(LowerError::InvalidBitStorage(storage)),
        }
    }

    fn checked_work_address(&self, address: usize) -> Result<u16, LowerError> {
        (address <= 0x1ff)
            .then_some(address as u16)
            .ok_or(LowerError::InvalidWorkRamAddress(address))
    }

    fn storage_work_address(
        &self,
        storage: StorageId,
        byte_offset: usize,
    ) -> Result<u16, LowerError> {
        let storage = self.storage(storage)?;
        let base = match storage.placement {
            PhysicalPlacement::ShadowStack { frame_offset } => frame_offset.get() as usize,
            PhysicalPlacement::WorkRam { offset } => offset.get() as usize,
            placement => return Err(LowerError::UnsupportedPlacement(placement)),
        };
        let length = match storage.kind {
            StorageKind::Byte => 1,
            StorageKind::Bytes { length } => length.get(),
            StorageKind::Bit | StorageKind::Carry => 0,
        };
        if byte_offset > length {
            return Err(LowerError::InvalidByteOffset(ByteStorage {
                storage: storage.id,
                offset: byte_offset,
            }));
        }
        self.checked_work_address(base + byte_offset)
    }

    fn storage_is_flash(&self, storage: StorageId) -> bool {
        self.storage(storage)
            .is_ok_and(|storage| storage.placement == PhysicalPlacement::Flash)
    }

    fn flash_label(&self, storage: StorageId) -> Result<String, LowerError> {
        let storage = self.storage(storage)?;
        let name = storage.name.as_deref().unwrap_or("data");
        Ok(format!(
            "__waterbear_static_{}_{}",
            storage.id.0,
            sanitize_label(name)
        ))
    }

    fn storage_address_expression(
        &self,
        storage: StorageId,
        offset: usize,
    ) -> Result<Expr, LowerError> {
        match self.storage(storage)?.placement {
            PhysicalPlacement::ShadowStack { .. } | PhysicalPlacement::WorkRam { .. } => {
                Ok(Expr::num(self.storage_work_address(storage, offset)? as i32))
            }
            PhysicalPlacement::Flash => {
                let label = Expr::name(&self.flash_label(storage)?);
                Ok(if offset == 0 {
                    label
                } else {
                    Expr::Plus(
                        Span::default(),
                        Box::new(label),
                        Box::new(Expr::num(offset as i32)),
                    )
                })
            }
            placement => Err(LowerError::UnsupportedPlacement(placement)),
        }
    }

    fn static_byte_expression(&self, byte: &StaticByte) -> Result<Expr, LowerError> {
        Ok(match byte {
            StaticByte::Value(value) => Expr::num(i32::from(*value)),
            StaticByte::StorageAddress {
                storage,
                offset,
                byte,
            } => {
                let address = self.storage_address_expression(*storage, *offset)?;
                match byte {
                    AddressByte::Low => lower_byte(address),
                    AddressByte::High => upper_byte(address),
                }
            }
            StaticByte::FunctionAddress { function, byte } => {
                let address = Expr::name(self.function_label(function)?);
                match byte {
                    AddressByte::Low => lower_byte(address),
                    AddressByte::High => upper_byte(address),
                }
            }
        })
    }

    fn load_immediate(&mut self, value: u8) {
        if self.accumulator == Some(AccumulatorValue::Immediate(value)) {
            return;
        }
        self.instr(Instr::Mov_d9(Expr::num(value as i32), direct(ACC)));
        self.accumulator = Some(AccumulatorValue::Immediate(value));
    }

    fn load_expression(&mut self, expression: Expr) {
        self.instr(Instr::Mov_d9(expression, direct(ACC)));
    }

    fn load_code_byte(&mut self) {
        // LDC reads from TR + ACC, so ordinary pointer loads must explicitly
        // select offset zero rather than inheriting the previous ACC value.
        self.load_immediate(0);
        self.instr(Instr::Ldc);
    }

    fn load_operand(&mut self, operand: ByteOperand) -> Result<(), LowerError> {
        match operand {
            ByteOperand::Immediate(value) => self.load_immediate(value),
            ByteOperand::Storage(storage) => self.load_storage(storage)?,
        }
        Ok(())
    }

    fn load_storage(&mut self, storage: ByteStorage) -> Result<(), LowerError> {
        let location = self.byte_location(storage)?;
        if self.accumulator == Some(AccumulatorValue::Location(location)) {
            return Ok(());
        }
        match location {
            ByteLocation::Direct(address) => self.instr(Instr::Ld_d9(direct(address))),
            ByteLocation::WorkRam(address) => {
                self.select_work_address(address);
                self.instr(Instr::Ld_d9(direct(VTRBF)));
            }
            ByteLocation::Flash { storage, offset } => {
                let address = self.storage_address_expression(storage, offset)?;
                self.instr(Instr::Mov_d9(lower_byte(address.clone()), direct(TRL)));
                self.instr(Instr::Mov_d9(upper_byte(address), direct(TRH)));
                self.load_code_byte();
            }
            ByteLocation::XRam { bank, offset } => {
                self.instr(Instr::Mov_d9(Expr::num(bank as i32), direct(XBNK)));
                self.instr(Instr::Ld_d9(direct(XRAM_BASE + offset as u16)));
            }
        }
        self.accumulator = Some(AccumulatorValue::Location(location));
        Ok(())
    }

    fn store_accumulator(&mut self, storage: ByteStorage) -> Result<(), LowerError> {
        let location = self.byte_location(storage)?;
        if self.accumulator == Some(AccumulatorValue::Location(location)) {
            return Ok(());
        }
        match location {
            ByteLocation::Direct(address) => self.instr(Instr::St_d9(direct(address))),
            ByteLocation::WorkRam(address) => {
                self.select_work_address(address);
                self.instr(Instr::St_d9(direct(VTRBF)));
            }
            ByteLocation::XRam { bank, offset } => {
                self.instr(Instr::Mov_d9(Expr::num(bank as i32), direct(XBNK)));
                self.instr(Instr::St_d9(direct(XRAM_BASE + offset as u16)));
            }
            ByteLocation::Flash { .. } => {
                return Err(LowerError::UnsupportedInstruction(
                    "writes to flash storage",
                ));
            }
        }
        self.accumulator = Some(AccumulatorValue::Location(location));
        Ok(())
    }

    fn select_work_address(&mut self, address: u16) {
        if self.selected_work_address == Some(address) {
            return;
        }
        let previous = self.selected_work_address;
        if previous.is_none_or(|previous| previous as u8 != address as u8) {
            self.instr(Instr::Mov_d9(
                Expr::num((address & 0xff) as i32),
                direct(VRMAD1),
            ));
        }
        if previous.is_none_or(|previous| (previous >> 8) as u8 != (address >> 8) as u8) {
            self.instr(Instr::Mov_d9(
                Expr::num((address >> 8) as i32),
                direct(VRMAD2),
            ));
        }
        self.selected_work_address = Some(address);
    }

    fn copy_byte(
        &mut self,
        destination: ByteStorage,
        source: ByteOperand,
    ) -> Result<(), LowerError> {
        if let ByteOperand::Storage(source) = source
            && self.byte_location(source)? == self.byte_location(destination)?
        {
            return Ok(());
        }
        self.load_operand(source)?;
        self.store_accumulator(destination)
    }

    fn set_bit(&mut self, destination: StorageId, value: bool) -> Result<(), LowerError> {
        let location = self.bit_location(destination)?;
        self.set_bit_location(location, value);
        Ok(())
    }

    fn set_bit_location(&mut self, location: BitLocation, value: bool) {
        let (address, bit) = match location {
            BitLocation::Direct { address, bit } => (address, bit),
            BitLocation::WorkRam { address, bit } => {
                self.select_work_address(address);
                (VTRBF, bit)
            }
            BitLocation::Carry => (PSW, 7),
        };
        self.instr(if value {
            Instr::Set1(direct(address), Expr::num(bit as i32))
        } else {
            Instr::Clr1(direct(address), Expr::num(bit as i32))
        });
    }

    fn toggle_bit(&mut self, destination: StorageId) -> Result<(), LowerError> {
        let location = self.bit_location(destination)?;
        let (address, bit) = match location {
            BitLocation::Direct { address, bit } => (address, bit),
            BitLocation::WorkRam { address, bit } => {
                self.select_work_address(address);
                (VTRBF, bit)
            }
            BitLocation::Carry => (PSW, 7),
        };
        self.instr(Instr::Not1(direct(address), Expr::num(bit as i32)));
        Ok(())
    }

    fn copy_bit(&mut self, destination: StorageId, source: BitOperand) -> Result<(), LowerError> {
        match source {
            BitOperand::Constant(value) => self.set_bit(destination, value),
            BitOperand::Storage(source)
                if source == destination
                    || self.bit_location(source)? == self.bit_location(destination)? =>
            {
                Ok(())
            }
            BitOperand::Storage(_) => {
                let set = self.fresh_label("copy_bit_set");
                let done = self.fresh_label("copy_bit_done");
                self.set_bit(destination, false)?;
                self.branch_if_bit_operand(source, true, &set)?;
                self.jmpf(&done);
                self.emit_label(&set);
                self.set_bit(destination, true)?;
                self.emit_label(&done);
                Ok(())
            }
        }
    }

    fn byte_unary(
        &mut self,
        op: ByteUnaryOp,
        destination: ByteStorage,
        input: ByteOperand,
    ) -> Result<(), LowerError> {
        match op {
            ByteUnaryOp::Not => {
                self.load_operand(input)?;
                self.instr(Instr::Xor_i8(Expr::num(0xff)));
            }
            ByteUnaryOp::Negate => {
                self.materialize_operand(input, SCRATCH0)?;
                self.load_immediate(0);
                self.instr(Instr::Sub_d9(direct(SCRATCH0)));
            }
            ByteUnaryOp::Increment => {
                self.load_operand(input)?;
                self.instr(Instr::Add_i8(Expr::num(1)));
            }
            ByteUnaryOp::Decrement => {
                self.load_operand(input)?;
                self.instr(Instr::Sub_i8(Expr::num(1)));
            }
        }
        self.store_accumulator(destination)
    }

    fn byte_binary(
        &mut self,
        op: ByteBinaryOp,
        destination: ByteStorage,
        left: ByteOperand,
        right: ByteOperand,
    ) -> Result<(), LowerError> {
        let (left, right) =
            if self.accumulator_matches(right)? && !self.accumulator_matches(left)? {
                (right, left)
            } else {
                (left, right)
            };
        self.load_operand(left)?;
        self.emit_acc_binary(op, right)?;
        self.store_accumulator(destination)
    }

    fn accumulator_matches(&self, operand: ByteOperand) -> Result<bool, LowerError> {
        Ok(match operand {
            ByteOperand::Immediate(value) => {
                self.accumulator == Some(AccumulatorValue::Immediate(value))
            }
            ByteOperand::Storage(storage) => {
                self.accumulator == Some(AccumulatorValue::Location(self.byte_location(storage)?))
            }
        })
    }

    fn emit_acc_binary(&mut self, op: ByteBinaryOp, right: ByteOperand) -> Result<(), LowerError> {
        let direct_right = match right {
            ByteOperand::Immediate(value) => {
                self.instr(match op {
                    ByteBinaryOp::And => Instr::And_i8(Expr::num(value as i32)),
                    ByteBinaryOp::Or => Instr::Or_i8(Expr::num(value as i32)),
                    ByteBinaryOp::Xor => Instr::Xor_i8(Expr::num(value as i32)),
                });
                return Ok(());
            }
            ByteOperand::Storage(storage) => match self.byte_location(storage)? {
                ByteLocation::Direct(address) => address,
                _ => {
                    self.instr(Instr::St_d9(direct(SCRATCH0)));
                    self.materialize_operand(right, SCRATCH1)?;
                    self.instr(Instr::Ld_d9(direct(SCRATCH0)));
                    SCRATCH1
                }
            },
        };
        self.instr(match op {
            ByteBinaryOp::And => Instr::And_d9(direct(direct_right)),
            ByteBinaryOp::Or => Instr::Or_d9(direct(direct_right)),
            ByteBinaryOp::Xor => Instr::Xor_d9(direct(direct_right)),
        });
        Ok(())
    }

    fn materialize_operand(
        &mut self,
        operand: ByteOperand,
        scratch: u16,
    ) -> Result<(), LowerError> {
        self.load_operand(operand)?;
        self.instr(Instr::St_d9(direct(scratch)));
        Ok(())
    }

    fn add_subtract(
        &mut self,
        subtract: bool,
        destination: ByteStorage,
        left: ByteOperand,
        right: ByteOperand,
        carry_in: Option<StorageId>,
        carry_out: Option<StorageId>,
    ) -> Result<(), LowerError> {
        let right = self.prepare_arithmetic_operand(right)?;
        if let Some(carry) = carry_in {
            self.copy_bit_to_carry(carry)?;
        }
        self.load_operand(left)?;
        match (subtract, carry_in.is_some(), right) {
            (false, false, ArithmeticOperand::Immediate(value)) => {
                self.instr(Instr::Add_i8(Expr::num(value as i32)))
            }
            (false, true, ArithmeticOperand::Immediate(value)) => {
                self.instr(Instr::Addc_i8(Expr::num(value as i32)))
            }
            (true, false, ArithmeticOperand::Immediate(value)) => {
                self.instr(Instr::Sub_i8(Expr::num(value as i32)))
            }
            (true, true, ArithmeticOperand::Immediate(value)) => {
                self.instr(Instr::Subc_i8(Expr::num(value as i32)))
            }
            (false, false, ArithmeticOperand::Direct(address)) => {
                self.instr(Instr::Add_d9(direct(address)))
            }
            (false, true, ArithmeticOperand::Direct(address)) => {
                self.instr(Instr::Addc_d9(direct(address)))
            }
            (true, false, ArithmeticOperand::Direct(address)) => {
                self.instr(Instr::Sub_d9(direct(address)))
            }
            (true, true, ArithmeticOperand::Direct(address)) => {
                self.instr(Instr::Subc_d9(direct(address)))
            }
        }
        self.store_accumulator(destination)?;
        if let Some(carry) = carry_out {
            self.copy_carry_to_bit(carry)?;
        }
        Ok(())
    }

    fn prepare_arithmetic_operand(
        &mut self,
        operand: ByteOperand,
    ) -> Result<ArithmeticOperand, LowerError> {
        match operand {
            ByteOperand::Immediate(value) => Ok(ArithmeticOperand::Immediate(value)),
            ByteOperand::Storage(storage) => match self.byte_location(storage)? {
                ByteLocation::Direct(address) => Ok(ArithmeticOperand::Direct(address)),
                _ => {
                    self.materialize_operand(operand, SCRATCH1)?;
                    Ok(ArithmeticOperand::Direct(SCRATCH1))
                }
            },
        }
    }

    fn copy_bit_to_carry(&mut self, storage: StorageId) -> Result<(), LowerError> {
        let location = self.bit_location(storage)?;
        if matches!(location, BitLocation::Carry) {
            return Ok(());
        }
        let set = self.fresh_label("carry_set");
        let done = self.fresh_label("carry_ready");
        self.set_bit_location(BitLocation::Carry, false);
        self.branch_if_bit_location(location, true, &set);
        self.jmpf(&done);
        self.emit_label(&set);
        self.set_bit_location(BitLocation::Carry, true);
        self.emit_label(&done);
        Ok(())
    }

    fn copy_carry_to_bit(&mut self, storage: StorageId) -> Result<(), LowerError> {
        if matches!(self.bit_location(storage)?, BitLocation::Carry) {
            return Ok(());
        }
        let set = self.fresh_label("carry_out_set");
        let done = self.fresh_label("carry_out_done");
        self.set_bit(storage, false)?;
        self.branch_if_bit_location(BitLocation::Carry, true, &set);
        self.jmpf(&done);
        self.emit_label(&set);
        self.set_bit(storage, true)?;
        self.emit_label(&done);
        Ok(())
    }

    fn multiply(
        &mut self,
        left_high: ByteOperand,
        left_low: ByteOperand,
        right: ByteOperand,
        result_high: ByteStorage,
        result_middle: ByteStorage,
        result_low: ByteStorage,
    ) -> Result<(), LowerError> {
        self.load_operand(left_low)?;
        self.instr(Instr::St_d9(direct(C)));
        self.load_operand(right)?;
        self.instr(Instr::St_d9(direct(B)));
        self.load_operand(left_high)?;
        self.instr(Instr::Mul);
        self.copy_direct_to_storage(B, result_high)?;
        self.copy_direct_to_storage(ACC, result_middle)?;
        self.copy_direct_to_storage(C, result_low)
    }

    fn divide(
        &mut self,
        dividend_high: ByteOperand,
        dividend_low: ByteOperand,
        divisor: ByteOperand,
        quotient_high: ByteStorage,
        quotient_low: ByteStorage,
        remainder: ByteStorage,
    ) -> Result<(), LowerError> {
        self.load_operand(dividend_low)?;
        self.instr(Instr::St_d9(direct(C)));
        self.load_operand(divisor)?;
        self.instr(Instr::St_d9(direct(B)));
        self.load_operand(dividend_high)?;
        self.instr(Instr::Div);
        self.copy_direct_to_storage(ACC, quotient_high)?;
        self.copy_direct_to_storage(C, quotient_low)?;
        self.copy_direct_to_storage(B, remainder)
    }

    fn copy_direct_to_storage(
        &mut self,
        source: u16,
        destination: ByteStorage,
    ) -> Result<(), LowerError> {
        self.instr(Instr::Ld_d9(direct(source)));
        self.store_accumulator(destination)
    }

    fn rotate(
        &mut self,
        direction: RotateDirection,
        through_carry: bool,
        destination: ByteStorage,
        input: ByteOperand,
        carry_in: Option<StorageId>,
        carry_out: Option<StorageId>,
    ) -> Result<(), LowerError> {
        if let Some(carry) = carry_in {
            self.copy_bit_to_carry(carry)?;
        }
        self.load_operand(input)?;
        self.instr(match (direction, through_carry) {
            (RotateDirection::Left, false) => Instr::Rol,
            (RotateDirection::Left, true) => Instr::Rolc,
            (RotateDirection::Right, false) => Instr::Ror,
            (RotateDirection::Right, true) => Instr::Rorc,
        });
        self.store_accumulator(destination)?;
        if let Some(carry) = carry_out {
            self.copy_carry_to_bit(carry)?;
        }
        Ok(())
    }

    fn load_indirect(
        &mut self,
        destination: ByteStorage,
        address: &Address,
    ) -> Result<(), LowerError> {
        match address.space() {
            AddressSpace::MainMemory => self.load_main_memory(address)?,
            AddressSpace::WorkRam | AddressSpace::ShadowStack => {
                self.select_dynamic_work_address(address)?;
                self.instr(Instr::Ld_d9(direct(VTRBF)));
            }
            AddressSpace::MainRam(RamBank::Application) => {
                self.select_indirect_ram_address(address)?;
                self.instr(Instr::Ld_Ri(IndirectionMode::R0));
            }
            AddressSpace::SpecialFunctionRegisters | AddressSpace::XRam => {
                self.select_indirect_sfr_address(address)?;
                self.instr(Instr::Ld_Ri(IndirectionMode::R2));
            }
            space => return Err(LowerError::UnsupportedAddressSpace(space)),
        }
        self.store_accumulator(destination)
    }

    fn store_indirect(&mut self, address: &Address, value: ByteOperand) -> Result<(), LowerError> {
        match address.space() {
            AddressSpace::MainMemory | AddressSpace::WorkRam | AddressSpace::ShadowStack => {
                self.select_dynamic_work_address(address)?;
                self.load_operand(value)?;
                self.instr(Instr::St_d9(direct(VTRBF)));
            }
            AddressSpace::MainRam(RamBank::Application) => {
                self.select_indirect_ram_address(address)?;
                self.load_operand(value)?;
                self.instr(Instr::St_Ri(IndirectionMode::R0));
            }
            AddressSpace::SpecialFunctionRegisters | AddressSpace::XRam => {
                self.select_indirect_sfr_address(address)?;
                self.load_operand(value)?;
                self.instr(Instr::St_Ri(IndirectionMode::R2));
            }
            space => return Err(LowerError::UnsupportedAddressSpace(space)),
        }
        self.selected_work_address = None;
        Ok(())
    }

    fn load_main_memory(&mut self, address: &Address) -> Result<(), LowerError> {
        self.materialize_dynamic_address(address)?;
        let flash = self.fresh_label("load_flash");
        let done = self.fresh_label("load_memory_done");

        // Work RAM occupies 0000h-01ffh. Packaged code and read-only data begin
        // at or above 0200h, so the upper seven bits select the access path.
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::And_i8(Expr::num(0xfe)));
        self.instr(Instr::Bnz(Expr::name(&flash)));

        self.instr(Instr::Ld_d9(direct(SCRATCH0)));
        self.instr(Instr::St_d9(direct(VRMAD1)));
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::And_i8(Expr::num(1)));
        self.instr(Instr::St_d9(direct(VRMAD2)));
        self.instr(Instr::Ld_d9(direct(VTRBF)));
        self.jmpf(&done);

        self.emit_label(&flash);
        self.instr(Instr::Ld_d9(direct(SCRATCH0)));
        self.instr(Instr::St_d9(direct(TRL)));
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::St_d9(direct(TRH)));
        self.load_code_byte();
        self.emit_label(&done);
        Ok(())
    }

    fn materialize_dynamic_address(&mut self, address: &Address) -> Result<(), LowerError> {
        let low = address
            .bytes()
            .first()
            .copied()
            .unwrap_or(ByteOperand::Immediate(0));
        let high = address
            .bytes()
            .get(1)
            .copied()
            .unwrap_or(ByteOperand::Immediate(0));
        let offset = address.offset();
        self.materialize_operand(low, SCRATCH0)?;
        self.materialize_operand(high, SCRATCH1)?;
        self.instr(Instr::Ld_d9(direct(SCRATCH0)));
        self.instr(Instr::Add_i8(Expr::num((offset & 0xff) as i32)));
        self.instr(Instr::St_d9(direct(SCRATCH0)));
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::Addc_i8(Expr::num((offset >> 8) as i32)));
        self.instr(Instr::St_d9(direct(SCRATCH1)));
        Ok(())
    }

    fn select_dynamic_work_address(&mut self, address: &Address) -> Result<(), LowerError> {
        let low = address
            .bytes()
            .first()
            .copied()
            .unwrap_or(ByteOperand::Immediate(0));
        let high = address
            .bytes()
            .get(1)
            .copied()
            .unwrap_or(ByteOperand::Immediate(0));
        let offset = address.offset();
        // Both address bytes must be captured before writing VRMAD: either
        // source may itself reside in work RAM and therefore use VRMAD.
        self.materialize_operand(low, SCRATCH0)?;
        self.materialize_operand(high, SCRATCH1)?;
        self.instr(Instr::Ld_d9(direct(SCRATCH0)));
        self.instr(Instr::Add_i8(Expr::num((offset & 0xff) as i32)));
        self.instr(Instr::St_d9(direct(VRMAD1)));
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::Addc_i8(Expr::num((offset >> 8) as i32)));
        self.instr(Instr::And_i8(Expr::num(1)));
        self.instr(Instr::St_d9(direct(VRMAD2)));
        self.selected_work_address = None;
        Ok(())
    }

    fn select_indirect_ram_address(&mut self, address: &Address) -> Result<(), LowerError> {
        let low = address
            .bytes()
            .first()
            .copied()
            .unwrap_or(ByteOperand::Immediate(0));
        self.load_operand(low)?;
        self.instr(Instr::Add_i8(Expr::num(address.offset() as u8 as i32)));
        self.instr(Instr::St_d9(direct(0x00)));
        Ok(())
    }

    fn select_indirect_sfr_address(&mut self, address: &Address) -> Result<(), LowerError> {
        let low = address
            .bytes()
            .first()
            .copied()
            .unwrap_or(ByteOperand::Immediate(0));
        self.load_operand(low)?;
        self.instr(Instr::Add_i8(Expr::num(address.offset() as u8 as i32)));
        self.instr(Instr::St_d9(direct(R2)));
        Ok(())
    }

    fn push_operand(&mut self, operand: ByteOperand) -> Result<(), LowerError> {
        let address = match operand {
            ByteOperand::Storage(storage) => match self.byte_location(storage)? {
                ByteLocation::Direct(address) => address,
                _ => {
                    self.materialize_operand(operand, SCRATCH0)?;
                    SCRATCH0
                }
            },
            ByteOperand::Immediate(_) => {
                self.materialize_operand(operand, SCRATCH0)?;
                SCRATCH0
            }
        };
        self.instr(Instr::Push(direct(address)));
        Ok(())
    }

    fn pop_storage(&mut self, destination: ByteStorage) -> Result<(), LowerError> {
        match self.byte_location(destination)? {
            ByteLocation::Direct(address) => self.instr(Instr::Pop(direct(address))),
            _ => {
                self.instr(Instr::Pop(direct(SCRATCH0)));
                self.instr(Instr::Ld_d9(direct(SCRATCH0)));
                self.store_accumulator(destination)?;
            }
        }
        Ok(())
    }

    fn address_of(
        &mut self,
        destination_low: ByteStorage,
        destination_high: ByteStorage,
        storage: StorageId,
        offset: usize,
    ) -> Result<(), LowerError> {
        let address = self.storage_address_expression(storage, offset)?;
        self.load_expression(lower_byte(address.clone()));
        self.store_accumulator(destination_low)?;
        self.load_expression(upper_byte(address));
        self.store_accumulator(destination_high)
    }

    fn function_address(
        &mut self,
        destination_low: ByteStorage,
        destination_high: ByteStorage,
        function: &str,
    ) -> Result<(), LowerError> {
        let label = self.function_label(function)?.to_owned();
        self.instr(Instr::Mov_d9(lower_byte(Expr::name(&label)), direct(ACC)));
        self.store_accumulator(destination_low)?;
        self.instr(Instr::Mov_d9(upper_byte(Expr::name(&label)), direct(ACC)));
        self.store_accumulator(destination_high)
    }

    fn shift_bytes(
        &mut self,
        op: ShiftOp,
        destinations: &[ByteStorage],
        value: &[ByteOperand],
        amount: &[ByteOperand],
        width: u8,
    ) -> Result<(), LowerError> {
        for (destination, source) in destinations.iter().zip(value) {
            self.copy_byte(*destination, *source)?;
        }
        if width == 0 || destinations.is_empty() {
            return Ok(());
        }

        if let Some(amount) = immediate_integer(amount) {
            for _ in 0..amount % width as u128 {
                self.shift_once(op, destinations, width)?;
            }
        } else {
            let loop_label = self.fresh_label("shift_loop");
            let body = self.fresh_label("shift_body");
            let done = self.fresh_label("shift_done");
            self.load_operand(amount[0])?;
            self.instr(Instr::And_i8(Expr::num((width - 1) as i32)));
            self.instr(Instr::St_d9(direct(SCRATCH2)));
            self.emit_label(&loop_label);
            self.instr(Instr::Ld_d9(direct(SCRATCH2)));
            self.instr(Instr::Bnz(Expr::name(&body)));
            self.jmpf(&done);
            self.emit_label(&body);
            self.shift_once(op, destinations, width)?;
            self.instr(Instr::Dec_d9(direct(SCRATCH2)));
            self.jmpf(&loop_label);
            self.emit_label(&done);
        }

        self.mask_unused_high_bits(destinations, width)
    }

    fn shift_once(
        &mut self,
        op: ShiftOp,
        destinations: &[ByteStorage],
        width: u8,
    ) -> Result<(), LowerError> {
        match op {
            ShiftOp::Left => {
                self.set_bit_location(BitLocation::Carry, false);
                for destination in destinations {
                    self.load_storage(*destination)?;
                    self.instr(Instr::Rolc);
                    self.store_accumulator(*destination)?;
                }
            }
            ShiftOp::RightUnsigned => {
                self.set_bit_location(BitLocation::Carry, false);
                for destination in destinations.iter().rev() {
                    self.load_storage(*destination)?;
                    self.instr(Instr::Rorc);
                    self.store_accumulator(*destination)?;
                }
            }
            ShiftOp::RightSigned => {
                let high = *destinations.last().unwrap();
                self.set_carry_from_byte_bit(ByteOperand::Storage(high), (width - 1) % 8)?;
                for destination in destinations.iter().rev() {
                    self.load_storage(*destination)?;
                    self.instr(Instr::Rorc);
                    self.store_accumulator(*destination)?;
                }
            }
            ShiftOp::RotateLeft => {
                let high = *destinations.last().unwrap();
                self.set_carry_from_byte_bit(ByteOperand::Storage(high), (width - 1) % 8)?;
                for destination in destinations {
                    self.load_storage(*destination)?;
                    self.instr(Instr::Rolc);
                    self.store_accumulator(*destination)?;
                }
            }
            ShiftOp::RotateRight => {
                self.set_carry_from_byte_bit(ByteOperand::Storage(destinations[0]), 0)?;
                for destination in destinations.iter().rev() {
                    self.load_storage(*destination)?;
                    self.instr(Instr::Rorc);
                    self.store_accumulator(*destination)?;
                }
            }
        }
        Ok(())
    }

    fn set_carry_from_byte_bit(&mut self, operand: ByteOperand, bit: u8) -> Result<(), LowerError> {
        self.materialize_operand(operand, SCRATCH0)?;
        let set = self.fresh_label("shift_carry_set");
        let done = self.fresh_label("shift_carry_done");
        self.set_bit_location(BitLocation::Carry, false);
        self.instr(Instr::Bp(
            direct(SCRATCH0),
            Expr::num(bit as i32),
            Expr::name(&set),
        ));
        self.jmpf(&done);
        self.emit_label(&set);
        self.set_bit_location(BitLocation::Carry, true);
        self.emit_label(&done);
        Ok(())
    }

    fn mask_unused_high_bits(
        &mut self,
        destinations: &[ByteStorage],
        width: u8,
    ) -> Result<(), LowerError> {
        let used = width % 8;
        if used == 0 {
            return Ok(());
        }
        let high = *destinations.last().unwrap();
        self.load_storage(high)?;
        self.instr(Instr::And_i8(Expr::num(((1u16 << used) - 1) as i32)));
        self.store_accumulator(high)
    }

    fn compare_bytes(
        &mut self,
        op: CompareOp,
        destination: StorageId,
        left: &[ByteOperand],
        right: &[ByteOperand],
        width: u8,
    ) -> Result<(), LowerError> {
        self.set_bit(destination, false)?;
        let set = self.fresh_label("compare_true");
        let done = self.fresh_label("compare_done");
        match op {
            CompareOp::Equal | CompareOp::NotEqual => {
                let mismatch = if op == CompareOp::NotEqual {
                    set.clone()
                } else {
                    done.clone()
                };
                for (left, right) in left.iter().zip(right) {
                    let equal = self.fresh_label("compare_equal_byte");
                    self.branch_if_equal(*left, *right, true, &equal)?;
                    self.jmpf(&mismatch);
                    self.emit_label(&equal);
                }
                if op == CompareOp::Equal {
                    self.jmpf(&set);
                } else {
                    self.jmpf(&done);
                }
            }
            _ => {
                let (left, right, signed, or_equal) = normalize_ordering(op, left, right);
                if signed {
                    self.signed_sign_difference(left, right, width, &set, &done)?;
                }
                for (left, right) in left.iter().zip(right).rev() {
                    let equal = self.fresh_label("compare_order_equal");
                    self.materialize_operand(*left, SCRATCH0)?;
                    self.materialize_operand(*right, SCRATCH1)?;
                    self.instr(Instr::Ld_d9(direct(SCRATCH0)));
                    self.instr(Instr::Be_d9(direct(SCRATCH1), Expr::name(&equal)));
                    self.instr(Instr::Bp(direct(PSW), Expr::num(7), Expr::name(&set)));
                    self.jmpf(&done);
                    self.emit_label(&equal);
                }
                self.jmpf(if or_equal { &set } else { &done });
            }
        }
        self.emit_label(&set);
        self.set_bit(destination, true)?;
        self.emit_label(&done);
        Ok(())
    }

    fn signed_sign_difference(
        &mut self,
        left: &[ByteOperand],
        right: &[ByteOperand],
        width: u8,
        less: &str,
        not_less: &str,
    ) -> Result<(), LowerError> {
        let bit = (width - 1) % 8;
        let left_negative = self.fresh_label("left_negative");
        let same_sign = self.fresh_label("same_sign");
        self.materialize_operand(*left.last().unwrap(), SCRATCH0)?;
        self.materialize_operand(*right.last().unwrap(), SCRATCH1)?;
        self.instr(Instr::Bp(
            direct(SCRATCH0),
            Expr::num(bit as i32),
            Expr::name(&left_negative),
        ));
        self.instr(Instr::Bp(
            direct(SCRATCH1),
            Expr::num(bit as i32),
            Expr::name(not_less),
        ));
        self.jmpf(&same_sign);
        self.emit_label(&left_negative);
        self.instr(Instr::Bp(
            direct(SCRATCH1),
            Expr::num(bit as i32),
            Expr::name(&same_sign),
        ));
        self.jmpf(less);
        self.emit_label(&same_sign);
        Ok(())
    }

    fn select_byte(
        &mut self,
        destination: ByteStorage,
        condition: BitOperand,
        when_true: ByteOperand,
        when_false: ByteOperand,
    ) -> Result<(), LowerError> {
        let true_label = self.fresh_label("select_byte_true");
        let done = self.fresh_label("select_byte_done");
        self.branch_if_bit_operand(condition, true, &true_label)?;
        self.copy_byte(destination, when_false)?;
        self.jmpf(&done);
        self.emit_label(&true_label);
        self.copy_byte(destination, when_true)?;
        self.emit_label(&done);
        Ok(())
    }

    fn select_bit(
        &mut self,
        destination: StorageId,
        condition: BitOperand,
        when_true: BitOperand,
        when_false: BitOperand,
    ) -> Result<(), LowerError> {
        let true_label = self.fresh_label("select_bit_true");
        let done = self.fresh_label("select_bit_done");
        self.branch_if_bit_operand(condition, true, &true_label)?;
        self.copy_bit(destination, when_false)?;
        self.jmpf(&done);
        self.emit_label(&true_label);
        self.copy_bit(destination, when_true)?;
        self.emit_label(&done);
        Ok(())
    }

    fn bit_to_byte(
        &mut self,
        destination: ByteStorage,
        source: BitOperand,
    ) -> Result<(), LowerError> {
        let set = self.fresh_label("bit_to_byte_set");
        let done = self.fresh_label("bit_to_byte_done");
        self.copy_byte(destination, ByteOperand::Immediate(0))?;
        self.branch_if_bit_operand(source, true, &set)?;
        self.jmpf(&done);
        self.emit_label(&set);
        self.copy_byte(destination, ByteOperand::Immediate(1))?;
        self.emit_label(&done);
        Ok(())
    }

    fn byte_to_bit(
        &mut self,
        destination: StorageId,
        source: ByteOperand,
    ) -> Result<(), LowerError> {
        let set = self.fresh_label("byte_to_bit_set");
        let done = self.fresh_label("byte_to_bit_done");
        self.set_bit(destination, false)?;
        self.load_operand(source)?;
        self.instr(Instr::Bnz(Expr::name(&set)));
        self.jmpf(&done);
        self.emit_label(&set);
        self.set_bit(destination, true)?;
        self.emit_label(&done);
        Ok(())
    }

    fn sign_extend(
        &mut self,
        destinations: &[ByteStorage],
        source: &[ByteOperand],
        source_width: u8,
    ) -> Result<(), LowerError> {
        for (destination, source) in destinations.iter().zip(source) {
            self.copy_byte(*destination, *source)?;
        }
        if destinations.len() <= source.len() {
            return Ok(());
        }
        let negative = self.fresh_label("sign_extend_negative");
        let done = self.fresh_label("sign_extend_done");
        self.materialize_operand(*source.last().unwrap(), SCRATCH0)?;
        self.instr(Instr::Bp(
            direct(SCRATCH0),
            Expr::num(((source_width - 1) % 8) as i32),
            Expr::name(&negative),
        ));
        for destination in &destinations[source.len()..] {
            self.copy_byte(*destination, ByteOperand::Immediate(0))?;
        }
        self.jmpf(&done);
        self.emit_label(&negative);
        for destination in &destinations[source.len()..] {
            self.copy_byte(*destination, ByteOperand::Immediate(0xff))?;
        }
        self.emit_label(&done);
        self.mask_unused_high_bits(destinations, destinations.len() as u8 * 8)
    }

    fn integer_unary(
        &mut self,
        op: IntegerUnaryBytesOp,
        destinations: &[ByteStorage],
        input: &[ByteOperand],
        width: u8,
    ) -> Result<(), LowerError> {
        for destination in destinations {
            self.copy_byte(*destination, ByteOperand::Immediate(0))?;
        }
        match op {
            IntegerUnaryBytesOp::PopulationCount => {
                for (byte_index, input) in input.iter().enumerate() {
                    self.materialize_operand(*input, SCRATCH0)?;
                    for bit in 0..8 {
                        if byte_index * 8 + bit >= width as usize {
                            break;
                        }
                        let increment = self.fresh_label("popcount_increment");
                        let next = self.fresh_label("popcount_next");
                        self.instr(Instr::Bp(
                            direct(SCRATCH0),
                            Expr::num(bit as i32),
                            Expr::name(&increment),
                        ));
                        self.jmpf(&next);
                        self.emit_label(&increment);
                        self.increment_bytes(destinations)?;
                        self.emit_label(&next);
                    }
                }
            }
            IntegerUnaryBytesOp::CountLeadingZeros | IntegerUnaryBytesOp::CountTrailingZeros => {
                let done = self.fresh_label("bit_count_done");
                let positions: Vec<usize> = match op {
                    IntegerUnaryBytesOp::CountLeadingZeros => (0..width as usize).rev().collect(),
                    IntegerUnaryBytesOp::CountTrailingZeros => (0..width as usize).collect(),
                    IntegerUnaryBytesOp::PopulationCount => unreachable!(),
                };
                for position in positions {
                    let byte = position / 8;
                    let bit = position % 8;
                    self.materialize_operand(input[byte], SCRATCH0)?;
                    self.instr(Instr::Bp(
                        direct(SCRATCH0),
                        Expr::num(bit as i32),
                        Expr::name(&done),
                    ));
                    self.increment_bytes(destinations)?;
                }
                self.emit_label(&done);
            }
        }
        Ok(())
    }

    fn increment_bytes(&mut self, bytes: &[ByteStorage]) -> Result<(), LowerError> {
        for (index, byte) in bytes.iter().enumerate() {
            self.load_storage(*byte)?;
            self.instr(if index == 0 {
                Instr::Add_i8(Expr::num(1))
            } else {
                Instr::Addc_i8(Expr::num(0))
            });
            self.store_accumulator(*byte)?;
        }
        Ok(())
    }

    fn integer_binary(
        &mut self,
        op: IntegerBinaryBytesOp,
        destinations: &[ByteStorage],
        left: &[ByteOperand],
        right: &[ByteOperand],
        _width: u8,
    ) -> Result<(), LowerError> {
        match op {
            IntegerBinaryBytesOp::Multiply => self.wide_multiply(destinations, left, right),
            IntegerBinaryBytesOp::SaturatingAddUnsigned => {
                self.saturating_unsigned(destinations, left, right, false)
            }
            IntegerBinaryBytesOp::SaturatingSubtractUnsigned => {
                self.saturating_unsigned(destinations, left, right, true)
            }
            IntegerBinaryBytesOp::DivideUnsigned
            | IntegerBinaryBytesOp::DivideSigned
            | IntegerBinaryBytesOp::RemainderUnsigned
            | IntegerBinaryBytesOp::RemainderSigned => Err(LowerError::UnsupportedInstruction(
                "wide integer division and remainder",
            )),
            IntegerBinaryBytesOp::SaturatingAddSigned
            | IntegerBinaryBytesOp::SaturatingSubtractSigned => Err(
                LowerError::UnsupportedInstruction("signed saturating wide arithmetic"),
            ),
        }
    }

    fn wide_multiply(
        &mut self,
        destinations: &[ByteStorage],
        left: &[ByteOperand],
        right: &[ByteOperand],
    ) -> Result<(), LowerError> {
        for destination in destinations {
            self.copy_byte(*destination, ByteOperand::Immediate(0))?;
        }
        for (left_index, left) in left.iter().enumerate() {
            for (right_index, right) in right.iter().enumerate() {
                let result_index = left_index + right_index;
                if result_index >= destinations.len() {
                    continue;
                }
                self.load_operand(*left)?;
                self.instr(Instr::St_d9(direct(C)));
                self.load_operand(*right)?;
                self.instr(Instr::St_d9(direct(B)));
                self.load_immediate(0);
                self.instr(Instr::Mul);
                self.instr(Instr::Ld_d9(direct(C)));
                self.instr(Instr::St_d9(direct(SCRATCH0)));
                self.instr(Instr::Ld_d9(direct(ACC)));
                self.instr(Instr::St_d9(direct(SCRATCH1)));

                self.load_storage(destinations[result_index])?;
                self.instr(Instr::Add_d9(direct(SCRATCH0)));
                self.store_accumulator(destinations[result_index])?;
                if result_index + 1 < destinations.len() {
                    self.load_storage(destinations[result_index + 1])?;
                    self.instr(Instr::Addc_d9(direct(SCRATCH1)));
                    self.store_accumulator(destinations[result_index + 1])?;
                    for destination in &destinations[result_index + 2..] {
                        self.load_storage(*destination)?;
                        self.instr(Instr::Addc_i8(Expr::num(0)));
                        self.store_accumulator(*destination)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn saturating_unsigned(
        &mut self,
        destinations: &[ByteStorage],
        left: &[ByteOperand],
        right: &[ByteOperand],
        subtract: bool,
    ) -> Result<(), LowerError> {
        for index in 0..destinations.len() {
            let right = self.prepare_arithmetic_operand(right[index])?;
            self.load_operand(left[index])?;
            let with_carry = index != 0;
            match (subtract, with_carry, right) {
                (false, false, ArithmeticOperand::Immediate(value)) => {
                    self.instr(Instr::Add_i8(Expr::num(value as i32)))
                }
                (false, true, ArithmeticOperand::Immediate(value)) => {
                    self.instr(Instr::Addc_i8(Expr::num(value as i32)))
                }
                (true, false, ArithmeticOperand::Immediate(value)) => {
                    self.instr(Instr::Sub_i8(Expr::num(value as i32)))
                }
                (true, true, ArithmeticOperand::Immediate(value)) => {
                    self.instr(Instr::Subc_i8(Expr::num(value as i32)))
                }
                (false, false, ArithmeticOperand::Direct(address)) => {
                    self.instr(Instr::Add_d9(direct(address)))
                }
                (false, true, ArithmeticOperand::Direct(address)) => {
                    self.instr(Instr::Addc_d9(direct(address)))
                }
                (true, false, ArithmeticOperand::Direct(address)) => {
                    self.instr(Instr::Sub_d9(direct(address)))
                }
                (true, true, ArithmeticOperand::Direct(address)) => {
                    self.instr(Instr::Subc_d9(direct(address)))
                }
            }
            self.store_accumulator(destinations[index])?;
        }
        let saturated = self.fresh_label("saturate_unsigned");
        let done = self.fresh_label("saturate_done");
        self.instr(Instr::Bp(direct(PSW), Expr::num(7), Expr::name(&saturated)));
        self.jmpf(&done);
        self.emit_label(&saturated);
        for destination in destinations {
            self.copy_byte(
                *destination,
                ByteOperand::Immediate(if subtract { 0 } else { 0xff }),
            )?;
        }
        self.emit_label(&done);
        Ok(())
    }

    fn branch(
        &mut self,
        condition: &BranchCondition,
        when_true: BlockId,
        when_false: BlockId,
    ) -> Result<(), LowerError> {
        let when_true = self.block_label(when_true);
        self.branch_if_condition(condition, &when_true)?;
        self.jmpf(self.block_label(when_false));
        Ok(())
    }

    fn branch_if_condition(
        &mut self,
        condition: &BranchCondition,
        target: &str,
    ) -> Result<(), LowerError> {
        match condition {
            BranchCondition::BitSet(storage) => self.branch_if_bit(*storage, true, target),
            BranchCondition::BitClear(storage) => self.branch_if_bit(*storage, false, target),
            BranchCondition::ByteZero(operand) => {
                self.load_operand(*operand)?;
                self.branch_macro("bzo", vec![Arg::Ex(Expr::name(target))]);
                Ok(())
            }
            BranchCondition::ByteNonZero(operand) => {
                self.load_operand(*operand)?;
                self.branch_macro("bnzo", vec![Arg::Ex(Expr::name(target))]);
                Ok(())
            }
            BranchCondition::CarrySet(storage) => self.branch_if_bit(*storage, true, target),
            BranchCondition::CarryClear(storage) => self.branch_if_bit(*storage, false, target),
            BranchCondition::Equal { left, right } => {
                self.branch_if_equal(*left, *right, true, target)
            }
            BranchCondition::NotEqual { left, right } => {
                self.branch_if_equal(*left, *right, false, target)
            }
        }
    }

    fn branch_if_bit(
        &mut self,
        storage: StorageId,
        set: bool,
        target: &str,
    ) -> Result<(), LowerError> {
        let location = self.bit_location(storage)?;
        self.branch_if_bit_location(location, set, target);
        Ok(())
    }

    fn branch_if_bit_operand(
        &mut self,
        operand: BitOperand,
        set: bool,
        target: &str,
    ) -> Result<(), LowerError> {
        match operand {
            BitOperand::Constant(value) => {
                if value == set {
                    self.jmpf(target);
                }
                Ok(())
            }
            BitOperand::Storage(storage) => self.branch_if_bit(storage, set, target),
        }
    }

    fn branch_if_bit_location(&mut self, location: BitLocation, set: bool, target: &str) {
        let (address, bit) = match location {
            BitLocation::Direct { address, bit } => (address, bit),
            BitLocation::WorkRam { address, bit } => {
                self.select_work_address(address);
                (VTRBF, bit)
            }
            BitLocation::Carry => (PSW, 7),
        };
        self.branch_macro(
            if set { "bpo" } else { "bno" },
            vec![
                Arg::Ex(direct(address)),
                Arg::Ex(Expr::num(bit as i32)),
                Arg::Ex(Expr::name(target)),
            ],
        );
    }

    fn branch_macro(&mut self, name: &str, args: Vec<Arg>) {
        self.output.push_macro_call(name, args);
    }

    fn branch_if_equal(
        &mut self,
        left: ByteOperand,
        right: ByteOperand,
        equal: bool,
        target: &str,
    ) -> Result<(), LowerError> {
        self.materialize_operand(left, SCRATCH0)?;
        self.materialize_operand(right, SCRATCH1)?;
        self.instr(Instr::Ld_d9(direct(SCRATCH0)));
        self.branch_macro(
            if equal { "beo" } else { "bneo" },
            vec![Arg::Ex(direct(SCRATCH1)), Arg::Ex(Expr::name(target))],
        );
        Ok(())
    }

    fn switch(
        &mut self,
        value: &[ByteOperand],
        _width: u8,
        cases: &[(u128, BlockId)],
        default: BlockId,
    ) -> Result<(), LowerError> {
        for (case, target) in cases {
            let next_case = self.fresh_label("switch_next");
            for (index, value) in value.iter().enumerate() {
                let equal = self.fresh_label("switch_byte_equal");
                let expected = ((*case >> (index * 8)) & 0xff) as u8;
                self.branch_if_equal(*value, ByteOperand::Immediate(expected), true, &equal)?;
                self.jmpf(&next_case);
                self.emit_label(&equal);
            }
            self.jmpf(self.block_label(*target));
            self.emit_label(&next_case);
        }
        self.jmpf(self.block_label(default));
        Ok(())
    }

    fn call_indirect(&mut self, target: &CodeAddress) -> Result<(), LowerError> {
        match target {
            CodeAddress::Function(function) => {
                let label = self.function_label(function)?.to_owned();
                self.instr(Instr::Callf(Expr::name(&label)));
            }
            CodeAddress::Computed { low, high } => {
                self.materialize_operand(*low, SCRATCH0)?;
                self.materialize_operand(*high, SCRATCH1)?;
                let invoke = self.fresh_label("indirect_call");
                let done = self.fresh_label("indirect_call_done");
                self.instr(Instr::Callf(Expr::name(&invoke)));
                self.jmpf(&done);
                self.emit_label(&invoke);
                self.instr(Instr::Push(direct(SCRATCH0)));
                self.instr(Instr::Push(direct(SCRATCH1)));
                self.instr(Instr::Ret);
                self.emit_label(&done);
            }
        }
        Ok(())
    }

    fn tail_call_indirect(&mut self, target: &CodeAddress) -> Result<(), LowerError> {
        match target {
            CodeAddress::Function(function) => {
                let label = self.function_label(function)?.to_owned();
                self.jmpf(label);
            }
            CodeAddress::Computed { low, high } => {
                self.materialize_operand(*low, SCRATCH0)?;
                self.materialize_operand(*high, SCRATCH1)?;
                self.instr(Instr::Push(direct(SCRATCH0)));
                self.instr(Instr::Push(direct(SCRATCH1)));
                self.instr(Instr::Ret);
            }
        }
        Ok(())
    }

    fn intrinsic(
        &mut self,
        operation: Intrinsic,
        inputs: &[ValueOperand],
        outputs: &[ValueStorage],
    ) -> Result<(), LowerError> {
        match operation {
            Intrinsic::SfrRead(sfr) => {
                let [ValueStorage::Byte(output)] = outputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                if !inputs.is_empty() {
                    return Err(LowerError::MalformedIntrinsic(operation));
                }
                self.instr(Instr::Ld_d9(direct(sfr_address(sfr))));
                self.store_accumulator(*output)
            }
            Intrinsic::SfrWrite(sfr) => {
                let [ValueOperand::Byte(input)] = inputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                if !outputs.is_empty() {
                    return Err(LowerError::MalformedIntrinsic(operation));
                }
                self.load_operand(*input)?;
                self.instr(Instr::St_d9(direct(sfr_address(sfr))));
                Ok(())
            }
            Intrinsic::SfrBitRead(sfr) => {
                let [ValueOperand::Byte(bit)] = inputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                let [ValueStorage::Byte(output)] = outputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                self.sfr_bit_read(sfr, *bit, *output)
            }
            Intrinsic::SfrBitWrite(sfr) => {
                let [ValueOperand::Byte(bit), ValueOperand::Byte(value)] = inputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                if !outputs.is_empty() {
                    return Err(LowerError::MalformedIntrinsic(operation));
                }
                self.sfr_bit_write(sfr, *bit, *value)
            }
            Intrinsic::SfrBitToggle(sfr) => {
                let [ValueOperand::Byte(bit)] = inputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                if !outputs.is_empty() {
                    return Err(LowerError::MalformedIntrinsic(operation));
                }
                self.sfr_bit_toggle(sfr, *bit)
            }
            Intrinsic::XramRead => {
                let [ValueOperand::Byte(bank), ValueOperand::Byte(offset)] = inputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                let [ValueStorage::Byte(output)] = outputs else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                self.select_xram(*bank, *offset)?;
                self.instr(Instr::Ld_Ri(IndirectionMode::R2));
                self.store_accumulator(*output)
            }
            Intrinsic::XramWrite => {
                let [
                    ValueOperand::Byte(bank),
                    ValueOperand::Byte(offset),
                    ValueOperand::Byte(value),
                ] = inputs
                else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                if !outputs.is_empty() {
                    return Err(LowerError::MalformedIntrinsic(operation));
                }
                self.select_xram(*bank, *offset)?;
                self.load_operand(*value)?;
                self.instr(Instr::St_Ri(IndirectionMode::R2));
                Ok(())
            }
            Intrinsic::XramCopyFromMain => {
                let [
                    ValueOperand::Byte(bank),
                    ValueOperand::Byte(offset),
                    ValueOperand::Byte(source_low),
                    ValueOperand::Byte(source_high),
                    ValueOperand::Byte(length),
                ] = inputs
                else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                if !outputs.is_empty() {
                    return Err(LowerError::MalformedIntrinsic(operation));
                }
                self.xram_copy(true, *bank, *offset, *source_low, *source_high, *length)
            }
            Intrinsic::XramCopyToMain => {
                let [
                    ValueOperand::Byte(destination_low),
                    ValueOperand::Byte(destination_high),
                    ValueOperand::Byte(bank),
                    ValueOperand::Byte(offset),
                    ValueOperand::Byte(length),
                ] = inputs
                else {
                    return Err(LowerError::MalformedIntrinsic(operation));
                };
                if !outputs.is_empty() {
                    return Err(LowerError::MalformedIntrinsic(operation));
                }
                self.xram_copy(
                    false,
                    *bank,
                    *offset,
                    *destination_low,
                    *destination_high,
                    *length,
                )
            }
        }
    }

    fn select_xram(&mut self, bank: ByteOperand, offset: ByteOperand) -> Result<(), LowerError> {
        self.load_operand(bank)?;
        self.instr(Instr::St_d9(direct(XBNK)));
        self.load_operand(offset)?;
        self.instr(Instr::Add_i8(Expr::num(0x80)));
        self.instr(Instr::St_d9(direct(R2)));
        Ok(())
    }

    fn xram_copy(
        &mut self,
        from_main: bool,
        bank: ByteOperand,
        offset: ByteOperand,
        work_low: ByteOperand,
        work_high: ByteOperand,
        length: ByteOperand,
    ) -> Result<(), LowerError> {
        if from_main {
            return self.xram_copy_from_memory(bank, offset, work_low, work_high, length);
        }

        self.select_xram(bank, offset)?;
        self.load_operand(work_low)?;
        self.instr(Instr::St_d9(direct(VRMAD1)));
        self.load_operand(work_high)?;
        self.instr(Instr::And_i8(Expr::num(1)));
        self.instr(Instr::St_d9(direct(VRMAD2)));
        self.materialize_operand(length, SCRATCH2)?;
        self.instr(Instr::Set1(direct(VSEL), Expr::num(4)));

        let loop_label = self.fresh_label("xram_copy_loop");
        let body = self.fresh_label("xram_copy_body");
        let done = self.fresh_label("xram_copy_done");
        self.emit_label(&loop_label);
        self.instr(Instr::Ld_d9(direct(SCRATCH2)));
        self.instr(Instr::Bnz(Expr::name(&body)));
        self.jmpf(&done);
        self.emit_label(&body);
        self.instr(Instr::Ld_Ri(IndirectionMode::R2));
        self.instr(Instr::St_d9(direct(VTRBF)));
        self.instr(Instr::Inc_d9(direct(R2)));
        self.instr(Instr::Dec_d9(direct(SCRATCH2)));
        self.jmpf(&loop_label);
        self.emit_label(&done);
        self.instr(Instr::Clr1(direct(VSEL), Expr::num(4)));
        Ok(())
    }

    fn xram_copy_from_memory(
        &mut self,
        bank: ByteOperand,
        offset: ByteOperand,
        source_low: ByteOperand,
        source_high: ByteOperand,
        length: ByteOperand,
    ) -> Result<(), LowerError> {
        self.materialize_operand(source_low, SCRATCH0)?;
        self.materialize_operand(source_high, SCRATCH1)?;
        self.materialize_operand(length, SCRATCH2)?;
        self.select_xram(bank, offset)?;

        let flash_setup = self.fresh_label("xram_copy_flash");
        let work_loop = self.fresh_label("xram_copy_work_loop");
        let work_body = self.fresh_label("xram_copy_work_body");
        let work_done = self.fresh_label("xram_copy_work_done");
        let flash_loop = self.fresh_label("xram_copy_flash_loop");
        let flash_body = self.fresh_label("xram_copy_flash_body");
        let flash_incremented = self.fresh_label("xram_copy_flash_incremented");
        let done = self.fresh_label("xram_copy_done");

        // A lifted pointer below 0200h names work RAM; all higher addresses
        // name the read-only data emitted with the packaged program.
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::And_i8(Expr::num(0xfe)));
        self.instr(Instr::Bnz(Expr::name(&flash_setup)));

        self.instr(Instr::Ld_d9(direct(SCRATCH0)));
        self.instr(Instr::St_d9(direct(VRMAD1)));
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::And_i8(Expr::num(1)));
        self.instr(Instr::St_d9(direct(VRMAD2)));
        self.instr(Instr::Set1(direct(VSEL), Expr::num(4)));
        self.emit_label(&work_loop);
        self.instr(Instr::Ld_d9(direct(SCRATCH2)));
        self.instr(Instr::Bnz(Expr::name(&work_body)));
        self.jmpf(&work_done);
        self.emit_label(&work_body);
        self.instr(Instr::Ld_d9(direct(VTRBF)));
        self.instr(Instr::St_Ri(IndirectionMode::R2));
        self.instr(Instr::Inc_d9(direct(R2)));
        self.instr(Instr::Dec_d9(direct(SCRATCH2)));
        self.jmpf(&work_loop);
        self.emit_label(&work_done);
        self.instr(Instr::Clr1(direct(VSEL), Expr::num(4)));
        self.jmpf(&done);

        self.emit_label(&flash_setup);
        self.instr(Instr::Ld_d9(direct(SCRATCH0)));
        self.instr(Instr::St_d9(direct(TRL)));
        self.instr(Instr::Ld_d9(direct(SCRATCH1)));
        self.instr(Instr::St_d9(direct(TRH)));
        self.emit_label(&flash_loop);
        self.instr(Instr::Ld_d9(direct(SCRATCH2)));
        self.instr(Instr::Bnz(Expr::name(&flash_body)));
        self.jmpf(&done);
        self.emit_label(&flash_body);
        self.load_code_byte();
        self.instr(Instr::St_Ri(IndirectionMode::R2));
        self.instr(Instr::Inc_d9(direct(TRL)));
        self.instr(Instr::Ld_d9(direct(TRL)));
        self.instr(Instr::Bnz(Expr::name(&flash_incremented)));
        self.instr(Instr::Inc_d9(direct(TRH)));
        self.emit_label(&flash_incremented);
        self.instr(Instr::Inc_d9(direct(R2)));
        self.instr(Instr::Dec_d9(direct(SCRATCH2)));
        self.jmpf(&flash_loop);
        self.emit_label(&done);
        Ok(())
    }

    fn sfr_bit_read(
        &mut self,
        sfr: Sfr,
        bit: ByteOperand,
        output: ByteStorage,
    ) -> Result<(), LowerError> {
        self.copy_byte(output, ByteOperand::Immediate(0))?;
        let set = self.fresh_label("sfr_bit_read_set");
        let done = self.fresh_label("sfr_bit_read_done");
        let cases = self.sfr_bit_dispatch(bit, &done)?;
        for (index, label) in cases.into_iter().enumerate() {
            self.emit_label(&label);
            self.instr(Instr::Bp(
                direct(sfr_address(sfr)),
                Expr::num(index as i32),
                Expr::name(&set),
            ));
            self.jmpf(&done);
        }
        self.emit_label(&set);
        self.copy_byte(output, ByteOperand::Immediate(1))?;
        self.emit_label(&done);
        Ok(())
    }

    fn sfr_bit_write(
        &mut self,
        sfr: Sfr,
        bit: ByteOperand,
        value: ByteOperand,
    ) -> Result<(), LowerError> {
        let done = self.fresh_label("sfr_bit_write_done");
        let cases = self.sfr_bit_dispatch(bit, &done)?;
        for (index, label) in cases.into_iter().enumerate() {
            let set = self.fresh_label("sfr_bit_write_set");
            self.emit_label(&label);
            self.load_operand(value)?;
            self.instr(Instr::Bnz(Expr::name(&set)));
            self.instr(Instr::Clr1(
                direct(sfr_address(sfr)),
                Expr::num(index as i32),
            ));
            self.jmpf(&done);
            self.emit_label(&set);
            self.instr(Instr::Set1(
                direct(sfr_address(sfr)),
                Expr::num(index as i32),
            ));
            self.jmpf(&done);
        }
        self.emit_label(&done);
        Ok(())
    }

    fn sfr_bit_toggle(&mut self, sfr: Sfr, bit: ByteOperand) -> Result<(), LowerError> {
        let done = self.fresh_label("sfr_bit_toggle_done");
        let cases = self.sfr_bit_dispatch(bit, &done)?;
        for (index, label) in cases.into_iter().enumerate() {
            self.emit_label(&label);
            self.instr(Instr::Not1(
                direct(sfr_address(sfr)),
                Expr::num(index as i32),
            ));
            self.jmpf(&done);
        }
        self.emit_label(&done);
        Ok(())
    }

    fn sfr_bit_dispatch(
        &mut self,
        bit: ByteOperand,
        invalid: &str,
    ) -> Result<Vec<String>, LowerError> {
        let mut cases = Vec::with_capacity(8);
        for index in 0..8 {
            let label = self.fresh_label("sfr_bit_case");
            self.branch_if_equal(bit, ByteOperand::Immediate(index), true, &label)?;
            cases.push(label);
        }
        self.jmpf(invalid);
        Ok(cases)
    }
}

#[derive(Clone, Copy)]
enum ArithmeticOperand {
    Immediate(u8),
    Direct(u16),
}

fn direct(address: u16) -> Expr {
    Expr::num(address as i32)
}

fn lower_byte(expression: Expr) -> Expr {
    Expr::LowerByte(crate::location::Span::default(), Box::new(expression))
}

fn upper_byte(expression: Expr) -> Expr {
    Expr::UpperByte(crate::location::Span::default(), Box::new(expression))
}

fn immediate_integer(bytes: &[ByteOperand]) -> Option<u128> {
    bytes
        .iter()
        .enumerate()
        .try_fold(0u128, |value, (index, byte)| match byte {
            ByteOperand::Immediate(byte) => Some(value | (*byte as u128) << (index * 8)),
            ByteOperand::Storage(_) => None,
        })
}

fn normalize_ordering<'a>(
    op: CompareOp,
    left: &'a [ByteOperand],
    right: &'a [ByteOperand],
) -> (&'a [ByteOperand], &'a [ByteOperand], bool, bool) {
    match op {
        CompareOp::LessThanUnsigned => (left, right, false, false),
        CompareOp::LessThanSigned => (left, right, true, false),
        CompareOp::LessThanOrEqualUnsigned => (left, right, false, true),
        CompareOp::LessThanOrEqualSigned => (left, right, true, true),
        CompareOp::GreaterThanUnsigned => (right, left, false, false),
        CompareOp::GreaterThanSigned => (right, left, true, false),
        CompareOp::GreaterThanOrEqualUnsigned => (right, left, false, true),
        CompareOp::GreaterThanOrEqualSigned => (right, left, true, true),
        CompareOp::Equal | CompareOp::NotEqual => unreachable!(),
    }
}

fn special_register_address(register: super::lir::SpecialRegister) -> u16 {
    use super::lir::SpecialRegister;
    match register {
        SpecialRegister::ProgramStatus => PSW,
        SpecialRegister::B => B,
        SpecialRegister::C => C,
        SpecialRegister::TableReferenceLow => TRL,
        SpecialRegister::TableReferenceHigh => TRH,
        SpecialRegister::StackPointer => 0x106,
    }
}

fn xram_bank(bank: super::lir::XRamBank) -> u8 {
    use super::lir::XRamBank;
    match bank {
        XRamBank::DisplayTop => 0,
        XRamBank::DisplayBottom => 1,
        XRamBank::Icons => 2,
    }
}

fn storage_aliases(storage: &Storage<PhysicalPlacement>) -> Vec<(String, u16)> {
    let Some(source_name) = &storage.name else {
        return Vec::new();
    };
    let owner = match &storage.owner {
        StorageOwner::Program => "program".to_owned(),
        StorageOwner::Function(function) => sanitize_label(function),
    };
    let base = format!(
        "{}_{}_s{}",
        owner,
        sanitize_label(source_name),
        storage.id.0
    );
    match storage.placement {
        PhysicalPlacement::ApplicationRam { offset } => {
            let length = match storage.kind {
                StorageKind::Byte => 1,
                StorageKind::Bytes { length } => length.get(),
                StorageKind::Bit | StorageKind::Carry => 0,
            };
            (0..length)
                .map(|index| (format!("{base}_b{index}"), offset as u16 + index as u16))
                .collect()
        }
        PhysicalPlacement::ApplicationBit { offset, bit } => {
            vec![(format!("{base}_bit{}", bit.get()), offset as u16)]
        }
        _ => Vec::new(),
    }
}

fn sfr_address(sfr: Sfr) -> u16 {
    match sfr {
        Sfr::InterruptEnable => 0x108,
        Sfr::InterruptPriority => 0x109,
        Sfr::OscillationControl => 0x10e,
        Sfr::Timer0Control => 0x110,
        Sfr::Timer0Prescaler => 0x111,
        Sfr::Timer0Low => 0x112,
        Sfr::Timer0LowReload => 0x113,
        Sfr::Timer0High => 0x114,
        Sfr::Timer0HighReload => 0x115,
        Sfr::LcdModeControl => 0x120,
        Sfr::LcdStartAddress => 0x122,
        Sfr::LcdContrastControl => 0x127,
        Sfr::Port3 => 0x14c,
        Sfr::Port7 => 0x15c,
        Sfr::BaseTimerControl => 0x17f,
    }
}

fn all_functions(program: &AllocatedProgram) -> Vec<&Function> {
    let mut functions = vec![&program.main];
    functions.extend(interrupt_functions(program));
    functions.extend(program.functions.iter());
    functions
}

pub(super) fn function_labels(program: &AllocatedProgram) -> BTreeMap<String, String> {
    let mut used = BTreeMap::<String, usize>::new();
    let mut labels = BTreeMap::new();
    for function in all_functions(program) {
        let base = sanitize_label(&function.name);
        let occurrence = used.entry(base.clone()).or_default();
        let label = if *occurrence == 0 {
            base
        } else {
            format!("{base}_{}", occurrence)
        };
        *occurrence += 1;
        labels.insert(function.name.clone(), label);
    }
    labels
}

fn interrupt_functions(program: &AllocatedProgram) -> Vec<&Function> {
    let handlers = &program.interrupt_handlers;
    [
        handlers.external_0.as_ref(),
        handlers.external_1.as_ref(),
        handlers.external_2_or_timer_0_low.as_ref(),
        handlers.external_3_or_base_timer.as_ref(),
        handlers.timer_0_high.as_ref(),
        handlers.timer_1.as_ref(),
        handlers.serial_0.as_ref(),
        handlers.serial_1.as_ref(),
        handlers.dreamcast_interface.as_ref(),
        handlers.port_3.as_ref(),
    ]
    .into_iter()
    .flatten()
    .collect()
}

fn sanitize_label(name: &str) -> String {
    let mut label = String::with_capacity(name.len() + 5);
    for character in name.chars() {
        if character.is_ascii_alphanumeric() || character == '_' {
            label.push(character);
        } else {
            label.push('_');
        }
    }
    if label.is_empty() || label.as_bytes()[0].is_ascii_digit() {
        label.insert_str(0, "wasm_");
    }
    label
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use super::lower;
    use crate::wasm::intrinsics::Intrinsic;
    use crate::wasm::lir::{
        Block, BlockId, BranchCondition, ByteOperand, ByteStorage, Function, FunctionAbi,
        FunctionKind, Instruction, InstructionKind, InterruptHandlers, PhysicalPlacement, Program,
        Static, StaticByte, Storage, StorageId, StorageKind, StorageOwner, Terminator,
        TerminatorKind, ValueOperand,
    };

    fn returning_function(name: &str, kind: FunctionKind) -> Function {
        Function {
            name: name.into(),
            kind,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: Some("entry".into()),
                instructions: Vec::new(),
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        }
    }

    #[test]
    fn emits_far_calls_and_jumps() {
        let program = Program::<PhysicalPlacement> {
            main: returning_function("main", FunctionKind::Normal),
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: Vec::new(),
        };

        let output = lower(&program).unwrap().to_string();
        assert!(output.contains("jmpf  .block0"));
        assert!(output.contains("main:"));
        assert!(output.contains("ret"));
    }

    #[test]
    fn emits_long_range_branch_macros_for_cfg_edges() {
        let mut main = returning_function("main", FunctionKind::Normal);
        main.blocks = vec![
            Block {
                id: BlockId(0),
                name: Some("entry".into()),
                instructions: Vec::new(),
                terminator: Terminator {
                    kind: TerminatorKind::Branch {
                        condition: BranchCondition::ByteNonZero(ByteOperand::Immediate(1)),
                        when_true: BlockId(1),
                        when_false: BlockId(2),
                    },
                },
            },
            Block {
                id: BlockId(1),
                name: Some("true".into()),
                instructions: Vec::new(),
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            },
            Block {
                id: BlockId(2),
                name: Some("false".into()),
                instructions: Vec::new(),
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            },
        ];
        let program = Program::<PhysicalPlacement> {
            main,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: Vec::new(),
        };

        let output = lower(&program).unwrap().to_string();
        assert!(output.contains("  bnzo .block1"));
        assert!(output.contains("  jmpf  .block2"));
    }

    #[test]
    fn emits_readable_application_ram_aliases() {
        let program = Program::<PhysicalPlacement> {
            main: returning_function("main", FunctionKind::Normal),
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: vec![Storage {
                id: StorageId(7),
                name: Some("running total".into()),
                owner: StorageOwner::Function("main".into()),
                kind: StorageKind::Byte,
                placement: PhysicalPlacement::ApplicationRam { offset: 0x20 },
            }],
        };

        let output = lower(&program).unwrap().to_string();
        assert!(output.contains("main_running_total_s7_b0"));
    }

    #[test]
    fn emits_flash_statics_without_runtime_initialization() {
        let mut main = returning_function("main", FunctionKind::Normal);
        main.blocks[0].instructions = vec![
            Instruction {
                kind: InstructionKind::CopyByte {
                    destination: ByteStorage {
                        storage: StorageId(1),
                        offset: 0,
                    },
                    source: ByteOperand::Storage(ByteStorage {
                        storage: StorageId(0),
                        offset: 0,
                    }),
                },
            },
            Instruction {
                kind: InstructionKind::Intrinsic {
                    operation: Intrinsic::XramCopyFromMain,
                    inputs: vec![
                        ValueOperand::Byte(ByteOperand::Immediate(0)),
                        ValueOperand::Byte(ByteOperand::Immediate(0)),
                        ValueOperand::Byte(ByteOperand::Immediate(0)),
                        ValueOperand::Byte(ByteOperand::Immediate(2)),
                        ValueOperand::Byte(ByteOperand::Immediate(1)),
                    ],
                    outputs: Vec::new(),
                },
            },
        ];
        let program = Program::<PhysicalPlacement> {
            main,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: vec![Static {
                name: "message".into(),
                storage: StorageId(0),
                initializer: vec![
                    StaticByte::Value(0x12),
                    StaticByte::Value(0x34),
                    StaticByte::Value(0x56),
                ],
                mutable: false,
            }],
            storages: vec![
                Storage {
                    id: StorageId(0),
                    name: Some("message".into()),
                    owner: StorageOwner::Program,
                    kind: StorageKind::Bytes {
                        length: NonZeroUsize::new(3).unwrap(),
                    },
                    placement: PhysicalPlacement::Flash,
                },
                Storage {
                    id: StorageId(1),
                    name: Some("loaded byte".into()),
                    owner: StorageOwner::Function("main".into()),
                    kind: StorageKind::Byte,
                    placement: PhysicalPlacement::ApplicationRam { offset: 0x20 },
                },
            ],
        };

        let output = lower(&program).unwrap().to_string();

        assert!(output.contains("__waterbear_static_0_message:"));
        assert!(output.contains(".byte $12,$34,$56"));
        assert!(!output.contains("initialize compiler-owned work RAM"));

        let lines = output.lines().collect::<Vec<_>>();
        let code_loads = lines
            .iter()
            .enumerate()
            .filter(|(_, line)| line.trim() == "ldc")
            .collect::<Vec<_>>();
        assert_eq!(code_loads.len(), 2);
        for (index, _) in code_loads {
            assert_eq!(lines[index - 1].trim(), "mov   #$00, $0100");
        }
    }
}
