//! Shared data-flow queries for VMU LIR optimization and allocation.

use std::collections::{BTreeMap, BTreeSet};

use super::lir::{
    Address, BitOperand, BlockId, BranchCondition, ByteOperand, ByteStorage, CodeAddress, Function,
    FunctionAbi, InstructionKind, InterruptHandlers, Program, StorageId, TerminatorKind,
    ValueOperand, ValueStorage, VirtualPlacement,
};

pub type StorageSet = BTreeSet<StorageId>;
pub type InterferenceGraph = BTreeMap<StorageId, StorageSet>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StorageUnit {
    Byte(ByteStorage),
    Bit(StorageId),
    /// A whole object whose individual byte lane is not known.
    Object(StorageId),
}

impl StorageUnit {
    pub fn storage(self) -> StorageId {
        match self {
            Self::Byte(byte) => byte.storage,
            Self::Bit(storage) | Self::Object(storage) => storage,
        }
    }
}

pub type UnitSet = BTreeSet<StorageUnit>;

#[derive(Clone, Debug, Default)]
pub struct ProgramSemantics {
    functions: BTreeMap<String, FunctionAbi>,
}

impl ProgramSemantics {
    pub fn new(program: &Program<VirtualPlacement>) -> Self {
        let functions = functions(program)
            .map(|function| (function.name.clone(), function.abi.clone()))
            .collect();
        Self { functions }
    }

    fn abi(&self, function: &str) -> Option<&FunctionAbi> {
        self.functions.get(function)
    }
}

#[derive(Clone, Debug, Default)]
pub struct FunctionLiveness {
    pub live_in: BTreeMap<BlockId, UnitSet>,
    pub live_out: BTreeMap<BlockId, UnitSet>,
}

pub fn liveness(function: &Function, semantics: &ProgramSemantics) -> FunctionLiveness {
    let mut result = FunctionLiveness::default();
    for block in &function.blocks {
        result.live_in.insert(block.id, UnitSet::new());
        result.live_out.insert(block.id, UnitSet::new());
    }

    loop {
        let mut changed = false;
        for block in function.blocks.iter().rev() {
            let live_out = successors(&block.terminator.kind)
                .into_iter()
                .filter_map(|successor| result.live_in.get(&successor))
                .flatten()
                .copied()
                .collect::<UnitSet>();
            let mut live_in = live_out.clone();
            apply_transfer(
                &mut live_in,
                &terminator_unit_definitions(&block.terminator.kind, semantics),
                &terminator_unit_uses(&block.terminator.kind, function, semantics),
            );
            for instruction in block.instructions.iter().rev() {
                apply_transfer(
                    &mut live_in,
                    &instruction_unit_definitions(&instruction.kind, semantics),
                    &instruction_unit_uses(&instruction.kind, semantics),
                );
            }

            changed |= result.live_out.get(&block.id) != Some(&live_out);
            changed |= result.live_in.get(&block.id) != Some(&live_in);
            result.live_out.insert(block.id, live_out);
            result.live_in.insert(block.id, live_in);
        }
        if !changed {
            return result;
        }
    }
}

pub fn interference_graph(
    program: &Program<VirtualPlacement>,
) -> (InterferenceGraph, BTreeSet<(StorageId, StorageId)>) {
    let semantics = ProgramSemantics::new(program);
    let mut graph = InterferenceGraph::new();
    let mut copies = BTreeSet::new();

    for function in functions(program) {
        let liveness = liveness(function, &semantics);
        for block in &function.blocks {
            let mut live = liveness.live_out[&block.id].clone();
            apply_transfer(
                &mut live,
                &terminator_unit_definitions(&block.terminator.kind, &semantics),
                &terminator_unit_uses(&block.terminator.kind, function, &semantics),
            );
            for instruction in block.instructions.iter().rev() {
                let definitions = instruction_unit_definitions(&instruction.kind, &semantics);
                let uses = instruction_unit_uses(&instruction.kind, &semantics);
                let copy_source = copy_pair(&instruction.kind).map(|(destination, source)| {
                    let pair = ordered_pair(destination, source);
                    copies.insert(pair);
                    source
                });
                for definition in &definitions {
                    for other in live.iter().copied().filter(|other| {
                        other.storage() != definition.storage()
                            && Some(other.storage()) != copy_source
                    }) {
                        add_interference(&mut graph, definition.storage(), other.storage());
                    }
                }
                apply_transfer(&mut live, &definitions, &uses);
            }
        }
    }

    // An object whose address escapes needs a stable location for its owner's
    // entire activation. This is intentionally stronger than the ordinary
    // value liveness of the AddressOf instruction.
    let addressed = address_taken(program);
    for storage in &program.storages {
        if !addressed.contains(&storage.id) {
            continue;
        }
        for other in &program.storages {
            if storage.owner == other.owner && storage.id != other.id {
                add_interference(&mut graph, storage.id, other.id);
            }
        }
    }

    (graph, copies)
}

pub fn instruction_unit_uses(
    instruction: &InstructionKind,
    semantics: &ProgramSemantics,
) -> UnitSet {
    let mut uses = UnitSet::new();
    match instruction {
        InstructionKind::CopyByte { source, .. } => unit_byte_operand(&mut uses, *source),
        InstructionKind::CopyBit { source, .. } => unit_bit_operand(&mut uses, *source),
        InstructionKind::SetBit(_) | InstructionKind::ClearBit(_) => {}
        InstructionKind::ToggleBit(storage) => {
            uses.insert(StorageUnit::Bit(*storage));
        }
        InstructionKind::ByteUnary { input, .. } => unit_byte_operand(&mut uses, *input),
        InstructionKind::ByteBinary { left, right, .. }
        | InstructionKind::Add { left, right, .. }
        | InstructionKind::Subtract { left, right, .. } => {
            unit_byte_operand(&mut uses, *left);
            unit_byte_operand(&mut uses, *right);
            match instruction {
                InstructionKind::Add { carry_in, .. } => unit_optional_bit(&mut uses, *carry_in),
                InstructionKind::Subtract { borrow_in, .. } => {
                    unit_optional_bit(&mut uses, *borrow_in)
                }
                _ => {}
            }
        }
        InstructionKind::Multiply {
            left_high,
            left_low,
            right,
            ..
        } => {
            unit_byte_operand(&mut uses, *left_high);
            unit_byte_operand(&mut uses, *left_low);
            unit_byte_operand(&mut uses, *right);
        }
        InstructionKind::Divide {
            dividend_high,
            dividend_low,
            divisor,
            ..
        } => {
            unit_byte_operand(&mut uses, *dividend_high);
            unit_byte_operand(&mut uses, *dividend_low);
            unit_byte_operand(&mut uses, *divisor);
        }
        InstructionKind::Rotate {
            input, carry_in, ..
        } => {
            unit_byte_operand(&mut uses, *input);
            unit_optional_bit(&mut uses, *carry_in);
        }
        InstructionKind::LoadIndirect { address, .. } => unit_address(&mut uses, address),
        InstructionKind::StoreIndirect { address, value } => {
            unit_address(&mut uses, address);
            unit_byte_operand(&mut uses, *value);
        }
        InstructionKind::Push(value) => unit_byte_operand(&mut uses, *value),
        InstructionKind::Pop(_) | InstructionKind::FunctionAddress { .. } => {}
        InstructionKind::AddressOf { storage, .. } => {
            uses.insert(StorageUnit::Object(*storage));
        }
        InstructionKind::ShiftBytes { value, amount, .. } => {
            unit_byte_operands(&mut uses, value);
            unit_byte_operands(&mut uses, amount);
        }
        InstructionKind::CompareBytes { left, right, .. }
        | InstructionKind::IntegerBinaryBytes { left, right, .. } => {
            unit_byte_operands(&mut uses, left);
            unit_byte_operands(&mut uses, right);
        }
        InstructionKind::SelectByte {
            condition,
            when_true,
            when_false,
            ..
        } => {
            unit_bit_operand(&mut uses, *condition);
            unit_byte_operand(&mut uses, *when_true);
            unit_byte_operand(&mut uses, *when_false);
        }
        InstructionKind::SelectBit {
            condition,
            when_true,
            when_false,
            ..
        } => {
            unit_bit_operand(&mut uses, *condition);
            unit_bit_operand(&mut uses, *when_true);
            unit_bit_operand(&mut uses, *when_false);
        }
        InstructionKind::BitToByte { source, .. } => unit_bit_operand(&mut uses, *source),
        InstructionKind::ByteToBit { source, .. } => unit_byte_operand(&mut uses, *source),
        InstructionKind::SignExtend { source, .. } => unit_byte_operands(&mut uses, source),
        InstructionKind::IntegerUnaryBytes { input, .. } => unit_byte_operands(&mut uses, input),
        InstructionKind::Call { function } => {
            if let Some(abi) = semantics.abi(function) {
                unit_abi_values(&mut uses, &abi.parameters);
            }
        }
        InstructionKind::CallIndirect { target, abi } => {
            unit_code_address(&mut uses, target);
            unit_abi_values(&mut uses, &abi.parameters);
        }
        InstructionKind::Intrinsic { inputs, .. } => {
            for input in inputs {
                unit_value_operand(&mut uses, *input);
            }
        }
    }
    uses
}

pub fn instruction_unit_definitions(
    instruction: &InstructionKind,
    semantics: &ProgramSemantics,
) -> UnitSet {
    let mut definitions = UnitSet::new();
    match instruction {
        InstructionKind::CopyByte { destination, .. }
        | InstructionKind::ByteUnary { destination, .. }
        | InstructionKind::ByteBinary { destination, .. }
        | InstructionKind::LoadIndirect { destination, .. }
        | InstructionKind::Pop(destination)
        | InstructionKind::BitToByte { destination, .. }
        | InstructionKind::SelectByte { destination, .. } => {
            definitions.insert(StorageUnit::Byte(*destination));
        }
        InstructionKind::CopyBit { destination, .. }
        | InstructionKind::SetBit(destination)
        | InstructionKind::ClearBit(destination)
        | InstructionKind::ToggleBit(destination)
        | InstructionKind::ByteToBit { destination, .. }
        | InstructionKind::CompareBytes { destination, .. }
        | InstructionKind::SelectBit { destination, .. } => {
            definitions.insert(StorageUnit::Bit(*destination));
        }
        InstructionKind::Add {
            destination,
            carry_out,
            ..
        }
        | InstructionKind::Subtract {
            destination,
            borrow_out: carry_out,
            ..
        }
        | InstructionKind::Rotate {
            destination,
            carry_out,
            ..
        } => {
            definitions.insert(StorageUnit::Byte(*destination));
            unit_optional_bit(&mut definitions, *carry_out);
        }
        InstructionKind::Multiply {
            result_high,
            result_middle,
            result_low,
            ..
        } => unit_byte_destinations(
            &mut definitions,
            [*result_high, *result_middle, *result_low],
        ),
        InstructionKind::Divide {
            quotient_high,
            quotient_low,
            remainder,
            ..
        } => unit_byte_destinations(
            &mut definitions,
            [*quotient_high, *quotient_low, *remainder],
        ),
        InstructionKind::AddressOf {
            destination_low,
            destination_high,
            ..
        }
        | InstructionKind::FunctionAddress {
            destination_low,
            destination_high,
            ..
        } => unit_byte_destinations(&mut definitions, [*destination_low, *destination_high]),
        InstructionKind::ShiftBytes { destinations, .. }
        | InstructionKind::SignExtend { destinations, .. }
        | InstructionKind::IntegerUnaryBytes { destinations, .. }
        | InstructionKind::IntegerBinaryBytes { destinations, .. } => {
            unit_byte_destinations(&mut definitions, destinations.iter().copied())
        }
        InstructionKind::Call { function } => {
            if let Some(abi) = semantics.abi(function) {
                unit_abi_values(&mut definitions, &abi.results);
                definitions.extend(abi.clobbered.iter().copied().map(StorageUnit::Object));
            }
        }
        InstructionKind::CallIndirect { abi, .. } => {
            unit_abi_values(&mut definitions, &abi.results);
            definitions.extend(abi.clobbered.iter().copied().map(StorageUnit::Object));
        }
        InstructionKind::Intrinsic { outputs, .. } => {
            for output in outputs {
                unit_value_storage(&mut definitions, *output);
            }
        }
        InstructionKind::StoreIndirect { .. } | InstructionKind::Push(_) => {}
    }
    definitions
}

pub fn terminator_unit_uses(
    terminator: &TerminatorKind,
    function: &Function,
    semantics: &ProgramSemantics,
) -> UnitSet {
    let mut uses = UnitSet::new();
    match terminator {
        TerminatorKind::Jump(_) | TerminatorKind::Trap | TerminatorKind::Unreachable => {}
        TerminatorKind::Branch { condition, .. } => unit_branch_condition(&mut uses, condition),
        TerminatorKind::Switch { value, .. } => unit_byte_operands(&mut uses, value),
        TerminatorKind::TailCall { function } => {
            if let Some(abi) = semantics.abi(function) {
                unit_abi_values(&mut uses, &abi.parameters);
            }
        }
        TerminatorKind::TailCallIndirect { target, abi } => {
            unit_code_address(&mut uses, target);
            unit_abi_values(&mut uses, &abi.parameters);
        }
        TerminatorKind::Return => unit_abi_values(&mut uses, &function.abi.results),
    }
    uses
}

pub fn terminator_unit_definitions(
    _terminator: &TerminatorKind,
    _semantics: &ProgramSemantics,
) -> UnitSet {
    UnitSet::new()
}

pub fn instruction_uses(instruction: &InstructionKind, semantics: &ProgramSemantics) -> StorageSet {
    let mut uses = StorageSet::new();
    match instruction {
        InstructionKind::CopyByte { source, .. } => byte_operand(&mut uses, *source),
        InstructionKind::CopyBit { source, .. } => bit_operand(&mut uses, *source),
        InstructionKind::SetBit(_) | InstructionKind::ClearBit(_) => {}
        InstructionKind::ToggleBit(storage) => {
            uses.insert(*storage);
        }
        InstructionKind::ByteUnary { input, .. } => byte_operand(&mut uses, *input),
        InstructionKind::ByteBinary { left, right, .. }
        | InstructionKind::Add { left, right, .. }
        | InstructionKind::Subtract { left, right, .. } => {
            byte_operand(&mut uses, *left);
            byte_operand(&mut uses, *right);
            match instruction {
                InstructionKind::Add { carry_in, .. } => optional_storage(&mut uses, *carry_in),
                InstructionKind::Subtract { borrow_in, .. } => {
                    optional_storage(&mut uses, *borrow_in)
                }
                _ => {}
            }
        }
        InstructionKind::Multiply {
            left_high,
            left_low,
            right,
            ..
        } => {
            byte_operand(&mut uses, *left_high);
            byte_operand(&mut uses, *left_low);
            byte_operand(&mut uses, *right);
        }
        InstructionKind::Divide {
            dividend_high,
            dividend_low,
            divisor,
            ..
        } => {
            byte_operand(&mut uses, *dividend_high);
            byte_operand(&mut uses, *dividend_low);
            byte_operand(&mut uses, *divisor);
        }
        InstructionKind::Rotate {
            input, carry_in, ..
        } => {
            byte_operand(&mut uses, *input);
            optional_storage(&mut uses, *carry_in);
        }
        InstructionKind::LoadIndirect { address, .. } => address_operands(&mut uses, address),
        InstructionKind::StoreIndirect { address, value } => {
            address_operands(&mut uses, address);
            byte_operand(&mut uses, *value);
        }
        InstructionKind::Push(value) => byte_operand(&mut uses, *value),
        InstructionKind::Pop(_) | InstructionKind::FunctionAddress { .. } => {}
        InstructionKind::AddressOf { storage, .. } => {
            uses.insert(*storage);
        }
        InstructionKind::ShiftBytes { value, amount, .. } => {
            byte_operands(&mut uses, value);
            byte_operands(&mut uses, amount);
        }
        InstructionKind::CompareBytes { left, right, .. }
        | InstructionKind::IntegerBinaryBytes { left, right, .. } => {
            byte_operands(&mut uses, left);
            byte_operands(&mut uses, right);
        }
        InstructionKind::SelectByte {
            condition,
            when_true,
            when_false,
            ..
        } => {
            bit_operand(&mut uses, *condition);
            byte_operand(&mut uses, *when_true);
            byte_operand(&mut uses, *when_false);
        }
        InstructionKind::SelectBit {
            condition,
            when_true,
            when_false,
            ..
        } => {
            bit_operand(&mut uses, *condition);
            bit_operand(&mut uses, *when_true);
            bit_operand(&mut uses, *when_false);
        }
        InstructionKind::BitToByte { source, .. } => bit_operand(&mut uses, *source),
        InstructionKind::ByteToBit { source, .. } => byte_operand(&mut uses, *source),
        InstructionKind::SignExtend { source, .. } => byte_operands(&mut uses, source),
        InstructionKind::IntegerUnaryBytes { input, .. } => byte_operands(&mut uses, input),
        InstructionKind::Call { function } => {
            if let Some(abi) = semantics.abi(function) {
                abi_values(&mut uses, &abi.parameters);
            }
        }
        InstructionKind::CallIndirect { target, abi } => {
            code_address(&mut uses, target);
            abi_values(&mut uses, &abi.parameters);
        }
        InstructionKind::Intrinsic { inputs, .. } => {
            for input in inputs {
                value_operand(&mut uses, *input);
            }
        }
    }
    uses
}

pub fn instruction_definitions(
    instruction: &InstructionKind,
    semantics: &ProgramSemantics,
) -> StorageSet {
    let mut definitions = StorageSet::new();
    match instruction {
        InstructionKind::CopyByte { destination, .. }
        | InstructionKind::ByteUnary { destination, .. }
        | InstructionKind::ByteBinary { destination, .. }
        | InstructionKind::LoadIndirect { destination, .. }
        | InstructionKind::Pop(destination)
        | InstructionKind::BitToByte { destination, .. } => {
            definitions.insert(destination.storage);
        }
        InstructionKind::CopyBit { destination, .. }
        | InstructionKind::SetBit(destination)
        | InstructionKind::ClearBit(destination)
        | InstructionKind::ToggleBit(destination)
        | InstructionKind::ByteToBit { destination, .. }
        | InstructionKind::CompareBytes { destination, .. }
        | InstructionKind::SelectBit { destination, .. } => {
            definitions.insert(*destination);
        }
        InstructionKind::Add {
            destination,
            carry_out,
            ..
        }
        | InstructionKind::Subtract {
            destination,
            borrow_out: carry_out,
            ..
        }
        | InstructionKind::Rotate {
            destination,
            carry_out,
            ..
        } => {
            definitions.insert(destination.storage);
            optional_storage(&mut definitions, *carry_out);
        }
        InstructionKind::Multiply {
            result_high,
            result_middle,
            result_low,
            ..
        } => byte_destinations(
            &mut definitions,
            [*result_high, *result_middle, *result_low],
        ),
        InstructionKind::Divide {
            quotient_high,
            quotient_low,
            remainder,
            ..
        } => byte_destinations(
            &mut definitions,
            [*quotient_high, *quotient_low, *remainder],
        ),
        InstructionKind::AddressOf {
            destination_low,
            destination_high,
            ..
        }
        | InstructionKind::FunctionAddress {
            destination_low,
            destination_high,
            ..
        } => byte_destinations(&mut definitions, [*destination_low, *destination_high]),
        InstructionKind::ShiftBytes { destinations, .. }
        | InstructionKind::SignExtend { destinations, .. }
        | InstructionKind::IntegerUnaryBytes { destinations, .. }
        | InstructionKind::IntegerBinaryBytes { destinations, .. } => {
            byte_destinations(&mut definitions, destinations.iter().copied())
        }
        InstructionKind::SelectByte { destination, .. } => {
            definitions.insert(destination.storage);
        }
        InstructionKind::Call { function } => {
            if let Some(abi) = semantics.abi(function) {
                abi_values(&mut definitions, &abi.results);
                definitions.extend(abi.clobbered.iter().copied());
            }
        }
        InstructionKind::CallIndirect { abi, .. } => {
            abi_values(&mut definitions, &abi.results);
            definitions.extend(abi.clobbered.iter().copied());
        }
        InstructionKind::Intrinsic { outputs, .. } => {
            for output in outputs {
                value_storage(&mut definitions, *output);
            }
        }
        InstructionKind::StoreIndirect { .. } | InstructionKind::Push(_) => {}
    }
    definitions
}

pub fn terminator_uses(
    terminator: &TerminatorKind,
    function: &Function,
    semantics: &ProgramSemantics,
) -> StorageSet {
    let mut uses = StorageSet::new();
    match terminator {
        TerminatorKind::Jump(_) | TerminatorKind::Trap | TerminatorKind::Unreachable => {}
        TerminatorKind::Branch { condition, .. } => branch_condition(&mut uses, condition),
        TerminatorKind::Switch { value, .. } => byte_operands(&mut uses, value),
        TerminatorKind::TailCall { function } => {
            if let Some(abi) = semantics.abi(function) {
                abi_values(&mut uses, &abi.parameters);
            }
        }
        TerminatorKind::TailCallIndirect { target, abi } => {
            code_address(&mut uses, target);
            abi_values(&mut uses, &abi.parameters);
        }
        TerminatorKind::Return => abi_values(&mut uses, &function.abi.results),
    }
    uses
}

pub fn terminator_definitions(
    _terminator: &TerminatorKind,
    _semantics: &ProgramSemantics,
) -> StorageSet {
    StorageSet::new()
}

pub fn has_observable_effect(instruction: &InstructionKind) -> bool {
    match instruction {
        InstructionKind::LoadIndirect { .. }
        | InstructionKind::StoreIndirect { .. }
        | InstructionKind::Push(_)
        | InstructionKind::Pop(_)
        | InstructionKind::Call { .. }
        | InstructionKind::CallIndirect { .. }
        | InstructionKind::Intrinsic { .. }
        | InstructionKind::Divide { .. } => true,
        InstructionKind::IntegerBinaryBytes { op, .. } => matches!(
            op,
            super::lir::IntegerBinaryBytesOp::DivideUnsigned
                | super::lir::IntegerBinaryBytesOp::DivideSigned
                | super::lir::IntegerBinaryBytesOp::RemainderUnsigned
                | super::lir::IntegerBinaryBytesOp::RemainderSigned
        ),
        _ => false,
    }
}

fn address_taken(program: &Program<VirtualPlacement>) -> StorageSet {
    functions(program)
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction.kind {
            InstructionKind::AddressOf { storage, .. } => Some(storage),
            _ => None,
        })
        .collect()
}

fn copy_pair(instruction: &InstructionKind) -> Option<(StorageId, StorageId)> {
    match instruction {
        InstructionKind::CopyByte {
            destination,
            source: ByteOperand::Storage(source),
        } => Some((destination.storage, source.storage)),
        InstructionKind::CopyBit {
            destination,
            source: BitOperand::Storage(source),
        } => Some((*destination, *source)),
        _ => None,
    }
}

fn ordered_pair(left: StorageId, right: StorageId) -> (StorageId, StorageId) {
    if left <= right {
        (left, right)
    } else {
        (right, left)
    }
}

fn add_interference(graph: &mut InterferenceGraph, left: StorageId, right: StorageId) {
    graph.entry(left).or_default().insert(right);
    graph.entry(right).or_default().insert(left);
}

fn apply_transfer(live: &mut UnitSet, definitions: &UnitSet, uses: &UnitSet) {
    live.retain(|storage| !definitions.contains(storage));
    live.extend(uses);
}

fn successors(terminator: &TerminatorKind) -> Vec<BlockId> {
    match terminator {
        TerminatorKind::Jump(target) => vec![*target],
        TerminatorKind::Branch {
            when_true,
            when_false,
            ..
        } => vec![*when_true, *when_false],
        TerminatorKind::Switch { cases, default, .. } => cases
            .iter()
            .map(|(_, block)| *block)
            .chain([*default])
            .collect(),
        TerminatorKind::TailCall { .. }
        | TerminatorKind::TailCallIndirect { .. }
        | TerminatorKind::Return
        | TerminatorKind::Trap
        | TerminatorKind::Unreachable => Vec::new(),
    }
}

fn functions(program: &Program<VirtualPlacement>) -> impl Iterator<Item = &Function> {
    std::iter::once(&program.main)
        .chain(interrupt_functions(&program.interrupt_handlers))
        .chain(program.functions.iter())
}

fn interrupt_functions(handlers: &InterruptHandlers) -> impl Iterator<Item = &Function> {
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
}

fn byte_operand(storages: &mut StorageSet, operand: ByteOperand) {
    if let ByteOperand::Storage(storage) = operand {
        storages.insert(storage.storage);
    }
}

fn unit_byte_operand(storages: &mut UnitSet, operand: ByteOperand) {
    if let ByteOperand::Storage(storage) = operand {
        storages.insert(StorageUnit::Byte(storage));
    }
}

fn unit_byte_operands(storages: &mut UnitSet, operands: &[ByteOperand]) {
    for operand in operands {
        unit_byte_operand(storages, *operand);
    }
}

fn unit_bit_operand(storages: &mut UnitSet, operand: BitOperand) {
    if let BitOperand::Storage(storage) = operand {
        storages.insert(StorageUnit::Bit(storage));
    }
}

fn unit_value_operand(storages: &mut UnitSet, operand: ValueOperand) {
    match operand {
        ValueOperand::Bit(operand) => unit_bit_operand(storages, operand),
        ValueOperand::Byte(operand) => unit_byte_operand(storages, operand),
    }
}

fn unit_value_storage(storages: &mut UnitSet, storage: ValueStorage) {
    storages.insert(match storage {
        ValueStorage::Bit(storage) => StorageUnit::Bit(storage),
        ValueStorage::Byte(storage) => StorageUnit::Byte(storage),
    });
}

fn unit_byte_destinations(
    storages: &mut UnitSet,
    destinations: impl IntoIterator<Item = ByteStorage>,
) {
    storages.extend(destinations.into_iter().map(StorageUnit::Byte));
}

fn unit_optional_bit(storages: &mut UnitSet, storage: Option<StorageId>) {
    storages.extend(storage.map(StorageUnit::Bit));
}

fn unit_address(storages: &mut UnitSet, address: &Address) {
    unit_byte_operands(storages, address.bytes());
}

fn unit_code_address(storages: &mut UnitSet, address: &CodeAddress) {
    if let CodeAddress::Computed { low, high } = address {
        unit_byte_operand(storages, *low);
        unit_byte_operand(storages, *high);
    }
}

fn unit_abi_values(storages: &mut UnitSet, values: &[super::lir::AbiValue]) {
    for value in values {
        for piece in &value.pieces {
            unit_value_storage(storages, *piece);
        }
    }
}

fn unit_branch_condition(storages: &mut UnitSet, condition: &BranchCondition) {
    match condition {
        BranchCondition::BitSet(storage)
        | BranchCondition::BitClear(storage)
        | BranchCondition::CarrySet(storage)
        | BranchCondition::CarryClear(storage) => {
            storages.insert(StorageUnit::Bit(*storage));
        }
        BranchCondition::ByteZero(operand) | BranchCondition::ByteNonZero(operand) => {
            unit_byte_operand(storages, *operand)
        }
        BranchCondition::Equal { left, right } | BranchCondition::NotEqual { left, right } => {
            unit_byte_operand(storages, *left);
            unit_byte_operand(storages, *right);
        }
    }
}

fn byte_operands(storages: &mut StorageSet, operands: &[ByteOperand]) {
    for operand in operands {
        byte_operand(storages, *operand);
    }
}

fn bit_operand(storages: &mut StorageSet, operand: BitOperand) {
    if let BitOperand::Storage(storage) = operand {
        storages.insert(storage);
    }
}

fn value_operand(storages: &mut StorageSet, operand: ValueOperand) {
    match operand {
        ValueOperand::Bit(operand) => bit_operand(storages, operand),
        ValueOperand::Byte(operand) => byte_operand(storages, operand),
    }
}

fn value_storage(storages: &mut StorageSet, storage: ValueStorage) {
    storages.insert(match storage {
        ValueStorage::Bit(storage) => storage,
        ValueStorage::Byte(storage) => storage.storage,
    });
}

fn byte_destinations(
    storages: &mut StorageSet,
    destinations: impl IntoIterator<Item = super::lir::ByteStorage>,
) {
    storages.extend(
        destinations
            .into_iter()
            .map(|destination| destination.storage),
    );
}

fn optional_storage(storages: &mut StorageSet, storage: Option<StorageId>) {
    storages.extend(storage);
}

fn address_operands(storages: &mut StorageSet, address: &Address) {
    byte_operands(storages, address.bytes());
}

fn code_address(storages: &mut StorageSet, address: &CodeAddress) {
    if let CodeAddress::Computed { low, high } = address {
        byte_operand(storages, *low);
        byte_operand(storages, *high);
    }
}

fn abi_values(storages: &mut StorageSet, values: &[super::lir::AbiValue]) {
    for value in values {
        for piece in &value.pieces {
            value_storage(storages, *piece);
        }
    }
}

fn branch_condition(storages: &mut StorageSet, condition: &BranchCondition) {
    match condition {
        BranchCondition::BitSet(storage)
        | BranchCondition::BitClear(storage)
        | BranchCondition::CarrySet(storage)
        | BranchCondition::CarryClear(storage) => {
            storages.insert(*storage);
        }
        BranchCondition::ByteZero(operand) | BranchCondition::ByteNonZero(operand) => {
            byte_operand(storages, *operand)
        }
        BranchCondition::Equal { left, right } | BranchCondition::NotEqual { left, right } => {
            byte_operand(storages, *left);
            byte_operand(storages, *right);
        }
    }
}
