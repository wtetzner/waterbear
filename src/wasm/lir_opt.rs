//! Conservative simplification of virtual VMU LIR.

use std::collections::{BTreeMap, BTreeSet};

use super::{
    lir::{
        BitOperand, BranchCondition, ByteBinaryOp, ByteOperand, ByteStorage, ByteUnaryOp,
        CodeAddress, CompareOp, Function, InstructionKind, Program, RotateDirection, StorageId,
        TerminatorKind, ValueOperand, VirtualPlacement,
    },
    lir_analysis::{
        ProgramSemantics, UnitSet, has_observable_effect, instruction_definitions,
        instruction_unit_definitions, instruction_unit_uses, instruction_uses, liveness,
        terminator_unit_definitions, terminator_unit_uses, terminator_uses,
    },
};

#[derive(Default)]
struct Facts {
    bytes: BTreeMap<ByteStorage, ByteOperand>,
    byte_bits: BTreeMap<ByteStorage, KnownByteBits>,
    bits: BTreeMap<StorageId, BitOperand>,
}

#[derive(Clone, Copy, Default)]
struct KnownByteBits {
    zero: u8,
    one: u8,
}

impl KnownByteBits {
    fn constant(value: u8) -> Self {
        Self {
            zero: !value,
            one: value,
        }
    }
}

pub fn optimize(
    program: &mut Program<VirtualPlacement>,
) -> Result<(), super::lir_validate::ValidationError> {
    super::lir_validate::validate_virtual(program)?;
    for _ in 0..3 {
        let semantics = ProgramSemantics::new(program);
        for_each_function_mut(program, |function| simplify_function(function, &semantics));

        let semantics = ProgramSemantics::new(program);
        let addressed = address_taken(program);
        for_each_function_mut(program, |function| {
            eliminate_dead_writes(function, &semantics, &addressed)
        });
    }
    remove_unused_storages(program);
    super::lir_validate::validate_virtual(program)
}

fn simplify_function(function: &mut Function, semantics: &ProgramSemantics) {
    for block in &mut function.blocks {
        let mut facts = Facts::default();
        for instruction in &mut block.instructions {
            rewrite_instruction(&mut instruction.kind, &facts);
            fold_instruction(&mut instruction.kind, &facts);
            invalidate_definitions(&mut facts, &instruction.kind, semantics);
            record_facts(&mut facts, &instruction.kind);
            if invalidates_memory_facts(&instruction.kind) {
                facts = Facts::default();
            }
        }
        rewrite_terminator(&mut block.terminator.kind, &facts);
    }
}

fn eliminate_dead_writes(
    function: &mut Function,
    semantics: &ProgramSemantics,
    addressed: &BTreeSet<StorageId>,
) {
    let liveness = liveness(function, semantics);
    let function_snapshot = function.clone();
    for block in &mut function.blocks {
        let mut live = liveness.live_out[&block.id].clone();
        apply_transfer(
            &mut live,
            &terminator_unit_definitions(&block.terminator.kind, semantics),
            &terminator_unit_uses(&block.terminator.kind, &function_snapshot, semantics),
        );

        let mut retained = Vec::with_capacity(block.instructions.len());
        for instruction in block.instructions.iter().rev() {
            let definitions = instruction_unit_definitions(&instruction.kind, semantics);
            let uses = instruction_unit_uses(&instruction.kind, semantics);
            let needed = definitions.is_empty()
                || definitions.iter().any(|storage| live.contains(storage))
                || definitions
                    .iter()
                    .any(|storage| addressed.contains(&storage.storage()))
                || has_observable_effect(&instruction.kind);
            if needed {
                apply_transfer(&mut live, &definitions, &uses);
                retained.push(instruction.clone());
            }
        }
        retained.reverse();
        block.instructions = retained;
    }
}

fn rewrite_instruction(instruction: &mut InstructionKind, facts: &Facts) {
    match instruction {
        InstructionKind::CopyByte { source, .. }
        | InstructionKind::ByteUnary { input: source, .. }
        | InstructionKind::Push(source)
        | InstructionKind::ByteToBit { source, .. } => rewrite_byte(source, facts),
        InstructionKind::CopyBit { source, .. } | InstructionKind::BitToByte { source, .. } => {
            rewrite_bit(source, facts)
        }
        InstructionKind::ByteBinary { left, right, .. }
        | InstructionKind::Add { left, right, .. }
        | InstructionKind::Subtract { left, right, .. } => {
            rewrite_byte(left, facts);
            rewrite_byte(right, facts);
        }
        InstructionKind::Multiply {
            left_high,
            left_low,
            right,
            ..
        } => {
            rewrite_byte(left_high, facts);
            rewrite_byte(left_low, facts);
            rewrite_byte(right, facts);
        }
        InstructionKind::Divide {
            dividend_high,
            dividend_low,
            divisor,
            ..
        } => {
            rewrite_byte(dividend_high, facts);
            rewrite_byte(dividend_low, facts);
            rewrite_byte(divisor, facts);
        }
        InstructionKind::Rotate { input, .. } => rewrite_byte(input, facts),
        InstructionKind::StoreIndirect { value, .. } => rewrite_byte(value, facts),
        InstructionKind::ShiftBytes { value, amount, .. } => {
            rewrite_bytes(value, facts);
            rewrite_bytes(amount, facts);
        }
        InstructionKind::CompareBytes { left, right, .. }
        | InstructionKind::IntegerBinaryBytes { left, right, .. } => {
            rewrite_bytes(left, facts);
            rewrite_bytes(right, facts);
        }
        InstructionKind::SelectByte {
            condition,
            when_true,
            when_false,
            ..
        } => {
            rewrite_bit(condition, facts);
            rewrite_byte(when_true, facts);
            rewrite_byte(when_false, facts);
        }
        InstructionKind::SelectBit {
            condition,
            when_true,
            when_false,
            ..
        } => {
            rewrite_bit(condition, facts);
            rewrite_bit(when_true, facts);
            rewrite_bit(when_false, facts);
        }
        InstructionKind::SignExtend { source, .. }
        | InstructionKind::IntegerUnaryBytes { input: source, .. } => rewrite_bytes(source, facts),
        InstructionKind::CallIndirect { target, .. } => rewrite_code_address(target, facts),
        InstructionKind::Intrinsic { inputs, .. } => {
            for input in inputs {
                match input {
                    ValueOperand::Bit(operand) => rewrite_bit(operand, facts),
                    ValueOperand::Byte(operand) => rewrite_byte(operand, facts),
                }
            }
        }
        InstructionKind::SetBit(_)
        | InstructionKind::ClearBit(_)
        | InstructionKind::ToggleBit(_)
        | InstructionKind::LoadIndirect { .. }
        | InstructionKind::Pop(_)
        | InstructionKind::AddressOf { .. }
        | InstructionKind::FunctionAddress { .. }
        | InstructionKind::Call { .. } => {}
    }
}

fn fold_instruction(instruction: &mut InstructionKind, facts: &Facts) {
    let replacement = match instruction {
        InstructionKind::CopyByte {
            destination,
            source: ByteOperand::Storage(source),
        } if *destination == *source => None,
        InstructionKind::CopyBit {
            destination,
            source: BitOperand::Storage(source),
        } if *destination == *source => None,
        InstructionKind::ToggleBit(destination) => Some(InstructionKind::ToggleBit(*destination)),
        InstructionKind::ByteUnary {
            op,
            destination,
            input: ByteOperand::Immediate(input),
        } => Some(InstructionKind::CopyByte {
            destination: *destination,
            source: ByteOperand::Immediate(match op {
                ByteUnaryOp::Not => !*input,
                ByteUnaryOp::Negate => input.wrapping_neg(),
                ByteUnaryOp::Increment => input.wrapping_add(1),
                ByteUnaryOp::Decrement => input.wrapping_sub(1),
            }),
        }),
        InstructionKind::ByteBinary {
            op,
            destination,
            left,
            right,
        } => fold_byte_binary(*op, *destination, *left, *right, facts),
        InstructionKind::Add {
            destination,
            left,
            right,
            carry_in: None,
            carry_out: None,
        } => fold_add_subtract(false, *destination, *left, *right),
        InstructionKind::Subtract {
            destination,
            left,
            right,
            borrow_in: None,
            borrow_out: None,
        } => fold_add_subtract(true, *destination, *left, *right),
        InstructionKind::Rotate {
            direction,
            through_carry: false,
            destination,
            input: ByteOperand::Immediate(input),
            carry_in: None,
            carry_out: None,
        } => Some(InstructionKind::CopyByte {
            destination: *destination,
            source: ByteOperand::Immediate(match direction {
                RotateDirection::Left => input.rotate_left(1),
                RotateDirection::Right => input.rotate_right(1),
            }),
        }),
        InstructionKind::SelectByte {
            destination,
            condition: BitOperand::Constant(condition),
            when_true,
            when_false,
        } => Some(InstructionKind::CopyByte {
            destination: *destination,
            source: if *condition { *when_true } else { *when_false },
        }),
        InstructionKind::SelectBit {
            destination,
            condition: BitOperand::Constant(condition),
            when_true,
            when_false,
        } => Some(InstructionKind::CopyBit {
            destination: *destination,
            source: if *condition { *when_true } else { *when_false },
        }),
        InstructionKind::BitToByte {
            destination,
            source: BitOperand::Constant(value),
        } => Some(InstructionKind::CopyByte {
            destination: *destination,
            source: ByteOperand::Immediate(u8::from(*value)),
        }),
        InstructionKind::ByteToBit {
            destination,
            source: ByteOperand::Immediate(value),
        } => Some(if *value == 0 {
            InstructionKind::ClearBit(*destination)
        } else {
            InstructionKind::SetBit(*destination)
        }),
        InstructionKind::CompareBytes {
            op,
            destination,
            left,
            right,
            width,
        } => fold_compare(*op, *destination, left, right, *width),
        _ => return,
    };

    if let Some(replacement) = replacement {
        *instruction = replacement;
    }
}

fn fold_byte_binary(
    op: ByteBinaryOp,
    destination: ByteStorage,
    left: ByteOperand,
    right: ByteOperand,
    facts: &Facts,
) -> Option<InstructionKind> {
    let value = match (left, right) {
        (ByteOperand::Immediate(left), ByteOperand::Immediate(right)) => {
            ByteOperand::Immediate(match op {
                ByteBinaryOp::And => left & right,
                ByteBinaryOp::Or => left | right,
                ByteBinaryOp::Xor => left ^ right,
            })
        }
        (_, ByteOperand::Immediate(0xff)) if op == ByteBinaryOp::And => left,
        (ByteOperand::Immediate(0xff), _) if op == ByteBinaryOp::And => right,
        (_, ByteOperand::Immediate(0)) if matches!(op, ByteBinaryOp::Or | ByteBinaryOp::Xor) => {
            left
        }
        (ByteOperand::Immediate(0), _) if matches!(op, ByteBinaryOp::Or | ByteBinaryOp::Xor) => {
            right
        }
        (_, ByteOperand::Immediate(0)) | (ByteOperand::Immediate(0), _)
            if op == ByteBinaryOp::And =>
        {
            ByteOperand::Immediate(0)
        }
        (ByteOperand::Storage(left), ByteOperand::Storage(right))
            if left == right && op == ByteBinaryOp::Xor =>
        {
            ByteOperand::Immediate(0)
        }
        (ByteOperand::Storage(left), ByteOperand::Storage(right))
            if left == right && matches!(op, ByteBinaryOp::And | ByteBinaryOp::Or) =>
        {
            ByteOperand::Storage(left)
        }
        (left, ByteOperand::Immediate(mask)) if op == ByteBinaryOp::And => {
            let known = known_byte_bits(left, facts);
            if known.zero | mask == u8::MAX {
                left
            } else {
                return None;
            }
        }
        (ByteOperand::Immediate(mask), right) if op == ByteBinaryOp::And => {
            let known = known_byte_bits(right, facts);
            if known.zero | mask == u8::MAX {
                right
            } else {
                return None;
            }
        }
        (left, ByteOperand::Immediate(mask)) if op == ByteBinaryOp::Or => {
            let known = known_byte_bits(left, facts);
            if known.one & mask == mask {
                left
            } else {
                return None;
            }
        }
        (ByteOperand::Immediate(mask), right) if op == ByteBinaryOp::Or => {
            let known = known_byte_bits(right, facts);
            if known.one & mask == mask {
                right
            } else {
                return None;
            }
        }
        _ => return None,
    };
    Some(InstructionKind::CopyByte {
        destination,
        source: value,
    })
}

fn fold_add_subtract(
    subtract: bool,
    destination: ByteStorage,
    left: ByteOperand,
    right: ByteOperand,
) -> Option<InstructionKind> {
    let value = match (left, right) {
        (ByteOperand::Immediate(left), ByteOperand::Immediate(right)) => {
            ByteOperand::Immediate(if subtract {
                left.wrapping_sub(right)
            } else {
                left.wrapping_add(right)
            })
        }
        (left, ByteOperand::Immediate(0)) => left,
        (ByteOperand::Immediate(0), right) if !subtract => right,
        _ => return None,
    };
    Some(InstructionKind::CopyByte {
        destination,
        source: value,
    })
}

fn fold_compare(
    op: CompareOp,
    destination: StorageId,
    left: &[ByteOperand],
    right: &[ByteOperand],
    width: u8,
) -> Option<InstructionKind> {
    let left = immediate_integer(left)?;
    let right = immediate_integer(right)?;
    let result = match op {
        CompareOp::Equal => left == right,
        CompareOp::NotEqual => left != right,
        CompareOp::LessThanUnsigned => left < right,
        CompareOp::LessThanOrEqualUnsigned => left <= right,
        CompareOp::GreaterThanUnsigned => left > right,
        CompareOp::GreaterThanOrEqualUnsigned => left >= right,
        CompareOp::LessThanSigned => signed(left, width) < signed(right, width),
        CompareOp::LessThanOrEqualSigned => signed(left, width) <= signed(right, width),
        CompareOp::GreaterThanSigned => signed(left, width) > signed(right, width),
        CompareOp::GreaterThanOrEqualSigned => signed(left, width) >= signed(right, width),
    };
    Some(if result {
        InstructionKind::SetBit(destination)
    } else {
        InstructionKind::ClearBit(destination)
    })
}

fn signed(value: u128, width: u8) -> i128 {
    if width == 128 {
        value as i128
    } else {
        let shift = 128 - width;
        ((value << shift) as i128) >> shift
    }
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

fn invalidate_definitions(
    facts: &mut Facts,
    instruction: &InstructionKind,
    semantics: &ProgramSemantics,
) {
    for definition in instruction_definitions(instruction, semantics) {
        facts.bytes.retain(|storage, value| {
            storage.storage != definition
                && !matches!(value, ByteOperand::Storage(source) if source.storage == definition)
        });
        facts
            .byte_bits
            .retain(|storage, _| storage.storage != definition);
        facts.bits.retain(|storage, value| {
            *storage != definition
                && !matches!(value, BitOperand::Storage(source) if *source == definition)
        });
    }
}

fn record_facts(facts: &mut Facts, instruction: &InstructionKind) {
    match instruction {
        InstructionKind::CopyByte {
            destination,
            source,
        } => {
            facts
                .bytes
                .insert(*destination, resolve_byte(*source, facts));
            facts
                .byte_bits
                .insert(*destination, known_byte_bits(*source, facts));
        }
        InstructionKind::ByteUnary {
            op: ByteUnaryOp::Not,
            destination,
            input,
        } => {
            let input = known_byte_bits(*input, facts);
            facts.byte_bits.insert(
                *destination,
                KnownByteBits {
                    zero: input.one,
                    one: input.zero,
                },
            );
        }
        InstructionKind::ByteBinary {
            op,
            destination,
            left,
            right,
        } => {
            let left = known_byte_bits(*left, facts);
            let right = known_byte_bits(*right, facts);
            let result = match op {
                ByteBinaryOp::And => KnownByteBits {
                    zero: left.zero | right.zero,
                    one: left.one & right.one,
                },
                ByteBinaryOp::Or => KnownByteBits {
                    zero: left.zero & right.zero,
                    one: left.one | right.one,
                },
                ByteBinaryOp::Xor => KnownByteBits {
                    zero: (left.zero & right.zero) | (left.one & right.one),
                    one: (left.zero & right.one) | (left.one & right.zero),
                },
            };
            facts.byte_bits.insert(*destination, result);
        }
        InstructionKind::CopyBit {
            destination,
            source,
        } => {
            facts.bits.insert(*destination, resolve_bit(*source, facts));
        }
        InstructionKind::SetBit(destination) => {
            facts.bits.insert(*destination, BitOperand::Constant(true));
        }
        InstructionKind::ClearBit(destination) => {
            facts.bits.insert(*destination, BitOperand::Constant(false));
        }
        InstructionKind::ToggleBit(destination) => {
            if let Some(BitOperand::Constant(value)) = facts.bits.get(destination).copied() {
                facts
                    .bits
                    .insert(*destination, BitOperand::Constant(!value));
            }
        }
        _ => {}
    }
}

fn known_byte_bits(operand: ByteOperand, facts: &Facts) -> KnownByteBits {
    match resolve_byte(operand, facts) {
        ByteOperand::Immediate(value) => KnownByteBits::constant(value),
        ByteOperand::Storage(storage) => facts.byte_bits.get(&storage).copied().unwrap_or_default(),
    }
}

fn invalidates_memory_facts(instruction: &InstructionKind) -> bool {
    matches!(
        instruction,
        InstructionKind::StoreIndirect { .. }
            | InstructionKind::Call { .. }
            | InstructionKind::CallIndirect { .. }
            | InstructionKind::Intrinsic { .. }
    )
}

fn rewrite_terminator(terminator: &mut TerminatorKind, facts: &Facts) {
    match terminator {
        TerminatorKind::Branch {
            condition,
            when_true,
            when_false,
        } => {
            rewrite_condition(condition, facts);
            let selected = match condition {
                BranchCondition::BitSet(storage) => bit_constant(*storage, facts),
                BranchCondition::BitClear(storage) => bit_constant(*storage, facts).map(|v| !v),
                BranchCondition::ByteZero(ByteOperand::Immediate(value)) => Some(*value == 0),
                BranchCondition::ByteNonZero(ByteOperand::Immediate(value)) => Some(*value != 0),
                BranchCondition::Equal {
                    left: ByteOperand::Immediate(left),
                    right: ByteOperand::Immediate(right),
                } => Some(left == right),
                BranchCondition::NotEqual {
                    left: ByteOperand::Immediate(left),
                    right: ByteOperand::Immediate(right),
                } => Some(left != right),
                _ => None,
            };
            if let Some(selected) = selected {
                *terminator = TerminatorKind::Jump(if selected { *when_true } else { *when_false });
            }
        }
        TerminatorKind::Switch {
            value,
            cases,
            default,
            ..
        } => {
            rewrite_bytes(value, facts);
            if let Some(value) = immediate_integer(value) {
                let target = cases
                    .iter()
                    .find_map(|(case, target)| (*case == value).then_some(*target))
                    .unwrap_or(*default);
                *terminator = TerminatorKind::Jump(target);
            }
        }
        TerminatorKind::TailCallIndirect { target, .. } => rewrite_code_address(target, facts),
        TerminatorKind::Jump(_)
        | TerminatorKind::TailCall { .. }
        | TerminatorKind::Return
        | TerminatorKind::Trap
        | TerminatorKind::Unreachable => {}
    }
}

fn rewrite_condition(condition: &mut BranchCondition, facts: &Facts) {
    match condition {
        BranchCondition::ByteZero(operand) | BranchCondition::ByteNonZero(operand) => {
            rewrite_byte(operand, facts)
        }
        BranchCondition::Equal { left, right } | BranchCondition::NotEqual { left, right } => {
            rewrite_byte(left, facts);
            rewrite_byte(right, facts);
        }
        _ => {}
    }
}

fn rewrite_code_address(address: &mut CodeAddress, facts: &Facts) {
    if let CodeAddress::Computed { low, high } = address {
        rewrite_byte(low, facts);
        rewrite_byte(high, facts);
    }
}

fn rewrite_byte(operand: &mut ByteOperand, facts: &Facts) {
    *operand = resolve_byte(*operand, facts);
}

fn rewrite_bytes(operands: &mut [ByteOperand], facts: &Facts) {
    for operand in operands {
        rewrite_byte(operand, facts);
    }
}

fn rewrite_bit(operand: &mut BitOperand, facts: &Facts) {
    *operand = resolve_bit(*operand, facts);
}

fn resolve_byte(mut operand: ByteOperand, facts: &Facts) -> ByteOperand {
    let mut seen = BTreeSet::new();
    while let ByteOperand::Storage(storage) = operand {
        if !seen.insert(storage) {
            break;
        }
        let Some(next) = facts.bytes.get(&storage).copied() else {
            break;
        };
        operand = next;
    }
    operand
}

fn resolve_bit(mut operand: BitOperand, facts: &Facts) -> BitOperand {
    let mut seen = BTreeSet::new();
    while let BitOperand::Storage(storage) = operand {
        if !seen.insert(storage) {
            break;
        }
        let Some(next) = facts.bits.get(&storage).copied() else {
            break;
        };
        operand = next;
    }
    operand
}

fn bit_constant(storage: StorageId, facts: &Facts) -> Option<bool> {
    match resolve_bit(BitOperand::Storage(storage), facts) {
        BitOperand::Constant(value) => Some(value),
        BitOperand::Storage(_) => None,
    }
}

fn apply_transfer(live: &mut UnitSet, definitions: &UnitSet, uses: &UnitSet) {
    live.retain(|storage| !definitions.contains(storage));
    live.extend(uses);
}

fn address_taken(program: &Program<VirtualPlacement>) -> BTreeSet<StorageId> {
    functions(program)
        .flat_map(|function| &function.blocks)
        .flat_map(|block| &block.instructions)
        .filter_map(|instruction| match instruction.kind {
            InstructionKind::AddressOf { storage, .. } => Some(storage),
            _ => None,
        })
        .collect()
}

fn remove_unused_storages(program: &mut Program<VirtualPlacement>) {
    let semantics = ProgramSemantics::new(program);
    let mut referenced = BTreeSet::new();
    for function in functions(program) {
        for value in function.abi.parameters.iter().chain(&function.abi.results) {
            for piece in &value.pieces {
                referenced.insert(match piece {
                    super::lir::ValueStorage::Bit(storage) => *storage,
                    super::lir::ValueStorage::Byte(storage) => storage.storage,
                });
            }
        }
        referenced.extend(function.abi.preserved.iter().copied());
        referenced.extend(function.abi.clobbered.iter().copied());
        for block in &function.blocks {
            for instruction in &block.instructions {
                referenced.extend(instruction_uses(&instruction.kind, &semantics));
                referenced.extend(instruction_definitions(&instruction.kind, &semantics));
            }
            referenced.extend(terminator_uses(
                &block.terminator.kind,
                function,
                &semantics,
            ));
        }
    }
    for static_ in &program.statics {
        referenced.insert(static_.storage);
        referenced.extend(static_.initializer.iter().filter_map(|byte| match byte {
            super::lir::StaticByte::StorageAddress { storage, .. } => Some(*storage),
            super::lir::StaticByte::Value(_) | super::lir::StaticByte::FunctionAddress { .. } => {
                None
            }
        }));
    }
    program
        .storages
        .retain(|storage| referenced.contains(&storage.id) || storage.placement.fixed.is_some());
}

fn for_each_function_mut(
    program: &mut Program<VirtualPlacement>,
    mut action: impl FnMut(&mut Function),
) {
    for function in program.all_functions_mut() {
        action(function);
    }
}

fn functions(program: &Program<VirtualPlacement>) -> impl Iterator<Item = &Function> {
    program.all_functions()
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use super::optimize;
    use crate::wasm::lir::{
        Block, BlockId, ByteBinaryOp, ByteOperand, ByteStorage, ByteUnaryOp, Function, FunctionAbi,
        FunctionKind, Instruction, InstructionKind, InterruptHandlers, Program, Storage, StorageId,
        StorageKind, StorageOwner, StorageRegion, Terminator, TerminatorKind, VirtualPlacement,
    };

    #[test]
    fn dead_write_elimination_tracks_individual_byte_lanes() {
        let storage = StorageId(0);
        let input = StorageId(1);
        let byte = |offset| ByteStorage { storage, offset };
        let program_function = Function {
            name: "main".into(),
            kind: FunctionKind::Normal,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: None,
                instructions: vec![
                    Instruction {
                        kind: InstructionKind::ByteUnary {
                            op: ByteUnaryOp::Not,
                            destination: byte(0),
                            input: ByteOperand::Storage(ByteStorage {
                                storage: input,
                                offset: 0,
                            }),
                        },
                    },
                    Instruction {
                        kind: InstructionKind::CopyByte {
                            destination: byte(1),
                            source: ByteOperand::Immediate(2),
                        },
                    },
                    Instruction {
                        kind: InstructionKind::Push(ByteOperand::Storage(byte(0))),
                    },
                ],
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        };
        let mut program = Program {
            main: program_function,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: vec![
                Storage {
                    id: storage,
                    name: Some("pair".into()),
                    owner: StorageOwner::Function("main".into()),
                    kind: StorageKind::Bytes {
                        length: NonZeroUsize::new(2).unwrap(),
                    },
                    placement: VirtualPlacement {
                        allowed_regions: vec![StorageRegion::ApplicationRam],
                        must_be_direct: false,
                        fixed: None,
                    },
                },
                Storage {
                    id: input,
                    name: Some("input".into()),
                    owner: StorageOwner::Function("main".into()),
                    kind: StorageKind::Byte,
                    placement: VirtualPlacement {
                        allowed_regions: vec![StorageRegion::ApplicationRam],
                        must_be_direct: false,
                        fixed: None,
                    },
                },
            ],
        };

        optimize(&mut program).unwrap();

        let instructions = &program.main.blocks[0].instructions;
        assert!(instructions.iter().any(|instruction| matches!(
            instruction.kind,
            InstructionKind::ByteUnary { destination, .. } if destination == byte(0)
        )));
        assert!(!instructions.iter().any(|instruction| matches!(
            instruction.kind,
            InstructionKind::CopyByte { destination, .. } if destination == byte(1)
        )));
    }

    #[test]
    fn removes_a_redundant_byte_mask() {
        let byte = |storage| ByteStorage {
            storage: StorageId(storage),
            offset: 0,
        };
        let program_function = Function {
            name: "main".into(),
            kind: FunctionKind::Normal,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: None,
                instructions: vec![
                    Instruction {
                        kind: InstructionKind::ByteBinary {
                            op: ByteBinaryOp::And,
                            destination: byte(1),
                            left: ByteOperand::Storage(byte(0)),
                            right: ByteOperand::Immediate(7),
                        },
                    },
                    Instruction {
                        kind: InstructionKind::ByteBinary {
                            op: ByteBinaryOp::And,
                            destination: byte(2),
                            left: ByteOperand::Storage(byte(1)),
                            right: ByteOperand::Immediate(7),
                        },
                    },
                    Instruction {
                        kind: InstructionKind::Push(ByteOperand::Storage(byte(2))),
                    },
                ],
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        };
        let placement = || VirtualPlacement {
            allowed_regions: vec![StorageRegion::ApplicationRam],
            must_be_direct: false,
            fixed: None,
        };
        let mut program = Program {
            main: program_function,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: (0..3)
                .map(|index| Storage {
                    id: StorageId(index),
                    name: None,
                    owner: StorageOwner::Function("main".into()),
                    kind: StorageKind::Byte,
                    placement: placement(),
                })
                .collect(),
        };

        optimize(&mut program).unwrap();

        let masks = program.main.blocks[0]
            .instructions
            .iter()
            .filter(|instruction| {
                matches!(
                    instruction.kind,
                    InstructionKind::ByteBinary {
                        op: ByteBinaryOp::And,
                        right: ByteOperand::Immediate(7),
                        ..
                    }
                )
            })
            .count();
        assert_eq!(masks, 1);
    }
}
