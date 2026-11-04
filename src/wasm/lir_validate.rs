//! Whole-program validation for virtual and allocated VMU LIR.
//!
//! The verifier is deliberately independent of instruction selection. A
//! malformed program is rejected at the phase boundary where it first exists,
//! with enough context to identify the violated invariant.

use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt,
};

use super::{
    intrinsics::AbiType,
    lir::*,
    target::{
        APPLICATION_RAM_BYTES, FIRST_ALLOCATABLE_APPLICATION_BYTE, WORK_RAM_BYTES,
        xram_offset_is_addressable,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValidationError {
    context: String,
    message: String,
}

impl ValidationError {
    fn new(context: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            context: context.into(),
            message: message.into(),
        }
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.context, self.message)
    }
}

impl Error for ValidationError {}

pub fn validate_virtual(program: &VirtualProgram) -> Result<(), ValidationError> {
    Validator::new(program)?.validate(program, validate_virtual_placement)?;
    validate_flash_eligibility(program, |placement| {
        placement.allowed_regions.contains(&StorageRegion::Flash)
    })
}

pub fn validate_allocated(program: &AllocatedProgram) -> Result<(), ValidationError> {
    let validator = Validator::new(program)?;
    validator.validate(program, validate_physical_placement)?;
    validate_flash_eligibility(program, |placement| *placement == PhysicalPlacement::Flash)
}

fn validate_flash_eligibility<P>(
    program: &Program<P>,
    uses_flash: impl Fn(&P) -> bool,
) -> Result<(), ValidationError> {
    for storage in &program.storages {
        if !uses_flash(&storage.placement) {
            continue;
        }
        let static_ = program
            .statics
            .iter()
            .find(|static_| static_.storage == storage.id)
            .ok_or_else(|| {
                ValidationError::new(
                    format!("storage %s{}", storage.id.0),
                    "flash storage is not an immutable static",
                )
            })?;
        if static_.mutable {
            return Err(ValidationError::new(
                format!("static {:?}", static_.name),
                "mutable static uses or permits read-only flash storage",
            ));
        }
    }
    Ok(())
}

struct Validator<'a, P> {
    functions: BTreeMap<&'a str, &'a Function>,
    storages: BTreeMap<StorageId, &'a Storage<P>>,
}

impl<'a, P> Validator<'a, P> {
    fn new(program: &'a Program<P>) -> Result<Self, ValidationError> {
        let mut functions = BTreeMap::new();
        for function in program.all_functions() {
            if function.name.is_empty() {
                return Err(ValidationError::new(
                    "LIR program",
                    "function name is empty",
                ));
            }
            if functions.insert(function.name.as_str(), function).is_some() {
                return Err(ValidationError::new(
                    "LIR program",
                    format!("duplicate function name {:?}", function.name),
                ));
            }
        }

        let mut storages = BTreeMap::new();
        for storage in &program.storages {
            if storages.insert(storage.id, storage).is_some() {
                return Err(ValidationError::new(
                    "LIR storage table",
                    format!("duplicate storage ID %s{}", storage.id.0),
                ));
            }
        }
        Ok(Self {
            functions,
            storages,
        })
    }

    fn validate(
        &self,
        program: &Program<P>,
        validate_placement: impl Fn(&Storage<P>, &str) -> Result<(), ValidationError>,
    ) -> Result<(), ValidationError> {
        if program.main.kind != FunctionKind::Normal {
            return Err(ValidationError::new(
                format!("function {:?}", program.main.name),
                "main function is marked as an interrupt handler",
            ));
        }
        for function in program.interrupt_handlers.iter() {
            if function.kind != FunctionKind::Interrupt {
                return Err(ValidationError::new(
                    format!("function {:?}", function.name),
                    "interrupt-vector function is not marked as an interrupt handler",
                ));
            }
        }
        for function in &program.functions {
            if function.kind != FunctionKind::Normal {
                return Err(ValidationError::new(
                    format!("function {:?}", function.name),
                    "ordinary function is marked as an interrupt handler",
                ));
            }
        }

        for storage in &program.storages {
            let context = format!("storage %s{}", storage.id.0);
            if let StorageOwner::Function(function) = &storage.owner
                && !self.functions.contains_key(function.as_str())
            {
                return Err(ValidationError::new(
                    context,
                    format!("storage owner {function:?} is not a function"),
                ));
            }
            validate_placement(storage, &context)?;
        }

        self.validate_statics(program)?;
        for function in program.all_functions() {
            self.validate_function(function)?;
        }
        self.validate_reentrancy(program)
    }

    fn validate_statics(&self, program: &Program<P>) -> Result<(), ValidationError> {
        let mut names = BTreeSet::new();
        for static_ in &program.statics {
            let context = format!("static {:?}", static_.name);
            if static_.name.is_empty() {
                return Err(ValidationError::new(context, "static name is empty"));
            }
            if !names.insert(&static_.name) {
                return Err(ValidationError::new(context, "duplicate static name"));
            }
            let storage = self.storage(static_.storage, &context)?;
            if storage.owner != StorageOwner::Program {
                return Err(ValidationError::new(
                    context,
                    "static uses function-owned storage",
                ));
            }
            let length = storage_length(storage.kind);
            if static_.initializer.len() != length {
                return Err(ValidationError::new(
                    context,
                    format!(
                        "initializer has {} bytes for {length}-byte storage",
                        static_.initializer.len()
                    ),
                ));
            }
            for byte in &static_.initializer {
                match byte {
                    StaticByte::Value(_) => {}
                    StaticByte::StorageAddress {
                        storage, offset, ..
                    } => {
                        let target = self.storage(*storage, &context)?;
                        if *offset > storage_length(target.kind) {
                            return Err(ValidationError::new(
                                &context,
                                format!("address offset {offset} exceeds storage %s{}", storage.0),
                            ));
                        }
                    }
                    StaticByte::FunctionAddress { function, .. } => {
                        self.function(function, &context)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn validate_function(&self, function: &Function) -> Result<(), ValidationError> {
        let function_context = format!("function {:?}", function.name);
        self.validate_abi(&function.abi, &function_context)?;
        if function.blocks.is_empty() {
            return Err(ValidationError::new(
                function_context,
                "function has no basic blocks",
            ));
        }

        let mut block_ids = BTreeSet::new();
        for (index, block) in function.blocks.iter().enumerate() {
            if block.id != BlockId(index) {
                return Err(ValidationError::new(
                    &function_context,
                    format!(
                        "block at index {index} has ID %b{}, expected %b{index}",
                        block.id.0
                    ),
                ));
            }
            block_ids.insert(block.id);
        }
        if !block_ids.contains(&function.entry) {
            return Err(ValidationError::new(
                &function_context,
                format!("entry block %b{} does not exist", function.entry.0),
            ));
        }

        for block in &function.blocks {
            let block_context = format!("{function_context}, block %b{}", block.id.0);
            for (index, instruction) in block.instructions.iter().enumerate() {
                self.validate_instruction(
                    &instruction.kind,
                    function,
                    &format!("{block_context}, instruction {index}"),
                )?;
            }
            self.validate_terminator(
                &block.terminator.kind,
                function,
                &block_ids,
                &format!("{block_context}, terminator"),
            )?;
        }
        Ok(())
    }

    fn validate_abi(&self, abi: &FunctionAbi, context: &str) -> Result<(), ValidationError> {
        for (kind, values) in [("parameter", &abi.parameters), ("result", &abi.results)] {
            for (index, value) in values.iter().enumerate() {
                if value.logical_index != index {
                    return Err(ValidationError::new(
                        context,
                        format!(
                            "ABI {kind} {index} has logical index {}",
                            value.logical_index
                        ),
                    ));
                }
                if value.pieces.is_empty() {
                    return Err(ValidationError::new(
                        context,
                        format!("ABI {kind} {index} has no storage pieces"),
                    ));
                }
                for piece in &value.pieces {
                    self.value_storage(*piece, None, context)?;
                    let storage = self.storage(piece_storage(*piece), context)?;
                    if storage.owner != StorageOwner::Program {
                        return Err(ValidationError::new(
                            context,
                            format!("ABI {kind} {index} uses function-owned storage"),
                        ));
                    }
                }
            }
        }

        let mut preserved = BTreeSet::new();
        for storage in &abi.preserved {
            self.storage(*storage, context)?;
            if !preserved.insert(*storage) {
                return Err(ValidationError::new(
                    context,
                    format!("ABI preserves storage %s{} more than once", storage.0),
                ));
            }
        }
        let mut clobbered = BTreeSet::new();
        for storage in &abi.clobbered {
            self.storage(*storage, context)?;
            if !clobbered.insert(*storage) {
                return Err(ValidationError::new(
                    context,
                    format!("ABI clobbers storage %s{} more than once", storage.0),
                ));
            }
            if preserved.contains(storage) {
                return Err(ValidationError::new(
                    context,
                    format!("ABI both preserves and clobbers storage %s{}", storage.0),
                ));
            }
        }
        Ok(())
    }

    fn validate_instruction(
        &self,
        instruction: &InstructionKind,
        function: &Function,
        context: &str,
    ) -> Result<(), ValidationError> {
        use InstructionKind::*;
        match instruction {
            CopyByte {
                destination,
                source,
            } => {
                self.byte(*destination, Some(function), context)?;
                self.byte_operand(*source, Some(function), context)
            }
            CopyBit {
                destination,
                source,
            } => {
                self.bit(*destination, Some(function), context)?;
                self.bit_operand(*source, Some(function), context)
            }
            SetBit(storage) | ClearBit(storage) | ToggleBit(storage) => {
                self.bit(*storage, Some(function), context)
            }
            ByteUnary {
                destination, input, ..
            } => {
                self.byte(*destination, Some(function), context)?;
                self.byte_operand(*input, Some(function), context)
            }
            ByteBinary {
                destination,
                left,
                right,
                ..
            } => {
                self.byte(*destination, Some(function), context)?;
                self.byte_operand(*left, Some(function), context)?;
                self.byte_operand(*right, Some(function), context)
            }
            Add {
                destination,
                left,
                right,
                carry_in,
                carry_out,
            }
            | Subtract {
                destination,
                left,
                right,
                borrow_in: carry_in,
                borrow_out: carry_out,
            } => {
                self.byte(*destination, Some(function), context)?;
                self.byte_operand(*left, Some(function), context)?;
                self.byte_operand(*right, Some(function), context)?;
                self.optional_bit(*carry_in, Some(function), context)?;
                self.optional_bit(*carry_out, Some(function), context)
            }
            Multiply {
                left_high,
                left_low,
                right,
                result_high,
                result_middle,
                result_low,
            } => {
                for operand in [*left_high, *left_low, *right] {
                    self.byte_operand(operand, Some(function), context)?;
                }
                for destination in [*result_high, *result_middle, *result_low] {
                    self.byte(destination, Some(function), context)?;
                }
                Ok(())
            }
            Divide {
                dividend_high,
                dividend_low,
                divisor,
                quotient_high,
                quotient_low,
                remainder,
            } => {
                for operand in [*dividend_high, *dividend_low, *divisor] {
                    self.byte_operand(operand, Some(function), context)?;
                }
                for destination in [*quotient_high, *quotient_low, *remainder] {
                    self.byte(destination, Some(function), context)?;
                }
                Ok(())
            }
            Rotate {
                destination,
                input,
                carry_in,
                carry_out,
                ..
            } => {
                self.byte(*destination, Some(function), context)?;
                self.byte_operand(*input, Some(function), context)?;
                self.optional_bit(*carry_in, Some(function), context)?;
                self.optional_bit(*carry_out, Some(function), context)
            }
            LoadIndirect {
                destination,
                address,
            } => {
                self.byte(*destination, Some(function), context)?;
                self.address(address, function, context)
            }
            StoreIndirect { address, value } => {
                self.address(address, function, context)?;
                self.byte_operand(*value, Some(function), context)
            }
            Push(value) => self.byte_operand(*value, Some(function), context),
            Pop(destination) => self.byte(*destination, Some(function), context),
            AddressOf {
                destination_low,
                destination_high,
                storage,
                offset,
            } => {
                self.byte(*destination_low, Some(function), context)?;
                self.byte(*destination_high, Some(function), context)?;
                let target = self.storage(*storage, context)?;
                self.require_accessible(target, function, context)?;
                if *offset <= storage_length(target.kind) {
                    Ok(())
                } else {
                    Err(ValidationError::new(
                        context,
                        format!("address offset {offset} exceeds storage %s{}", storage.0),
                    ))
                }
            }
            FunctionAddress {
                destination_low,
                destination_high,
                function: target,
            } => {
                self.byte(*destination_low, Some(function), context)?;
                self.byte(*destination_high, Some(function), context)?;
                self.function(target, context).map(|_| ())
            }
            ShiftBytes {
                destinations,
                value,
                amount,
                width,
                ..
            } => {
                self.byte_vector(destinations, Some(function), context)?;
                self.byte_operands(value, Some(function), context)?;
                self.byte_operands(amount, Some(function), context)?;
                require_same_length(
                    destinations.len(),
                    value.len(),
                    "shift result and input",
                    context,
                )?;
                validate_width(*width, value.len(), context)
            }
            CompareBytes {
                destination,
                left,
                right,
                width,
                ..
            } => {
                self.bit(*destination, Some(function), context)?;
                self.byte_operands(left, Some(function), context)?;
                self.byte_operands(right, Some(function), context)?;
                require_same_length(left.len(), right.len(), "comparison operands", context)?;
                validate_width(*width, left.len(), context)
            }
            SelectByte {
                destination,
                condition,
                when_true,
                when_false,
            } => {
                self.byte(*destination, Some(function), context)?;
                self.bit_operand(*condition, Some(function), context)?;
                self.byte_operand(*when_true, Some(function), context)?;
                self.byte_operand(*when_false, Some(function), context)
            }
            SelectBit {
                destination,
                condition,
                when_true,
                when_false,
            } => {
                self.bit(*destination, Some(function), context)?;
                self.bit_operand(*condition, Some(function), context)?;
                self.bit_operand(*when_true, Some(function), context)?;
                self.bit_operand(*when_false, Some(function), context)
            }
            BitToByte {
                destination,
                source,
            } => {
                self.byte(*destination, Some(function), context)?;
                self.bit_operand(*source, Some(function), context)
            }
            ByteToBit {
                destination,
                source,
            } => {
                self.bit(*destination, Some(function), context)?;
                self.byte_operand(*source, Some(function), context)
            }
            SignExtend {
                destinations,
                source,
                source_width,
            } => {
                self.byte_vector(destinations, Some(function), context)?;
                self.byte_operands(source, Some(function), context)?;
                if destinations.len() < source.len() {
                    return Err(ValidationError::new(
                        context,
                        "sign extension destination is narrower than source",
                    ));
                }
                validate_width(*source_width, source.len(), context)
            }
            IntegerUnaryBytes {
                destinations,
                input,
                width,
                ..
            } => {
                self.byte_vector(destinations, Some(function), context)?;
                self.byte_operands(input, Some(function), context)?;
                require_same_length(
                    destinations.len(),
                    input.len(),
                    "unary result and input",
                    context,
                )?;
                validate_width(*width, input.len(), context)
            }
            IntegerBinaryBytes {
                destinations,
                left,
                right,
                width,
                ..
            } => {
                self.byte_vector(destinations, Some(function), context)?;
                self.byte_operands(left, Some(function), context)?;
                self.byte_operands(right, Some(function), context)?;
                require_same_length(
                    destinations.len(),
                    left.len(),
                    "binary result and left input",
                    context,
                )?;
                require_same_length(left.len(), right.len(), "binary operands", context)?;
                validate_width(*width, left.len(), context)
            }
            Call { function: target } => self.function(target, context).map(|_| ()),
            CallIndirect { target, abi } => {
                self.code_address(target, function, context)?;
                self.validate_abi(abi, context)
            }
            Intrinsic {
                operation,
                inputs,
                outputs,
            } => self.validate_intrinsic(*operation, inputs, outputs, function, context),
        }
    }

    fn validate_terminator(
        &self,
        terminator: &TerminatorKind,
        function: &Function,
        blocks: &BTreeSet<BlockId>,
        context: &str,
    ) -> Result<(), ValidationError> {
        match terminator {
            TerminatorKind::Jump(target) => validate_block_target(*target, blocks, context),
            TerminatorKind::Branch {
                condition,
                when_true,
                when_false,
            } => {
                self.branch_condition(condition, function, context)?;
                validate_block_target(*when_true, blocks, context)?;
                validate_block_target(*when_false, blocks, context)
            }
            TerminatorKind::Switch {
                value,
                width,
                cases,
                default,
            } => {
                self.byte_operands(value, Some(function), context)?;
                validate_width(*width, value.len(), context)?;
                let mut values = BTreeSet::new();
                for (value, target) in cases {
                    if *width < 128 && *value >= (1_u128 << *width) {
                        return Err(ValidationError::new(
                            context,
                            format!("switch case {value} does not fit {width} bits"),
                        ));
                    }
                    if !values.insert(*value) {
                        return Err(ValidationError::new(
                            context,
                            format!("duplicate switch case {value}"),
                        ));
                    }
                    validate_block_target(*target, blocks, context)?;
                }
                validate_block_target(*default, blocks, context)
            }
            TerminatorKind::TailCall { function: target } => {
                self.function(target, context).map(|_| ())
            }
            TerminatorKind::TailCallIndirect { target, abi } => {
                self.code_address(target, function, context)?;
                self.validate_abi(abi, context)
            }
            TerminatorKind::Return | TerminatorKind::Trap | TerminatorKind::Unreachable => Ok(()),
        }
    }

    fn validate_intrinsic(
        &self,
        operation: super::intrinsics::Intrinsic,
        inputs: &[ValueOperand],
        outputs: &[ValueStorage],
        function: &Function,
        context: &str,
    ) -> Result<(), ValidationError> {
        let definition = operation.definition().ok_or_else(|| {
            ValidationError::new(context, format!("unregistered intrinsic {operation:?}"))
        })?;
        let mut input_index = 0;
        for parameter in &definition.parameters {
            match parameter.ty {
                AbiType::U8 => {
                    let Some(ValueOperand::Byte(value)) = inputs.get(input_index) else {
                        return Err(ValidationError::new(
                            context,
                            format!("intrinsic input {input_index} is not a byte"),
                        ));
                    };
                    self.byte_operand(*value, Some(function), context)?;
                    input_index += 1;
                }
                AbiType::ConstU8Pointer | AbiType::MutU8Pointer => {
                    for _ in 0..2 {
                        let Some(ValueOperand::Byte(value)) = inputs.get(input_index) else {
                            return Err(ValidationError::new(
                                context,
                                format!("intrinsic pointer input is missing byte {input_index}"),
                            ));
                        };
                        self.byte_operand(*value, Some(function), context)?;
                        input_index += 1;
                    }
                }
            }
        }
        if input_index != inputs.len() {
            return Err(ValidationError::new(
                context,
                format!(
                    "intrinsic has {} extra input pieces",
                    inputs.len() - input_index
                ),
            ));
        }
        match (definition.result, outputs) {
            (None, []) => Ok(()),
            (Some(AbiType::U8), [ValueStorage::Byte(output)]) => {
                self.byte(*output, Some(function), context)
            }
            (result, _) => Err(ValidationError::new(
                context,
                format!("intrinsic result {result:?} does not match output storage"),
            )),
        }
    }

    fn branch_condition(
        &self,
        condition: &BranchCondition,
        function: &Function,
        context: &str,
    ) -> Result<(), ValidationError> {
        match condition {
            BranchCondition::BitSet(storage)
            | BranchCondition::BitClear(storage)
            | BranchCondition::CarrySet(storage)
            | BranchCondition::CarryClear(storage) => self.bit(*storage, Some(function), context),
            BranchCondition::ByteZero(value) | BranchCondition::ByteNonZero(value) => {
                self.byte_operand(*value, Some(function), context)
            }
            BranchCondition::Equal { left, right } | BranchCondition::NotEqual { left, right } => {
                self.byte_operand(*left, Some(function), context)?;
                self.byte_operand(*right, Some(function), context)
            }
        }
    }

    fn address(
        &self,
        address: &Address,
        function: &Function,
        context: &str,
    ) -> Result<(), ValidationError> {
        if address.bytes().len() > 2 {
            return Err(ValidationError::new(
                context,
                format!(
                    "address has {} bytes; VMU addresses have at most 2",
                    address.bytes().len()
                ),
            ));
        }
        self.byte_operands(address.bytes(), Some(function), context)
    }

    fn code_address(
        &self,
        address: &CodeAddress,
        function: &Function,
        context: &str,
    ) -> Result<(), ValidationError> {
        match address {
            CodeAddress::Function(target) => self.function(target, context).map(|_| ()),
            CodeAddress::Computed { low, high } => {
                self.byte_operand(*low, Some(function), context)?;
                self.byte_operand(*high, Some(function), context)
            }
        }
    }

    fn byte_vector(
        &self,
        values: &[ByteStorage],
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        if values.is_empty() {
            return Err(ValidationError::new(context, "byte vector is empty"));
        }
        for value in values {
            self.byte(*value, function, context)?;
        }
        Ok(())
    }

    fn byte_operands(
        &self,
        values: &[ByteOperand],
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        if values.is_empty() {
            return Err(ValidationError::new(
                context,
                "byte operand vector is empty",
            ));
        }
        for value in values {
            self.byte_operand(*value, function, context)?;
        }
        Ok(())
    }

    fn byte_operand(
        &self,
        operand: ByteOperand,
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        match operand {
            ByteOperand::Immediate(_) => Ok(()),
            ByteOperand::Storage(byte) => self.byte(byte, function, context),
        }
    }

    fn bit_operand(
        &self,
        operand: BitOperand,
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        match operand {
            BitOperand::Constant(_) => Ok(()),
            BitOperand::Storage(storage) => self.bit(storage, function, context),
        }
    }

    fn optional_bit(
        &self,
        storage: Option<StorageId>,
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        storage.map_or(Ok(()), |storage| self.bit(storage, function, context))
    }

    fn value_storage(
        &self,
        value: ValueStorage,
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        match value {
            ValueStorage::Bit(storage) => self.bit(storage, function, context),
            ValueStorage::Byte(storage) => self.byte(storage, function, context),
        }
    }

    fn byte(
        &self,
        byte: ByteStorage,
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        let storage = self.storage(byte.storage, context)?;
        if let Some(function) = function {
            self.require_accessible(storage, function, context)?;
        }
        let length = match storage.kind {
            StorageKind::Byte => 1,
            StorageKind::Bytes { length } => length.get(),
            StorageKind::Bit | StorageKind::Carry => {
                return Err(ValidationError::new(
                    context,
                    format!("storage %s{} is not byte-addressable", byte.storage.0),
                ));
            }
        };
        if byte.offset < length {
            Ok(())
        } else {
            Err(ValidationError::new(
                context,
                format!(
                    "byte {} is outside {length}-byte storage %s{}",
                    byte.offset, byte.storage.0
                ),
            ))
        }
    }

    fn bit(
        &self,
        id: StorageId,
        function: Option<&Function>,
        context: &str,
    ) -> Result<(), ValidationError> {
        let storage = self.storage(id, context)?;
        if let Some(function) = function {
            self.require_accessible(storage, function, context)?;
        }
        if matches!(storage.kind, StorageKind::Bit | StorageKind::Carry) {
            Ok(())
        } else {
            Err(ValidationError::new(
                context,
                format!("storage %s{} is not bit-addressable", id.0),
            ))
        }
    }

    fn require_accessible(
        &self,
        storage: &Storage<P>,
        function: &Function,
        context: &str,
    ) -> Result<(), ValidationError> {
        if storage.owner == StorageOwner::Program
            || storage.owner == StorageOwner::Function(function.name.clone())
        {
            Ok(())
        } else {
            Err(ValidationError::new(
                context,
                format!(
                    "function {:?} accesses storage owned by {:?}",
                    function.name, storage.owner
                ),
            ))
        }
    }

    fn storage(&self, id: StorageId, context: &str) -> Result<&Storage<P>, ValidationError> {
        self.storages
            .get(&id)
            .copied()
            .ok_or_else(|| ValidationError::new(context, format!("unknown storage %s{}", id.0)))
    }

    fn function(&self, name: &str, context: &str) -> Result<&Function, ValidationError> {
        self.functions
            .get(name)
            .copied()
            .ok_or_else(|| ValidationError::new(context, format!("unknown function {name:?}")))
    }

    fn validate_reentrancy(&self, program: &Program<P>) -> Result<(), ValidationError> {
        let edges = call_edges(program, &self.functions);

        // Function-owned WRAM is currently statically placed. A call cycle
        // containing an ordinary call can reactivate a live frame and is not
        // correct until ShadowStack becomes genuinely frame-relative.
        for (caller, outgoing) in &edges {
            for edge in outgoing.iter().filter(|edge| !edge.tail) {
                if reachable(&edges, edge.target, caller) {
                    return Err(ValidationError::new(
                        format!("function {caller:?}"),
                        format!(
                            "ordinary call to {:?} can re-enter a live function; dynamic shadow frames are not implemented",
                            edge.target
                        ),
                    ));
                }
            }
        }

        let mut roots = vec![program.main.name.as_str()];
        roots.extend(
            program
                .interrupt_handlers
                .iter()
                .map(|function| function.name.as_str()),
        );
        let reachable_sets = roots
            .iter()
            .map(|root| (*root, reachable_set(&edges, root)))
            .collect::<Vec<_>>();
        for left in 0..reachable_sets.len() {
            for right in left + 1..reachable_sets.len() {
                let overlap = reachable_sets[left]
                    .1
                    .intersection(&reachable_sets[right].1)
                    .next();
                if let Some(function) = overlap {
                    return Err(ValidationError::new(
                        "LIR call graph",
                        format!(
                            "entry {:?} and interrupt entry {:?} can both activate function {:?}; static shadow storage is not reentrant",
                            reachable_sets[left].0, reachable_sets[right].0, function
                        ),
                    ));
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct CallEdge<'a> {
    target: &'a str,
    tail: bool,
}

fn call_edges<'a, P>(
    program: &'a Program<P>,
    functions: &BTreeMap<&'a str, &'a Function>,
) -> BTreeMap<&'a str, Vec<CallEdge<'a>>> {
    let mut edges = BTreeMap::new();
    for function in program.all_functions() {
        let outgoing = edges.entry(function.name.as_str()).or_insert_with(Vec::new);
        for block in &function.blocks {
            for instruction in &block.instructions {
                match &instruction.kind {
                    InstructionKind::Call { function } => outgoing.push(CallEdge {
                        target: function,
                        tail: false,
                    }),
                    InstructionKind::CallIndirect { target, abi } => {
                        add_indirect_edges(outgoing, target, abi, false, functions)
                    }
                    _ => {}
                }
            }
            match &block.terminator.kind {
                TerminatorKind::TailCall { function } => outgoing.push(CallEdge {
                    target: function,
                    tail: true,
                }),
                TerminatorKind::TailCallIndirect { target, abi } => {
                    add_indirect_edges(outgoing, target, abi, true, functions)
                }
                _ => {}
            }
        }
    }
    edges
}

fn add_indirect_edges<'a>(
    edges: &mut Vec<CallEdge<'a>>,
    target: &'a CodeAddress,
    abi: &FunctionAbi,
    tail: bool,
    functions: &BTreeMap<&'a str, &'a Function>,
) {
    match target {
        CodeAddress::Function(function) => edges.push(CallEdge {
            target: function,
            tail,
        }),
        CodeAddress::Computed { .. } => edges.extend(
            functions
                .iter()
                .filter(|(_, function)| function.abi == *abi)
                .map(|(name, _)| CallEdge { target: name, tail }),
        ),
    }
}

fn reachable<'a>(
    edges: &BTreeMap<&'a str, Vec<CallEdge<'a>>>,
    start: &'a str,
    target: &str,
) -> bool {
    reachable_set(edges, start).contains(target)
}

fn reachable_set<'a>(
    edges: &BTreeMap<&'a str, Vec<CallEdge<'a>>>,
    start: &'a str,
) -> BTreeSet<&'a str> {
    let mut seen = BTreeSet::new();
    let mut pending = vec![start];
    while let Some(function) = pending.pop() {
        if !seen.insert(function) {
            continue;
        }
        pending.extend(
            edges
                .get(function)
                .into_iter()
                .flatten()
                .map(|edge| edge.target),
        );
    }
    seen
}

fn validate_virtual_placement(
    storage: &Storage<VirtualPlacement>,
    context: &str,
) -> Result<(), ValidationError> {
    let placement = &storage.placement;
    if placement.allowed_regions.is_empty() {
        return Err(ValidationError::new(context, "no allowed storage regions"));
    }
    let unique = placement
        .allowed_regions
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    if unique.len() != placement.allowed_regions.len() {
        return Err(ValidationError::new(
            context,
            "allowed storage regions contain duplicates",
        ));
    }
    if placement.must_be_direct
        && !placement.allowed_regions.iter().any(|region| {
            matches!(
                region,
                StorageRegion::Accumulator
                    | StorageRegion::CarryFlag
                    | StorageRegion::ApplicationRam
                    | StorageRegion::SpecialRegister
                    | StorageRegion::IndirectRegister
            )
        })
    {
        return Err(ValidationError::new(
            context,
            "direct storage has no directly addressable allowed region",
        ));
    }
    if let Some(fixed) = placement.fixed {
        validate_fixed_kind(storage.kind, fixed, context)?;
        let region = placement_region(fixed);
        if !placement.allowed_regions.contains(&region) {
            return Err(ValidationError::new(
                context,
                format!("fixed placement {fixed:?} is outside allowed regions"),
            ));
        }
        validate_physical_bounds(storage.kind, fixed, context)?;
    }
    Ok(())
}

fn validate_physical_placement(
    storage: &Storage<PhysicalPlacement>,
    context: &str,
) -> Result<(), ValidationError> {
    validate_fixed_kind(storage.kind, storage.placement, context)?;
    validate_physical_bounds(storage.kind, storage.placement, context)?;
    match storage.placement {
        PhysicalPlacement::ShadowStack { .. } | PhysicalPlacement::ShadowStackBit { .. } => {
            if matches!(storage.owner, StorageOwner::Function(_)) {
                Ok(())
            } else {
                Err(ValidationError::new(
                    context,
                    "shadow-stack placement is not function-owned",
                ))
            }
        }
        PhysicalPlacement::Flash => {
            if storage.owner == StorageOwner::Program {
                Ok(())
            } else {
                Err(ValidationError::new(
                    context,
                    "flash placement is not program-owned",
                ))
            }
        }
        _ => Ok(()),
    }
}

fn validate_fixed_kind(
    kind: StorageKind,
    placement: PhysicalPlacement,
    context: &str,
) -> Result<(), ValidationError> {
    let valid = match kind {
        StorageKind::Bit | StorageKind::Carry => matches!(
            placement,
            PhysicalPlacement::ApplicationBit { .. }
                | PhysicalPlacement::ShadowStackBit { .. }
                | PhysicalPlacement::WorkRamBit { .. }
                | PhysicalPlacement::CarryFlag
        ),
        StorageKind::Byte => matches!(
            placement,
            PhysicalPlacement::Accumulator
                | PhysicalPlacement::ApplicationRam { .. }
                | PhysicalPlacement::ShadowStack { .. }
                | PhysicalPlacement::SpecialRegister(_)
                | PhysicalPlacement::IndirectRegister { .. }
                | PhysicalPlacement::WorkRam { .. }
                | PhysicalPlacement::Flash
                | PhysicalPlacement::XRam { .. }
        ),
        StorageKind::Bytes { length } => {
            length.get() == 1
                || matches!(
                    placement,
                    PhysicalPlacement::ApplicationRam { .. }
                        | PhysicalPlacement::ShadowStack { .. }
                        | PhysicalPlacement::WorkRam { .. }
                        | PhysicalPlacement::Flash
                )
        }
    };
    if valid {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("storage kind {kind:?} cannot use placement {placement:?}"),
        ))
    }
}

fn validate_physical_bounds(
    kind: StorageKind,
    placement: PhysicalPlacement,
    context: &str,
) -> Result<(), ValidationError> {
    let length = storage_length(kind);
    let valid = match placement {
        PhysicalPlacement::ApplicationRam { offset } => {
            usize::from(offset) >= FIRST_ALLOCATABLE_APPLICATION_BYTE
                && usize::from(offset)
                    .checked_add(length)
                    .is_some_and(|end| end <= APPLICATION_RAM_BYTES)
        }
        PhysicalPlacement::ApplicationBit { offset, .. } => {
            usize::from(offset) >= FIRST_ALLOCATABLE_APPLICATION_BYTE
        }
        PhysicalPlacement::ShadowStack { frame_offset }
        | PhysicalPlacement::WorkRam {
            offset: frame_offset,
        } => usize::from(frame_offset.get())
            .checked_add(length)
            .is_some_and(|end| end <= WORK_RAM_BYTES),
        PhysicalPlacement::ShadowStackBit { frame_offset, .. }
        | PhysicalPlacement::WorkRamBit {
            offset: frame_offset,
            ..
        } => usize::from(frame_offset.get()) < WORK_RAM_BYTES,
        PhysicalPlacement::XRam { offset, .. } => (0..length).all(|index| {
            u8::try_from(index)
                .ok()
                .and_then(|index| offset.get().checked_add(index))
                .is_some_and(xram_offset_is_addressable)
        }),
        PhysicalPlacement::Accumulator
        | PhysicalPlacement::CarryFlag
        | PhysicalPlacement::SpecialRegister(_)
        | PhysicalPlacement::IndirectRegister { .. }
        | PhysicalPlacement::Flash => true,
    };
    if valid {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("placement {placement:?} exceeds its VMU storage region"),
        ))
    }
}

fn placement_region(placement: PhysicalPlacement) -> StorageRegion {
    match placement {
        PhysicalPlacement::Accumulator => StorageRegion::Accumulator,
        PhysicalPlacement::CarryFlag => StorageRegion::CarryFlag,
        PhysicalPlacement::ApplicationRam { .. } | PhysicalPlacement::ApplicationBit { .. } => {
            StorageRegion::ApplicationRam
        }
        PhysicalPlacement::ShadowStack { .. } | PhysicalPlacement::ShadowStackBit { .. } => {
            StorageRegion::ShadowStack
        }
        PhysicalPlacement::WorkRam { .. } | PhysicalPlacement::WorkRamBit { .. } => {
            StorageRegion::WorkRam
        }
        PhysicalPlacement::SpecialRegister(_) => StorageRegion::SpecialRegister,
        PhysicalPlacement::IndirectRegister { .. } => StorageRegion::IndirectRegister,
        PhysicalPlacement::Flash => StorageRegion::Flash,
        PhysicalPlacement::XRam { .. } => StorageRegion::XRam,
    }
}

fn validate_block_target(
    target: BlockId,
    blocks: &BTreeSet<BlockId>,
    context: &str,
) -> Result<(), ValidationError> {
    if blocks.contains(&target) {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("branch targets unknown block %b{}", target.0),
        ))
    }
}

fn validate_width(width: u8, bytes: usize, context: &str) -> Result<(), ValidationError> {
    if width != 0 && usize::from(width) <= bytes.saturating_mul(8) {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("width {width} does not fit {bytes} bytes"),
        ))
    }
}

fn require_same_length(
    left: usize,
    right: usize,
    description: &str,
    context: &str,
) -> Result<(), ValidationError> {
    if left == right {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("{description} have lengths {left} and {right}"),
        ))
    }
}

fn storage_length(kind: StorageKind) -> usize {
    match kind {
        StorageKind::Bit | StorageKind::Byte | StorageKind::Carry => 1,
        StorageKind::Bytes { length } => length.get(),
    }
}

fn piece_storage(piece: ValueStorage) -> StorageId {
    match piece {
        ValueStorage::Bit(storage) => storage,
        ValueStorage::Byte(storage) => storage.storage,
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use super::{validate_allocated, validate_virtual};
    use crate::wasm::lir::{
        Block, BlockId, ByteOperand, ByteStorage, Function, FunctionAbi, FunctionKind, Instruction,
        InstructionKind, InterruptHandlers, PhysicalPlacement, Program, Static, StaticByte,
        Storage, StorageId, StorageKind, StorageOwner, StorageRegion, Terminator, TerminatorKind,
        VirtualPlacement, WorkRamOffset,
    };

    fn storage(id: usize) -> Storage<VirtualPlacement> {
        Storage {
            id: StorageId(id),
            name: None,
            owner: StorageOwner::Function("main".into()),
            kind: StorageKind::Byte,
            placement: VirtualPlacement {
                allowed_regions: vec![StorageRegion::ApplicationRam],
                must_be_direct: false,
                fixed: None,
            },
        }
    }

    fn program(
        instructions: Vec<Instruction>,
        storages: Vec<Storage<VirtualPlacement>>,
    ) -> Program<VirtualPlacement> {
        Program {
            main: Function {
                name: "main".into(),
                kind: FunctionKind::Normal,
                abi: FunctionAbi::default(),
                blocks: vec![Block {
                    id: BlockId(0),
                    name: None,
                    instructions,
                    terminator: Terminator {
                        kind: TerminatorKind::Return,
                    },
                }],
                entry: BlockId(0),
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages,
        }
    }

    #[test]
    fn rejects_out_of_bounds_byte_lanes() {
        let invalid = Instruction {
            kind: InstructionKind::CopyByte {
                destination: ByteStorage {
                    storage: StorageId(0),
                    offset: 1,
                },
                source: ByteOperand::Immediate(0),
            },
        };
        let error = validate_virtual(&program(vec![invalid], vec![storage(0)])).unwrap_err();
        assert!(error.to_string().contains("outside 1-byte storage"));
    }

    #[test]
    fn rejects_flash_eligible_storage_that_is_not_an_immutable_static() {
        let mut flash_candidate = storage(0);
        flash_candidate.owner = StorageOwner::Program;
        flash_candidate.placement.allowed_regions =
            vec![StorageRegion::Flash, StorageRegion::WorkRam];

        let error = validate_virtual(&program(Vec::new(), vec![flash_candidate])).unwrap_err();
        assert!(error.to_string().contains("not an immutable static"));

        let mut program = program(Vec::new(), vec![storage(0)]);
        program.storages[0].owner = StorageOwner::Program;
        program.storages[0].placement.allowed_regions =
            vec![StorageRegion::Flash, StorageRegion::WorkRam];
        program.statics.push(Static {
            name: "mutable".into(),
            storage: StorageId(0),
            initializer: vec![StaticByte::Value(0)],
            mutable: true,
        });

        let error = validate_virtual(&program).unwrap_err();
        assert!(error.to_string().contains("mutable static"));
    }

    #[test]
    fn rejects_normal_call_cycles_with_static_shadow_storage() {
        let mut program = program(
            vec![Instruction {
                kind: InstructionKind::Call {
                    function: "worker".into(),
                },
            }],
            Vec::new(),
        );
        program.functions.push(Function {
            name: "worker".into(),
            kind: FunctionKind::Normal,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: None,
                instructions: vec![Instruction {
                    kind: InstructionKind::Call {
                        function: "main".into(),
                    },
                }],
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        });

        let error = validate_virtual(&program).unwrap_err();
        assert!(error.to_string().contains("re-enter a live function"));
    }

    #[test]
    fn permits_pure_tail_recursive_cycles() {
        let mut program = program(Vec::new(), Vec::new());
        program.main.blocks[0].terminator.kind = TerminatorKind::TailCall {
            function: "main".into(),
        };
        validate_virtual(&program).unwrap();
    }

    #[test]
    fn rejects_physical_objects_that_cross_the_end_of_work_ram() {
        let virtual_program = program(Vec::new(), Vec::new());
        let program = Program {
            main: virtual_program.main,
            interrupt_handlers: virtual_program.interrupt_handlers,
            functions: virtual_program.functions,
            statics: virtual_program.statics,
            storages: vec![Storage {
                id: StorageId(0),
                name: None,
                owner: StorageOwner::Program,
                kind: StorageKind::Bytes {
                    length: NonZeroUsize::new(2).unwrap(),
                },
                placement: PhysicalPlacement::WorkRam {
                    offset: WorkRamOffset::new(WorkRamOffset::MAX).unwrap(),
                },
            }],
        };

        let error = validate_allocated(&program).unwrap_err();
        assert!(error.to_string().contains("exceeds its VMU storage region"));
    }
}
