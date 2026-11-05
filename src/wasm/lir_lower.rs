//! Lowering from VMU HIR into byte- and bit-oriented virtual LIR.

use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt,
    num::NonZeroUsize,
};

use super::{
    hir,
    lir::{self, *},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LowerError {
    DuplicateName(String),
    UnknownFunction(String),
    UnknownPlace(String),
    InvalidPlace(String),
    TypeMismatch {
        expected: hir::Type,
        actual: hir::Type,
    },
    UnsupportedType(hir::Type),
    UnsupportedExpression(String),
    InvalidControlTarget(Option<String>),
    InvalidStatic(String),
    InvalidHir(String),
    InvalidOutput(String),
}

impl fmt::Display for LowerError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DuplicateName(name) => write!(formatter, "duplicate HIR name {name:?}"),
            Self::UnknownFunction(name) => write!(formatter, "unknown HIR function {name:?}"),
            Self::UnknownPlace(name) => write!(formatter, "unknown HIR place {name:?}"),
            Self::InvalidPlace(message) => write!(formatter, "invalid HIR place: {message}"),
            Self::TypeMismatch { expected, actual } => {
                write!(
                    formatter,
                    "expected HIR type {expected:?}, found {actual:?}"
                )
            }
            Self::UnsupportedType(ty) => {
                write!(formatter, "cannot lower HIR type {ty:?} to VMU LIR")
            }
            Self::UnsupportedExpression(message) => {
                write!(
                    formatter,
                    "cannot lower HIR expression to VMU LIR: {message}"
                )
            }
            Self::InvalidControlTarget(label) => match label {
                Some(label) => write!(formatter, "no active HIR control target named {label:?}"),
                None => write!(formatter, "no active HIR control target"),
            },
            Self::InvalidStatic(message) => write!(formatter, "invalid HIR static: {message}"),
            Self::InvalidHir(message) => write!(formatter, "invalid VMU HIR: {message}"),
            Self::InvalidOutput(message) => {
                write!(formatter, "HIR lowering produced invalid LIR: {message}")
            }
        }
    }
}

impl Error for LowerError {}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Signature {
    parameters: Vec<hir::Type>,
    result: hir::Type,
}

impl Signature {
    fn of(function: &hir::Function) -> Self {
        Self {
            parameters: function
                .parameters
                .iter()
                .map(|parameter| parameter.ty.clone())
                .collect(),
            result: function.result.clone(),
        }
    }

    fn from_function_type(function: &hir::FunctionType) -> Self {
        Self {
            parameters: function.parameters.clone(),
            result: function.result.clone(),
        }
    }
}

#[derive(Clone)]
struct Location {
    storage: StorageId,
    offset: usize,
    ty: hir::Type,
    bit: bool,
}

#[derive(Clone)]
struct Value {
    ty: hir::Type,
    repr: ValueRepr,
}

#[derive(Clone)]
enum ValueRepr {
    Unit,
    Bit(BitOperand),
    Bytes(Vec<ByteOperand>),
    Aggregate(Vec<Value>),
}

struct ProgramLowerer {
    storages: Vec<Storage<VirtualPlacement>>,
    statics: Vec<lir::Static>,
    static_locations: BTreeMap<String, Location>,
    signatures: BTreeMap<String, Signature>,
    parameter_abis: Vec<(Vec<hir::Type>, Vec<AbiValue>)>,
    result_abis: Vec<(hir::Type, Vec<AbiValue>)>,
}

/// Lowers a complete VMU HIR program into virtual LIR.
pub fn lower(program: &hir::Program) -> Result<VirtualProgram, LowerError> {
    super::hir_validate::validate(program)
        .map_err(|error| LowerError::InvalidHir(error.to_string()))?;
    let program = ProgramLowerer::new(program)?.lower(program)?;
    super::lir_validate::validate_virtual(&program)
        .map_err(|error| LowerError::InvalidOutput(error.to_string()))?;
    Ok(program)
}

impl ProgramLowerer {
    fn new(program: &hir::Program) -> Result<Self, LowerError> {
        let mut signatures = BTreeMap::new();
        insert_signature(&mut signatures, &program.main)?;
        for function in interrupt_handlers(&program.interrupt_handlers)
            .into_iter()
            .flatten()
        {
            insert_signature(&mut signatures, function)?;
        }
        for function in &program.functions {
            insert_signature(&mut signatures, function)?;
        }

        Ok(Self {
            storages: Vec::new(),
            statics: Vec::new(),
            static_locations: BTreeMap::new(),
            signatures,
            parameter_abis: Vec::new(),
            result_abis: Vec::new(),
        })
    }

    fn lower(mut self, program: &hir::Program) -> Result<VirtualProgram, LowerError> {
        for static_ in &program.statics {
            self.lower_static(static_)?;
        }

        let main = self.lower_function(&program.main, FunctionKind::Normal)?;
        let interrupt_handlers = lir::InterruptHandlers {
            external_0: self.lower_optional_interrupt(&program.interrupt_handlers.external_0)?,
            external_1: self.lower_optional_interrupt(&program.interrupt_handlers.external_1)?,
            external_2_or_timer_0_low: self
                .lower_optional_interrupt(&program.interrupt_handlers.external_2_or_timer_0_low)?,
            external_3_or_base_timer: self
                .lower_optional_interrupt(&program.interrupt_handlers.external_3_or_base_timer)?,
            timer_0_high: self
                .lower_optional_interrupt(&program.interrupt_handlers.timer_0_high)?,
            timer_1: self.lower_optional_interrupt(&program.interrupt_handlers.timer_1)?,
            serial_0: self.lower_optional_interrupt(&program.interrupt_handlers.serial_0)?,
            serial_1: self.lower_optional_interrupt(&program.interrupt_handlers.serial_1)?,
            dreamcast_interface: self
                .lower_optional_interrupt(&program.interrupt_handlers.dreamcast_interface)?,
            port_3: self.lower_optional_interrupt(&program.interrupt_handlers.port_3)?,
        };
        let functions = program
            .functions
            .iter()
            .map(|function| self.lower_function(function, FunctionKind::Normal))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(lir::Program {
            main,
            interrupt_handlers,
            functions,
            statics: self.statics,
            storages: self.storages,
        })
    }

    fn lower_optional_interrupt(
        &mut self,
        function: &Option<hir::Function>,
    ) -> Result<Option<lir::Function>, LowerError> {
        function
            .as_ref()
            .map(|function| self.lower_function(function, FunctionKind::Interrupt))
            .transpose()
    }

    fn lower_static(&mut self, static_: &hir::Static) -> Result<(), LowerError> {
        if self.static_locations.contains_key(&static_.name) {
            return Err(LowerError::DuplicateName(static_.name.clone()));
        }
        let size = type_size(&static_.ty)?;
        let length = NonZeroUsize::new(size).ok_or_else(|| {
            LowerError::InvalidStatic(format!("{:?} has zero size", static_.name))
        })?;
        let allowed_regions = if static_.mutable {
            vec![StorageRegion::WorkRam]
        } else {
            vec![StorageRegion::Flash, StorageRegion::WorkRam]
        };
        let storage = self.allocate_storage(
            Some(static_.name.clone()),
            StorageOwner::Program,
            if size == 1 {
                StorageKind::Byte
            } else {
                StorageKind::Bytes { length }
            },
            allowed_regions,
        );
        let initializer = flatten_static_initializer(&static_.initializer, &static_.ty)?;
        if initializer.len() != size {
            return Err(LowerError::InvalidStatic(format!(
                "{:?} initializes {} bytes, but its type occupies {size}",
                static_.name,
                initializer.len()
            )));
        }
        self.static_locations.insert(
            static_.name.clone(),
            Location {
                storage,
                offset: 0,
                ty: static_.ty.clone(),
                bit: false,
            },
        );
        self.statics.push(lir::Static {
            name: static_.name.clone(),
            storage,
            initializer,
            mutable: static_.mutable,
        });
        Ok(())
    }

    fn lower_function(
        &mut self,
        function: &hir::Function,
        kind: FunctionKind,
    ) -> Result<lir::Function, LowerError> {
        let signature = Signature::of(function);
        let abi = self.abi(&signature)?;
        FunctionLowerer::new(self, function, kind, abi).lower()
    }

    fn abi(&mut self, signature: &Signature) -> Result<FunctionAbi, LowerError> {
        let parameters = if let Some((_, parameters)) = self
            .parameter_abis
            .iter()
            .find(|(types, _)| types == &signature.parameters)
        {
            parameters.clone()
        } else {
            let parameters = signature
                .parameters
                .iter()
                .enumerate()
                .map(|(index, ty)| {
                    Ok(AbiValue {
                        logical_index: index,
                        pieces: self.allocate_abi_pieces(ty, format!("arg{index}"))?,
                    })
                })
                .collect::<Result<Vec<_>, LowerError>>()?;
            self.parameter_abis
                .push((signature.parameters.clone(), parameters.clone()));
            parameters
        };
        let results = if signature.result == hir::Type::Unit {
            Vec::new()
        } else if let Some((_, results)) = self
            .result_abis
            .iter()
            .find(|(ty, _)| ty == &signature.result)
        {
            results.clone()
        } else {
            let results = vec![AbiValue {
                logical_index: 0,
                pieces: self.allocate_abi_pieces(&signature.result, "result".into())?,
            }];
            self.result_abis
                .push((signature.result.clone(), results.clone()));
            results
        };

        let clobbered = parameters
            .iter()
            .chain(&results)
            .flat_map(|value| value.pieces.iter())
            .map(|piece| match piece {
                ValueStorage::Bit(storage) => *storage,
                ValueStorage::Byte(storage) => storage.storage,
            })
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        Ok(FunctionAbi {
            parameters,
            results,
            preserved: Vec::new(),
            clobbered,
        })
    }

    fn allocate_abi_pieces(
        &mut self,
        ty: &hir::Type,
        name: String,
    ) -> Result<Vec<ValueStorage>, LowerError> {
        let mut pieces = Vec::new();
        self.allocate_abi_type(ty, &name, &mut pieces)?;
        Ok(pieces)
    }

    fn allocate_abi_type(
        &mut self,
        ty: &hir::Type,
        name: &str,
        pieces: &mut Vec<ValueStorage>,
    ) -> Result<(), LowerError> {
        match ty {
            hir::Type::Unit => {}
            hir::Type::Bool => {
                let storage = self.allocate_storage(
                    Some(name.into()),
                    StorageOwner::Program,
                    StorageKind::Bit,
                    vec![StorageRegion::ApplicationRam, StorageRegion::WorkRam],
                );
                pieces.push(ValueStorage::Bit(storage));
            }
            hir::Type::Bits(_) | hir::Type::Pointer => {
                for index in 0..type_size(ty)? {
                    let storage = self.allocate_storage(
                        Some(format!("{name}.b{index}")),
                        StorageOwner::Program,
                        StorageKind::Byte,
                        vec![StorageRegion::ApplicationRam, StorageRegion::WorkRam],
                    );
                    pieces.push(ValueStorage::Byte(ByteStorage { storage, offset: 0 }));
                }
            }
            hir::Type::Array { element, length } => {
                for index in 0..*length {
                    self.allocate_abi_type(element, &format!("{name}.{index}"), pieces)?;
                }
            }
            hir::Type::Struct(fields) | hir::Type::Tuple(fields) => {
                for (index, field) in fields.iter().enumerate() {
                    self.allocate_abi_type(field, &format!("{name}.{index}"), pieces)?;
                }
            }
            hir::Type::OpaqueBytes(length) => {
                for index in 0..*length {
                    let storage = self.allocate_storage(
                        Some(format!("{name}.b{index}")),
                        StorageOwner::Program,
                        StorageKind::Byte,
                        vec![StorageRegion::ApplicationRam, StorageRegion::WorkRam],
                    );
                    pieces.push(ValueStorage::Byte(ByteStorage { storage, offset: 0 }));
                }
            }
            hir::Type::Float(_) => return Err(LowerError::UnsupportedType(ty.clone())),
        }
        Ok(())
    }

    fn allocate_storage(
        &mut self,
        name: Option<String>,
        owner: StorageOwner,
        kind: StorageKind,
        allowed_regions: Vec<StorageRegion>,
    ) -> StorageId {
        let id = StorageId(self.storages.len());
        self.storages.push(Storage {
            id,
            name,
            owner,
            kind,
            placement: VirtualPlacement {
                allowed_regions,
                must_be_direct: false,
                fixed: None,
            },
        });
        id
    }
}

struct OpenBlock {
    id: BlockId,
    name: Option<String>,
    instructions: Vec<Instruction>,
    terminator: Option<Terminator>,
}

struct ControlTarget {
    label: Option<String>,
    break_target: BlockId,
    continue_target: Option<BlockId>,
}

struct FunctionLowerer<'a> {
    program: &'a mut ProgramLowerer,
    function: &'a hir::Function,
    kind: FunctionKind,
    abi: FunctionAbi,
    variables: BTreeMap<String, Location>,
    blocks: Vec<OpenBlock>,
    current: BlockId,
    controls: Vec<ControlTarget>,
    next_temporary: usize,
}

impl<'a> FunctionLowerer<'a> {
    fn new(
        program: &'a mut ProgramLowerer,
        function: &'a hir::Function,
        kind: FunctionKind,
        abi: FunctionAbi,
    ) -> Self {
        Self {
            program,
            function,
            kind,
            abi,
            variables: BTreeMap::new(),
            blocks: vec![OpenBlock {
                id: BlockId(0),
                name: Some("entry".into()),
                instructions: Vec::new(),
                terminator: None,
            }],
            current: BlockId(0),
            controls: Vec::new(),
            next_temporary: 0,
        }
    }

    fn lower(mut self) -> Result<lir::Function, LowerError> {
        for (parameter, abi) in self
            .function
            .parameters
            .iter()
            .zip(self.abi.parameters.clone())
        {
            let location = self.allocate_location(&parameter.name, &parameter.ty)?;
            let value = self.value_from_abi(&parameter.ty, &abi.pieces)?;
            self.copy_to_location(&location, value)?;
            self.variables.insert(parameter.name.clone(), location);
        }

        self.lower_statements(&self.function.body.statements)?;
        if !self.is_terminated() {
            if self.function.result == hir::Type::Unit {
                self.terminate(TerminatorKind::Return);
            } else {
                self.terminate(TerminatorKind::Unreachable);
            }
        }

        let blocks = self
            .blocks
            .into_iter()
            .map(|block| Block {
                id: block.id,
                name: block.name,
                instructions: block.instructions,
                terminator: block.terminator.unwrap_or(Terminator {
                    kind: TerminatorKind::Unreachable,
                }),
            })
            .collect();
        Ok(lir::Function {
            name: self.function.name.clone(),
            kind: self.kind,
            abi: self.abi,
            blocks,
            entry: BlockId(0),
        })
    }

    fn lower_statements(&mut self, statements: &[hir::Statement]) -> Result<(), LowerError> {
        for statement in statements {
            if self.is_terminated() {
                break;
            }
            self.lower_statement(statement)?;
        }
        Ok(())
    }

    fn lower_statement(&mut self, statement: &hir::Statement) -> Result<(), LowerError> {
        use hir::Statement::*;
        match statement {
            Declare {
                name,
                ty,
                initializer,
                ..
            } => {
                if self.variables.contains_key(name) {
                    return Err(LowerError::DuplicateName(name.clone()));
                }
                let location = self.allocate_location(name, ty)?;
                if let Some(initializer) = initializer {
                    let value = self.lower_expression(initializer)?;
                    self.copy_to_location(&location, value)?;
                }
                self.variables.insert(name.clone(), location);
            }
            Assign { destination, value } => {
                let value = self.lower_expression(value)?;
                let location = self.resolve_place(destination, &value.ty)?;
                self.copy_to_location(&location, value)?;
            }
            Load {
                destination,
                address,
                ty,
                ..
            } => {
                let address = self.lower_address(address)?;
                let temporary = self.allocate_temporary(ty)?;
                for (index, destination) in self.location_bytes(&temporary)?.into_iter().enumerate()
                {
                    self.emit(InstructionKind::LoadIndirect {
                        destination,
                        address: address.clone().with_offset(index as u16),
                    });
                }
                if let Some(destination) = destination {
                    let value = self.read_location(&temporary)?;
                    let destination = self.resolve_place(destination, ty)?;
                    self.copy_to_location(&destination, value)?;
                }
            }
            Store {
                address, value, ty, ..
            } => {
                let address = self.lower_address(address)?;
                let value = self.lower_expression(value)?;
                let bytes = self.value_as_memory_bytes(value, ty)?;
                for (index, value) in bytes.into_iter().enumerate() {
                    self.emit(InstructionKind::StoreIndirect {
                        address: address.clone().with_offset(index as u16),
                        value,
                    });
                }
            }
            MemoryFill {
                destination,
                value,
                length,
            } => self.lower_memory_fill(destination, value, length)?,
            Call {
                destination,
                function,
                arguments,
            } => self.lower_call(destination.as_ref(), function, arguments)?,
            CallIndirect {
                destination,
                target,
                signature,
                arguments,
            } => self.lower_indirect_call(destination.as_ref(), target, signature, arguments)?,
            TailCall {
                function,
                arguments,
            } => self.lower_tail_call(function, arguments)?,
            TailCallIndirect {
                target,
                signature,
                arguments,
            } => self.lower_indirect_tail_call(target, signature, arguments)?,
            Intrinsic {
                destination,
                operation,
                arguments,
            } => self.lower_intrinsic(destination.as_ref(), *operation, arguments)?,
            Block { label, body } => self.lower_block(label, body)?,
            If {
                condition,
                then_body,
                else_body,
            } => self.lower_if(condition, then_body, else_body.as_ref())?,
            Loop { label, body } => self.lower_loop(label, body)?,
            Switch {
                value,
                cases,
                default,
            } => self.lower_switch(value, cases, default)?,
            Break { label } => {
                let target = self.find_control(label.as_deref(), false)?;
                self.terminate(TerminatorKind::Jump(target));
            }
            Continue { label } => {
                let target = self.find_control(label.as_deref(), true)?;
                self.terminate(TerminatorKind::Jump(target));
            }
            Return(value) => {
                if let Some(value) = value {
                    let value = self.lower_expression(value)?;
                    let pieces = self
                        .abi
                        .results
                        .first()
                        .ok_or_else(|| LowerError::TypeMismatch {
                            expected: hir::Type::Unit,
                            actual: value.ty.clone(),
                        })?
                        .pieces
                        .clone();
                    self.copy_to_abi(&pieces, value)?;
                } else if self.function.result != hir::Type::Unit {
                    return Err(LowerError::TypeMismatch {
                        expected: self.function.result.clone(),
                        actual: hir::Type::Unit,
                    });
                }
                self.terminate(TerminatorKind::Return);
            }
            Trap => self.terminate(TerminatorKind::Trap),
            Unreachable => self.terminate(TerminatorKind::Unreachable),
        }
        Ok(())
    }

    fn lower_memory_fill(
        &mut self,
        destination: &hir::Expression,
        value: &hir::Expression,
        length: &hir::Expression,
    ) -> Result<(), LowerError> {
        let destination = self.lower_address(destination)?;
        let destination_bytes = destination.bytes().to_vec();
        if destination_bytes.len() != 2 {
            return Err(LowerError::UnsupportedExpression(format!(
                "memory fill address occupies {} bytes instead of 2",
                destination_bytes.len()
            )));
        }

        let value = value_bytes(self.lower_expression(value)?)?
            .into_iter()
            .next()
            .ok_or_else(|| {
                LowerError::UnsupportedExpression("memory fill value is empty".into())
            })?;
        let length = value_bytes(self.lower_expression(length)?)?;
        if length.is_empty() {
            return Err(LowerError::UnsupportedExpression(
                "memory fill length is empty".into(),
            ));
        }

        self.require_zero_high_memory_length(length.get(2..).unwrap_or_default());
        if self.is_terminated() {
            return Ok(());
        }

        let pointer = self.allocate_temporary(&hir::Type::Pointer)?;
        let pointer_bytes = self.location_bytes(&pointer)?;
        for (destination, source) in pointer_bytes.iter().copied().zip(destination_bytes) {
            self.emit(InstructionKind::CopyByte {
                destination,
                source,
            });
        }

        let counter_size = length.len().min(2);
        let counter_type = hir::Type::Bits(
            hir::BitWidth::new((counter_size * 8) as u8)
                .expect("one- or two-byte memory fill counter"),
        );
        let counter = self.allocate_temporary(&counter_type)?;
        let counter_bytes = self.location_bytes(&counter)?;
        for (destination, source) in counter_bytes.iter().copied().zip(length) {
            self.emit(InstructionKind::CopyByte {
                destination,
                source,
            });
        }

        let header = self.new_block(Some("memory_fill_test".into()));
        let body = self.new_block(Some("memory_fill_body".into()));
        let after = self.new_block(Some("after_memory_fill".into()));
        self.terminate(TerminatorKind::Jump(header));

        self.current = header;
        let condition = if let [counter] = counter_bytes.as_slice() {
            BranchCondition::ByteZero(ByteOperand::Storage(*counter))
        } else {
            let is_zero = self.allocate_bit_temporary("memory_fill_empty");
            self.emit(InstructionKind::CompareBytes {
                op: CompareOp::Equal,
                destination: is_zero,
                left: counter_bytes
                    .iter()
                    .copied()
                    .map(ByteOperand::Storage)
                    .collect(),
                right: vec![ByteOperand::Immediate(0); counter_bytes.len()],
                width: (counter_bytes.len() * 8) as u8,
            });
            BranchCondition::BitSet(is_zero)
        };
        self.terminate(TerminatorKind::Branch {
            condition,
            when_true: after,
            when_false: body,
        });

        self.current = body;
        let address = Address::new(
            destination.space(),
            pointer_bytes
                .iter()
                .copied()
                .map(ByteOperand::Storage)
                .collect(),
        )
        .expect("memory fill pointer is nonempty");
        self.emit(InstructionKind::StoreIndirect { address, value });
        self.increment_bytes(&pointer_bytes, "memory_fill_pointer_carry");
        self.decrement_bytes(&counter_bytes, "memory_fill_counter_borrow");
        self.terminate(TerminatorKind::Jump(header));
        self.current = after;
        Ok(())
    }

    fn require_zero_high_memory_length(&mut self, high_bytes: &[ByteOperand]) {
        if high_bytes.is_empty()
            || high_bytes
                .iter()
                .all(|byte| *byte == ByteOperand::Immediate(0))
        {
            return;
        }
        if high_bytes
            .iter()
            .any(|byte| matches!(byte, ByteOperand::Immediate(value) if *value != 0))
        {
            self.terminate(TerminatorKind::Trap);
            return;
        }

        let in_range = self.new_block(Some("memory_fill_length_in_range".into()));
        let trap = self.new_block(Some("memory_fill_length_too_large".into()));
        let high_is_zero = self.allocate_bit_temporary("memory_fill_high_zero");
        self.emit(InstructionKind::CompareBytes {
            op: CompareOp::Equal,
            destination: high_is_zero,
            left: high_bytes.to_vec(),
            right: vec![ByteOperand::Immediate(0); high_bytes.len()],
            width: (high_bytes.len() * 8) as u8,
        });
        self.terminate(TerminatorKind::Branch {
            condition: BranchCondition::BitSet(high_is_zero),
            when_true: in_range,
            when_false: trap,
        });
        self.current = trap;
        self.terminate(TerminatorKind::Trap);
        self.current = in_range;
    }

    fn increment_bytes(&mut self, bytes: &[ByteStorage], carry_name: &str) {
        let carry = if bytes.len() > 1 {
            Some(self.allocate_carry(carry_name))
        } else {
            None
        };
        for (index, destination) in bytes.iter().copied().enumerate() {
            self.emit(InstructionKind::Add {
                destination,
                left: ByteOperand::Storage(destination),
                right: ByteOperand::Immediate(u8::from(index == 0)),
                carry_in: if index == 0 { None } else { carry },
                carry_out: if index + 1 == bytes.len() {
                    None
                } else {
                    carry
                },
            });
        }
    }

    fn decrement_bytes(&mut self, bytes: &[ByteStorage], borrow_name: &str) {
        let borrow = if bytes.len() > 1 {
            Some(self.allocate_carry(borrow_name))
        } else {
            None
        };
        for (index, destination) in bytes.iter().copied().enumerate() {
            self.emit(InstructionKind::Subtract {
                destination,
                left: ByteOperand::Storage(destination),
                right: ByteOperand::Immediate(u8::from(index == 0)),
                borrow_in: if index == 0 { None } else { borrow },
                borrow_out: if index + 1 == bytes.len() {
                    None
                } else {
                    borrow
                },
            });
        }
    }

    fn lower_call(
        &mut self,
        destination: Option<&hir::Place>,
        function: &str,
        arguments: &[hir::Expression],
    ) -> Result<(), LowerError> {
        let signature = self
            .program
            .signatures
            .get(function)
            .cloned()
            .ok_or_else(|| LowerError::UnknownFunction(function.into()))?;
        let abi = self.program.abi(&signature)?;
        let values = self.lower_expressions(arguments)?;
        self.write_arguments(&abi, &signature.parameters, values)?;
        self.emit(InstructionKind::Call {
            function: function.into(),
        });
        self.write_call_result(destination, &signature.result, &abi)
    }

    fn lower_indirect_call(
        &mut self,
        destination: Option<&hir::Place>,
        target: &hir::Expression,
        signature: &hir::FunctionType,
        arguments: &[hir::Expression],
    ) -> Result<(), LowerError> {
        let signature = Signature::from_function_type(signature);
        let abi = self.program.abi(&signature)?;
        let values = self.lower_expressions(arguments)?;
        let target = self.lower_code_address(target)?;
        self.write_arguments(&abi, &signature.parameters, values)?;
        self.emit(InstructionKind::CallIndirect {
            target,
            abi: abi.clone(),
        });
        self.write_call_result(destination, &signature.result, &abi)
    }

    fn lower_tail_call(
        &mut self,
        function: &str,
        arguments: &[hir::Expression],
    ) -> Result<(), LowerError> {
        let signature = self
            .program
            .signatures
            .get(function)
            .cloned()
            .ok_or_else(|| LowerError::UnknownFunction(function.into()))?;
        let abi = self.program.abi(&signature)?;
        let values = self.lower_expressions(arguments)?;
        self.write_arguments(&abi, &signature.parameters, values)?;
        self.terminate(TerminatorKind::TailCall {
            function: function.into(),
        });
        Ok(())
    }

    fn lower_indirect_tail_call(
        &mut self,
        target: &hir::Expression,
        signature: &hir::FunctionType,
        arguments: &[hir::Expression],
    ) -> Result<(), LowerError> {
        let signature = Signature::from_function_type(signature);
        let abi = self.program.abi(&signature)?;
        let values = self.lower_expressions(arguments)?;
        let target = self.lower_code_address(target)?;
        self.write_arguments(&abi, &signature.parameters, values)?;
        self.terminate(TerminatorKind::TailCallIndirect { target, abi });
        Ok(())
    }

    fn lower_intrinsic(
        &mut self,
        destination: Option<&hir::Place>,
        operation: super::intrinsics::Intrinsic,
        arguments: &[hir::Expression],
    ) -> Result<(), LowerError> {
        let values = self.lower_expressions(arguments)?;
        let definition = operation.definition().ok_or_else(|| {
            LowerError::UnsupportedExpression(format!(
                "intrinsic {} has no registered definition",
                operation.display_name()
            ))
        })?;
        if definition.parameters.len() != values.len() {
            return Err(LowerError::UnsupportedExpression(format!(
                "intrinsic {} has {} arguments for {} parameters",
                operation.display_name(),
                values.len(),
                definition.parameters.len()
            )));
        }
        for (parameter, value) in definition.parameters.iter().zip(&values) {
            let expected = intrinsic_abi_type(parameter.ty);
            if value.ty != expected {
                return Err(LowerError::TypeMismatch {
                    expected,
                    actual: value.ty.clone(),
                });
            }
        }
        let inputs = values
            .into_iter()
            .flat_map(flatten_value_operands)
            .collect();
        let result_ty = definition
            .result
            .map(intrinsic_abi_type)
            .unwrap_or(hir::Type::Unit);
        let result = if result_ty == hir::Type::Unit {
            None
        } else {
            Some(self.allocate_temporary(&result_ty)?)
        };
        let outputs = result
            .as_ref()
            .map(|location| self.location_value_storages(location))
            .transpose()?
            .unwrap_or_default();
        self.emit(InstructionKind::Intrinsic {
            operation,
            inputs,
            outputs,
        });
        if let (Some(destination), Some(result)) = (destination, result) {
            let value = self.read_location(&result)?;
            let destination = self.resolve_place(destination, &result_ty)?;
            self.copy_to_location(&destination, value)?;
        }
        Ok(())
    }

    fn lower_block(&mut self, label: &Option<String>, body: &hir::Block) -> Result<(), LowerError> {
        let after = self.new_block(label.as_ref().map(|label| format!("after_{label}")));
        self.controls.push(ControlTarget {
            label: label.clone(),
            break_target: after,
            continue_target: None,
        });
        self.lower_statements(&body.statements)?;
        self.controls.pop();
        if !self.is_terminated() {
            self.terminate(TerminatorKind::Jump(after));
        }
        self.current = after;
        Ok(())
    }

    fn lower_if(
        &mut self,
        condition: &hir::Expression,
        then_body: &hir::Block,
        else_body: Option<&hir::Block>,
    ) -> Result<(), LowerError> {
        let condition = self.expression_as_bit(condition)?;
        let then_block = self.new_block(Some("if_true".into()));
        let else_block = self.new_block(Some("if_false".into()));
        let after = self.new_block(Some("after_if".into()));
        let condition = bit_storage(condition, self)?;
        self.terminate(TerminatorKind::Branch {
            condition: BranchCondition::BitSet(condition),
            when_true: then_block,
            when_false: else_block,
        });

        self.current = then_block;
        self.lower_statements(&then_body.statements)?;
        if !self.is_terminated() {
            self.terminate(TerminatorKind::Jump(after));
        }

        self.current = else_block;
        if let Some(else_body) = else_body {
            self.lower_statements(&else_body.statements)?;
        }
        if !self.is_terminated() {
            self.terminate(TerminatorKind::Jump(after));
        }
        self.current = after;
        Ok(())
    }

    fn lower_loop(&mut self, label: &Option<String>, body: &hir::Block) -> Result<(), LowerError> {
        let header = self.new_block(label.clone().or_else(|| Some("loop".into())));
        let after = self.new_block(label.as_ref().map(|label| format!("after_{label}")));
        self.terminate(TerminatorKind::Jump(header));
        self.current = header;
        self.controls.push(ControlTarget {
            label: label.clone(),
            break_target: after,
            continue_target: Some(header),
        });
        self.lower_statements(&body.statements)?;
        self.controls.pop();
        if !self.is_terminated() {
            self.terminate(TerminatorKind::Jump(header));
        }
        self.current = after;
        Ok(())
    }

    fn lower_switch(
        &mut self,
        value: &hir::Expression,
        cases: &[hir::SwitchCase],
        default: &hir::Block,
    ) -> Result<(), LowerError> {
        let value = self.lower_expression(value)?;
        let width = integer_width(&value.ty)?;
        let value = value_bytes(value)?;
        let after = self.new_block(Some("after_switch".into()));
        let case_blocks = cases
            .iter()
            .enumerate()
            .map(|(index, _)| self.new_block(Some(format!("case_{index}"))))
            .collect::<Vec<_>>();
        let default_block = self.new_block(Some("switch_default".into()));
        self.terminate(TerminatorKind::Switch {
            value,
            width,
            cases: cases
                .iter()
                .zip(&case_blocks)
                .map(|(case, block)| (case.value.bits(), *block))
                .collect(),
            default: default_block,
        });

        for (case, block) in cases.iter().zip(case_blocks) {
            self.current = block;
            self.lower_statements(&case.body.statements)?;
            if !self.is_terminated() {
                self.terminate(TerminatorKind::Jump(after));
            }
        }
        self.current = default_block;
        self.lower_statements(&default.statements)?;
        if !self.is_terminated() {
            self.terminate(TerminatorKind::Jump(after));
        }
        self.current = after;
        Ok(())
    }

    fn lower_expressions(
        &mut self,
        expressions: &[hir::Expression],
    ) -> Result<Vec<Value>, LowerError> {
        expressions
            .iter()
            .map(|expression| self.lower_expression(expression))
            .collect()
    }

    fn lower_expression(&mut self, expression: &hir::Expression) -> Result<Value, LowerError> {
        use hir::ExpressionKind::*;
        match &expression.kind {
            Constant(constant) => self.lower_constant(constant, &expression.ty),
            Read(place) => {
                let location = self.resolve_place(place, &expression.ty)?;
                self.read_location(&location)
            }
            IntegerUnary { op, operand } => {
                let operand = self.lower_expression(operand)?;
                self.lower_integer_unary(*op, operand, &expression.ty)
            }
            IntegerBinary { op, left, right } => {
                let left = self.lower_expression(left)?;
                let right = self.lower_expression(right)?;
                self.lower_integer_binary(*op, left, right, &expression.ty)
            }
            FloatUnary { .. } | FloatBinary { .. } => Err(LowerError::UnsupportedExpression(
                "floating-point operations require a soft-float lowering pass".into(),
            )),
            Compare { op, left, right } => {
                let left = self.lower_expression(left)?;
                let right = self.lower_expression(right)?;
                self.lower_compare(*op, left, right)
            }
            Convert { op, operand } => {
                let operand = self.lower_expression(operand)?;
                self.lower_conversion(*op, operand, &expression.ty)
            }
            Select {
                when_true,
                when_false,
                condition,
            } => {
                let when_true = self.lower_expression(when_true)?;
                let when_false = self.lower_expression(when_false)?;
                let condition = self.expression_as_bit(condition)?;
                self.lower_select(condition, when_true, when_false, &expression.ty)
            }
            AddressOf(place) => self.lower_address_of(place),
            PointerOffset { base, offset } => {
                let base = self.lower_expression(base)?;
                let offset = self.lower_expression(offset)?;
                self.lower_pointer_offset(base, offset)
            }
            Aggregate(elements) => Ok(Value {
                ty: expression.ty.clone(),
                repr: ValueRepr::Aggregate(self.lower_expressions(elements)?),
            }),
            ExtractElement { aggregate, index } => {
                let aggregate = self.lower_expression(aggregate)?;
                let ValueRepr::Aggregate(elements) = aggregate.repr else {
                    return Err(LowerError::UnsupportedExpression(
                        "extract_element source is not an aggregate".into(),
                    ));
                };
                elements.get(*index).cloned().ok_or_else(|| {
                    LowerError::UnsupportedExpression(format!(
                        "aggregate has no element at index {index}"
                    ))
                })
            }
            InsertElement {
                aggregate,
                index,
                value,
            } => {
                let aggregate = self.lower_expression(aggregate)?;
                let value = self.lower_expression(value)?;
                let ValueRepr::Aggregate(mut elements) = aggregate.repr else {
                    return Err(LowerError::UnsupportedExpression(
                        "insert_element destination is not an aggregate".into(),
                    ));
                };
                let Some(element) = elements.get_mut(*index) else {
                    return Err(LowerError::UnsupportedExpression(format!(
                        "aggregate has no element at index {index}"
                    )));
                };
                *element = value;
                Ok(Value {
                    ty: expression.ty.clone(),
                    repr: ValueRepr::Aggregate(elements),
                })
            }
        }
    }

    fn lower_constant(
        &mut self,
        constant: &hir::Constant,
        ty: &hir::Type,
    ) -> Result<Value, LowerError> {
        use hir::Constant::*;
        match constant {
            Bool(value) => Ok(Value {
                ty: ty.clone(),
                repr: ValueRepr::Bit(BitOperand::Constant(*value)),
            }),
            Integer(value) => Ok(Value {
                ty: ty.clone(),
                repr: ValueRepr::Bytes(integer_bytes(value.bits(), type_size(ty)?)),
            }),
            Float32(_) | Float64(_) => Err(LowerError::UnsupportedType(ty.clone())),
            NullPointer => Ok(Value {
                ty: ty.clone(),
                repr: ValueRepr::Bytes(vec![ByteOperand::Immediate(0), ByteOperand::Immediate(0)]),
            }),
            StaticAddress { name, offset } => {
                let location = self
                    .program
                    .static_locations
                    .get(name)
                    .cloned()
                    .ok_or_else(|| LowerError::UnknownPlace(name.clone()))?;
                self.materialize_location_address(&location, *offset)
            }
            FunctionAddress(function) => {
                let temporary = self.allocate_temporary(&hir::Type::Pointer)?;
                let [low, high] = self.location_bytes(&temporary)?.try_into().unwrap();
                self.emit(InstructionKind::FunctionAddress {
                    destination_low: low,
                    destination_high: high,
                    function: function.clone(),
                });
                self.read_location(&temporary)
            }
        }
    }

    fn lower_integer_unary(
        &mut self,
        op: hir::IntegerUnaryOp,
        operand: Value,
        ty: &hir::Type,
    ) -> Result<Value, LowerError> {
        let input = value_bytes(operand)?;
        let temporary = self.allocate_temporary(ty)?;
        let destinations = self.location_bytes(&temporary)?;
        match op {
            hir::IntegerUnaryOp::Not => {
                for (destination, input) in destinations.iter().copied().zip(input) {
                    self.emit(InstructionKind::ByteUnary {
                        op: ByteUnaryOp::Not,
                        destination,
                        input,
                    });
                }
            }
            hir::IntegerUnaryOp::Negate => {
                let borrow = self.allocate_carry("negate_borrow");
                let length = destinations.len();
                for (index, (destination, input)) in
                    destinations.iter().copied().zip(input).enumerate()
                {
                    self.emit(InstructionKind::Subtract {
                        destination,
                        left: ByteOperand::Immediate(0),
                        right: input,
                        borrow_in: (index != 0).then_some(borrow),
                        borrow_out: (index + 1 != length).then_some(borrow),
                    });
                }
            }
            hir::IntegerUnaryOp::CountLeadingZeros
            | hir::IntegerUnaryOp::CountTrailingZeros
            | hir::IntegerUnaryOp::PopulationCount => {
                let op = match op {
                    hir::IntegerUnaryOp::CountLeadingZeros => {
                        IntegerUnaryBytesOp::CountLeadingZeros
                    }
                    hir::IntegerUnaryOp::CountTrailingZeros => {
                        IntegerUnaryBytesOp::CountTrailingZeros
                    }
                    hir::IntegerUnaryOp::PopulationCount => IntegerUnaryBytesOp::PopulationCount,
                    _ => unreachable!(),
                };
                self.emit(InstructionKind::IntegerUnaryBytes {
                    op,
                    destinations: destinations.clone(),
                    input,
                    width: integer_width(ty)?,
                });
            }
        }
        self.mask_location(&temporary)?;
        self.read_location(&temporary)
    }

    fn lower_integer_binary(
        &mut self,
        op: hir::IntegerBinaryOp,
        left: Value,
        right: Value,
        ty: &hir::Type,
    ) -> Result<Value, LowerError> {
        let left = value_bytes(left)?;
        let right = value_bytes(right)?;
        let temporary = self.allocate_temporary(ty)?;
        let destinations = self.location_bytes(&temporary)?;
        match op {
            hir::IntegerBinaryOp::Add | hir::IntegerBinaryOp::Subtract => {
                let carry = self.allocate_carry(match op {
                    hir::IntegerBinaryOp::Add => "add_carry",
                    _ => "subtract_borrow",
                });
                let length = destinations.len();
                for (index, ((destination, left), right)) in destinations
                    .iter()
                    .copied()
                    .zip(left)
                    .zip(right)
                    .enumerate()
                {
                    if op == hir::IntegerBinaryOp::Add {
                        self.emit(InstructionKind::Add {
                            destination,
                            left,
                            right,
                            carry_in: (index != 0).then_some(carry),
                            carry_out: (index + 1 != length).then_some(carry),
                        });
                    } else {
                        self.emit(InstructionKind::Subtract {
                            destination,
                            left,
                            right,
                            borrow_in: (index != 0).then_some(carry),
                            borrow_out: (index + 1 != length).then_some(carry),
                        });
                    }
                }
            }
            hir::IntegerBinaryOp::And | hir::IntegerBinaryOp::Or | hir::IntegerBinaryOp::Xor => {
                let op = match op {
                    hir::IntegerBinaryOp::And => ByteBinaryOp::And,
                    hir::IntegerBinaryOp::Or => ByteBinaryOp::Or,
                    hir::IntegerBinaryOp::Xor => ByteBinaryOp::Xor,
                    _ => unreachable!(),
                };
                for ((destination, left), right) in
                    destinations.iter().copied().zip(left).zip(right)
                {
                    self.emit(InstructionKind::ByteBinary {
                        op,
                        destination,
                        left,
                        right,
                    });
                }
            }
            hir::IntegerBinaryOp::ShiftLeft
            | hir::IntegerBinaryOp::ShiftRight(_)
            | hir::IntegerBinaryOp::RotateLeft
            | hir::IntegerBinaryOp::RotateRight => {
                let op = match op {
                    hir::IntegerBinaryOp::ShiftLeft => ShiftOp::Left,
                    hir::IntegerBinaryOp::ShiftRight(hir::Signedness::Unsigned) => {
                        ShiftOp::RightUnsigned
                    }
                    hir::IntegerBinaryOp::ShiftRight(hir::Signedness::Signed) => {
                        ShiftOp::RightSigned
                    }
                    hir::IntegerBinaryOp::RotateLeft => ShiftOp::RotateLeft,
                    hir::IntegerBinaryOp::RotateRight => ShiftOp::RotateRight,
                    _ => unreachable!(),
                };
                let width = integer_width(ty)?;
                let byte_rotate = matches!(op, ShiftOp::RotateLeft | ShiftOp::RotateRight)
                    && width == 8
                    && destinations.len() == 1
                    && left.len() == 1
                    && immediate_bytes(&right).is_some();
                if byte_rotate {
                    let amount = (immediate_bytes(&right).unwrap() % 8) as usize;
                    let (direction, count) = match (op, amount <= 4) {
                        (ShiftOp::RotateLeft, true) | (ShiftOp::RotateRight, false) => {
                            (RotateDirection::Left, amount.min(8 - amount))
                        }
                        (ShiftOp::RotateRight, true) | (ShiftOp::RotateLeft, false) => {
                            (RotateDirection::Right, amount.min(8 - amount))
                        }
                        _ => unreachable!(),
                    };
                    if count == 0 {
                        self.emit(InstructionKind::CopyByte {
                            destination: destinations[0],
                            source: left[0],
                        });
                    } else {
                        for index in 0..count {
                            self.emit(InstructionKind::Rotate {
                                direction,
                                through_carry: false,
                                destination: destinations[0],
                                input: if index == 0 {
                                    left[0]
                                } else {
                                    ByteOperand::Storage(destinations[0])
                                },
                                carry_in: None,
                                carry_out: None,
                            });
                        }
                    }
                } else {
                    self.emit(InstructionKind::ShiftBytes {
                        op,
                        destinations: destinations.clone(),
                        value: left,
                        amount: right,
                        width,
                    });
                }
            }
            _ => {
                let op = integer_binary_bytes_op(op);
                self.emit(InstructionKind::IntegerBinaryBytes {
                    op,
                    destinations: destinations.clone(),
                    left,
                    right,
                    width: integer_width(ty)?,
                });
            }
        }
        self.mask_location(&temporary)?;
        self.read_location(&temporary)
    }

    fn lower_compare(
        &mut self,
        op: hir::CompareOp,
        left: Value,
        right: Value,
    ) -> Result<Value, LowerError> {
        let width = integer_width(&left.ty)?;
        let left = value_bytes(left)?;
        let right = value_bytes(right)?;
        let destination = self.allocate_bit_temporary("compare");
        self.emit(InstructionKind::CompareBytes {
            op: compare_op(op)?,
            destination,
            left,
            right,
            width,
        });
        Ok(Value {
            ty: hir::Type::Bool,
            repr: ValueRepr::Bit(BitOperand::Storage(destination)),
        })
    }

    fn lower_conversion(
        &mut self,
        op: hir::ConversionOp,
        operand: Value,
        ty: &hir::Type,
    ) -> Result<Value, LowerError> {
        match op {
            hir::ConversionOp::Truncate | hir::ConversionOp::ZeroExtend => {
                let mut bytes = value_bytes(operand)?;
                bytes.resize(type_size(ty)?, ByteOperand::Immediate(0));
                bytes.truncate(type_size(ty)?);
                self.normalized_bytes(ty, bytes)
            }
            hir::ConversionOp::SignExtend => {
                let source_width = integer_width(&operand.ty)?;
                let source = value_bytes(operand)?;
                let temporary = self.allocate_temporary(ty)?;
                let destinations = self.location_bytes(&temporary)?;
                self.emit(InstructionKind::SignExtend {
                    destinations,
                    source,
                    source_width,
                });
                self.mask_location(&temporary)?;
                self.read_location(&temporary)
            }
            hir::ConversionOp::BooleanToInteger => {
                let source = value_bit(operand)?;
                let temporary = self.allocate_temporary(ty)?;
                let destinations = self.location_bytes(&temporary)?;
                if let Some(first) = destinations.first().copied() {
                    self.emit(InstructionKind::BitToByte {
                        destination: first,
                        source,
                    });
                }
                for destination in destinations.into_iter().skip(1) {
                    self.emit(InstructionKind::CopyByte {
                        destination,
                        source: ByteOperand::Immediate(0),
                    });
                }
                self.read_location(&temporary)
            }
            hir::ConversionOp::IntegerToBoolean => {
                let width = integer_width(&operand.ty)?;
                let left = value_bytes(operand)?;
                let right = vec![ByteOperand::Immediate(0); left.len()];
                let destination = self.allocate_bit_temporary("integer_to_boolean");
                self.emit(InstructionKind::CompareBytes {
                    op: lir::CompareOp::NotEqual,
                    destination,
                    left,
                    right,
                    width,
                });
                Ok(Value {
                    ty: hir::Type::Bool,
                    repr: ValueRepr::Bit(BitOperand::Storage(destination)),
                })
            }
            hir::ConversionOp::Reinterpret => {
                if type_size(&operand.ty)? != type_size(ty)? {
                    return Err(LowerError::TypeMismatch {
                        expected: ty.clone(),
                        actual: operand.ty,
                    });
                }
                Ok(Value {
                    ty: ty.clone(),
                    repr: operand.repr,
                })
            }
            _ => Err(LowerError::UnsupportedExpression(format!(
                "conversion {op:?} requires soft-float support"
            ))),
        }
    }

    fn lower_select(
        &mut self,
        condition: BitOperand,
        when_true: Value,
        when_false: Value,
        ty: &hir::Type,
    ) -> Result<Value, LowerError> {
        let temporary = self.allocate_temporary(ty)?;
        self.select_to_location(&temporary, condition, when_true, when_false)?;
        self.read_location(&temporary)
    }

    fn lower_address_of(&mut self, place: &hir::Place) -> Result<Value, LowerError> {
        let (location, offset) = self.resolve_addressed_place(place)?;
        self.materialize_location_address(&location, offset)
    }

    fn lower_pointer_offset(&mut self, base: Value, offset: Value) -> Result<Value, LowerError> {
        let base = value_bytes(base)?;
        let mut offset = value_bytes(offset)?;
        offset.resize(2, ByteOperand::Immediate(0));
        let temporary = self.allocate_temporary(&hir::Type::Pointer)?;
        let destinations = self.location_bytes(&temporary)?;
        let carry = self.allocate_carry("pointer_carry");
        for index in 0..2 {
            self.emit(InstructionKind::Add {
                destination: destinations[index],
                left: base[index],
                right: offset[index],
                carry_in: (index != 0).then_some(carry),
                carry_out: (index == 0).then_some(carry),
            });
        }
        self.read_location(&temporary)
    }

    fn lower_address(&mut self, expression: &hir::Expression) -> Result<Address, LowerError> {
        let value = self.lower_expression(expression)?;
        let bytes = value_bytes(value)?;
        Address::new(AddressSpace::MainMemory, bytes)
            .ok_or_else(|| LowerError::UnsupportedExpression("empty address".into()))
    }

    fn normalized_bytes(
        &mut self,
        ty: &hir::Type,
        bytes: Vec<ByteOperand>,
    ) -> Result<Value, LowerError> {
        let value = Value {
            ty: ty.clone(),
            repr: ValueRepr::Bytes(bytes),
        };
        let hir::Type::Bits(width) = ty else {
            return Ok(value);
        };
        if width.get() % 8 == 0 {
            return Ok(value);
        }
        let temporary = self.allocate_temporary(ty)?;
        self.copy_to_location(&temporary, value)?;
        self.mask_location(&temporary)?;
        self.read_location(&temporary)
    }

    fn lower_code_address(
        &mut self,
        expression: &hir::Expression,
    ) -> Result<CodeAddress, LowerError> {
        if let hir::ExpressionKind::Constant(hir::Constant::FunctionAddress(function)) =
            &expression.kind
        {
            return Ok(CodeAddress::Function(function.clone()));
        }
        let bytes = value_bytes(self.lower_expression(expression)?)?;
        if bytes.len() != 2 {
            return Err(LowerError::UnsupportedExpression(format!(
                "indirect call target occupies {} bytes instead of 2",
                bytes.len()
            )));
        }
        Ok(CodeAddress::Computed {
            low: bytes[0],
            high: bytes[1],
        })
    }

    fn expression_as_bit(
        &mut self,
        expression: &hir::Expression,
    ) -> Result<BitOperand, LowerError> {
        let value = self.lower_expression(expression)?;
        if matches!(value.repr, ValueRepr::Bit(_)) {
            value_bit(value)
        } else {
            let width = integer_width(&value.ty)?;
            let left = value_bytes(value)?;
            let right = vec![ByteOperand::Immediate(0); left.len()];
            let destination = self.allocate_bit_temporary("condition");
            self.emit(InstructionKind::CompareBytes {
                op: lir::CompareOp::NotEqual,
                destination,
                left,
                right,
                width,
            });
            Ok(BitOperand::Storage(destination))
        }
    }

    fn write_arguments(
        &mut self,
        abi: &FunctionAbi,
        expected_types: &[hir::Type],
        values: Vec<Value>,
    ) -> Result<(), LowerError> {
        if abi.parameters.len() != values.len() || expected_types.len() != values.len() {
            return Err(LowerError::UnsupportedExpression(format!(
                "call has {} arguments for {} ABI parameters",
                values.len(),
                abi.parameters.len()
            )));
        }
        for ((parameter, expected), value) in abi.parameters.iter().zip(expected_types).zip(values)
        {
            if &value.ty != expected {
                return Err(LowerError::TypeMismatch {
                    expected: expected.clone(),
                    actual: value.ty,
                });
            }
            self.copy_to_abi(&parameter.pieces, value)?;
        }
        Ok(())
    }

    fn write_call_result(
        &mut self,
        destination: Option<&hir::Place>,
        result_ty: &hir::Type,
        abi: &FunctionAbi,
    ) -> Result<(), LowerError> {
        let Some(destination) = destination else {
            return Ok(());
        };
        let result = abi
            .results
            .first()
            .ok_or_else(|| LowerError::TypeMismatch {
                expected: hir::Type::Unit,
                actual: result_ty.clone(),
            })?;
        let value = self.value_from_abi(result_ty, &result.pieces)?;
        let destination = self.resolve_place(destination, result_ty)?;
        self.copy_to_location(&destination, value)
    }

    fn copy_to_abi(&mut self, pieces: &[ValueStorage], value: Value) -> Result<(), LowerError> {
        let operands = flatten_value_operands(value);
        if pieces.len() != operands.len() {
            return Err(LowerError::UnsupportedExpression(format!(
                "value has {} pieces for {} ABI pieces",
                operands.len(),
                pieces.len()
            )));
        }
        for (destination, source) in pieces.iter().copied().zip(operands) {
            self.copy_piece(destination, source)?;
        }
        Ok(())
    }

    fn value_from_abi(
        &mut self,
        ty: &hir::Type,
        pieces: &[ValueStorage],
    ) -> Result<Value, LowerError> {
        let mut index = 0;
        let value = self.value_from_pieces(ty, pieces, &mut index)?;
        if index != pieces.len() {
            return Err(LowerError::UnsupportedExpression(
                "ABI contains unused pieces".into(),
            ));
        }
        Ok(value)
    }

    fn value_from_pieces(
        &mut self,
        ty: &hir::Type,
        pieces: &[ValueStorage],
        index: &mut usize,
    ) -> Result<Value, LowerError> {
        match ty {
            hir::Type::Unit => Ok(Value {
                ty: ty.clone(),
                repr: ValueRepr::Unit,
            }),
            hir::Type::Bool => {
                let piece = *pieces.get(*index).ok_or_else(|| {
                    LowerError::UnsupportedExpression("ABI is missing a boolean piece".into())
                })?;
                *index += 1;
                let ValueStorage::Bit(storage) = piece else {
                    return Err(LowerError::UnsupportedExpression(
                        "boolean ABI piece is not a bit".into(),
                    ));
                };
                Ok(Value {
                    ty: ty.clone(),
                    repr: ValueRepr::Bit(BitOperand::Storage(storage)),
                })
            }
            hir::Type::Bits(_) | hir::Type::Pointer | hir::Type::OpaqueBytes(_) => {
                let mut bytes = Vec::new();
                for _ in 0..type_size(ty)? {
                    let piece = *pieces.get(*index).ok_or_else(|| {
                        LowerError::UnsupportedExpression("ABI is missing a byte piece".into())
                    })?;
                    *index += 1;
                    let ValueStorage::Byte(storage) = piece else {
                        return Err(LowerError::UnsupportedExpression(
                            "byte ABI piece is not a byte".into(),
                        ));
                    };
                    bytes.push(ByteOperand::Storage(storage));
                }
                Ok(Value {
                    ty: ty.clone(),
                    repr: ValueRepr::Bytes(bytes),
                })
            }
            hir::Type::Array { element, length } => {
                let mut elements = Vec::new();
                for _ in 0..*length {
                    elements.push(self.value_from_pieces(element, pieces, index)?);
                }
                Ok(Value {
                    ty: ty.clone(),
                    repr: ValueRepr::Aggregate(elements),
                })
            }
            hir::Type::Struct(fields) | hir::Type::Tuple(fields) => Ok(Value {
                ty: ty.clone(),
                repr: ValueRepr::Aggregate(
                    fields
                        .iter()
                        .map(|field| self.value_from_pieces(field, pieces, index))
                        .collect::<Result<Vec<_>, _>>()?,
                ),
            }),
            hir::Type::Float(_) => Err(LowerError::UnsupportedType(ty.clone())),
        }
    }

    fn allocate_location(&mut self, name: &str, ty: &hir::Type) -> Result<Location, LowerError> {
        let owner = StorageOwner::Function(self.function.name.clone());
        if *ty == hir::Type::Bool {
            let storage = self.program.allocate_storage(
                Some(name.into()),
                owner,
                StorageKind::Bit,
                vec![
                    StorageRegion::ApplicationRam,
                    StorageRegion::ShadowStack,
                    StorageRegion::WorkRam,
                ],
            );
            return Ok(Location {
                storage,
                offset: 0,
                ty: ty.clone(),
                bit: true,
            });
        }
        let size = type_size(ty)?;
        let length =
            NonZeroUsize::new(size).ok_or_else(|| LowerError::UnsupportedType(ty.clone()))?;
        let storage = self.program.allocate_storage(
            Some(name.into()),
            owner,
            if size == 1 {
                StorageKind::Byte
            } else {
                StorageKind::Bytes { length }
            },
            vec![StorageRegion::ApplicationRam, StorageRegion::ShadowStack],
        );
        Ok(Location {
            storage,
            offset: 0,
            ty: ty.clone(),
            bit: false,
        })
    }

    fn allocate_temporary(&mut self, ty: &hir::Type) -> Result<Location, LowerError> {
        let name = format!("tmp{}", self.next_temporary);
        self.next_temporary += 1;
        self.allocate_location(&name, ty)
    }

    fn allocate_bit_temporary(&mut self, purpose: &str) -> StorageId {
        let name = format!("{purpose}{}", self.next_temporary);
        self.next_temporary += 1;
        self.program.allocate_storage(
            Some(name),
            StorageOwner::Function(self.function.name.clone()),
            StorageKind::Bit,
            vec![StorageRegion::ApplicationRam, StorageRegion::ShadowStack],
        )
    }

    fn allocate_carry(&mut self, purpose: &str) -> StorageId {
        let name = format!("{purpose}{}", self.next_temporary);
        self.next_temporary += 1;
        self.program.allocate_storage(
            Some(name),
            StorageOwner::Function(self.function.name.clone()),
            StorageKind::Carry,
            vec![StorageRegion::CarryFlag],
        )
    }

    fn resolve_place(
        &self,
        place: &hir::Place,
        expected_ty: &hir::Type,
    ) -> Result<Location, LowerError> {
        match place {
            hir::Place::Variable(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| LowerError::UnknownPlace(name.clone()))
                .and_then(|location| location.with_type(expected_ty)),
            hir::Place::Static(name) => self
                .program
                .static_locations
                .get(name)
                .cloned()
                .ok_or_else(|| LowerError::UnknownPlace(name.clone()))
                .and_then(|location| location.with_type(expected_ty)),
            hir::Place::Field { base, index } => {
                let base = self.resolve_place_base(base)?;
                let (offset, ty) = field_offset(&base.ty, *index)?;
                if &ty != expected_ty {
                    return Err(LowerError::TypeMismatch {
                        expected: expected_ty.clone(),
                        actual: ty,
                    });
                }
                Ok(Location {
                    storage: base.storage,
                    offset: base.offset + offset,
                    ty: expected_ty.clone(),
                    bit: false,
                })
            }
            hir::Place::Element { base, index } => {
                let base = self.resolve_place_base(base)?;
                let hir::Type::Array { element, length } = &base.ty else {
                    return Err(LowerError::InvalidPlace(
                        "element base is not an array".into(),
                    ));
                };
                let index = constant_usize(index).ok_or_else(|| {
                    LowerError::InvalidPlace("dynamic direct-place index is not lowered yet".into())
                })?;
                if index >= *length {
                    return Err(LowerError::InvalidPlace(format!(
                        "array index {index} exceeds length {length}"
                    )));
                }
                if element.as_ref() != expected_ty {
                    return Err(LowerError::TypeMismatch {
                        expected: expected_ty.clone(),
                        actual: element.as_ref().clone(),
                    });
                }
                Ok(Location {
                    storage: base.storage,
                    offset: base.offset + index * type_size(element)?,
                    ty: expected_ty.clone(),
                    bit: false,
                })
            }
            hir::Place::ByteOffset { base, offset } => {
                let base = self.resolve_place_base(base)?;
                let offset = constant_usize(offset).ok_or_else(|| {
                    LowerError::InvalidPlace("dynamic byte offset is not lowered yet".into())
                })?;
                Ok(Location {
                    storage: base.storage,
                    offset: base.offset + offset,
                    ty: expected_ty.clone(),
                    bit: false,
                })
            }
        }
    }

    fn resolve_place_base(&self, place: &hir::Place) -> Result<Location, LowerError> {
        match place {
            hir::Place::Variable(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| LowerError::UnknownPlace(name.clone())),
            hir::Place::Static(name) => self
                .program
                .static_locations
                .get(name)
                .cloned()
                .ok_or_else(|| LowerError::UnknownPlace(name.clone())),
            hir::Place::Field { base, index } => {
                let base = self.resolve_place_base(base)?;
                let (offset, ty) = field_offset(&base.ty, *index)?;
                Ok(Location {
                    storage: base.storage,
                    offset: base.offset + offset,
                    ty,
                    bit: false,
                })
            }
            hir::Place::Element { base, index } => {
                let base = self.resolve_place_base(base)?;
                let hir::Type::Array { element, length } = &base.ty else {
                    return Err(LowerError::InvalidPlace(
                        "element base is not an array".into(),
                    ));
                };
                let index = constant_usize(index).ok_or_else(|| {
                    LowerError::InvalidPlace("dynamic address index is not lowered yet".into())
                })?;
                if index >= *length {
                    return Err(LowerError::InvalidPlace(format!(
                        "array index {index} exceeds length {length}"
                    )));
                }
                Ok(Location {
                    storage: base.storage,
                    offset: base.offset + index * type_size(element)?,
                    ty: element.as_ref().clone(),
                    bit: false,
                })
            }
            hir::Place::ByteOffset { base, offset } => {
                let base = self.resolve_place_base(base)?;
                let offset = constant_usize(offset).ok_or_else(|| {
                    LowerError::InvalidPlace(
                        "dynamic address byte offset is not lowered yet".into(),
                    )
                })?;
                Ok(Location {
                    storage: base.storage,
                    offset: base.offset + offset,
                    ty: hir::Type::OpaqueBytes(type_size(&base.ty)?.saturating_sub(offset)),
                    bit: false,
                })
            }
        }
    }

    fn resolve_addressed_place(&self, place: &hir::Place) -> Result<(Location, usize), LowerError> {
        let location = self.resolve_place_base(place)?;
        Ok((location.clone(), location.offset))
    }

    fn read_location(&mut self, location: &Location) -> Result<Value, LowerError> {
        if location.bit {
            return Ok(Value {
                ty: location.ty.clone(),
                repr: ValueRepr::Bit(BitOperand::Storage(location.storage)),
            });
        }
        self.read_bytes_as_type(location, &location.ty, location.offset)
    }

    fn read_bytes_as_type(
        &mut self,
        location: &Location,
        ty: &hir::Type,
        offset: usize,
    ) -> Result<Value, LowerError> {
        match ty {
            hir::Type::Unit => Ok(Value {
                ty: ty.clone(),
                repr: ValueRepr::Unit,
            }),
            hir::Type::Bool => {
                let destination = self.allocate_bit_temporary("read_bool");
                self.emit(InstructionKind::ByteToBit {
                    destination,
                    source: ByteOperand::Storage(ByteStorage {
                        storage: location.storage,
                        offset,
                    }),
                });
                Ok(Value {
                    ty: ty.clone(),
                    repr: ValueRepr::Bit(BitOperand::Storage(destination)),
                })
            }
            hir::Type::Bits(_) | hir::Type::Pointer | hir::Type::OpaqueBytes(_) => Ok(Value {
                ty: ty.clone(),
                repr: ValueRepr::Bytes(
                    (0..type_size(ty)?)
                        .map(|index| {
                            ByteOperand::Storage(ByteStorage {
                                storage: location.storage,
                                offset: offset + index,
                            })
                        })
                        .collect(),
                ),
            }),
            hir::Type::Array { element, length } => {
                let size = type_size(element)?;
                Ok(Value {
                    ty: ty.clone(),
                    repr: ValueRepr::Aggregate(
                        (0..*length)
                            .map(|index| {
                                self.read_bytes_as_type(location, element, offset + index * size)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    ),
                })
            }
            hir::Type::Struct(fields) | hir::Type::Tuple(fields) => {
                let mut field_offset = offset;
                let mut values = Vec::new();
                for field in fields {
                    values.push(self.read_bytes_as_type(location, field, field_offset)?);
                    field_offset += type_size(field)?;
                }
                Ok(Value {
                    ty: ty.clone(),
                    repr: ValueRepr::Aggregate(values),
                })
            }
            hir::Type::Float(_) => Err(LowerError::UnsupportedType(ty.clone())),
        }
    }

    fn copy_to_location(&mut self, location: &Location, value: Value) -> Result<(), LowerError> {
        if value.ty != location.ty {
            return Err(LowerError::TypeMismatch {
                expected: location.ty.clone(),
                actual: value.ty,
            });
        }
        if location.bit {
            self.emit(InstructionKind::CopyBit {
                destination: location.storage,
                source: value_bit(value)?,
            });
            return Ok(());
        }
        self.copy_value_to_bytes(location, location.offset, value)
    }

    fn copy_value_to_bytes(
        &mut self,
        location: &Location,
        offset: usize,
        value: Value,
    ) -> Result<(), LowerError> {
        match value.repr {
            ValueRepr::Unit => Ok(()),
            ValueRepr::Bit(source) => {
                self.emit(InstructionKind::BitToByte {
                    destination: ByteStorage {
                        storage: location.storage,
                        offset,
                    },
                    source,
                });
                Ok(())
            }
            ValueRepr::Bytes(bytes) => {
                for (index, source) in bytes.into_iter().enumerate() {
                    self.emit(InstructionKind::CopyByte {
                        destination: ByteStorage {
                            storage: location.storage,
                            offset: offset + index,
                        },
                        source,
                    });
                }
                Ok(())
            }
            ValueRepr::Aggregate(elements) => {
                let mut element_offset = offset;
                for element in elements {
                    let size = type_size(&element.ty)?;
                    self.copy_value_to_bytes(location, element_offset, element)?;
                    element_offset += size;
                }
                Ok(())
            }
        }
    }

    fn copy_piece(
        &mut self,
        destination: ValueStorage,
        source: ValueOperand,
    ) -> Result<(), LowerError> {
        match (destination, source) {
            (ValueStorage::Bit(destination), ValueOperand::Bit(source)) => {
                self.emit(InstructionKind::CopyBit {
                    destination,
                    source,
                });
            }
            (ValueStorage::Byte(destination), ValueOperand::Byte(source)) => {
                self.emit(InstructionKind::CopyByte {
                    destination,
                    source,
                });
            }
            (ValueStorage::Byte(destination), ValueOperand::Bit(source)) => {
                self.emit(InstructionKind::BitToByte {
                    destination,
                    source,
                });
            }
            (ValueStorage::Bit(destination), ValueOperand::Byte(source)) => {
                self.emit(InstructionKind::ByteToBit {
                    destination,
                    source,
                });
            }
        }
        Ok(())
    }

    fn select_to_location(
        &mut self,
        location: &Location,
        condition: BitOperand,
        when_true: Value,
        when_false: Value,
    ) -> Result<(), LowerError> {
        if when_true.ty != when_false.ty || when_true.ty != location.ty {
            return Err(LowerError::TypeMismatch {
                expected: location.ty.clone(),
                actual: when_true.ty,
            });
        }
        let true_pieces = flatten_value_operands(when_true);
        let false_pieces = flatten_value_operands(when_false);
        let destinations = self.location_value_storages(location)?;
        for ((destination, when_true), when_false) in
            destinations.into_iter().zip(true_pieces).zip(false_pieces)
        {
            match (destination, when_true, when_false) {
                (
                    ValueStorage::Bit(destination),
                    ValueOperand::Bit(when_true),
                    ValueOperand::Bit(when_false),
                ) => self.emit(InstructionKind::SelectBit {
                    destination,
                    condition,
                    when_true,
                    when_false,
                }),
                (
                    ValueStorage::Byte(destination),
                    ValueOperand::Byte(when_true),
                    ValueOperand::Byte(when_false),
                ) => self.emit(InstructionKind::SelectByte {
                    destination,
                    condition,
                    when_true,
                    when_false,
                }),
                _ => {
                    return Err(LowerError::UnsupportedExpression(
                        "select operands have incompatible pieces".into(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn materialize_location_address(
        &mut self,
        location: &Location,
        offset: usize,
    ) -> Result<Value, LowerError> {
        if location.bit {
            return Err(LowerError::InvalidPlace(
                "a packed boolean cannot be addressed directly".into(),
            ));
        }
        self.require_addressable(location.storage);
        let temporary = self.allocate_temporary(&hir::Type::Pointer)?;
        let [low, high] = self.location_bytes(&temporary)?.try_into().unwrap();
        self.emit(InstructionKind::AddressOf {
            destination_low: low,
            destination_high: high,
            storage: location.storage,
            offset,
        });
        self.read_location(&temporary)
    }

    fn require_addressable(&mut self, storage: StorageId) {
        let storage = &mut self.program.storages[storage.0];
        if matches!(storage.owner, StorageOwner::Function(_)) {
            storage.placement.allowed_regions = vec![StorageRegion::ShadowStack];
        }
    }

    fn value_as_memory_bytes(
        &mut self,
        value: Value,
        ty: &hir::Type,
    ) -> Result<Vec<ByteOperand>, LowerError> {
        if &value.ty != ty {
            return Err(LowerError::TypeMismatch {
                expected: ty.clone(),
                actual: value.ty,
            });
        }
        let temporary = self.allocate_temporary(ty)?;
        self.copy_to_location(&temporary, value)?;
        self.location_bytes(&temporary)
            .map(|bytes| bytes.into_iter().map(ByteOperand::Storage).collect())
    }

    fn location_bytes(&self, location: &Location) -> Result<Vec<ByteStorage>, LowerError> {
        if location.bit {
            return Err(LowerError::UnsupportedExpression(
                "bit storage has no independently addressable byte".into(),
            ));
        }
        Ok((0..type_size(&location.ty)?)
            .map(|index| ByteStorage {
                storage: location.storage,
                offset: location.offset + index,
            })
            .collect())
    }

    fn location_value_storages(
        &self,
        location: &Location,
    ) -> Result<Vec<ValueStorage>, LowerError> {
        if location.bit {
            Ok(vec![ValueStorage::Bit(location.storage)])
        } else {
            Ok(self
                .location_bytes(location)?
                .into_iter()
                .map(ValueStorage::Byte)
                .collect())
        }
    }

    fn mask_location(&mut self, location: &Location) -> Result<(), LowerError> {
        let hir::Type::Bits(width) = location.ty else {
            return Ok(());
        };
        let remainder = width.get() % 8;
        if remainder == 0 {
            return Ok(());
        }
        let destination = self.location_bytes(location)?.last().copied().unwrap();
        self.emit(InstructionKind::ByteBinary {
            op: ByteBinaryOp::And,
            destination,
            left: ByteOperand::Storage(destination),
            right: ByteOperand::Immediate((1_u16 << remainder) as u8 - 1),
        });
        Ok(())
    }

    fn find_control(&self, label: Option<&str>, continuing: bool) -> Result<BlockId, LowerError> {
        self.controls
            .iter()
            .rev()
            .find_map(|control| {
                (label.is_none() || control.label.as_deref() == label)
                    .then_some({
                        if continuing {
                            control.continue_target
                        } else {
                            Some(control.break_target)
                        }
                    })
                    .flatten()
            })
            .ok_or_else(|| LowerError::InvalidControlTarget(label.map(str::to_owned)))
    }

    fn new_block(&mut self, name: Option<String>) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(OpenBlock {
            id,
            name,
            instructions: Vec::new(),
            terminator: None,
        });
        id
    }

    fn emit(&mut self, kind: InstructionKind) {
        self.blocks[self.current.0]
            .instructions
            .push(Instruction { kind });
    }

    fn terminate(&mut self, kind: TerminatorKind) {
        self.blocks[self.current.0].terminator = Some(Terminator { kind });
    }

    fn is_terminated(&self) -> bool {
        self.blocks[self.current.0].terminator.is_some()
    }
}

impl Location {
    fn with_type(mut self, expected: &hir::Type) -> Result<Self, LowerError> {
        if &self.ty != expected {
            return Err(LowerError::TypeMismatch {
                expected: expected.clone(),
                actual: self.ty,
            });
        }
        self.ty = expected.clone();
        Ok(self)
    }
}

fn insert_signature(
    signatures: &mut BTreeMap<String, Signature>,
    function: &hir::Function,
) -> Result<(), LowerError> {
    if signatures
        .insert(function.name.clone(), Signature::of(function))
        .is_some()
    {
        Err(LowerError::DuplicateName(function.name.clone()))
    } else {
        Ok(())
    }
}

fn interrupt_handlers(handlers: &hir::InterruptHandlers) -> [&Option<hir::Function>; 10] {
    [
        &handlers.external_0,
        &handlers.external_1,
        &handlers.external_2_or_timer_0_low,
        &handlers.external_3_or_base_timer,
        &handlers.timer_0_high,
        &handlers.timer_1,
        &handlers.serial_0,
        &handlers.serial_1,
        &handlers.dreamcast_interface,
        &handlers.port_3,
    ]
}

fn type_size(ty: &hir::Type) -> Result<usize, LowerError> {
    Ok(match ty {
        hir::Type::Unit => 0,
        hir::Type::Bool => 1,
        hir::Type::Bits(width) => usize::from(width.get()).div_ceil(8),
        hir::Type::Float(hir::FloatWidth::Bits32) => 4,
        hir::Type::Float(hir::FloatWidth::Bits64) => 8,
        hir::Type::Pointer => 2,
        hir::Type::Array { element, length } => type_size(element)? * length,
        hir::Type::Struct(fields) | hir::Type::Tuple(fields) => fields
            .iter()
            .map(type_size)
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .sum(),
        hir::Type::OpaqueBytes(length) => *length,
    })
}

fn integer_width(ty: &hir::Type) -> Result<u8, LowerError> {
    match ty {
        hir::Type::Bits(width) => Ok(width.get()),
        hir::Type::Pointer => Ok(16),
        _ => Err(LowerError::UnsupportedType(ty.clone())),
    }
}

fn intrinsic_abi_type(ty: super::intrinsics::AbiType) -> hir::Type {
    match ty {
        super::intrinsics::AbiType::U8 => {
            hir::Type::Bits(hir::BitWidth::new(8).expect("valid intrinsic width"))
        }
        super::intrinsics::AbiType::ConstU8Pointer | super::intrinsics::AbiType::MutU8Pointer => {
            hir::Type::Pointer
        }
    }
}

fn value_bytes(value: Value) -> Result<Vec<ByteOperand>, LowerError> {
    match value.repr {
        ValueRepr::Bytes(bytes) => Ok(bytes),
        _ => Err(LowerError::UnsupportedExpression(format!(
            "expected byte value, found {:?}",
            value.ty
        ))),
    }
}

fn value_bit(value: Value) -> Result<BitOperand, LowerError> {
    match value.repr {
        ValueRepr::Bit(bit) => Ok(bit),
        _ => Err(LowerError::UnsupportedExpression(format!(
            "expected bit value, found {:?}",
            value.ty
        ))),
    }
}

fn flatten_value_operands(value: Value) -> Vec<ValueOperand> {
    match value.repr {
        ValueRepr::Unit => Vec::new(),
        ValueRepr::Bit(bit) => vec![ValueOperand::Bit(bit)],
        ValueRepr::Bytes(bytes) => bytes.into_iter().map(ValueOperand::Byte).collect(),
        ValueRepr::Aggregate(elements) => elements
            .into_iter()
            .flat_map(flatten_value_operands)
            .collect(),
    }
}

fn integer_bytes(bits: u128, length: usize) -> Vec<ByteOperand> {
    (0..length)
        .map(|index| ByteOperand::Immediate((bits >> (index * 8)) as u8))
        .collect()
}

fn constant_usize(expression: &hir::Expression) -> Option<usize> {
    let hir::ExpressionKind::Constant(hir::Constant::Integer(value)) = expression.kind else {
        return None;
    };
    usize::try_from(value.bits()).ok()
}

fn field_offset(ty: &hir::Type, index: usize) -> Result<(usize, hir::Type), LowerError> {
    let fields = match ty {
        hir::Type::Struct(fields) | hir::Type::Tuple(fields) => fields,
        _ => {
            return Err(LowerError::InvalidPlace(
                "field base is not a struct or tuple".into(),
            ));
        }
    };
    let field = fields
        .get(index)
        .cloned()
        .ok_or_else(|| LowerError::InvalidPlace(format!("no field at index {index}")))?;
    let offset = fields[..index]
        .iter()
        .map(type_size)
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .sum();
    Ok((offset, field))
}

fn compare_op(op: hir::CompareOp) -> Result<lir::CompareOp, LowerError> {
    use hir::{CompareOp as H, Signedness};
    Ok(match op {
        H::Equal => lir::CompareOp::Equal,
        H::NotEqual => lir::CompareOp::NotEqual,
        H::LessThan(Signedness::Unsigned) => lir::CompareOp::LessThanUnsigned,
        H::LessThan(Signedness::Signed) => lir::CompareOp::LessThanSigned,
        H::LessThanOrEqual(Signedness::Unsigned) => lir::CompareOp::LessThanOrEqualUnsigned,
        H::LessThanOrEqual(Signedness::Signed) => lir::CompareOp::LessThanOrEqualSigned,
        H::GreaterThan(Signedness::Unsigned) => lir::CompareOp::GreaterThanUnsigned,
        H::GreaterThan(Signedness::Signed) => lir::CompareOp::GreaterThanSigned,
        H::GreaterThanOrEqual(Signedness::Unsigned) => lir::CompareOp::GreaterThanOrEqualUnsigned,
        H::GreaterThanOrEqual(Signedness::Signed) => lir::CompareOp::GreaterThanOrEqualSigned,
        _ => {
            return Err(LowerError::UnsupportedExpression(
                "floating-point comparison requires soft-float support".into(),
            ));
        }
    })
}

fn integer_binary_bytes_op(op: hir::IntegerBinaryOp) -> IntegerBinaryBytesOp {
    use hir::{IntegerBinaryOp as H, Signedness};
    match op {
        H::Multiply => IntegerBinaryBytesOp::Multiply,
        H::Divide(Signedness::Unsigned) => IntegerBinaryBytesOp::DivideUnsigned,
        H::Divide(Signedness::Signed) => IntegerBinaryBytesOp::DivideSigned,
        H::Remainder(Signedness::Unsigned) => IntegerBinaryBytesOp::RemainderUnsigned,
        H::Remainder(Signedness::Signed) => IntegerBinaryBytesOp::RemainderSigned,
        H::SaturatingAdd(Signedness::Unsigned) => IntegerBinaryBytesOp::SaturatingAddUnsigned,
        H::SaturatingAdd(Signedness::Signed) => IntegerBinaryBytesOp::SaturatingAddSigned,
        H::SaturatingSubtract(Signedness::Unsigned) => {
            IntegerBinaryBytesOp::SaturatingSubtractUnsigned
        }
        H::SaturatingSubtract(Signedness::Signed) => IntegerBinaryBytesOp::SaturatingSubtractSigned,
        _ => unreachable!("simple integer operations are lowered separately"),
    }
}

fn immediate_bytes(bytes: &[ByteOperand]) -> Option<u128> {
    bytes
        .iter()
        .enumerate()
        .try_fold(0u128, |value, (index, byte)| match byte {
            ByteOperand::Immediate(byte) => Some(value | (*byte as u128) << (index * 8)),
            ByteOperand::Storage(_) => None,
        })
}

fn bit_storage(
    bit: BitOperand,
    lowerer: &mut FunctionLowerer<'_>,
) -> Result<StorageId, LowerError> {
    match bit {
        BitOperand::Storage(storage) => Ok(storage),
        BitOperand::Constant(value) => {
            let storage = lowerer.allocate_bit_temporary("condition");
            lowerer.emit(if value {
                InstructionKind::SetBit(storage)
            } else {
                InstructionKind::ClearBit(storage)
            });
            Ok(storage)
        }
    }
}

fn flatten_static_initializer(
    initializer: &hir::StaticInitializer,
    ty: &hir::Type,
) -> Result<Vec<StaticByte>, LowerError> {
    match initializer {
        hir::StaticInitializer::Zeroed => Ok(vec![StaticByte::Value(0); type_size(ty)?]),
        hir::StaticInitializer::Bytes(bytes) => {
            Ok(bytes.iter().copied().map(StaticByte::Value).collect())
        }
        hir::StaticInitializer::Constant(constant) => static_constant(constant, ty),
        hir::StaticInitializer::Aggregate(elements) => {
            let element_types: Vec<&hir::Type> = match ty {
                hir::Type::Array { element, length } if *length == elements.len() => {
                    vec![element.as_ref(); *length]
                }
                hir::Type::Struct(fields) | hir::Type::Tuple(fields)
                    if fields.len() == elements.len() =>
                {
                    fields.iter().collect()
                }
                _ => {
                    return Err(LowerError::InvalidStatic(
                        "aggregate initializer does not match its type".into(),
                    ));
                }
            };
            elements
                .iter()
                .zip(element_types)
                .map(|(element, ty)| flatten_static_initializer(element, ty))
                .collect::<Result<Vec<_>, _>>()
                .map(|parts| parts.into_iter().flatten().collect())
        }
    }
}

fn static_constant(
    constant: &hir::Constant,
    ty: &hir::Type,
) -> Result<Vec<StaticByte>, LowerError> {
    let bytes = match constant {
        hir::Constant::Bool(value) => vec![u8::from(*value)],
        hir::Constant::Integer(value) => integer_bytes(value.bits(), type_size(ty)?)
            .into_iter()
            .map(|byte| match byte {
                ByteOperand::Immediate(byte) => byte,
                _ => unreachable!(),
            })
            .collect(),
        hir::Constant::Float32(bits) => bits.to_le_bytes().to_vec(),
        hir::Constant::Float64(bits) => bits.to_le_bytes().to_vec(),
        hir::Constant::NullPointer => vec![0, 0],
        hir::Constant::FunctionAddress(function) => {
            return Ok(vec![
                StaticByte::FunctionAddress {
                    function: function.clone(),
                    byte: AddressByte::Low,
                },
                StaticByte::FunctionAddress {
                    function: function.clone(),
                    byte: AddressByte::High,
                },
            ]);
        }
        hir::Constant::StaticAddress { .. } => {
            return Err(LowerError::InvalidStatic(
                "static-to-static address initialization requires storage allocation first".into(),
            ));
        }
    };
    Ok(bytes.into_iter().map(StaticByte::Value).collect())
}

#[cfg(test)]
mod tests {
    use super::lower;
    use crate::wasm::hir::{
        BitWidth, Block, Constant, Expression, ExpressionKind, Function, FunctionType,
        IntegerBinaryOp, IntegerConstant, InterruptHandlers, Parameter, Place, Program, Statement,
        Type,
    };
    use crate::wasm::lir::{
        CodeAddress, InstructionKind, StorageKind, StorageRegion, TerminatorKind,
    };

    fn bits(width: u8) -> Type {
        Type::Bits(BitWidth::new(width).unwrap())
    }

    fn read(name: &str, ty: Type) -> Expression {
        Expression {
            ty,
            kind: ExpressionKind::Read(Place::Variable(name.into())),
        }
    }

    #[test]
    fn lowers_wide_addition_and_return_through_the_shared_abi() {
        let ty = bits(16);
        let program = Program {
            main: Function {
                name: "main".into(),
                parameters: vec![
                    Parameter {
                        name: "left".into(),
                        ty: ty.clone(),
                    },
                    Parameter {
                        name: "right".into(),
                        ty: ty.clone(),
                    },
                ],
                result: ty.clone(),
                body: Block {
                    statements: vec![Statement::Return(Some(Expression {
                        ty: ty.clone(),
                        kind: ExpressionKind::IntegerBinary {
                            op: IntegerBinaryOp::Add,
                            left: Box::new(read("left", ty.clone())),
                            right: Box::new(read("right", ty)),
                        },
                    }))],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };

        let program = lower(&program).unwrap();
        let instructions = &program.main.blocks[0].instructions;

        assert_eq!(
            instructions
                .iter()
                .filter(|instruction| matches!(instruction.kind, InstructionKind::Add { .. }))
                .count(),
            2
        );
        assert!(matches!(
            program.main.blocks[0].terminator.kind,
            TerminatorKind::Return
        ));
        assert!(
            program
                .storages
                .iter()
                .any(|storage| matches!(storage.kind, StorageKind::Carry))
        );
    }

    #[test]
    fn unlabeled_continue_skips_non_loop_control_targets() {
        let program = Program {
            main: Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: Type::Unit,
                body: Block {
                    statements: vec![
                        Statement::Loop {
                            label: None,
                            body: Block {
                                statements: vec![Statement::Block {
                                    label: None,
                                    body: Block {
                                        statements: vec![Statement::Continue { label: None }],
                                    },
                                }],
                            },
                        },
                        Statement::Return(None),
                    ],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };

        lower(&program).unwrap();
    }

    #[test]
    fn keeps_boolean_variables_as_bit_storage() {
        let program = Program {
            main: Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: Type::Unit,
                body: Block {
                    statements: vec![
                        Statement::Declare {
                            name: "flag".into(),
                            ty: Type::Bool,
                            mutable: true,
                            initializer: Some(Expression {
                                ty: Type::Bool,
                                kind: ExpressionKind::Constant(Constant::Bool(true)),
                            }),
                        },
                        Statement::Return(None),
                    ],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };

        let program = lower(&program).unwrap();

        assert!(
            program
                .storages
                .iter()
                .any(|storage| matches!(storage.kind, StorageKind::Bit))
        );
        assert!(
            program.main.blocks[0]
                .instructions
                .iter()
                .any(|instruction| matches!(instruction.kind, InstructionKind::CopyBit { .. }))
        );
    }

    #[test]
    fn lowers_integer_constants_little_endian() {
        let ty = bits(16);
        let constant = IntegerConstant::new(BitWidth::new(16).unwrap(), 0x1234).unwrap();
        let program = Program {
            main: Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: ty.clone(),
                body: Block {
                    statements: vec![Statement::Return(Some(Expression {
                        ty,
                        kind: ExpressionKind::Constant(Constant::Integer(constant)),
                    }))],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };

        let program = lower(&program).unwrap();
        let copies = program.main.blocks[0]
            .instructions
            .iter()
            .filter_map(|instruction| match instruction.kind {
                InstructionKind::CopyByte { source, .. } => Some(source),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(copies.ends_with(&[
            crate::wasm::lir::ByteOperand::Immediate(0x34),
            crate::wasm::lir::ByteOperand::Immediate(0x12),
        ]));
    }

    #[test]
    fn address_taken_local_is_constrained_to_the_shadow_stack() {
        let program = Program {
            main: Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: Type::Unit,
                body: Block {
                    statements: vec![
                        Statement::Declare {
                            name: "object".into(),
                            ty: Type::OpaqueBytes(4),
                            mutable: true,
                            initializer: None,
                        },
                        Statement::Declare {
                            name: "pointer".into(),
                            ty: Type::Pointer,
                            mutable: false,
                            initializer: Some(Expression {
                                ty: Type::Pointer,
                                kind: ExpressionKind::AddressOf(Place::Variable("object".into())),
                            }),
                        },
                        Statement::Return(None),
                    ],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };

        let program = lower(&program).unwrap();
        let object = program
            .storages
            .iter()
            .find(|storage| storage.name.as_deref() == Some("object"))
            .unwrap();

        assert_eq!(
            object.placement.allowed_regions,
            vec![StorageRegion::ShadowStack]
        );
        assert!(
            program.main.blocks[0]
                .instructions
                .iter()
                .any(|instruction| matches!(instruction.kind, InstructionKind::AddressOf { .. }))
        );
    }

    #[test]
    fn lowers_symbolic_indirect_tail_calls() {
        let function_type = FunctionType {
            parameters: Vec::new(),
            result: Type::Unit,
        };
        let program = Program {
            main: Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: Type::Unit,
                body: Block {
                    statements: vec![Statement::TailCallIndirect {
                        target: Expression {
                            ty: Type::Pointer,
                            kind: ExpressionKind::Constant(Constant::FunctionAddress(
                                "target".into(),
                            )),
                        },
                        signature: function_type,
                        arguments: Vec::new(),
                    }],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: vec![Function {
                name: "target".into(),
                parameters: Vec::new(),
                result: Type::Unit,
                body: Block {
                    statements: vec![Statement::Return(None)],
                },
            }],
            statics: Vec::new(),
        };

        let program = lower(&program).unwrap();

        assert!(matches!(
            &program.main.blocks[0].terminator.kind,
            TerminatorKind::TailCallIndirect {
                target: CodeAddress::Function(function),
                ..
            } if function == "target"
        ));
    }
}
