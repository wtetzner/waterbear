//! Human-readable printer for virtual VMU LIR.

use std::fmt::Write;

use super::{intrinsics::Intrinsic, lir::*};

/// Renders a virtual LIR program in a compact assembly-like form.
pub fn render(program: &VirtualProgram) -> String {
    Printer::new(program).render()
}

struct Printer<'a> {
    program: &'a VirtualProgram,
    output: String,
}

impl<'a> Printer<'a> {
    fn new(program: &'a VirtualProgram) -> Self {
        Self {
            program,
            output: String::new(),
        }
    }

    fn render(mut self) -> String {
        self.line(0, format_args!("lir program {{"));
        for storage in &self.program.storages {
            self.write_storage(storage);
        }
        if !self.program.storages.is_empty() && (!self.program.statics.is_empty()) {
            self.blank_line();
        }
        for static_ in &self.program.statics {
            self.write_static(static_);
        }
        if !self.program.statics.is_empty() {
            self.blank_line();
        }

        self.write_function(1, "main", &self.program.main);
        for (slot, function) in interrupt_handlers(&self.program.interrupt_handlers) {
            if let Some(function) = function {
                self.blank_line();
                self.write_function(1, &format!("interrupt {slot}"), function);
            }
        }
        for function in &self.program.functions {
            self.blank_line();
            self.write_function(1, "", function);
        }
        self.line(0, format_args!("}}"));
        self.output
    }

    fn write_storage(&mut self, storage: &Storage<VirtualPlacement>) {
        let name = storage
            .name
            .as_ref()
            .map(|name| format!(" {:?}", name))
            .unwrap_or_default();
        let owner = match &storage.owner {
            StorageOwner::Program => "program".into(),
            StorageOwner::Function(function) => format!("fn {}", identifier(function)),
        };
        let regions = storage
            .placement
            .allowed_regions
            .iter()
            .map(|region| format_region(*region))
            .collect::<Vec<_>>()
            .join(" | ");
        let direct = if storage.placement.must_be_direct {
            ", direct"
        } else {
            ""
        };
        let fixed = storage
            .placement
            .fixed
            .as_ref()
            .map(|placement| format!(", fixed {placement:?}"))
            .unwrap_or_default();
        self.line(
            1,
            format_args!(
                "storage {}{name}: {} owner {owner} in [{regions}]{direct}{fixed};",
                storage_name(storage.id),
                format_storage_kind(storage.kind),
            ),
        );
    }

    fn write_static(&mut self, static_: &Static) {
        let mutability = if static_.mutable { "mut " } else { "" };
        self.line(
            1,
            format_args!(
                "static {mutability}@{} -> {} = [{}];",
                identifier(&static_.name),
                storage_name(static_.storage),
                static_
                    .initializer
                    .iter()
                    .map(format_static_byte)
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
        );
    }

    fn write_function(&mut self, indent: usize, role: &str, function: &Function) {
        let role = if role.is_empty() {
            String::new()
        } else {
            format!("{role} ")
        };
        self.line(
            indent,
            format_args!(
                "{role}fn {} {} {{",
                identifier(&function.name),
                format_abi(&function.abi),
            ),
        );
        for block in &function.blocks {
            self.write_block(indent + 1, block);
        }
        self.line(indent, format_args!("}}"));
    }

    fn write_block(&mut self, indent: usize, block: &Block) {
        let name = block
            .name
            .as_ref()
            .map(|name| format!(" {}", identifier(name)))
            .unwrap_or_default();
        self.line(indent, format_args!("{}{}:", block_name(block.id), name));
        for instruction in &block.instructions {
            self.line(
                indent + 1,
                format_args!("{}", format_instruction(instruction)),
            );
        }
        self.line(
            indent + 1,
            format_args!("{}", format_terminator(&block.terminator)),
        );
    }

    fn line(&mut self, indent: usize, arguments: std::fmt::Arguments<'_>) {
        for _ in 0..indent {
            self.output.push_str("  ");
        }
        self.output.write_fmt(arguments).expect("write to string");
        self.output.push('\n');
    }

    fn blank_line(&mut self) {
        self.output.push('\n');
    }
}

fn format_instruction(instruction: &Instruction) -> String {
    use InstructionKind::*;
    match &instruction.kind {
        CopyByte {
            destination,
            source,
        } => format!(
            "{} = copy {};",
            format_byte_storage(*destination),
            format_byte(*source)
        ),
        CopyBit {
            destination,
            source,
        } => format!(
            "{} = copy {};",
            storage_name(*destination),
            format_bit(*source)
        ),
        SetBit(destination) => format!("set {};", storage_name(*destination)),
        ClearBit(destination) => format!("clear {};", storage_name(*destination)),
        ToggleBit(destination) => format!("toggle {};", storage_name(*destination)),
        ByteUnary {
            op,
            destination,
            input,
        } => format!(
            "{} = {} {};",
            format_byte_storage(*destination),
            debug_name(op),
            format_byte(*input)
        ),
        ByteBinary {
            op,
            destination,
            left,
            right,
        } => format!(
            "{} = {} {}, {};",
            format_byte_storage(*destination),
            debug_name(op),
            format_byte(*left),
            format_byte(*right)
        ),
        Add {
            destination,
            left,
            right,
            carry_in,
            carry_out,
        } => format!(
            "{} = add {}, {}{}{};",
            format_byte_storage(*destination),
            format_byte(*left),
            format_byte(*right),
            carry_in
                .map(|carry| format!(", carry_in {}", storage_name(carry)))
                .unwrap_or_default(),
            carry_out
                .map(|carry| format!(", carry_out {}", storage_name(carry)))
                .unwrap_or_default(),
        ),
        Subtract {
            destination,
            left,
            right,
            borrow_in,
            borrow_out,
        } => format!(
            "{} = subtract {}, {}{}{};",
            format_byte_storage(*destination),
            format_byte(*left),
            format_byte(*right),
            borrow_in
                .map(|borrow| format!(", borrow_in {}", storage_name(borrow)))
                .unwrap_or_default(),
            borrow_out
                .map(|borrow| format!(", borrow_out {}", storage_name(borrow)))
                .unwrap_or_default(),
        ),
        Multiply {
            left_high,
            left_low,
            right,
            result_high,
            result_middle,
            result_low,
        } => format!(
            "({}, {}, {}) = multiply ({}, {}), {};",
            format_byte_storage(*result_low),
            format_byte_storage(*result_middle),
            format_byte_storage(*result_high),
            format_byte(*left_low),
            format_byte(*left_high),
            format_byte(*right),
        ),
        Divide {
            dividend_high,
            dividend_low,
            divisor,
            quotient_high,
            quotient_low,
            remainder,
        } => format!(
            "({}, {}; remainder {}) = divide ({}, {}), {};",
            format_byte_storage(*quotient_low),
            format_byte_storage(*quotient_high),
            format_byte_storage(*remainder),
            format_byte(*dividend_low),
            format_byte(*dividend_high),
            format_byte(*divisor),
        ),
        Rotate {
            direction,
            through_carry,
            destination,
            input,
            carry_in,
            carry_out,
        } => format!(
            "{} = rotate_{}{} {}{}{};",
            format_byte_storage(*destination),
            debug_name(direction),
            if *through_carry { "_through_carry" } else { "" },
            format_byte(*input),
            carry_in
                .map(|carry| format!(", carry_in {}", storage_name(carry)))
                .unwrap_or_default(),
            carry_out
                .map(|carry| format!(", carry_out {}", storage_name(carry)))
                .unwrap_or_default(),
        ),
        LoadIndirect {
            destination,
            address,
        } => format!(
            "{} = load {};",
            format_byte_storage(*destination),
            format_address(address)
        ),
        StoreIndirect { address, value } => {
            format!(
                "store {}, {};",
                format_address(address),
                format_byte(*value)
            )
        }
        Push(value) => format!("push {};", format_byte(*value)),
        Pop(destination) => format!("{} = pop;", format_byte_storage(*destination)),
        AddressOf {
            destination_low,
            destination_high,
            storage,
            offset,
        } => format!(
            "({}, {}) = address_of({} + {});",
            format_byte_storage(*destination_low),
            format_byte_storage(*destination_high),
            storage_name(*storage),
            offset,
        ),
        FunctionAddress {
            destination_low,
            destination_high,
            function,
        } => format!(
            "({}, {}) = address_of(fn {});",
            format_byte_storage(*destination_low),
            format_byte_storage(*destination_high),
            identifier(function),
        ),
        ShiftBytes {
            op,
            destinations,
            value,
            amount,
            width,
        } => format!(
            "{} = {}<{}> {}, {};",
            format_byte_storages(destinations),
            debug_name(op),
            width,
            format_bytes(value),
            format_bytes(amount),
        ),
        CompareBytes {
            op,
            destination,
            left,
            right,
            width,
        } => format!(
            "{} = compare.{}<{}> {}, {};",
            storage_name(*destination),
            debug_name(op),
            width,
            format_bytes(left),
            format_bytes(right),
        ),
        SelectByte {
            destination,
            condition,
            when_true,
            when_false,
        } => format!(
            "{} = select {}, {}, {};",
            format_byte_storage(*destination),
            format_bit(*condition),
            format_byte(*when_true),
            format_byte(*when_false),
        ),
        SelectBit {
            destination,
            condition,
            when_true,
            when_false,
        } => format!(
            "{} = select {}, {}, {};",
            storage_name(*destination),
            format_bit(*condition),
            format_bit(*when_true),
            format_bit(*when_false),
        ),
        BitToByte {
            destination,
            source,
        } => format!(
            "{} = bit_to_byte {};",
            format_byte_storage(*destination),
            format_bit(*source)
        ),
        ByteToBit {
            destination,
            source,
        } => format!(
            "{} = byte_to_bit {};",
            storage_name(*destination),
            format_byte(*source)
        ),
        SignExtend {
            destinations,
            source,
            source_width,
        } => format!(
            "{} = sign_extend<{}> {};",
            format_byte_storages(destinations),
            source_width,
            format_bytes(source),
        ),
        IntegerUnaryBytes {
            op,
            destinations,
            input,
            width,
        } => format!(
            "{} = {}<{}> {};",
            format_byte_storages(destinations),
            debug_name(op),
            width,
            format_bytes(input),
        ),
        IntegerBinaryBytes {
            op,
            destinations,
            left,
            right,
            width,
        } => format!(
            "{} = {}<{}> {}, {};",
            format_byte_storages(destinations),
            debug_name(op),
            width,
            format_bytes(left),
            format_bytes(right),
        ),
        Call { function } => format!("call {};", identifier(function)),
        CallIndirect { target, abi } => {
            format!(
                "call indirect {} {};",
                format_code_address(target),
                format_abi(abi)
            )
        }
        Intrinsic {
            operation,
            inputs,
            outputs,
        } => format!(
            "{}intrinsic {}({});",
            if outputs.is_empty() {
                String::new()
            } else {
                format!("{} = ", format_value_storages(outputs))
            },
            intrinsic_name(*operation),
            inputs
                .iter()
                .map(|input| format_value_operand(*input))
                .collect::<Vec<_>>()
                .join(", "),
        ),
    }
}

fn format_terminator(terminator: &Terminator) -> String {
    match &terminator.kind {
        TerminatorKind::Jump(target) => format!("jump {};", block_name(*target)),
        TerminatorKind::Branch {
            condition,
            when_true,
            when_false,
        } => format!(
            "branch {} ? {} : {};",
            format_branch_condition(condition),
            block_name(*when_true),
            block_name(*when_false)
        ),
        TerminatorKind::Switch {
            value,
            width,
            cases,
            default,
        } => format!(
            "switch<{}> {} [{}] default {};",
            width,
            format_bytes(value),
            cases
                .iter()
                .map(|(value, block)| format!("{value}: {}", block_name(*block)))
                .collect::<Vec<_>>()
                .join(", "),
            block_name(*default),
        ),
        TerminatorKind::TailCall { function } => {
            format!("tail call {};", identifier(function))
        }
        TerminatorKind::TailCallIndirect { target, abi } => format!(
            "tail call indirect {} {};",
            format_code_address(target),
            format_abi(abi)
        ),
        TerminatorKind::Return => "return;".into(),
        TerminatorKind::Trap => "trap;".into(),
        TerminatorKind::Unreachable => "unreachable;".into(),
    }
}

fn format_branch_condition(condition: &BranchCondition) -> String {
    match condition {
        BranchCondition::BitSet(storage) => format!("bit_set({})", storage_name(*storage)),
        BranchCondition::BitClear(storage) => format!("bit_clear({})", storage_name(*storage)),
        BranchCondition::ByteZero(value) => format!("zero({})", format_byte(*value)),
        BranchCondition::ByteNonZero(value) => format!("nonzero({})", format_byte(*value)),
        BranchCondition::CarrySet(storage) => format!("carry_set({})", storage_name(*storage)),
        BranchCondition::CarryClear(storage) => format!("carry_clear({})", storage_name(*storage)),
        BranchCondition::Equal { left, right } => {
            format!("{} == {}", format_byte(*left), format_byte(*right))
        }
        BranchCondition::NotEqual { left, right } => {
            format!("{} != {}", format_byte(*left), format_byte(*right))
        }
    }
}

fn format_abi(abi: &FunctionAbi) -> String {
    let parameters = abi
        .parameters
        .iter()
        .map(format_abi_value)
        .collect::<Vec<_>>()
        .join(", ");
    let results = abi
        .results
        .iter()
        .map(format_abi_value)
        .collect::<Vec<_>>()
        .join(", ");
    let preserved = format_storage_set("preserves", &abi.preserved);
    let clobbered = format_storage_set("clobbers", &abi.clobbered);
    format!("abi ({parameters}) -> ({results}){preserved}{clobbered}")
}

fn format_storage_set(label: &str, storages: &[StorageId]) -> String {
    if storages.is_empty() {
        String::new()
    } else {
        format!(
            " {label} [{}]",
            storages
                .iter()
                .map(|storage| storage_name(*storage))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn format_abi_value(value: &AbiValue) -> String {
    format!(
        "{}: {}",
        value.logical_index,
        format_value_storages(&value.pieces)
    )
}

fn format_value_storages(storages: &[ValueStorage]) -> String {
    format!(
        "({})",
        storages
            .iter()
            .map(|storage| match storage {
                ValueStorage::Bit(storage) => storage_name(*storage),
                ValueStorage::Byte(storage) => format_byte_storage(*storage),
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn format_value_operand(operand: ValueOperand) -> String {
    match operand {
        ValueOperand::Bit(bit) => format_bit(bit),
        ValueOperand::Byte(byte) => format_byte(byte),
    }
}

fn format_byte_storages(storages: &[ByteStorage]) -> String {
    format!(
        "({})",
        storages
            .iter()
            .map(|storage| format_byte_storage(*storage))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn format_bytes(bytes: &[ByteOperand]) -> String {
    format!(
        "({})",
        bytes
            .iter()
            .map(|byte| format_byte(*byte))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn format_byte(byte: ByteOperand) -> String {
    match byte {
        ByteOperand::Immediate(value) => format!("0x{value:02x}"),
        ByteOperand::Storage(storage) => format_byte_storage(storage),
    }
}

fn format_bit(bit: BitOperand) -> String {
    match bit {
        BitOperand::Constant(value) => value.to_string(),
        BitOperand::Storage(storage) => storage_name(storage),
    }
}

fn format_byte_storage(storage: ByteStorage) -> String {
    format!("{}.b{}", storage_name(storage.storage), storage.offset)
}

fn format_address(address: &Address) -> String {
    let offset = if address.offset() == 0 {
        String::new()
    } else {
        format!(" + {}", address.offset())
    };
    format!(
        "{}:{}{offset}",
        format_address_space(address.space()),
        format_bytes(address.bytes())
    )
}

fn format_code_address(address: &CodeAddress) -> String {
    match address {
        CodeAddress::Function(function) => format!("fn {}", identifier(function)),
        CodeAddress::Computed { low, high } => {
            format!("({}, {})", format_byte(*low), format_byte(*high))
        }
    }
}

fn format_static_byte(byte: &StaticByte) -> String {
    match byte {
        StaticByte::Value(value) => format!("0x{value:02x}"),
        StaticByte::StorageAddress {
            storage,
            offset,
            byte,
        } => format!(
            "{}address_of({} + {})",
            format_address_byte(*byte),
            storage_name(*storage),
            offset
        ),
        StaticByte::FunctionAddress { function, byte } => {
            format!("{}{}", format_address_byte(*byte), identifier(function))
        }
    }
}

fn format_address_byte(byte: AddressByte) -> &'static str {
    match byte {
        AddressByte::Low => "<",
        AddressByte::High => ">",
    }
}

fn format_storage_kind(kind: StorageKind) -> String {
    match kind {
        StorageKind::Bit => "bit".into(),
        StorageKind::Byte => "byte".into(),
        StorageKind::Bytes { length } => format!("bytes[{}]", length.get()),
        StorageKind::Carry => "carry".into(),
    }
}

fn format_region(region: StorageRegion) -> &'static str {
    match region {
        StorageRegion::Accumulator => "accumulator",
        StorageRegion::CarryFlag => "carry",
        StorageRegion::ApplicationRam => "application_ram",
        StorageRegion::SpecialRegister => "sfr",
        StorageRegion::IndirectRegister => "indirect_register",
        StorageRegion::ShadowStack => "shadow_stack",
        StorageRegion::WorkRam => "work_ram",
        StorageRegion::Flash => "flash",
        StorageRegion::XRam => "xram",
    }
}

fn format_address_space(space: AddressSpace) -> &'static str {
    match space {
        AddressSpace::MainRam(RamBank::System) => "system_ram",
        AddressSpace::MainRam(RamBank::Application) => "application_ram",
        AddressSpace::ShadowStack => "shadow_stack",
        AddressSpace::WorkRam => "work_ram",
        AddressSpace::Flash => "flash",
        AddressSpace::XRam => "xram",
        AddressSpace::SpecialFunctionRegisters => "sfr",
        AddressSpace::Code => "code",
    }
}

fn intrinsic_name(intrinsic: Intrinsic) -> String {
    identifier(&intrinsic.display_name())
}

fn storage_name(storage: StorageId) -> String {
    format!("%s{}", storage.0)
}

fn block_name(block: BlockId) -> String {
    format!("block{}", block.0)
}

fn debug_name(value: &impl std::fmt::Debug) -> String {
    let name = format!("{value:?}");
    let mut result = String::new();
    for (index, character) in name.chars().enumerate() {
        if index != 0 && character.is_ascii_uppercase() {
            result.push('_');
        }
        result.push(character.to_ascii_lowercase());
    }
    result
}

fn identifier(name: &str) -> String {
    let mut characters = name.chars();
    let simple = characters
        .next()
        .is_some_and(|character| character == '_' || character.is_ascii_alphabetic())
        && characters.all(|character| character == '_' || character.is_ascii_alphanumeric());
    if simple {
        name.into()
    } else {
        format!("{name:?}")
    }
}

fn interrupt_handlers(handlers: &InterruptHandlers) -> [(&'static str, &Option<Function>); 10] {
    [
        ("external_0", &handlers.external_0),
        ("external_1", &handlers.external_1),
        (
            "external_2_or_timer_0_low",
            &handlers.external_2_or_timer_0_low,
        ),
        (
            "external_3_or_base_timer",
            &handlers.external_3_or_base_timer,
        ),
        ("timer_0_high", &handlers.timer_0_high),
        ("timer_1", &handlers.timer_1),
        ("serial_0", &handlers.serial_0),
        ("serial_1", &handlers.serial_1),
        ("dreamcast_interface", &handlers.dreamcast_interface),
        ("port_3", &handlers.port_3),
    ]
}

#[cfg(test)]
mod tests {
    use super::render;
    use crate::wasm::{hir, lir_lower};

    #[test]
    fn renders_virtual_storage_and_blocks() {
        let hir = hir::Program {
            main: hir::Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: hir::Type::Unit,
                body: hir::Block {
                    statements: vec![hir::Statement::Return(None)],
                },
            },
            interrupt_handlers: hir::InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };
        let lir = lir_lower::lower(&hir).unwrap();
        let output = render(&lir);

        assert!(output.contains("lir program {"));
        assert!(output.contains("main fn main abi () -> ()"));
        assert!(output.contains("block0 entry:"));
        assert!(output.contains("return;"));
    }

    #[test]
    fn renders_function_addresses_with_waterbear_byte_operators() {
        let hir = hir::Program {
            main: hir::Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: hir::Type::Unit,
                body: hir::Block {
                    statements: vec![hir::Statement::Return(None)],
                },
            },
            interrupt_handlers: hir::InterruptHandlers::default(),
            functions: vec![hir::Function {
                name: "target".into(),
                parameters: Vec::new(),
                result: hir::Type::Unit,
                body: hir::Block {
                    statements: vec![hir::Statement::Return(None)],
                },
            }],
            statics: vec![hir::Static {
                name: "function_pointer".into(),
                ty: hir::Type::Pointer,
                mutable: false,
                initializer: hir::StaticInitializer::Constant(hir::Constant::FunctionAddress(
                    "target".into(),
                )),
            }],
        };
        let lir = lir_lower::lower(&hir).unwrap();

        let output = render(&lir);

        assert!(output.contains("[<target, >target]"));
    }
}
