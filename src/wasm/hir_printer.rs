//! Human-readable printer for the VMU high-level IR.

use std::fmt::Write;

use super::hir::{
    self, CompareOp, Constant, ConversionOp, Expression, ExpressionKind, FloatBinaryOp,
    FloatUnaryOp, FloatWidth, IntegerBinaryOp, IntegerConstant, IntegerUnaryOp, Place, Signedness,
    Statement, StaticInitializer, Type,
};

/// Renders a VMU HIR program in a source-like form.
pub fn render(program: &hir::Program) -> String {
    Printer::new().render(program)
}

struct Printer {
    output: String,
}

impl Printer {
    fn new() -> Self {
        Self {
            output: String::new(),
        }
    }

    fn render(mut self, program: &hir::Program) -> String {
        self.line(0, format_args!("program {{"));

        for static_ in &program.statics {
            self.write_static(1, static_);
        }
        if !program.statics.is_empty() {
            self.blank_line();
        }

        self.write_function(1, "main", &program.main);

        for (slot, function) in interrupt_handlers(&program.interrupt_handlers) {
            if let Some(function) = function {
                self.blank_line();
                self.write_function(1, &format!("interrupt {slot}"), function);
            }
        }

        for function in &program.functions {
            self.blank_line();
            self.write_function(1, "", function);
        }

        self.line(0, format_args!("}}"));
        self.output
    }

    fn write_static(&mut self, indent: usize, static_: &hir::Static) {
        let mutability = if static_.mutable { "mut " } else { "" };
        self.line(
            indent,
            format_args!(
                "static {mutability}@{}: {} = {};",
                identifier(&static_.name),
                format_type(&static_.ty),
                format_static_initializer(&static_.initializer),
            ),
        );
    }

    fn write_function(&mut self, indent: usize, role: &str, function: &hir::Function) {
        let role = if role.is_empty() {
            String::new()
        } else {
            format!("{role} ")
        };
        let parameters = function
            .parameters
            .iter()
            .map(|parameter| {
                format!(
                    "{}: {}",
                    identifier(&parameter.name),
                    format_type(&parameter.ty)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        self.line(
            indent,
            format_args!(
                "{role}fn {}({parameters}) -> {} {{",
                identifier(&function.name),
                format_type(&function.result),
            ),
        );
        self.write_block(indent + 1, &function.body);
        self.line(indent, format_args!("}}"));
    }

    fn write_block(&mut self, indent: usize, block: &hir::Block) {
        for statement in &block.statements {
            self.write_statement(indent, statement);
        }
    }

    fn write_statement(&mut self, indent: usize, statement: &Statement) {
        match statement {
            Statement::Declare {
                name,
                ty,
                mutable,
                initializer,
            } => {
                let mutability = if *mutable { "mut " } else { "" };
                let initializer = initializer
                    .as_ref()
                    .map(|value| format!(" = {}", format_expression(value)))
                    .unwrap_or_default();
                self.line(
                    indent,
                    format_args!(
                        "let {mutability}{}: {}{initializer};",
                        identifier(name),
                        format_type(ty),
                    ),
                );
            }
            Statement::Assign { destination, value } => self.line(
                indent,
                format_args!(
                    "{} = {};",
                    format_place(destination),
                    format_expression(value)
                ),
            ),
            Statement::Load {
                destination,
                address,
                ty,
                access,
            } => {
                let operation = format!(
                    "load<{}>({}{})",
                    format_type(ty),
                    format_expression(address),
                    format_memory_access(*access),
                );
                self.write_result_statement(indent, destination.as_ref(), &operation);
            }
            Statement::Store {
                address,
                value,
                ty,
                access,
            } => self.line(
                indent,
                format_args!(
                    "store<{}>({}, {}{});",
                    format_type(ty),
                    format_expression(address),
                    format_expression(value),
                    format_memory_access(*access),
                ),
            ),
            Statement::MemoryFill {
                destination,
                value,
                length,
            } => self.line(
                indent,
                format_args!(
                    "memory.fill({}, {}, {});",
                    format_expression(destination),
                    format_expression(value),
                    format_expression(length),
                ),
            ),
            Statement::Call {
                destination,
                function,
                arguments,
            } => {
                let operation = format!(
                    "call {}({})",
                    identifier(function),
                    format_expressions(arguments)
                );
                self.write_result_statement(indent, destination.as_ref(), &operation);
            }
            Statement::CallIndirect {
                destination,
                target,
                signature,
                arguments,
            } => {
                let operation = format!(
                    "call indirect {} as {}({})",
                    format_expression(target),
                    format_function_type(signature),
                    format_expressions(arguments),
                );
                self.write_result_statement(indent, destination.as_ref(), &operation);
            }
            Statement::TailCall {
                function,
                arguments,
            } => self.line(
                indent,
                format_args!(
                    "tail call {}({});",
                    identifier(function),
                    format_expressions(arguments)
                ),
            ),
            Statement::TailCallIndirect {
                target,
                signature,
                arguments,
            } => self.line(
                indent,
                format_args!(
                    "tail call indirect {} as {}({});",
                    format_expression(target),
                    format_function_type(signature),
                    format_expressions(arguments),
                ),
            ),
            Statement::Intrinsic {
                destination,
                operation,
                arguments,
            } => {
                let name = operation.display_name();
                let operation = format!(
                    "intrinsic {}({})",
                    identifier(&name),
                    format_expressions(arguments)
                );
                self.write_result_statement(indent, destination.as_ref(), &operation);
            }
            Statement::Block { label, body } => {
                let label = label
                    .as_ref()
                    .map(|label| format!(" {}", identifier(label)))
                    .unwrap_or_default();
                self.line(indent, format_args!("block{label} {{"));
                self.write_block(indent + 1, body);
                self.line(indent, format_args!("}}"));
            }
            Statement::If {
                condition,
                then_body,
                else_body,
            } => {
                self.line(
                    indent,
                    format_args!("if {} {{", format_expression(condition)),
                );
                self.write_block(indent + 1, then_body);
                if let Some(else_body) = else_body {
                    self.line(indent, format_args!("}} else {{"));
                    self.write_block(indent + 1, else_body);
                }
                self.line(indent, format_args!("}}"));
            }
            Statement::Loop { label, body } => {
                let label = label
                    .as_ref()
                    .map(|label| format!(" {}", identifier(label)))
                    .unwrap_or_default();
                self.line(indent, format_args!("loop{label} {{"));
                self.write_block(indent + 1, body);
                self.line(indent, format_args!("}}"));
            }
            Statement::Switch {
                value,
                cases,
                default,
            } => {
                self.line(
                    indent,
                    format_args!("switch {} {{", format_expression(value)),
                );
                for case in cases {
                    self.line(
                        indent + 1,
                        format_args!("case {} {{", format_integer_constant(case.value)),
                    );
                    self.write_block(indent + 2, &case.body);
                    self.line(indent + 1, format_args!("}}"));
                }
                self.line(indent + 1, format_args!("default {{"));
                self.write_block(indent + 2, default);
                self.line(indent + 1, format_args!("}}"));
                self.line(indent, format_args!("}}"));
            }
            Statement::Break { label } => {
                self.write_jump_statement(indent, "break", label.as_deref());
            }
            Statement::Continue { label } => {
                self.write_jump_statement(indent, "continue", label.as_deref());
            }
            Statement::Return(value) => {
                if let Some(value) = value {
                    self.line(indent, format_args!("return {};", format_expression(value)));
                } else {
                    self.line(indent, format_args!("return;"));
                }
            }
            Statement::Trap => self.line(indent, format_args!("trap;")),
            Statement::Unreachable => self.line(indent, format_args!("unreachable;")),
        }
    }

    fn write_result_statement(
        &mut self,
        indent: usize,
        destination: Option<&Place>,
        operation: &str,
    ) {
        if let Some(destination) = destination {
            self.line(
                indent,
                format_args!("{} = {operation};", format_place(destination)),
            );
        } else {
            self.line(indent, format_args!("{operation};"));
        }
    }

    fn write_jump_statement(&mut self, indent: usize, keyword: &str, label: Option<&str>) {
        if let Some(label) = label {
            self.line(indent, format_args!("{keyword} {};", identifier(label)));
        } else {
            self.line(indent, format_args!("{keyword};"));
        }
    }

    fn line(&mut self, indent: usize, arguments: std::fmt::Arguments<'_>) {
        for _ in 0..indent {
            self.output.push_str("  ");
        }
        self.output.write_fmt(arguments).unwrap();
        self.output.push('\n');
    }

    fn blank_line(&mut self) {
        self.output.push('\n');
    }
}

fn interrupt_handlers(
    handlers: &hir::InterruptHandlers,
) -> [(&'static str, Option<&hir::Function>); 10] {
    [
        ("external_0", handlers.external_0.as_ref()),
        ("external_1", handlers.external_1.as_ref()),
        (
            "external_2_or_timer_0_low",
            handlers.external_2_or_timer_0_low.as_ref(),
        ),
        (
            "external_3_or_base_timer",
            handlers.external_3_or_base_timer.as_ref(),
        ),
        ("timer_0_high", handlers.timer_0_high.as_ref()),
        ("timer_1", handlers.timer_1.as_ref()),
        ("serial_0", handlers.serial_0.as_ref()),
        ("serial_1", handlers.serial_1.as_ref()),
        ("dreamcast_interface", handlers.dreamcast_interface.as_ref()),
        ("port_3", handlers.port_3.as_ref()),
    ]
}

fn format_expression(expression: &Expression) -> String {
    format!(
        "({} : {})",
        format_expression_kind(&expression.kind),
        format_type(&expression.ty)
    )
}

fn format_expression_kind(expression: &ExpressionKind) -> String {
    match expression {
        ExpressionKind::Constant(constant) => format_constant(constant),
        ExpressionKind::Read(place) => format!("read({})", format_place(place)),
        ExpressionKind::IntegerUnary { op, operand } => {
            format_operation(format_integer_unary_op(*op), [operand.as_ref()])
        }
        ExpressionKind::IntegerBinary { op, left, right } => format_operation(
            format_integer_binary_op(*op),
            [left.as_ref(), right.as_ref()],
        ),
        ExpressionKind::FloatUnary { op, operand } => {
            format_operation(format_float_unary_op(*op), [operand.as_ref()])
        }
        ExpressionKind::FloatBinary { op, left, right } => {
            format_operation(format_float_binary_op(*op), [left.as_ref(), right.as_ref()])
        }
        ExpressionKind::Compare { op, left, right } => {
            format_operation(format_compare_op(*op), [left.as_ref(), right.as_ref()])
        }
        ExpressionKind::Convert { op, operand } => {
            format_operation(format_conversion_op(*op), [operand.as_ref()])
        }
        ExpressionKind::Select {
            when_true,
            when_false,
            condition,
        } => format_operation(
            "select".into(),
            [when_true.as_ref(), when_false.as_ref(), condition.as_ref()],
        ),
        ExpressionKind::AddressOf(place) => format!("address_of({})", format_place(place)),
        ExpressionKind::PointerOffset { base, offset } => {
            format_operation("pointer_offset".into(), [base.as_ref(), offset.as_ref()])
        }
        ExpressionKind::Aggregate(elements) => {
            format!("aggregate({})", format_expressions(elements))
        }
        ExpressionKind::ExtractElement { aggregate, index } => format!(
            "extract({}, {index})",
            format_expression(aggregate.as_ref())
        ),
        ExpressionKind::InsertElement {
            aggregate,
            index,
            value,
        } => format!(
            "insert({}, {index}, {})",
            format_expression(aggregate.as_ref()),
            format_expression(value.as_ref())
        ),
    }
}

fn format_operation<'a>(
    name: String,
    operands: impl IntoIterator<Item = &'a Expression>,
) -> String {
    format!(
        "{name}({})",
        operands
            .into_iter()
            .map(format_expression)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn format_expressions(expressions: &[Expression]) -> String {
    expressions
        .iter()
        .map(format_expression)
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_place(place: &Place) -> String {
    match place {
        Place::Variable(name) => identifier(name),
        Place::Static(name) => format!("@{}", identifier(name)),
        Place::Field { base, index } => format!("{}.{}", format_place(base), index),
        Place::Element { base, index } => {
            format!("{}[{}]", format_place(base), format_expression(index))
        }
        Place::ByteOffset { base, offset } => format!(
            "byte_offset({}, {})",
            format_place(base),
            format_expression(offset)
        ),
    }
}

fn format_type(ty: &Type) -> String {
    match ty {
        Type::Unit => "unit".into(),
        Type::Bool => "bool".into(),
        Type::Bits(width) => format!("bits<{}>", width.get()),
        Type::Float(FloatWidth::Bits32) => "f32".into(),
        Type::Float(FloatWidth::Bits64) => "f64".into(),
        Type::Pointer => "pointer".into(),
        Type::Array { element, length } => format!("[{}; {length}]", format_type(element)),
        Type::Struct(fields) => format!(
            "struct {{ {} }}",
            fields
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::Tuple(elements) => format!(
            "tuple({})",
            elements
                .iter()
                .map(format_type)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::OpaqueBytes(length) => format!("opaque_bytes<{length}>"),
    }
}

fn format_function_type(signature: &hir::FunctionType) -> String {
    let parameters = signature
        .parameters
        .iter()
        .map(format_type)
        .collect::<Vec<_>>()
        .join(", ");
    format!("fn({parameters}) -> {}", format_type(&signature.result))
}

fn format_static_initializer(initializer: &StaticInitializer) -> String {
    match initializer {
        StaticInitializer::Zeroed => "zeroed".into(),
        StaticInitializer::Constant(constant) => format_constant(constant),
        StaticInitializer::Bytes(bytes) => format!(
            "bytes[{}]",
            bytes
                .iter()
                .map(|byte| format!("0x{byte:02x}"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        StaticInitializer::Aggregate(elements) => format!(
            "[{}]",
            elements
                .iter()
                .map(format_static_initializer)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn format_constant(constant: &Constant) -> String {
    match constant {
        Constant::Bool(value) => value.to_string(),
        Constant::Integer(value) => value.bits().to_string(),
        Constant::Float32(bits) => format_float32(*bits),
        Constant::Float64(bits) => format_float64(*bits),
        Constant::NullPointer => "null".into(),
        Constant::StaticAddress { name, offset } if *offset == 0 => {
            format!("address_of(@{})", identifier(name))
        }
        Constant::StaticAddress { name, offset } => {
            format!("address_of(@{}) + {offset}", identifier(name))
        }
        Constant::FunctionAddress(name) => format!("address_of(fn {})", identifier(name)),
    }
}

fn format_float32(bits: u32) -> String {
    let value = f32::from_bits(bits);
    if value.is_finite() {
        format!("{value:?}f32")
    } else {
        format!("f32.from_bits(0x{bits:08x})")
    }
}

fn format_float64(bits: u64) -> String {
    let value = f64::from_bits(bits);
    if value.is_finite() {
        format!("{value:?}f64")
    } else {
        format!("f64.from_bits(0x{bits:016x})")
    }
}

fn format_integer_constant(value: IntegerConstant) -> String {
    format!("{}:bits<{}>", value.bits(), value.width().get())
}

fn format_memory_access(access: hir::MemoryAccess) -> String {
    if access.volatile {
        ", volatile".into()
    } else {
        String::new()
    }
}

fn format_integer_unary_op(op: IntegerUnaryOp) -> String {
    match op {
        IntegerUnaryOp::Negate => "negate",
        IntegerUnaryOp::Not => "not",
        IntegerUnaryOp::CountLeadingZeros => "count_leading_zeros",
        IntegerUnaryOp::CountTrailingZeros => "count_trailing_zeros",
        IntegerUnaryOp::PopulationCount => "population_count",
    }
    .into()
}

fn format_integer_binary_op(op: IntegerBinaryOp) -> String {
    match op {
        IntegerBinaryOp::Add => "add".into(),
        IntegerBinaryOp::Subtract => "subtract".into(),
        IntegerBinaryOp::Multiply => "multiply".into(),
        IntegerBinaryOp::Divide(signedness) => {
            format!("divide.{}", format_signedness(signedness))
        }
        IntegerBinaryOp::Remainder(signedness) => {
            format!("remainder.{}", format_signedness(signedness))
        }
        IntegerBinaryOp::And => "and".into(),
        IntegerBinaryOp::Or => "or".into(),
        IntegerBinaryOp::Xor => "xor".into(),
        IntegerBinaryOp::ShiftLeft => "shift_left".into(),
        IntegerBinaryOp::ShiftRight(signedness) => {
            format!("shift_right.{}", format_signedness(signedness))
        }
        IntegerBinaryOp::RotateLeft => "rotate_left".into(),
        IntegerBinaryOp::RotateRight => "rotate_right".into(),
        IntegerBinaryOp::SaturatingAdd(signedness) => {
            format!("saturating_add.{}", format_signedness(signedness))
        }
        IntegerBinaryOp::SaturatingSubtract(signedness) => {
            format!("saturating_subtract.{}", format_signedness(signedness))
        }
    }
}

fn format_float_unary_op(op: FloatUnaryOp) -> String {
    match op {
        FloatUnaryOp::AbsoluteValue => "float.absolute_value",
        FloatUnaryOp::Negate => "float.negate",
        FloatUnaryOp::SquareRoot => "float.square_root",
        FloatUnaryOp::Ceiling => "float.ceiling",
        FloatUnaryOp::Floor => "float.floor",
        FloatUnaryOp::Truncate => "float.truncate",
        FloatUnaryOp::Nearest => "float.nearest",
    }
    .into()
}

fn format_float_binary_op(op: FloatBinaryOp) -> String {
    match op {
        FloatBinaryOp::Add => "float.add",
        FloatBinaryOp::Subtract => "float.subtract",
        FloatBinaryOp::Multiply => "float.multiply",
        FloatBinaryOp::Divide => "float.divide",
        FloatBinaryOp::Minimum => "float.minimum",
        FloatBinaryOp::Maximum => "float.maximum",
        FloatBinaryOp::CopySign => "float.copy_sign",
    }
    .into()
}

fn format_compare_op(op: CompareOp) -> String {
    match op {
        CompareOp::Equal => "compare.equal".into(),
        CompareOp::NotEqual => "compare.not_equal".into(),
        CompareOp::LessThan(signedness) => {
            format!("compare.less_than.{}", format_signedness(signedness))
        }
        CompareOp::LessThanOrEqual(signedness) => format!(
            "compare.less_than_or_equal.{}",
            format_signedness(signedness)
        ),
        CompareOp::GreaterThan(signedness) => {
            format!("compare.greater_than.{}", format_signedness(signedness))
        }
        CompareOp::GreaterThanOrEqual(signedness) => format!(
            "compare.greater_than_or_equal.{}",
            format_signedness(signedness)
        ),
        CompareOp::FloatEqual => "float.compare.equal".into(),
        CompareOp::FloatNotEqual => "float.compare.not_equal".into(),
        CompareOp::FloatLessThan => "float.compare.less_than".into(),
        CompareOp::FloatLessThanOrEqual => "float.compare.less_than_or_equal".into(),
        CompareOp::FloatGreaterThan => "float.compare.greater_than".into(),
        CompareOp::FloatGreaterThanOrEqual => "float.compare.greater_than_or_equal".into(),
    }
}

fn format_conversion_op(op: ConversionOp) -> String {
    match op {
        ConversionOp::Truncate => "convert.truncate".into(),
        ConversionOp::ZeroExtend => "convert.zero_extend".into(),
        ConversionOp::SignExtend => "convert.sign_extend".into(),
        ConversionOp::IntegerToFloat(signedness) => {
            format!("convert.integer_to_float.{}", format_signedness(signedness))
        }
        ConversionOp::FloatToInteger {
            signedness,
            saturating: false,
        } => format!("convert.float_to_integer.{}", format_signedness(signedness)),
        ConversionOp::FloatToInteger {
            signedness,
            saturating: true,
        } => format!(
            "convert.float_to_integer.{}.saturating",
            format_signedness(signedness)
        ),
        ConversionOp::PromoteFloat => "convert.promote_float".into(),
        ConversionOp::DemoteFloat => "convert.demote_float".into(),
        ConversionOp::Reinterpret => "convert.reinterpret".into(),
        ConversionOp::BooleanToInteger => "convert.boolean_to_integer".into(),
        ConversionOp::IntegerToBoolean => "convert.integer_to_boolean".into(),
    }
}

fn format_signedness(signedness: Signedness) -> &'static str {
    match signedness {
        Signedness::Signed => "signed",
        Signedness::Unsigned => "unsigned",
    }
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

#[cfg(test)]
mod tests {
    use super::render;
    use crate::wasm::hir::{
        BitWidth, Block, Constant, Expression, ExpressionKind, Function, FunctionType,
        IntegerBinaryOp, IntegerConstant, InterruptHandlers, Parameter, Place, Program, Statement,
        Static, StaticInitializer, Type,
    };
    use crate::wasm::intrinsics::{Intrinsic, Sfr};

    #[test]
    fn renders_program_structure_types_and_expressions() {
        let bits_8 = BitWidth::new(8).unwrap();
        let byte = Type::Bits(bits_8);
        let read = |name: &str, ty: Type| Expression {
            ty,
            kind: ExpressionKind::Read(Place::Variable(name.into())),
        };
        let constant = |value| Expression {
            ty: byte.clone(),
            kind: ExpressionKind::Constant(Constant::Integer(
                IntegerConstant::new(bits_8, value).unwrap(),
            )),
        };

        let main = Function {
            name: "main".into(),
            parameters: vec![Parameter {
                name: "value".into(),
                ty: byte.clone(),
            }],
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
                    Statement::Assign {
                        destination: Place::Static("counter".into()),
                        value: Expression {
                            ty: byte.clone(),
                            kind: ExpressionKind::IntegerBinary {
                                op: IntegerBinaryOp::Add,
                                left: Box::new(read("value", byte.clone())),
                                right: Box::new(constant(1)),
                            },
                        },
                    },
                    Statement::Call {
                        destination: None,
                        function: "tick".into(),
                        arguments: vec![read("flag", Type::Bool)],
                    },
                    Statement::Return(None),
                ],
            },
        };
        let interrupt = Function {
            name: "external_irq".into(),
            parameters: Vec::new(),
            result: Type::Unit,
            body: Block {
                statements: vec![Statement::Return(None)],
            },
        };
        let helper = Function {
            name: "tick".into(),
            parameters: vec![Parameter {
                name: "enabled".into(),
                ty: Type::Bool,
            }],
            result: Type::Unit,
            body: Block {
                statements: vec![
                    Statement::If {
                        condition: read("enabled", Type::Bool),
                        then_body: Block {
                            statements: vec![Statement::Intrinsic {
                                destination: None,
                                operation: Intrinsic::SfrRead(Sfr::Port7),
                                arguments: Vec::new(),
                            }],
                        },
                        else_body: Some(Block {
                            statements: vec![Statement::Trap],
                        }),
                    },
                    Statement::Return(None),
                ],
            },
        };
        let program = Program {
            main,
            interrupt_handlers: InterruptHandlers {
                external_0: Some(interrupt),
                ..InterruptHandlers::default()
            },
            functions: vec![helper],
            statics: vec![Static {
                name: "counter".into(),
                ty: byte,
                mutable: true,
                initializer: StaticInitializer::Constant(Constant::Integer(
                    IntegerConstant::new(bits_8, 3).unwrap(),
                )),
            }],
        };

        let output = render(&program);

        assert_eq!(
            output,
            concat!(
                "program {\n",
                "  static mut @counter: bits<8> = 3;\n",
                "\n",
                "  main fn main(value: bits<8>) -> unit {\n",
                "    let mut flag: bool = (true : bool);\n",
                "    @counter = (add((read(value) : bits<8>), (1 : bits<8>)) : bits<8>);\n",
                "    call tick((read(flag) : bool));\n",
                "    return;\n",
                "  }\n",
                "\n",
                "  interrupt external_0 fn external_irq() -> unit {\n",
                "    return;\n",
                "  }\n",
                "\n",
                "  fn tick(enabled: bool) -> unit {\n",
                "    if (read(enabled) : bool) {\n",
                "      intrinsic waterbear_vmu_sfr_p7_read();\n",
                "    } else {\n",
                "      trap;\n",
                "    }\n",
                "    return;\n",
                "  }\n",
                "}\n",
            )
        );
    }

    #[test]
    fn quotes_names_that_are_not_plain_identifiers() {
        let program = Program {
            main: Function {
                name: "wasm entry".into(),
                parameters: Vec::new(),
                result: Type::Unit,
                body: Block {
                    statements: vec![Statement::Return(None)],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };

        assert!(render(&program).contains("main fn \"wasm entry\"() -> unit"));
    }

    #[test]
    fn renders_typed_indirect_calls() {
        let bits_8 = Type::Bits(BitWidth::new(8).unwrap());
        let read = |name: &str, ty: Type| Expression {
            ty,
            kind: ExpressionKind::Read(Place::Variable(name.into())),
        };
        let signature = FunctionType {
            parameters: vec![bits_8.clone()],
            result: bits_8.clone(),
        };
        let target = read("function", Type::Pointer);
        let argument = read("argument", bits_8.clone());
        let program = Program {
            main: Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: bits_8,
                body: Block {
                    statements: vec![
                        Statement::CallIndirect {
                            destination: Some(Place::Variable("result".into())),
                            target: target.clone(),
                            signature: signature.clone(),
                            arguments: vec![argument.clone()],
                        },
                        Statement::TailCallIndirect {
                            target,
                            signature,
                            arguments: vec![argument],
                        },
                    ],
                },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        };

        let output = render(&program);

        assert!(output.contains(
            "result = call indirect (read(function) : pointer) as fn(bits<8>) -> bits<8>((read(argument) : bits<8>));"
        ));
        assert!(output.contains(
            "tail call indirect (read(function) : pointer) as fn(bits<8>) -> bits<8>((read(argument) : bits<8>));"
        ));
    }
}
