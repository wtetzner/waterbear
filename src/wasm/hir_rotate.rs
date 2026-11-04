//! Recognition of byte-rotate idioms while HIR still retains expression trees.

use std::collections::{BTreeMap, BTreeSet};

use super::hir::{
    BitWidth, Block, Constant, ConversionOp, Expression, ExpressionKind, Function, IntegerBinaryOp,
    IntegerConstant, Place, Program, Signedness, Statement, Type,
};

type Aliases = BTreeMap<String, Expression>;

pub fn canonicalize(program: &mut Program) {
    for function in program.all_functions_mut() {
        canonicalize_function(function);
    }
}

fn canonicalize_function(function: &mut Function) {
    canonicalize_block(&mut function.body, &mut Aliases::new());
}

fn canonicalize_block(block: &mut Block, aliases: &mut Aliases) {
    for statement in &mut block.statements {
        match statement {
            Statement::Declare {
                name, initializer, ..
            } => {
                if let Some(initializer) = initializer {
                    canonicalize_expression(initializer, aliases);
                    aliases.insert(name.clone(), resolve_aliases(initializer, aliases));
                } else {
                    aliases.remove(name);
                }
            }
            Statement::Assign { destination, value } => {
                canonicalize_place(destination, aliases);
                canonicalize_expression(value, aliases);
                let replacement = resolve_aliases(value, aliases);
                if let Place::Variable(name) = destination {
                    invalidate_variable(aliases, name);
                    aliases.insert(name.clone(), replacement);
                } else {
                    aliases.clear();
                }
            }
            Statement::Load {
                destination,
                address,
                ..
            } => {
                canonicalize_expression(address, aliases);
                if let Some(destination) = destination {
                    canonicalize_place(destination, aliases);
                }
                aliases.clear();
            }
            Statement::Store { address, value, .. } => {
                canonicalize_expression(address, aliases);
                canonicalize_expression(value, aliases);
                aliases.clear();
            }
            Statement::Call {
                destination,
                arguments,
                ..
            }
            | Statement::Intrinsic {
                destination,
                arguments,
                ..
            } => {
                for argument in arguments {
                    canonicalize_expression(argument, aliases);
                }
                if let Some(destination) = destination {
                    canonicalize_place(destination, aliases);
                }
                aliases.clear();
            }
            Statement::CallIndirect {
                destination,
                target,
                arguments,
                ..
            } => {
                for argument in arguments {
                    canonicalize_expression(argument, aliases);
                }
                canonicalize_expression(target, aliases);
                if let Some(destination) = destination {
                    canonicalize_place(destination, aliases);
                }
                aliases.clear();
            }
            Statement::TailCall { arguments, .. } => {
                for argument in arguments {
                    canonicalize_expression(argument, aliases);
                }
                aliases.clear();
            }
            Statement::TailCallIndirect {
                target, arguments, ..
            } => {
                for argument in arguments {
                    canonicalize_expression(argument, aliases);
                }
                canonicalize_expression(target, aliases);
                aliases.clear();
            }
            Statement::Block { body, .. } | Statement::Loop { body, .. } => {
                canonicalize_block(body, &mut aliases.clone());
                aliases.clear();
            }
            Statement::If {
                condition,
                then_body,
                else_body,
            } => {
                canonicalize_expression(condition, aliases);
                canonicalize_block(then_body, &mut aliases.clone());
                if let Some(else_body) = else_body {
                    canonicalize_block(else_body, &mut aliases.clone());
                }
                aliases.clear();
            }
            Statement::Switch {
                value,
                cases,
                default,
            } => {
                canonicalize_expression(value, aliases);
                for case in cases {
                    canonicalize_block(&mut case.body, &mut aliases.clone());
                }
                canonicalize_block(default, &mut aliases.clone());
                aliases.clear();
            }
            Statement::Return(value) => {
                if let Some(value) = value {
                    canonicalize_expression(value, aliases);
                }
            }
            Statement::Break { .. }
            | Statement::Continue { .. }
            | Statement::Trap
            | Statement::Unreachable => {}
        }
    }
}

fn canonicalize_expression(expression: &mut Expression, aliases: &Aliases) {
    match &mut expression.kind {
        ExpressionKind::IntegerUnary { operand, .. }
        | ExpressionKind::FloatUnary { operand, .. }
        | ExpressionKind::Convert { operand, .. } => canonicalize_expression(operand, aliases),
        ExpressionKind::IntegerBinary { left, right, .. }
        | ExpressionKind::FloatBinary { left, right, .. }
        | ExpressionKind::Compare { left, right, .. }
        | ExpressionKind::PointerOffset {
            base: left,
            offset: right,
        } => {
            canonicalize_expression(left, aliases);
            canonicalize_expression(right, aliases);
        }
        ExpressionKind::Select {
            when_true,
            when_false,
            condition,
        } => {
            canonicalize_expression(when_true, aliases);
            canonicalize_expression(when_false, aliases);
            canonicalize_expression(condition, aliases);
        }
        ExpressionKind::Read(place) | ExpressionKind::AddressOf(place) => {
            canonicalize_place(place, aliases)
        }
        ExpressionKind::Aggregate(elements) => {
            for element in elements {
                canonicalize_expression(element, aliases);
            }
        }
        ExpressionKind::ExtractElement { aggregate, .. } => {
            canonicalize_expression(aggregate, aliases)
        }
        ExpressionKind::InsertElement {
            aggregate, value, ..
        } => {
            canonicalize_expression(aggregate, aliases);
            canonicalize_expression(value, aliases);
        }
        ExpressionKind::Constant(_) => {}
    }

    if let Some(replacement) = recognize_rotate(expression, aliases) {
        *expression = replacement;
    }
}

fn canonicalize_place(place: &mut Place, aliases: &Aliases) {
    match place {
        Place::Field { base, .. } => canonicalize_place(base, aliases),
        Place::Element { base, index } => {
            canonicalize_place(base, aliases);
            canonicalize_expression(index, aliases);
        }
        Place::ByteOffset { base, offset } => {
            canonicalize_place(base, aliases);
            canonicalize_expression(offset, aliases);
        }
        Place::Variable(_) | Place::Static(_) => {}
    }
}

fn recognize_rotate(expression: &Expression, aliases: &Aliases) -> Option<Expression> {
    if expression.ty != Type::Bits(BitWidth::new(8).unwrap()) {
        return None;
    }
    let ExpressionKind::IntegerBinary {
        op: IntegerBinaryOp::Or,
        left,
        right,
    } = &expression.kind
    else {
        return None;
    };

    recognize_direction(left, right, aliases, true)
        .or_else(|| recognize_direction(right, left, aliases, true))
        .or_else(|| recognize_direction(left, right, aliases, false))
        .or_else(|| recognize_direction(right, left, aliases, false))
}

fn recognize_direction(
    first: &Expression,
    second: &Expression,
    aliases: &Aliases,
    rotate_left: bool,
) -> Option<Expression> {
    let first_op = if rotate_left {
        IntegerBinaryOp::ShiftLeft
    } else {
        IntegerBinaryOp::ShiftRight(Signedness::Unsigned)
    };
    let second_op = if rotate_left {
        IntegerBinaryOp::ShiftRight(Signedness::Unsigned)
    } else {
        IntegerBinaryOp::ShiftLeft
    };
    let (first_value, first_amount) = shift(first, first_op)?;
    let (second_value, second_amount) = shift(second, second_op)?;
    if !(1..8).contains(&first_amount) || second_amount != 8 - first_amount {
        return None;
    }

    let first_mask = if rotate_left {
        (1u128 << (8 - first_amount)) - 1
    } else {
        0xff & !((1u128 << first_amount) - 1)
    };
    let second_mask = 0xff ^ first_mask;
    let first_source = preserved_source(first_value, first_mask, aliases, &mut BTreeSet::new())?;
    let second_source = preserved_source(second_value, second_mask, aliases, &mut BTreeSet::new())?;
    if first_source != second_source {
        return None;
    }

    let byte = coerce_to_byte(first_source)?;
    let amount = Expression {
        ty: Type::Bits(BitWidth::new(8).unwrap()),
        kind: ExpressionKind::Constant(Constant::Integer(
            IntegerConstant::new(BitWidth::new(8).unwrap(), first_amount).unwrap(),
        )),
    };
    Some(Expression {
        ty: Type::Bits(BitWidth::new(8).unwrap()),
        kind: ExpressionKind::IntegerBinary {
            op: if rotate_left {
                IntegerBinaryOp::RotateLeft
            } else {
                IntegerBinaryOp::RotateRight
            },
            left: Box::new(byte),
            right: Box::new(amount),
        },
    })
}

fn shift(expression: &Expression, expected: IntegerBinaryOp) -> Option<(&Expression, u128)> {
    let expression = strip_result_conversion(expression);
    let ExpressionKind::IntegerBinary { op, left, right } = &expression.kind else {
        return None;
    };
    if *op != expected {
        return None;
    }
    Some((left.as_ref(), integer_constant(right)?))
}

fn strip_result_conversion(mut expression: &Expression) -> &Expression {
    while let ExpressionKind::Convert {
        op: ConversionOp::Truncate,
        operand,
    } = &expression.kind
    {
        expression = operand;
    }
    expression
}

fn preserved_source(
    expression: &Expression,
    needed_mask: u128,
    aliases: &Aliases,
    seen: &mut BTreeSet<String>,
) -> Option<Expression> {
    match &expression.kind {
        ExpressionKind::Read(Place::Variable(name)) if aliases.contains_key(name) => {
            if !seen.insert(name.clone()) {
                return None;
            }
            preserved_source(&aliases[name], needed_mask, aliases, seen)
        }
        ExpressionKind::Read(_) => Some(expression.clone()),
        ExpressionKind::Convert { op, operand }
            if matches!(op, ConversionOp::Truncate | ConversionOp::ZeroExtend) =>
        {
            if matches!(op, ConversionOp::Truncate)
                && type_width(&expression.ty)
                    .is_none_or(|width| needed_mask >= (1u128 << width.min(127)))
            {
                return None;
            }
            preserved_source(operand, needed_mask, aliases, seen)
        }
        ExpressionKind::IntegerBinary {
            op: IntegerBinaryOp::And,
            left,
            right,
        } => {
            if integer_constant(right).is_some_and(|mask| mask & needed_mask == needed_mask) {
                preserved_source(left, needed_mask, aliases, seen)
            } else if integer_constant(left).is_some_and(|mask| mask & needed_mask == needed_mask) {
                preserved_source(right, needed_mask, aliases, seen)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn coerce_to_byte(expression: Expression) -> Option<Expression> {
    let width = type_width(&expression.ty)?;
    let byte = Type::Bits(BitWidth::new(8).unwrap());
    if width == 8 {
        Some(expression)
    } else {
        Some(Expression {
            ty: byte,
            kind: ExpressionKind::Convert {
                op: if width < 8 {
                    ConversionOp::ZeroExtend
                } else {
                    ConversionOp::Truncate
                },
                operand: Box::new(expression),
            },
        })
    }
}

fn resolve_aliases(expression: &Expression, aliases: &Aliases) -> Expression {
    let mut expression = expression.clone();
    let mut seen = BTreeSet::new();
    while let ExpressionKind::Read(Place::Variable(name)) = &expression.kind {
        if !seen.insert(name.clone()) {
            break;
        }
        let Some(alias) = aliases.get(name) else {
            break;
        };
        expression = alias.clone();
    }
    expression
}

fn invalidate_variable(aliases: &mut Aliases, variable: &str) {
    aliases.retain(|name, expression| {
        name != variable && !expression_reads_variable(expression, variable)
    });
}

fn expression_reads_variable(expression: &Expression, variable: &str) -> bool {
    match &expression.kind {
        ExpressionKind::Constant(_) => false,
        ExpressionKind::Read(place) | ExpressionKind::AddressOf(place) => {
            place_reads_variable(place, variable)
        }
        ExpressionKind::IntegerUnary { operand, .. }
        | ExpressionKind::FloatUnary { operand, .. }
        | ExpressionKind::Convert { operand, .. } => expression_reads_variable(operand, variable),
        ExpressionKind::IntegerBinary { left, right, .. }
        | ExpressionKind::FloatBinary { left, right, .. }
        | ExpressionKind::Compare { left, right, .. }
        | ExpressionKind::PointerOffset {
            base: left,
            offset: right,
        } => {
            expression_reads_variable(left, variable) || expression_reads_variable(right, variable)
        }
        ExpressionKind::Select {
            when_true,
            when_false,
            condition,
        } => {
            expression_reads_variable(when_true, variable)
                || expression_reads_variable(when_false, variable)
                || expression_reads_variable(condition, variable)
        }
        ExpressionKind::Aggregate(elements) => elements
            .iter()
            .any(|element| expression_reads_variable(element, variable)),
        ExpressionKind::ExtractElement { aggregate, .. } => {
            expression_reads_variable(aggregate, variable)
        }
        ExpressionKind::InsertElement {
            aggregate, value, ..
        } => {
            expression_reads_variable(aggregate, variable)
                || expression_reads_variable(value, variable)
        }
    }
}

fn place_reads_variable(place: &Place, variable: &str) -> bool {
    match place {
        Place::Variable(name) => name == variable,
        Place::Static(_) => false,
        Place::Field { base, .. } => place_reads_variable(base, variable),
        Place::Element { base, index } => {
            place_reads_variable(base, variable) || expression_reads_variable(index, variable)
        }
        Place::ByteOffset { base, offset } => {
            place_reads_variable(base, variable) || expression_reads_variable(offset, variable)
        }
    }
}

fn integer_constant(expression: &Expression) -> Option<u128> {
    match &expression.kind {
        ExpressionKind::Constant(Constant::Integer(value)) => Some(value.bits()),
        ExpressionKind::Constant(Constant::Bool(value)) => Some(u128::from(*value)),
        ExpressionKind::Convert {
            op: ConversionOp::ZeroExtend | ConversionOp::Truncate,
            operand,
        } => integer_constant(operand),
        _ => None,
    }
}

fn type_width(ty: &Type) -> Option<u8> {
    match ty {
        Type::Bool => Some(1),
        Type::Bits(width) => Some(width.get()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{Aliases, recognize_rotate};
    use crate::wasm::hir::{
        BitWidth, Constant, Expression, ExpressionKind, IntegerBinaryOp, IntegerConstant, Place,
        Signedness, Type,
    };

    #[test]
    fn recognizes_a_masked_byte_rotate() {
        let source = read("value");
        let left = binary(IntegerBinaryOp::ShiftLeft, source.clone(), integer(1));
        let high_bit = binary(IntegerBinaryOp::And, source, integer(0x80));
        let right = binary(
            IntegerBinaryOp::ShiftRight(Signedness::Unsigned),
            high_bit,
            integer(7),
        );
        let expression = binary(IntegerBinaryOp::Or, left, right);

        let rotated = recognize_rotate(&expression, &Aliases::new()).unwrap();
        assert!(matches!(
            rotated.kind,
            ExpressionKind::IntegerBinary {
                op: IntegerBinaryOp::RotateLeft,
                ..
            }
        ));
    }

    #[test]
    fn rejects_a_mask_that_discards_a_rotated_bit() {
        let source = read("value");
        let left = binary(IntegerBinaryOp::ShiftLeft, source.clone(), integer(1));
        let missing_high_bit = binary(IntegerBinaryOp::And, source, integer(0x7f));
        let right = binary(
            IntegerBinaryOp::ShiftRight(Signedness::Unsigned),
            missing_high_bit,
            integer(7),
        );
        let expression = binary(IntegerBinaryOp::Or, left, right);

        assert!(recognize_rotate(&expression, &Aliases::new()).is_none());
    }

    fn byte_type() -> Type {
        Type::Bits(BitWidth::new(8).unwrap())
    }

    fn read(name: &str) -> Expression {
        Expression {
            ty: byte_type(),
            kind: ExpressionKind::Read(Place::Variable(name.into())),
        }
    }

    fn integer(value: u128) -> Expression {
        Expression {
            ty: byte_type(),
            kind: ExpressionKind::Constant(Constant::Integer(
                IntegerConstant::new(BitWidth::new(8).unwrap(), value).unwrap(),
            )),
        }
    }

    fn binary(op: IntegerBinaryOp, left: Expression, right: Expression) -> Expression {
        Expression {
            ty: byte_type(),
            kind: ExpressionKind::IntegerBinary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
        }
    }
}
