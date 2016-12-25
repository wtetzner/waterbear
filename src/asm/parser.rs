
use pest::{StringInput, Token, Parser, Input};
use asm::ast::*;
use asm::instruction::*;
use asm::expression::*;

impl_rdp! {
    grammar! {
        num_expression = _{
            { ["("] ~ num_expression ~ [")"] | upper_byte | lower_byte | number | name }
            addition       = { plus  | minus }
            multiplication = { times | slash }
        }
        number = _{ hex_num | oct_num | dec_num }
        dec_num = @{ ["0"] | (['1'..'9'] ~ ['0'..'9']*) }
        oct_num = @{ ["0"] ~ ['0'..'9']+ }
        hex_num = _{ hex_num1 | hex_num2 }
        hex_num1 = @{ ["$"] ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
        hex_num2 = @{ ["0"] ~ (["X"] | ["x"]) ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
        plus   =  { ["+"] }
        minus  =  { ["-"] }
        times  =  { ["*"] }
        slash  =  { ["/"] }

        upper_byte = { [">"] ~ num_expression }
        lower_byte = { ["<"] ~ num_expression }

        label = @{ ["."] ~ name }

        name = @{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ["."]) ~ (['a'..'z'] | ['A'..'Z'] | ["$"] | ["_"] | ["."] | ['0'..'9'])* }

        // instruction = {}
        // add_i8

        imm8 = { ["#"] ~ num_expression }

        whitespace = _{ [" "] | ["\t"] }
    }

    process! {
        // Statement<Expression,Expression,Expression,Expression,Expression>
        to_expr(&self) -> Result<Expression,EvaluationError> {
            (&number: dec_num) => Ok(Expression::Number(number.parse::<i32>()?)),
            (&number: oct_num) =>  Ok(Expression::Number(i32::from_str_radix(number, 8)?)),
            (&number: hex_num1) => Ok(Expression::Number(i32::from_str_radix(&number[1..], 16)?)),
            (&number: hex_num2) =>  Ok(Expression::Number(i32::from_str_radix(&number[2..], 16)?)),
            (_: upper_byte, value: to_expr()) => Ok(Expression::UpperByte(Box::new(value?))),
            (_: lower_byte, value: to_expr()) => Ok(Expression::LowerByte(Box::new(value?))),
            (&name: name) => Ok(Expression::Name(name.to_string())),
            (_: addition, left: to_expr(), sign, right: to_expr()) => {
                match sign.rule {
                    Rule::plus  => Ok(Expression::Plus(Box::new(left?), Box::new(right?))),
                    Rule::minus => Ok(Expression::Minus(Box::new(left?), Box::new(right?))),
                    _ => unreachable!()
                }
            },
            (_: multiplication, left: to_expr(), sign, right: to_expr()) => {
                match sign.rule {
                    Rule::times => Ok(Expression::Times(Box::new(left?), Box::new(right?))),
                    Rule::slash => Ok(Expression::Divide(Box::new(left?), Box::new(right?))),
                    _ => unreachable!()
                }
            }
        }
    }
}

pub fn run_parser() {

    let mut parser = Rdp::new(StringInput::new("0xf7 - ((3 + (0x9 + 03 * 4 + (3 + 1) / 2 - 4)) * 2) + sam + >(5 - 8)"));

    assert!(parser.num_expression());
    assert!(parser.end());

    let result = parser.to_expr();
    println!("result: {:?}", result);

    // assert_eq!(result, 44);
}

