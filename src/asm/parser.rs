
use pest::{StringInput, Token, Parser, Input};
use ast;

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

        label = @{ ["."] ~ name }

        name = @{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ["."]) ~ (['a'..'z'] | ['A'..'Z'] | ["$"] | ["_"] | ["."] | ['0'..'9'])* }

        // instruction = {}
        // add_i8

        imm8 = { ["#"] ~ num_expression }
        upper_byte = { [">"] ~ num_expression }
        lower_byte = { ["<"] ~ num_expression }

        whitespace = _{ [" "] | ["\t"] }
    }

    process! {
        compute(&self) -> i32 {
            (&number: dec_num) => number.parse::<i32>().expect("Failed to parse decimal"),
            (&number: oct_num) =>  i32::from_str_radix(number, 8).expect("Failed to parse octal"),
            (&number: hex_num1) =>  i32::from_str_radix(&number[1..], 16).expect("Failed to parse $hex"),
            (&number: hex_num2) =>  i32::from_str_radix(&number[2..], 16).expect("Failed to parse 0xhex"),
            (_: addition, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::plus  => left + right,
                    Rule::minus => left - right,
                    _ => unreachable!()
                }
            },
            (_: multiplication, left: compute(), sign, right: compute()) => {
                match sign.rule {
                    Rule::times => left * right,
                    Rule::slash => left / right,
                    _ => unreachable!()
                }
            }
        }
    }
}

pub fn run_parser() {

    let mut parser = Rdp::new(StringInput::new("((3 + (0x9 + 03 * 4 + (3 + 1) / 2 - 4)) * 2) + sam"));

    assert!(parser.num_expression());

    let result = parser.compute();
    println!("result: {}", result);

    assert_eq!(result, 44);
}

