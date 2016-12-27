
use pest::{StringInput, Token, Parser, Input};
use asm::ast::*;
use asm::instruction::*;
use asm::expression::*;

#[derive(Debug,Clone)]
enum Arg {
    I8(Expression),
    Ri(IndirectionMode),
    Mem(Expression)
}

#[derive(Debug,Clone)]
enum Args {
    Empty,
    One(Arg),
    Two(Arg,Arg),
    Three(Arg,Arg,Arg),
    Many(Vec<Arg>)
}

impl Args {
    fn prepend(self, arg: Arg) -> Args {
        match self {
            Args::Empty => Args::One(arg),
            Args::One(arg1) => Args::Two(arg, arg1),
            Args::Two(arg1, arg2) => Args::Three(arg, arg1, arg2),
            Args::Three(arg1, arg2, arg3) => Args::Many(vec![arg, arg1, arg2, arg3]),
            Args::Many(vec) => {
                let mut result = Vec::with_capacity(vec.len() + 1);
                result.push(arg);
                for a in vec {
                    result.push(a);
                }
                Args::Many(result)
            }
        }
    }
}

impl_rdp! {
    grammar! {
        statements = { whitespace* ~ statement* ~ whitespace* }
        statement = _{ instruction | label | directive }

        num_expression = _{
            { ["("] ~ num_expression ~ [")"] | upper_byte | lower_byte | number | name }
            addition       = { plus  | minus }
            multiplication = { times | slash }
        }
        number = _{ bin_num | hex_num | oct_num | dec_num }
        dec_num = @{ ["0"] | (['1'..'9'] ~ ['0'..'9']*) }
        oct_num = @{ ["0"] ~ ['0'..'9']+ }
        hex_num = _{ hex_num1 | hex_num2 }
        hex_num1 = @{ ["$"] ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
        hex_num2 = @{ ["0"] ~ (["X"] | ["x"]) ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
        bin_num = @{ ["%"] ~ ['0'..'1']+ }
        plus   =  { ["+"] }
        minus  =  { ["-"] }
        times  =  { ["*"] }
        slash  =  { ["/"] }

        upper_byte = { [">"] ~ num_expression }
        lower_byte = { ["<"] ~ num_expression }

        directive = _{ include_dir | byte_dir | byte_dir_str | word_dir | org_dir }
        include_dir = { [".include"] ~ string }
        byte_dir = { [".byte"] ~ num_expression ~ ([","] ~ num_expression)* }
        byte_dir_str = { [".byte"] ~ string }
        word_dir = { [".word"] ~ num_expression ~ ([","] ~ num_expression)* }
        org_dir = { [".org"] ~ num_expression }

        string = @{ ["\""] ~ ((["\\"] ~ any) | (!["\""] ~ any))* ~ ["\""] }

        label = { name ~ [":"] }

        name = @{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ["."]) ~ (['a'..'z'] | ['A'..'Z'] | ["$"] | ["_"] | ["."] | ['0'..'9'])* }

        instruction = { name ~ args? }
        args = { (arg ~ ([","] ~ arg)*)}

        arg = _{ imm8 | ri | mem }

        imm8 = { ["#"] ~ num_expression }
        ri = { ["@R0"] | ["@R1"] | ["@R2"] | ["@R3"] | ["@r0"] | ["@r1"] | ["@r2"] | ["@r3"]}
        mem = { num_expression }

        comment = _{ [";"] ~ (!newline ~ any)* ~ newline }
        whitespace = _{ [" "] | ["\t"] | newline  }
        newline = _{ ["\r\n"] | ["\n"] }
    }
}

#[derive(Debug)]
pub struct Node<'a> {
    rule: Rule,
    start: usize,
    end: usize,
    text: &'a str,
    children: Vec<Node<'a>>
}

fn to_tree<'a, 'b>(input: &'a str, index: usize, tokens: &'b Vec<Token<Rule>>) -> (usize, Node<'a>) {
    let current = tokens[index];
    let mut children = vec![];
    let mut index = index + 1;

    loop {
        if index >= tokens.len() {
            break;
        }
        let token = tokens[index];
        if token.start >= current.start && token.start < current.end {
            let (new_idx, node) = to_tree(input, index, tokens);
            children.push(node);
            index = new_idx;
        } else {
            break;
        }
    }

    (index, Node {
        rule: current.rule,
        start: current.start,
        end: current.end,
        text: &input[current.start..current.end],
        children: children
    })
}

fn to_expr<'a>(node: &Node<'a>) -> Result<Expression,EvaluationError> {
    match node.rule {
        Rule::dec_num => Ok(Expression::Number(node.text.parse::<i32>()?)),
        Rule::oct_num => Ok(Expression::Number(i32::from_str_radix(node.text, 8)?)),
        Rule::hex_num1 => Ok(Expression::Number(i32::from_str_radix(&node.text[1..], 16)?)),
        Rule::hex_num2 => Ok(Expression::Number(i32::from_str_radix(&node.text[2..], 16)?)),
        Rule::bin_num => Ok(Expression::Number(i32::from_str_radix(&node.text[1..], 2)?)),
        Rule::upper_byte => Ok(Expression::UpperByte(Box::new(to_expr(&node.children[0])?))),
        Rule::lower_byte => Ok(Expression::LowerByte(Box::new(to_expr(&node.children[0])?))),
        Rule::name => Ok(Expression::Name(node.text.to_string())),
        Rule::addition => {
            let left = to_expr(&node.children[0])?;
            let right = to_expr(&node.children[2])?;
            match node.children[1].rule {
                Rule::plus => Ok(Expression::Plus(Box::new(left), Box::new(right))),
                Rule::minus => Ok(Expression::Minus(Box::new(left), Box::new(right))),
                _ => unreachable!()
            }
        },
        Rule::multiplication => {
            let left = to_expr(&node.children[0])?;
            let right = to_expr(&node.children[2])?;
            match node.children[1].rule {
                Rule::times => Ok(Expression::Times(Box::new(left), Box::new(right))),
                Rule::slash => Ok(Expression::Divide(Box::new(left), Box::new(right))),
                _ => unreachable!()
            }
        },
        _ => unreachable!()
    }
}

fn to_arg<'a>(node: &Node<'a>) -> Result<Arg,EvaluationError> {
    match node.rule {
        Rule::imm8 => Ok(Arg::I8(to_expr(&node.children[0])?)),
        Rule::ri => {
            match node.text.to_lowercase().as_ref() {
                "@r0" => Ok(Arg::Ri(IndirectionMode::R0)),
                "@r1" => Ok(Arg::Ri(IndirectionMode::R1)),
                "@r2" => Ok(Arg::Ri(IndirectionMode::R2)),
                "@r3" => Ok(Arg::Ri(IndirectionMode::R3)),
                _ => unreachable!()
            }
        },
        Rule::mem => Ok(Arg::Mem(to_expr(&node.children[0])?)),
        _ => unreachable!()
    }
}

fn to_args<'a>(node: &Node<'a>) -> Result<Args,EvaluationError> {
    let mut results = vec![];
    for n in node.children.iter() {
        results.push(to_arg(&n)?);
    }
    match results.len() {
        0 => Ok(Args::Empty),
        1 => Ok(Args::One(results[0].clone())),
        2 => Ok(Args::Two(results[0].clone(), results[1].clone())),
        3 => Ok(Args::Three(results[0].clone(), results[1].clone(), results[2].clone())),
        _ => Ok(Args::Many(results))
    }
}

fn to_instr<'a>(node: &Node<'a>) -> Result<Instruction<Expression,Expression,Expression,Expression,Expression>,EvaluationError> {
    let name = node.children[0].text.to_lowercase();
    let args = to_args(&node.children[1])?;
    match (name.as_ref(), args) {
        ("add", Args::One(Arg::I8(i8))) => Ok(Instruction::Add_i8(i8)),
        ("add", Args::One(Arg::Mem(mem))) => Ok(Instruction::Add_d9(mem)),
        ("add", Args::One(Arg::Ri(ri))) => Ok(Instruction::Add_Ri(ri)),
        ("add", args) => Err(EvaluationError::Failure("Invalid add instruction".to_string())),

        ("addc", Args::One(Arg::I8(i8))) => Ok(Instruction::Addc_i8(i8)),
        ("addc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Addc_d9(mem)),
        ("addc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Addc_Ri(ri)),
        ("addc", args) => Err(EvaluationError::Failure("Invalid addc instruction".to_string())),

        (n,_) => Err(EvaluationError::Failure(format!("Invalid instruction: {}", n)))
    }
}

// include_dir = { [".include"] ~ string }
// byte_dir = { [".byte"] ~ num_expression ~ ([","] ~ num_expression)* }
// byte_dir_str = { [".byte"] ~ string }
// word_dir = { [".word"] ~ num_expression ~ ([","] ~ num_expression)* }
// org_dir = { [".org"] ~ num_expression }

// string = @{ ["\""] ~ ((["\\"] ~ any) | (!["\""] ~ any))* ~ ["\""] }
fn to_string<'a>(node: &Node<'a>) -> Vec<u8> {
    let bytes = node.text[1..(node.text.len() - 1)].as_bytes();
    let mut cleaned = Vec::new();
    let mut index = 0;
    loop {
        if index >= bytes.len() {
            break;
        }
        let current = bytes[index];
        if current == 0x5C {
            let next = bytes[index + 1];
            match next {
                // \a
                0x61 => cleaned.push(0x07),

                // \b
                0x62 => cleaned.push(0x08),

                // \f
                0x66 => cleaned.push(0x0C),

                // \n
                0x6E => cleaned.push(0x0A),

                // \r
                0x72 => cleaned.push(0x0D),

                // \t
                0x74 => cleaned.push(0x09),

                // \v
                0x76 => cleaned.push(0x0B),

                _ => cleaned.push(next)
            }
            index = index + 2;
        } else {
            cleaned.push(current);
            index = index + 1;
        }
    }
    cleaned
}

// fn to_directive<'a>(node: &Node<'a>) -> Result<Directive,EvaluationError> {
//     match node.rule {
//         Rule::include_dir => Directive::
//     }
// }

fn to_statement<'a>(node: &Node<'a>) -> Result<Statement<Expression,Expression,Expression,Expression,Expression>,EvaluationError> {
    let num_children = node.children.len();
    match node.rule {
        Rule::instruction => Ok(Statement::Instruction(to_instr(node)?)),
        Rule::label => Ok(Statement::Label(node.text.to_string())),
        // Rule::directive => match node.text.to_lowercase().as_ref() {
        //     ".byte" => Ok(Statement::Directive(Directive::Byte)),
        //     ".org" => Ok(Statement::Directive(Directive::Org))
        // },
        _ => panic!(format!("Failed to match rule {:?} in to_statement", node.rule))
    }
}

fn to_statements<'a>(node: &Node<'a>) -> Result<Vec<Statement<Expression,Expression,Expression,Expression,Expression>>,EvaluationError> {
    match node.rule {
        Rule::statements => {
            let mut statements = vec![];
            for n in node.children.iter() {
                statements.push(to_statement(&n)?);
            }
            Ok(statements)
        },
        _ => unreachable!()
    }
}

pub fn run_parser() {
    let text = "  ;;; cool stuff in comment  \n;stuff\nadd #($FF + 3 * 2)\n add #$45".to_string();
    // let text = "; fred is cool\nx".to_string();
    // let text = "add #$FF add #$FF".to_string();
    // let text = include_str!("../../example.s").trim().to_string() + "\n";
    let mut parser: Rdp<StringInput> = Rdp::new(StringInput::new(&text));

    // assert!(parser.instruction());
    assert!(parser.statements());
    // assert!(parser.comment());
    // assert!(parser.end());

    println!("queue: {:?}", parser.queue());

    let (idx, node) = to_tree(&text, 0, parser.queue());
    println!("tree: {:?}", to_statements(&node));

    // let result = parser.to_statements();
    // println!("result: {:?}", result);

    // assert_eq!(result, 44);
}

