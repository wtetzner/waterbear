
use pest::{StringInput, Token, Parser, Input};
use asm::ast::*;
use asm::instruction::*;
use asm::expression::*;
use std;
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;
use asm::num;

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
        statement = _{ directive | label | instruction }

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

        directive = { include_dir | byte_dir | byte_dir_str | word_dir | org_dir }
        include_dir = { [".include"] ~ string }
        byte_dir = { [".byte"] ~ num_expression ~ ([","] ~ num_expression)* }
        byte_dir_str = { [".byte"] ~ string }
        word_dir = { [".word"] ~ num_expression ~ ([","] ~ num_expression)* }
        org_dir = { [".org"] ~ num_expression }

        string = @{ ["\""] ~ ((["\\"] ~ any) | (!["\""] ~ any))* ~ ["\""] }

        label = { name ~ [":"] }

        name = @{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ["."]) ~ (['a'..'z'] | ['A'..'Z'] | ["$"] | ["_"] | ["."] | ['0'..'9'])* }

        instruction = @{ name ~ ws* ~ args? }
        args = @{ (arg ~ ws* ~ ([","] ~ ws* ~ arg ~ ws*)*)}

        arg = _{ imm8 | ri | mem }

        imm8 = { ["#"] ~ num_expression }
        ri = { ["@R0"] | ["@R1"] | ["@R2"] | ["@R3"] | ["@r0"] | ["@r1"] | ["@r2"] | ["@r3"]}
        mem = { num_expression }

        comment = _{ [";"] ~ (!newline ~ any)* }
        whitespace = _{ ws | newline  }
        newline = _{ ["\r\n"] | ["\n"] }
        ws = _{ [" "] | ["\t"] }
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
    let args = if node.children.len() > 1 { to_args(&node.children[1])? } else { Args::Empty };
    match (name.as_ref(), args) {
        ("add", Args::One(Arg::I8(i8))) => Ok(Instruction::Add_i8(i8)),
        ("add", Args::One(Arg::Mem(mem))) => Ok(Instruction::Add_d9(mem)),
        ("add", Args::One(Arg::Ri(ri))) => Ok(Instruction::Add_Ri(ri)),
        ("add", args) => Err(EvaluationError::Failure("Invalid add instruction".to_string())),

        ("addc", Args::One(Arg::I8(i8))) => Ok(Instruction::Addc_i8(i8)),
        ("addc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Addc_d9(mem)),
        ("addc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Addc_Ri(ri)),
        ("addc", args) => Err(EvaluationError::Failure("Invalid addc instruction".to_string())),

        ("sub", Args::One(Arg::I8(i8))) => Ok(Instruction::Sub_i8(i8)),
        ("sub", Args::One(Arg::Mem(mem))) => Ok(Instruction::Sub_d9(mem)),
        ("sub", Args::One(Arg::Ri(ri))) => Ok(Instruction::Sub_Ri(ri)),
        ("sub", args) => Err(EvaluationError::Failure("Invalid sub instruction".to_string())),

        ("subc", Args::One(Arg::I8(i8))) => Ok(Instruction::Subc_i8(i8)),
        ("subc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Subc_d9(mem)),
        ("subc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Subc_Ri(ri)),
        ("subc", args) => Err(EvaluationError::Failure("Invalid subc instruction".to_string())),

        ("inc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Inc_d9(mem)),
        ("inc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Inc_Ri(ri)),
        ("inc", args) => Err(EvaluationError::Failure("Invalid inc instruction".to_string())),

        ("dec", Args::One(Arg::Mem(mem))) => Ok(Instruction::Dec_d9(mem)),
        ("dec", Args::One(Arg::Ri(ri))) => Ok(Instruction::Dec_Ri(ri)),
        ("dec", args) => Err(EvaluationError::Failure("Invalid dec instruction".to_string())),

        ("mul", Args::Empty) => Ok(Instruction::Mul),
        ("mul", args) => Err(EvaluationError::Failure("Invalid mul instruction".to_string())),

        ("div", Args::Empty) => Ok(Instruction::Div),
        ("div", args) => Err(EvaluationError::Failure("Invalid div instruction".to_string())),

        ("and", Args::One(Arg::I8(i8))) => Ok(Instruction::And_i8(i8)),
        ("and", Args::One(Arg::Mem(mem))) => Ok(Instruction::And_d9(mem)),
        ("and", Args::One(Arg::Ri(ri))) => Ok(Instruction::And_Ri(ri)),
        ("and", args) => Err(EvaluationError::Failure("Invalid and instruction".to_string())),

        ("or", Args::One(Arg::I8(i8))) => Ok(Instruction::Or_i8(i8)),
        ("or", Args::One(Arg::Mem(mem))) => Ok(Instruction::Or_d9(mem)),
        ("or", Args::One(Arg::Ri(ri))) => Ok(Instruction::Or_Ri(ri)),
        ("or", args) => Err(EvaluationError::Failure("Invalid or instruction".to_string())),

        ("xor", Args::One(Arg::I8(i8))) => Ok(Instruction::Xor_i8(i8)),
        ("xor", Args::One(Arg::Mem(mem))) => Ok(Instruction::Xor_d9(mem)),
        ("xor", Args::One(Arg::Ri(ri))) => Ok(Instruction::Xor_Ri(ri)),
        ("xor", args) => Err(EvaluationError::Failure("Invalid xor instruction".to_string())),

        ("rol", Args::Empty) => Ok(Instruction::Rol),
        ("rol", args) => Err(EvaluationError::Failure("Invalid rol instruction".to_string())),

        ("rolc", Args::Empty) => Ok(Instruction::Rolc),
        ("rolc", args) => Err(EvaluationError::Failure("Invalid rolc instruction".to_string())),

        ("ror", Args::Empty) => Ok(Instruction::Ror),
        ("ror", args) => Err(EvaluationError::Failure("Invalid ror instruction".to_string())),

        ("rorc", Args::Empty) => Ok(Instruction::Rorc),
        ("rorc", args) => Err(EvaluationError::Failure("Invalid rorc instruction".to_string())),

        ("ld", Args::One(Arg::Mem(mem))) => Ok(Instruction::Ld_d9(mem)),
        ("ld", Args::One(Arg::Ri(ri))) => Ok(Instruction::Ld_Ri(ri)),
        ("ld", args) => Err(EvaluationError::Failure("Invalid ld instruction".to_string())),

        ("st", Args::One(Arg::Mem(mem))) => Ok(Instruction::St_d9(mem)),
        ("st", Args::One(Arg::Ri(ri))) => Ok(Instruction::St_Ri(ri)),
        ("st", args) => Err(EvaluationError::Failure("Invalid st instruction".to_string())),

        ("mov", Args::Two(Arg::I8(i8), Arg::Mem(mem))) => Ok(Instruction::Mov_d9(i8, mem)),
        ("mov", Args::Two(Arg::I8(i8), Arg::Ri(ri))) => Ok(Instruction::Mov_Rj(i8, ri)),
        ("mov", args) => Err(EvaluationError::Failure("Invalid mov instruction".to_string())),

        ("ldc", Args::Empty) => Ok(Instruction::Ldc),
        ("ldc", args) => Err(EvaluationError::Failure("Invalid ldc instruction".to_string())),

        ("push", Args::One(Arg::Mem(mem))) => Ok(Instruction::Push(mem)),
        ("push", args) => Err(EvaluationError::Failure("Invalid push instruction".to_string())),

        ("pop", Args::One(Arg::Mem(mem))) => Ok(Instruction::Pop(mem)),
        ("pop", args) => Err(EvaluationError::Failure("Invalid pop instruction".to_string())),

        ("xch", Args::One(Arg::Mem(mem))) => Ok(Instruction::Xch_d9(mem)),
        ("xch", Args::One(Arg::Ri(ri))) => Ok(Instruction::Xch_Ri(ri)),
        ("xch", args) => Err(EvaluationError::Failure("Invalid xch instruction".to_string())),

        ("jmp", Args::One(Arg::Mem(mem))) => Ok(Instruction::Jmp(mem)),
        ("jmp", args) => Err(EvaluationError::Failure("Invalid jmp instruction".to_string())),

        ("jmpf", Args::One(Arg::Mem(mem))) => Ok(Instruction::Jmpf(mem)),
        ("jmpf", args) => Err(EvaluationError::Failure("Invalid jmpf instruction".to_string())),

        ("br", Args::One(Arg::I8(i8))) => Ok(Instruction::Br(i8)),
        ("br", args) => Err(EvaluationError::Failure("Invalid br instruction".to_string())),

        ("brf", Args::One(Arg::Mem(mem))) => Ok(Instruction::Brf(mem)),
        ("brf", args) => Err(EvaluationError::Failure("Invalid brf instruction".to_string())),

        ("bz", Args::One(Arg::I8(i8))) => Ok(Instruction::Bz(i8)),
        ("bz", args) => Err(EvaluationError::Failure("Invalid bz instruction".to_string())),

        ("bnz", Args::One(Arg::I8(i8))) => Ok(Instruction::Bnz(i8)),
        ("bnz", args) => Err(EvaluationError::Failure("Invalid bnz instruction".to_string())),

        ("dbnz", Args::Two(Arg::Mem(mem), Arg::I8(i8))) => Ok(Instruction::Dbnz_d9(mem, i8)),
        ("dbnz", Args::Two(Arg::Ri(ri), Arg::I8(i8))) => Ok(Instruction::Dbnz_Ri(ri, i8)),
        ("dbnz", args) => Err(EvaluationError::Failure("Invalid dbnz instruction".to_string())),

        ("be", Args::Two(Arg::I8(i81), Arg::I8(i82))) => Ok(Instruction::Be_i8(i81, i82)),
        ("be", Args::Two(Arg::Mem(mem), Arg::I8(i8))) => Ok(Instruction::Be_d9(mem, i8)),
        ("be", Args::Three(Arg::Ri(ri), Arg::I8(i81), Arg::I8(i82))) => Ok(Instruction::Be_Rj(ri, i81, i82)),
        ("be", args) => Err(EvaluationError::Failure("Invalid be instruction".to_string())),

        ("bne", Args::Two(Arg::I8(i81), Arg::I8(i82))) => Ok(Instruction::Bne_i8(i81, i82)),
        ("bne", Args::Two(Arg::Mem(mem), Arg::I8(i8))) => Ok(Instruction::Bne_d9(mem, i8)),
        ("bne", Args::Three(Arg::Ri(ri), Arg::I8(i81), Arg::I8(i82))) => Ok(Instruction::Bne_Rj(ri, i81, i82)),
        ("bne", args) => Err(EvaluationError::Failure("Invalid bne instruction".to_string())),

        ("call", Args::One(Arg::Mem(mem))) => Ok(Instruction::Call(mem)),
        ("call", args) => Err(EvaluationError::Failure("Invalid call instruction".to_string())),

        ("callf", Args::One(Arg::Mem(mem))) => Ok(Instruction::Callf(mem)),
        ("callf", args) => Err(EvaluationError::Failure("Invalid callf instruction".to_string())),

        ("callr", Args::One(Arg::Mem(mem))) => Ok(Instruction::Callr(mem)),
        ("callr", args) => Err(EvaluationError::Failure("Invalid callr instruction".to_string())),

        ("ret", Args::Empty) => Ok(Instruction::Ret),
        ("ret", args) => Err(EvaluationError::Failure("Invalid ret instruction".to_string())),

        ("reti", Args::Empty) => Ok(Instruction::Reti),
        ("reti", args) => Err(EvaluationError::Failure("Invalid reti instruction".to_string())),

        ("clr1", Args::Two(Arg::Mem(d9), Arg::Mem(b3))) => Ok(Instruction::Clr1(d9, b3)),
        ("clr1", args) => Err(EvaluationError::Failure("Invalid clr1 instruction".to_string())),

        ("set1", Args::Two(Arg::Mem(d9), Arg::Mem(b3))) => Ok(Instruction::Set1(d9, b3)),
        ("set1", args) => Err(EvaluationError::Failure("Invalid set1 instruction".to_string())),

        ("not1", Args::Two(Arg::Mem(d9), Arg::Mem(b3))) => Ok(Instruction::Not1(d9, b3)),
        ("not1", args) => Err(EvaluationError::Failure("Invalid not1 instruction".to_string())),

        ("nop", Args::Empty) => Ok(Instruction::Nop),
        ("nop", args) => Err(EvaluationError::Failure("Invalid nop instruction".to_string())),

        (n,_) => Err(EvaluationError::Failure(format!("Invalid instruction: {}", n)))
    }
}

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

fn to_directive<'a>(node: &Node<'a>) -> Result<Directive,EvaluationError> {
    match node.rule {
        Rule::include_dir => Ok(Directive::Include(String::from_utf8(to_string(&node.children[0]))?)),
        Rule::byte_dir => {
            let mut bytes = Vec::with_capacity(node.children.len());
            for b in node.children.iter() {
                bytes.push(to_expr(&b)?);
            }
            Ok(Directive::Byte(bytes))
        },
        Rule::byte_dir_str => Ok(Directive::ByteString(to_string(&node.children[0]))),
        Rule::word_dir => {
            let mut bytes = Vec::with_capacity(node.children.len());
            for b in node.children.iter() {
                bytes.push(to_expr(&b)?);
            }
            Ok(Directive::Byte(bytes))
        },
        Rule::org_dir => {
            let expr = to_expr(&node.children[0])?;
            let value = expr.eval(&HashMap::new())?;
            Ok(Directive::Org(num::to_usize(value)?))
        },
        _ => unreachable!()
    }
}

fn to_statement<'a>(node: &Node<'a>) -> Result<Statement<Expression,Expression,Expression,Expression,Expression>,EvaluationError> {
    let num_children = node.children.len();
    match node.rule {
        Rule::instruction => Ok(Statement::Instruction(to_instr(node)?)),
        Rule::label => Ok(Statement::Label(node.text[..(node.text.len() - 1)].to_string())),
        Rule::directive => Ok(Statement::Directive(to_directive(&node.children[0])?)),
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

// Replace *'s at the beginning of a line with ;
fn replace_stars(text: &mut String) {
    let mut bytes = unsafe { text.as_mut_vec() };
    let mut index = 0;
    loop {
        if index >= bytes.len() {
            break;
        }
        if bytes[index] == 0x2A {
            if index == 0 || bytes[index - 1] == 0x0A {
                bytes[index] = 0x3B;
            }
        }
        index = index + 1;
    }
}

#[derive(Debug)]
pub enum FileLoadError {
    FileLoadFailure(std::io::Error),
    Utf8Error(std::string::FromUtf8Error)
}

impl std::convert::From<std::io::Error> for FileLoadError {
    fn from(error: std::io::Error) -> FileLoadError {
        FileLoadError::FileLoadFailure(error)
    }
}

impl std::convert::From<std::string::FromUtf8Error> for FileLoadError {
    fn from(error: std::string::FromUtf8Error) -> FileLoadError {
        FileLoadError::Utf8Error(error)
    }
}

pub fn load_file(path: &str) -> Result<String, FileLoadError> {
    let mut file = File::open(path)?;
    let mut bytes = vec![];
    file.read_to_end(&mut bytes)?;

    Ok(String::from_utf8(bytes)?)
}

pub fn run_parser(path: &str) -> Result<(), FileLoadError> {
    // let mut text = "  ;;; cool stuff in comment  \r\n* stuff\nadd #($FF + 3 * 2)\n add #$45".to_string();
    let mut text = load_file(path)?;
    replace_stars(&mut text);
    println!("text: {}", text);
    // let text = "; fred is cool\nx".to_string();
    // let text = "add #$FF add #$FF".to_string();
    // let text = include_str!("../../example.s").trim().to_string() + "\n";
    let mut parser: Rdp<StringInput> = Rdp::new(StringInput::new(&text));

    // assert!(parser.instruction());
    assert!(parser.statements());
    // assert!(parser.comment());
    // assert!(parser.end());

    println!("queue: {:?}", parser.queue());

    println!("to_tree");
    let (idx, node) = to_tree(&text, 0, parser.queue());
    println!("tree: {:?}", node);
    println!("results:\n{}", Statements { statements: to_statements(&node).expect("Failure") });

    // let result = parser.to_statements();
    // println!("result: {:?}", result);

    // assert_eq!(result, 44);
    Ok(())
}

