
// use pest::{StringInput, Token, Parser, Input};
// use ast::*;
// use instruction::*;
// use expression::*;
// use std;
// use std::fs::File;
// use std::io::Read;
// use std::collections::HashMap;
// use num;

use input::Input;
use location::{Location,Span};
use ast::{Statement,Statements};
use lexer::{Token,TokenType,LexerError};
use lexer;
use expression::Expr;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedChar(Location),
    ExpectedTokenNotFound(TokenType, Token),
    UnexpectedEof
}

impl From<LexerError> for ParseError {
    fn from(error: LexerError) -> Self {
        use lexer::LexerError::*;
        match error {
            UnexpectedChar(loc) => ParseError::UnexpectedChar(loc.clone())
        }
    }
}

// pub fn parse_input(input: &Input) -> Result<Statements,ParseError> {
//     let tokens = lexer::lex_input(input)?;
// }

// pub fn parse_statement(tokens: &[Token], pos: usize) -> Result<Statement,ParseError> {
    
// }

pub fn parse_expr<'a>(tokens: TokenStream<'a>) -> Result<(TokenStream<'a>,Expr),ParseError> {
    match tokens.peek() {
        Some(tok) => {
            use lexer::TokenType::*;
            match tok.token_type() {
                Number(num) => Ok((tokens.at(1), Expr::Number(tok.span().clone(), *num))),
                _ => Err(ParseError::ExpectedTokenNotFound(Number(0), tok.clone()))
            }
        },
        None => Err(ParseError::UnexpectedEof)
    }
}

pub fn parse_i8<'a>(tokens: TokenStream<'a>) -> Result<(TokenStream<'a>,Expr),ParseError> {
    tokens.assert(TokenType::Hash)?;
    let tokens = tokens.at(1);
    parse_expr(tokens)
}

pub struct TokenStream<'a> {
    pos: usize,
    tokens: &'a [Token]
}

impl<'a> TokenStream<'a> {
    pub fn from<'b>(tokens: &'b [Token]) -> TokenStream<'b> {
        TokenStream {
            pos: 0,
            tokens: tokens
        }
    }

    pub fn at(&self, amount: usize) -> TokenStream<'a> {
        TokenStream {
            pos: self.pos + amount,
            tokens: self.tokens
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        if self.pos < self.tokens.len() {
            Some(&self.tokens[self.pos])
        } else {
            None
        }
    }

    pub fn assert(&self, token_type: TokenType) -> Result<(),ParseError> {
        match self.peek() {
            Some(tok) if *tok.token_type() == token_type => Ok(()),
            Some(tok) => Err(ParseError::ExpectedTokenNotFound(token_type, tok.clone())),
            None => Err(ParseError::UnexpectedEof)
        }
    }
}

// #[derive(Debug,Clone)]
// enum Arg {
//     I8(Expression),
//     Ri(IndirectionMode),
//     Mem(Expression)
// }

// #[derive(Debug,Clone)]
// enum Args {
//     Empty,
//     One(Arg),
//     Two(Arg,Arg),
//     Three(Arg,Arg,Arg),
//     Many(Vec<Arg>)
// }

// impl Args {
//     fn printed(&self) -> String {
//         match self {
//             &Args::Empty => "".to_string(),
//             &Args::One(ref arg) => format!("[{:?}]", arg),
//             &Args::Two(ref arg1, ref arg2) => format!("[{:?}, {:?}]", arg1, arg2),
//             &Args::Three(ref arg1, ref arg2, ref arg3) => format!("[{:?}, {:?}, {:?}]", arg1, arg2, arg3),
//             &Args::Many(ref args) => format!("{:?}", args)
//         }
//     }
// }

// impl_rdp! {
//     grammar! {
//         statements = { whitespace* ~ statement* ~ whitespace* }
//         statement = _{ variable | alias | directive | label | instruction }

//         num_expression = _{
//             { ["("] ~ num_expression ~ [")"] | upper_byte | lower_byte | number | name }
//             addition       = { plus  | minus }
//             multiplication = { times | slash }
//         }
//         number = _{ bin_num | hex_num | oct_num | dec_num }
//         dec_num = @{ ["0"] | (['1'..'9'] ~ ['0'..'9']*) }
//         oct_num = @{ ["0"] ~ ['0'..'9']+ }
//         hex_num = _{ hex_num1 | hex_num2 }
//         hex_num1 = @{ ["$"] ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
//         hex_num2 = @{ ["0"] ~ (["X"] | ["x"]) ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
//         bin_num = @{ ["%"] ~ ['0'..'1']+ }
//         plus   =  { ["+"] }
//         minus  =  { ["-"] }
//         times  =  { ["*"] }
//         slash  =  { ["/"] }

//         upper_byte = { [">"] ~ num_expression }
//         lower_byte = { ["<"] ~ num_expression }

//         alias = { name ~ ["EQU"] ~ num_expression }
//         variable = { name ~ ["="] ~ num_expression }

//         directive = { include_dir | byte_dir | byte_dir_str | word_dir | org_dir | cnop_dir }
//         cnop_dir = @{ [".cnop"] ~ ws* ~ num_expression ~ ws* ~ [","] ~ ws* ~ num_expression }
//         include_dir = { [".include"] ~ string }
//         byte_dir = { [".byte"] ~ num_expression ~ ([","] ~ num_expression)* }
//         byte_dir_str = { [".byte"] ~ string }
//         word_dir = { [".word"] ~ num_expression ~ ([","] ~ num_expression)* }
//         org_dir = { [".org"] ~ num_expression }

//         string = @{ ["\""] ~ ((["\\"] ~ any) | (!["\""] ~ any))* ~ ["\""] }

//         label = { name ~ [":"] }

//         name = @{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ["."]) ~ (['a'..'z'] | ['A'..'Z'] | ["$"] | ["_"] | ["."] | ['0'..'9'])* }

//         instruction = @{ name ~ ws* ~ args? }
//         args = @{ (arg ~ ws* ~ ([","] ~ ws* ~ arg ~ ws*)*)}

//         arg = _{ imm8 | ri | mem }

//         imm8 = { ["#"] ~ num_expression }
//         ri = { ["@R0"] | ["@R1"] | ["@R2"] | ["@R3"] | ["@r0"] | ["@r1"] | ["@r2"] | ["@r3"]}
//         mem = { num_expression }

//         comment = _{ [";"] ~ (!newline ~ any)* }
//         whitespace = _{ ws | newline  }
//         newline = _{ ["\r\n"] | ["\n"] }
//         ws = _{ [" "] | ["\t"] }
//     }
// }

// #[derive(Debug)]
// pub struct Node<'a> {
//     rule: Rule,
//     start: usize,
//     end: usize,
//     text: &'a str,
//     children: Vec<Node<'a>>
// }

// fn to_tree<'a, 'b>(input: &'a str, index: usize, tokens: &'b Vec<Token<Rule>>) -> (usize, Node<'a>) {
//     let current = tokens[index];
//     let mut children = vec![];
//     let mut index = index + 1;

//     loop {
//         if index >= tokens.len() {
//             break;
//         }
//         let token = tokens[index];
//         if token.start >= current.start && token.start < current.end {
//             let (new_idx, node) = to_tree(input, index, tokens);
//             children.push(node);
//             index = new_idx;
//         } else {
//             break;
//         }
//     }

//     (index, Node {
//         rule: current.rule,
//         start: current.start,
//         end: current.end,
//         text: &input[current.start..current.end],
//         children: children
//     })
// }

// fn to_expr<'a>(node: &Node<'a>) -> Result<Expression,EvaluationError> {
//     match node.rule {
//         Rule::dec_num => Ok(Expression::Number(node.text.parse::<i32>()?)),
//         Rule::oct_num => Ok(Expression::Number(i32::from_str_radix(node.text, 8)?)),
//         Rule::hex_num1 => Ok(Expression::Number(i32::from_str_radix(&node.text[1..], 16)?)),
//         Rule::hex_num2 => Ok(Expression::Number(i32::from_str_radix(&node.text[2..], 16)?)),
//         Rule::bin_num => Ok(Expression::Number(i32::from_str_radix(&node.text[1..], 2)?)),
//         Rule::upper_byte => Ok(Expression::UpperByte(Box::new(to_expr(&node.children[0])?))),
//         Rule::lower_byte => Ok(Expression::LowerByte(Box::new(to_expr(&node.children[0])?))),
//         Rule::name => Ok(Expression::Name(node.text.to_string())),
//         Rule::addition => {
//             let left = to_expr(&node.children[0])?;
//             let right = to_expr(&node.children[2])?;
//             match node.children[1].rule {
//                 Rule::plus => Ok(Expression::Plus(Box::new(left), Box::new(right))),
//                 Rule::minus => Ok(Expression::Minus(Box::new(left), Box::new(right))),
//                 _ => unreachable!()
//             }
//         },
//         Rule::multiplication => {
//             let left = to_expr(&node.children[0])?;
//             let right = to_expr(&node.children[2])?;
//             match node.children[1].rule {
//                 Rule::times => Ok(Expression::Times(Box::new(left), Box::new(right))),
//                 Rule::slash => Ok(Expression::Divide(Box::new(left), Box::new(right))),
//                 _ => unreachable!()
//             }
//         },
//         _ => unreachable!()
//     }
// }

// fn to_arg<'a>(node: &Node<'a>) -> Result<Arg,EvaluationError> {
//     match node.rule {
//         Rule::imm8 => Ok(Arg::I8(to_expr(&node.children[0])?)),
//         Rule::ri => {
//             match node.text.to_lowercase().as_ref() {
//                 "@r0" => Ok(Arg::Ri(IndirectionMode::R0)),
//                 "@r1" => Ok(Arg::Ri(IndirectionMode::R1)),
//                 "@r2" => Ok(Arg::Ri(IndirectionMode::R2)),
//                 "@r3" => Ok(Arg::Ri(IndirectionMode::R3)),
//                 _ => unreachable!()
//             }
//         },
//         Rule::mem => Ok(Arg::Mem(to_expr(&node.children[0])?)),
//         _ => unreachable!()
//     }
// }

// fn to_args<'a>(node: &Node<'a>) -> Result<Args,EvaluationError> {
//     let mut results = vec![];
//     for n in node.children.iter() {
//         results.push(to_arg(&n)?);
//     }
//     match results.len() {
//         0 => Ok(Args::Empty),
//         1 => Ok(Args::One(results[0].clone())),
//         2 => Ok(Args::Two(results[0].clone(), results[1].clone())),
//         3 => Ok(Args::Three(results[0].clone(), results[1].clone(), results[2].clone())),
//         _ => Ok(Args::Many(results))
//     }
// }

// fn to_instr<'a>(node: &Node<'a>) -> Result<Instruction<Expression,Expression,Expression,Expression,Expression>,EvaluationError> {
//     let name = node.children[0].text.to_lowercase();
//     let args = if node.children.len() > 1 { to_args(&node.children[1])? } else { Args::Empty };
//     match (name.as_ref(), args) {
//         ("add", Args::One(Arg::I8(i8))) => Ok(Instruction::Add_i8(i8)),
//         ("add", Args::One(Arg::Mem(mem))) => Ok(Instruction::Add_d9(mem)),
//         ("add", Args::One(Arg::Ri(ri))) => Ok(Instruction::Add_Ri(ri)),
//         ("add", args) => Err(EvaluationError::Failure(format!("Invalid add instruction; args: {}", args.printed()))),

//         ("addc", Args::One(Arg::I8(i8))) => Ok(Instruction::Addc_i8(i8)),
//         ("addc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Addc_d9(mem)),
//         ("addc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Addc_Ri(ri)),
//         ("addc", args) => Err(EvaluationError::Failure(format!("Invalid addc instruction; args: {}", args.printed()))),

//         ("sub", Args::One(Arg::I8(i8))) => Ok(Instruction::Sub_i8(i8)),
//         ("sub", Args::One(Arg::Mem(mem))) => Ok(Instruction::Sub_d9(mem)),
//         ("sub", Args::One(Arg::Ri(ri))) => Ok(Instruction::Sub_Ri(ri)),
//         ("sub", args) => Err(EvaluationError::Failure(format!("Invalid sub instruction; args: {}", args.printed()))),

//         ("subc", Args::One(Arg::I8(i8))) => Ok(Instruction::Subc_i8(i8)),
//         ("subc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Subc_d9(mem)),
//         ("subc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Subc_Ri(ri)),
//         ("subc", args) => Err(EvaluationError::Failure(format!("Invalid subc instruction; args: {}", args.printed()))),

//         ("inc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Inc_d9(mem)),
//         ("inc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Inc_Ri(ri)),
//         ("inc", args) => Err(EvaluationError::Failure(format!("Invalid inc instruction; args: {}", args.printed()))),

//         ("dec", Args::One(Arg::Mem(mem))) => Ok(Instruction::Dec_d9(mem)),
//         ("dec", Args::One(Arg::Ri(ri))) => Ok(Instruction::Dec_Ri(ri)),
//         ("dec", args) => Err(EvaluationError::Failure(format!("Invalid dec instruction; args: {}", args.printed()))),

//         ("mul", Args::Empty) => Ok(Instruction::Mul),
//         ("mul", args) => Err(EvaluationError::Failure(format!("Invalid mul instruction; args: {}", args.printed()))),

//         ("div", Args::Empty) => Ok(Instruction::Div),
//         ("div", args) => Err(EvaluationError::Failure(format!("Invalid div instruction; args: {}", args.printed()))),

//         ("and", Args::One(Arg::I8(i8))) => Ok(Instruction::And_i8(i8)),
//         ("and", Args::One(Arg::Mem(mem))) => Ok(Instruction::And_d9(mem)),
//         ("and", Args::One(Arg::Ri(ri))) => Ok(Instruction::And_Ri(ri)),
//         ("and", args) => Err(EvaluationError::Failure(format!("Invalid and instruction; args: {}", args.printed()))),

//         ("or", Args::One(Arg::I8(i8))) => Ok(Instruction::Or_i8(i8)),
//         ("or", Args::One(Arg::Mem(mem))) => Ok(Instruction::Or_d9(mem)),
//         ("or", Args::One(Arg::Ri(ri))) => Ok(Instruction::Or_Ri(ri)),
//         ("or", args) => Err(EvaluationError::Failure(format!("Invalid or instruction; args: {}", args.printed()))),

//         ("xor", Args::One(Arg::I8(i8))) => Ok(Instruction::Xor_i8(i8)),
//         ("xor", Args::One(Arg::Mem(mem))) => Ok(Instruction::Xor_d9(mem)),
//         ("xor", Args::One(Arg::Ri(ri))) => Ok(Instruction::Xor_Ri(ri)),
//         ("xor", args) => Err(EvaluationError::Failure(format!("Invalid xor instruction; args: {}", args.printed()))),

//         ("rol", Args::Empty) => Ok(Instruction::Rol),
//         ("rol", args) => Err(EvaluationError::Failure(format!("Invalid rol instruction; args: {}", args.printed()))),

//         ("rolc", Args::Empty) => Ok(Instruction::Rolc),
//         ("rolc", args) => Err(EvaluationError::Failure(format!("Invalid rolc instruction; args: {}", args.printed()))),

//         ("ror", Args::Empty) => Ok(Instruction::Ror),
//         ("ror", args) => Err(EvaluationError::Failure(format!("Invalid ror instruction; args: {}", args.printed()))),

//         ("rorc", Args::Empty) => Ok(Instruction::Rorc),
//         ("rorc", args) => Err(EvaluationError::Failure(format!("Invalid rorc instruction; args: {}", args.printed()))),

//         ("ld", Args::One(Arg::Mem(mem))) => Ok(Instruction::Ld_d9(mem)),
//         ("ld", Args::One(Arg::Ri(ri))) => Ok(Instruction::Ld_Ri(ri)),
//         ("ld", args) => Err(EvaluationError::Failure(format!("Invalid ld instruction; args: {}", args.printed()))),

//         ("st", Args::One(Arg::Mem(mem))) => Ok(Instruction::St_d9(mem)),
//         ("st", Args::One(Arg::Ri(ri))) => Ok(Instruction::St_Ri(ri)),
//         ("st", args) => Err(EvaluationError::Failure(format!("Invalid st instruction; args: {}", args.printed()))),

//         ("mov", Args::Two(Arg::I8(i8), Arg::Mem(mem))) => Ok(Instruction::Mov_d9(i8, mem)),
//         ("mov", Args::Two(Arg::I8(i8), Arg::Ri(ri))) => Ok(Instruction::Mov_Rj(i8, ri)),
//         ("mov", args) => Err(EvaluationError::Failure(format!("Invalid mov instruction; args: {}", args.printed()))),

//         ("ldc", Args::Empty) => Ok(Instruction::Ldc),
//         ("ldc", args) => Err(EvaluationError::Failure(format!("Invalid ldc instruction; args: {}", args.printed()))),

//         ("push", Args::One(Arg::Mem(mem))) => Ok(Instruction::Push(mem)),
//         ("push", args) => Err(EvaluationError::Failure(format!("Invalid push instruction; args: {}", args.printed()))),

//         ("pop", Args::One(Arg::Mem(mem))) => Ok(Instruction::Pop(mem)),
//         ("pop", args) => Err(EvaluationError::Failure(format!("Invalid pop instruction; args: {}", args.printed()))),

//         ("xch", Args::One(Arg::Mem(mem))) => Ok(Instruction::Xch_d9(mem)),
//         ("xch", Args::One(Arg::Ri(ri))) => Ok(Instruction::Xch_Ri(ri)),
//         ("xch", args) => Err(EvaluationError::Failure(format!("Invalid xch instruction; args: {}", args.printed()))),

//         ("jmp", Args::One(Arg::Mem(mem))) => Ok(Instruction::Jmp(mem)),
//         ("jmp", args) => Err(EvaluationError::Failure(format!("Invalid jmp instruction; args: {}", args.printed()))),

//         ("jmpf", Args::One(Arg::Mem(mem))) => Ok(Instruction::Jmpf(mem)),
//         ("jmpf", args) => Err(EvaluationError::Failure(format!("Invalid jmpf instruction; args: {}", args.printed()))),

//         ("br", Args::One(Arg::Mem(i8))) => Ok(Instruction::Br(i8)),
//         ("br", args) => Err(EvaluationError::Failure(format!("Invalid br instruction; args: {}", args.printed()))),

//         ("brf", Args::One(Arg::Mem(mem))) => Ok(Instruction::Brf(mem)),
//         ("brf", args) => Err(EvaluationError::Failure(format!("Invalid brf instruction; args: {}", args.printed()))),

//         ("bz", Args::One(Arg::Mem(i8))) => Ok(Instruction::Bz(i8)),
//         ("bz", args) => Err(EvaluationError::Failure(format!("Invalid bz instruction; args: {}", args.printed()))),

//         ("bnz", Args::One(Arg::Mem(i8))) => Ok(Instruction::Bnz(i8)),
//         ("bnz", args) => Err(EvaluationError::Failure(format!("Invalid bnz instruction; args: {}", args.printed()))),

//         ("bp", Args::Three(Arg::Mem(d9), Arg::Mem(b3), Arg::Mem(r8))) => Ok(Instruction::Bp(d9, b3, r8)),
//         ("bp", args) => Err(EvaluationError::Failure(format!("Invalid bp instruction; args: {}", args.printed()))),

//         ("bpc", Args::Three(Arg::Mem(d9), Arg::Mem(b3), Arg::Mem(r8))) => Ok(Instruction::Bpc(d9, b3, r8)),
//         ("bpc", args) => Err(EvaluationError::Failure(format!("Invalid bpc instruction; args: {}", args.printed()))),

//         ("bn", Args::Three(Arg::Mem(d9), Arg::Mem(b3), Arg::Mem(r8))) => Ok(Instruction::Bn(d9, b3, r8)),
//         ("bn", args) => Err(EvaluationError::Failure(format!("Invalid bn instruction; args: {}", args.printed()))),

//         ("dbnz", Args::Two(Arg::Mem(mem), Arg::Mem(i8))) => Ok(Instruction::Dbnz_d9(mem, i8)),
//         ("dbnz", Args::Two(Arg::Ri(ri), Arg::Mem(i8))) => Ok(Instruction::Dbnz_Ri(ri, i8)),
//         ("dbnz", args) => Err(EvaluationError::Failure(format!("Invalid dbnz instruction; args: {}", args.printed()))),

//         ("be", Args::Two(Arg::I8(i81), Arg::Mem(i82))) => Ok(Instruction::Be_i8(i81, i82)),
//         ("be", Args::Two(Arg::Mem(mem), Arg::Mem(i8))) => Ok(Instruction::Be_d9(mem, i8)),
//         ("be", Args::Three(Arg::Ri(ri), Arg::I8(i81), Arg::Mem(i82))) => Ok(Instruction::Be_Rj(ri, i81, i82)),
//         ("be", args) => Err(EvaluationError::Failure(format!("Invalid be instruction; args: {}", args.printed()))),

//         ("bne", Args::Two(Arg::I8(i81), Arg::Mem(i82))) => Ok(Instruction::Bne_i8(i81, i82)),
//         ("bne", Args::Two(Arg::Mem(mem), Arg::Mem(i8))) => Ok(Instruction::Bne_d9(mem, i8)),
//         ("bne", Args::Three(Arg::Ri(ri), Arg::I8(i81), Arg::Mem(i82))) => Ok(Instruction::Bne_Rj(ri, i81, i82)),
//         ("bne", args) => Err(EvaluationError::Failure(format!("Invalid bne instruction; args: {}", args.printed()))),

//         ("call", Args::One(Arg::Mem(mem))) => Ok(Instruction::Call(mem)),
//         ("call", args) => Err(EvaluationError::Failure(format!("Invalid call instruction; args: {}", args.printed()))),

//         ("callf", Args::One(Arg::Mem(mem))) => Ok(Instruction::Callf(mem)),
//         ("callf", args) => Err(EvaluationError::Failure(format!("Invalid callf instruction; args: {}", args.printed()))),

//         ("callr", Args::One(Arg::Mem(mem))) => Ok(Instruction::Callr(mem)),
//         ("callr", args) => Err(EvaluationError::Failure(format!("Invalid callr instruction; args: {}", args.printed()))),

//         ("ret", Args::Empty) => Ok(Instruction::Ret),
//         ("ret", args) => Err(EvaluationError::Failure(format!("Invalid ret instruction; args: {}", args.printed()))),

//         ("reti", Args::Empty) => Ok(Instruction::Reti),
//         ("reti", args) => Err(EvaluationError::Failure(format!("Invalid reti instruction; args: {}", args.printed()))),

//         ("clr1", Args::Two(Arg::Mem(d9), Arg::Mem(b3))) => Ok(Instruction::Clr1(d9, b3)),
//         ("clr1", args) => Err(EvaluationError::Failure(format!("Invalid clr1 instruction; args: {}", args.printed()))),

//         ("set1", Args::Two(Arg::Mem(d9), Arg::Mem(b3))) => Ok(Instruction::Set1(d9, b3)),
//         ("set1", args) => Err(EvaluationError::Failure(format!("Invalid set1 instruction; args: {}", args.printed()))),

//         ("not1", Args::Two(Arg::Mem(d9), Arg::Mem(b3))) => Ok(Instruction::Not1(d9, b3)),
//         ("not1", args) => Err(EvaluationError::Failure(format!("Invalid not1 instruction; args: {}", args.printed()))),

//         ("nop", Args::Empty) => Ok(Instruction::Nop),
//         ("nop", args) => Err(EvaluationError::Failure(format!("Invalid nop instruction; args: {}", args.printed()))),

//         (n,_) => Err(EvaluationError::Failure(format!("Invalid instruction: {}", n)))
//     }
// }

// fn to_string<'a>(node: &Node<'a>) -> Vec<u8> {
//     let bytes = node.text[1..(node.text.len() - 1)].as_bytes();
//     let mut cleaned = Vec::new();
//     let mut index = 0;
//     loop {
//         if index >= bytes.len() {
//             break;
//         }
//         let current = bytes[index];
//         if current == 0x5C {
//             let next = bytes[index + 1];
//             match next {
//                 // \a
//                 0x61 => cleaned.push(0x07),

//                 // \b
//                 0x62 => cleaned.push(0x08),

//                 // \f
//                 0x66 => cleaned.push(0x0C),

//                 // \n
//                 0x6E => cleaned.push(0x0A),

//                 // \r
//                 0x72 => cleaned.push(0x0D),

//                 // \t
//                 0x74 => cleaned.push(0x09),

//                 // \v
//                 0x76 => cleaned.push(0x0B),

//                 _ => cleaned.push(next)
//             }
//             index = index + 2;
//         } else {
//             cleaned.push(current);
//             index = index + 1;
//         }
//     }
//     cleaned
// }

// fn to_directive<'a>(node: &Node<'a>) -> Result<Directive,EvaluationError> {
//     match node.rule {
//         Rule::include_dir => Ok(Directive::Include(String::from_utf8(to_string(&node.children[0]))?)),
//         Rule::byte_dir => {
//             let mut bytes = Vec::with_capacity(node.children.len());
//             for b in node.children.iter() {
//                 bytes.push(to_expr(&b)?);
//             }
//             Ok(Directive::Byte(bytes))
//         },
//         Rule::byte_dir_str => Ok(Directive::ByteString(to_string(&node.children[0]))),
//         Rule::word_dir => {
//             let mut bytes = Vec::with_capacity(node.children.len());
//             for b in node.children.iter() {
//                 bytes.push(to_expr(&b)?);
//             }
//             Ok(Directive::Word(bytes))
//         },
//         Rule::org_dir => {
//             let expr = to_expr(&node.children[0])?;
//             let value = expr.eval(&HashMap::new())?;
//             Ok(Directive::Org(num::to_usize(value)?))
//         },
//         Rule::cnop_dir => {
//             let first = to_expr(&node.children[0])?;
//             let second = to_expr(&node.children[1])?;
//             Ok(Directive::Cnop(first, second))
//         }
//         _ => panic!(format!("Unknown directive: {}", node.text))
//     }
// }

// fn to_statement<'a>(node: &Node<'a>) -> Result<Statement<Expression,Expression,Expression,Expression,Expression>,EvaluationError> {
//     match node.rule {
//         Rule::instruction => Ok(Statement::Instruction(to_instr(node)?)),
//         Rule::label => Ok(Statement::Label(node.text[..(node.text.len() - 1)].to_string())),
//         Rule::directive => Ok(Statement::Directive(to_directive(&node.children[0])?)),
//         Rule::variable => Ok(Statement::Variable(node.children[0].text.to_string(), to_expr(&node.children[1])?)),
//         Rule::alias => Ok(Statement::Alias(node.children[0].text.to_string(), to_expr(&node.children[1])?)),
//         _ => panic!(format!("Failed to match rule {:?} in to_statement", node.rule))
//     }
// }

// fn to_statements<'a>(node: &Node<'a>) -> Result<Vec<Statement<Expression,Expression,Expression,Expression,Expression>>,EvaluationError> {
//     match node.rule {
//         Rule::statements => {
//             let mut statements = vec![];
//             for n in node.children.iter() {
//                 statements.push(to_statement(&n)?);
//             }
//             Ok(statements)
//         },
//         _ => unreachable!()
//     }
// }

// // Replace *'s at the beginning of a line with ;
// fn replace_stars(text: &mut String) {
//     let mut bytes = unsafe { text.as_mut_vec() };
//     let mut index = 0;
//     loop {
//         if index >= bytes.len() {
//             break;
//         }
//         if bytes[index] == 0x2A {
//             if index == 0 || bytes[index - 1] == 0x0A {
//                 bytes[index] = 0x3B;
//             }
//         }
//         index = index + 1;
//     }
// }

// #[derive(Debug)]
// pub enum FileLoadError {
//     FileLoadFailure(std::io::Error),
//     Utf8Error(std::string::FromUtf8Error)
// }

// impl FileLoadError {
//     pub fn to_string(&self) -> String {
//         match self {
//             &FileLoadError::FileLoadFailure(ref err) => format!("Failed to load file: {}", err),
//             &FileLoadError::Utf8Error(ref err) => format!("{}", err)
//         }
//     }
// }

// impl std::convert::From<std::io::Error> for FileLoadError {
//     fn from(error: std::io::Error) -> FileLoadError {
//         FileLoadError::FileLoadFailure(error)
//     }
// }

// impl std::convert::From<std::string::FromUtf8Error> for FileLoadError {
//     fn from(error: std::string::FromUtf8Error) -> FileLoadError {
//         FileLoadError::Utf8Error(error)
//     }
// }

// pub fn load_file(path: &str) -> Result<String, FileLoadError> {
//     let mut file = File::open(path)?;
//     let mut bytes = vec![];
//     file.read_to_end(&mut bytes)?;

//     Ok(String::from_utf8(bytes)?)
// }

// pub fn parse_file(path: &str) -> Result<Statements, EvaluationError> {
//     let mut text = load_file(path)?;
//     replace_stars(&mut text);
//     let mut parser: Rdp<StringInput> = Rdp::new(StringInput::new(&text));
//     assert!(parser.statements());
//     // assert!(parser.end());
//     let (_, node) = to_tree(&text, 0, parser.queue());
//     Ok(Statements { statements: to_statements(&node)? })
// }


