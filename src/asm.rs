
extern crate unicode_segmentation;
extern crate unicode_categories;
extern crate regex;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate instruction_derive;

pub mod instruction;
pub mod parser;
pub mod ast;
pub mod expression;
pub mod input;
pub mod lexer;
pub mod location;
pub mod files;
mod env;

use ast::{Statements};
use expression::{EvaluationError};
use std::collections::HashMap;
use location::{Span, Positioned};
use env::{Env,Names};
use instruction::{EncodingError};

// pub fn assemble_file(filename: &str) -> Result<Vec<u8>,EvaluationError> {
//     let statements = parser::parse_file(filename)?;
//     assemble(&statements)
// }

pub fn assemble(statements: &Statements) -> Result<Vec<u8>,AssemblyError> {
    let (max_pos, names) = compute_names(statements)?;
    let mut output = vec![0; max_pos];
    generate_bytes(statements, &names, &mut output)?;
    Ok(output)
}

fn generate_bytes(statements: &Statements, names: &Names, output: &mut Vec<u8>) -> Result<(),AssemblyError> {
    let mut pos: usize = 0;
    let mut current_global = "".to_owned();
    for statement in statements.statements.iter() {
        println!("{} - {:?}", pos, statement);
        use ast::Statement::*;
        match statement {
            Directive(_, dir) => {
                use ast::Directive::*;
                match dir {
                    Byte(_, bytes) => {
                        let env = names.as_env("Name", &current_global);
                        for expr in bytes.iter() {
                            let b: u8 = (expr.eval(&env)? | 0xFF) as u8;
                            output[pos] = b;
                            pos = pos + 1;
                        }
                    },
                    ByteString(_, bytes) => {
                        for b in bytes.iter() {
                            output[pos] = *b;
                            pos = pos + 1;
                        }
                    },
                    Org(span, location) => {
                        if *location > 0xFFFF {
                            return Err(AssemblyError::InvalidCodeLocation(span.clone(), *location as i32))
                        }
                        pos = *location;
                    },
                    Word(_, words) => {
                        let env = names.as_env("Name", &current_global);
                        for expr in words.iter() {
                            let w: u16 = (expr.eval(&env)? | 0xFFFF) as u16;
                            output[pos] = (w & 0xFF) as u8;
                            pos = pos + 1;
                            output[pos] = ((w >> 8) & 0xFF) as u8;
                            pos = pos + 1;
                        }
                    },
                    Include(_, _) => panic!("There should be no .include directives left at this point".to_string()),
                    Cnop(_, _add, _multiple) => {
                        pos += dir.size(pos as i32)? as usize;
                    }
                }
            },
            Label(_, name) => {
                if !name.starts_with(".") {
                    current_global = name.clone();
                }
            },
            Instruction(_, instr) => {
                let next_pos = pos + instr.size();
                let bytes = instr.eval(next_pos, &current_global, &names)?.encode();
                for b in bytes.iter() {
                    output[pos] = *b;
                    pos = pos + 1;
                }
            },
            Variable(_, _, _) | Alias(_, _, _) => {}
        }
    }
    Ok(())
}

#[derive(Debug)]
pub enum AssemblyError {
    NameNotFound(Span,String),
    DivideByZero(Span,String),
    MustBeLiteralNumber(Span),
    NameAlreadyExists(Span,Span,String),
    InvalidCodeLocation(Span,i32),
    NumOutOfRange {
        span: Span,
        bits: usize,
        value: i32
    },
    SignedNumOutOfRange {
        span: Span,
        bits: usize,
        value: i32
    },
    InvalidAddress {
        span: Span,
        value: i32
    },
    AddrBitsDontMatch {
        span: Span,
        pos: usize,
        value: i32,
        pos_top: u8,
        value_top: u8
    }
}

impl From<EncodingError> for AssemblyError {
    fn from(error: EncodingError) -> Self {
        use instruction::EncodingError::*;
        match error {
            NumOutOfRange { span, bits, value } => AssemblyError::NumOutOfRange { span, bits, value },
            SignedNumOutOfRange { span, bits, value } => AssemblyError::SignedNumOutOfRange { span, bits, value },
            InvalidAddress { span, value } => AssemblyError::InvalidAddress { span, value },
            AddrBitsDontMatch { span, pos, value, pos_top, value_top } => AssemblyError::AddrBitsDontMatch { span, pos, value, pos_top, value_top },
            EvalError(eval) => AssemblyError::from(eval)
        }
    }
}

impl From<EvaluationError> for AssemblyError {
    fn from(error: EvaluationError) -> Self {
        use expression::EvaluationError::*;
        match error {
            NameNotFound(span, name) => AssemblyError::NameNotFound(span.clone(), name.clone()),
            DivideByZero(span, message) => AssemblyError::DivideByZero(span.clone(), message.clone()),
            MustBeLiteralNumber(span) => AssemblyError::MustBeLiteralNumber(span.clone())
        }
    }
}

fn add_name(
    name: String,
    value: NameValue,
    names: &mut HashMap<String,NameValue>
) -> Result<(), AssemblyError> {
    if names.contains_key(&name) {
        Err(AssemblyError::NameAlreadyExists(value.span.clone(), names[&name].span.clone(), name.clone()))
    } else {
        names.insert(name, value);
        Ok(())
    }
}

#[derive(Debug)]
struct NameValue {
    span: Span,
    value: i32
}

#[derive(Debug)]
struct NamesBuilder {
    globals: HashMap<String,NameValue>,
    locals: HashMap<String,HashMap<String,NameValue>>
}

fn compute_labels(statements: &Statements) -> Result<NamesBuilder,AssemblyError> {
    let mut globals = HashMap::new();
    let mut locals = HashMap::new();
    let mut local = HashMap::new();
    let mut current_global = "".to_owned();
    let mut pos: i32 = 0;
    let mut max_pos = 0;
    for statement in statements.statements.iter() {
        use ast::Statement::*;
        match statement {
            Directive(_, dir) => {
                pos += dir.size(pos)?
            },
            Label(_, name) => {
                if name.starts_with(".") {
                    let val = NameValue { span: statement.span(), value: pos };
                    add_name(name.to_lowercase(), val, &mut local)?;
                } else {
                    let val = NameValue { span: statement.span(), value: pos };
                    add_name(name.to_lowercase(), val, &mut globals)?;
                    locals.insert(current_global.clone(), local);
                    local = HashMap::new();
                    current_global = name.to_lowercase();
                }
            },
            Instruction(_, instr) => {
                pos += instr.size() as i32;
            },
            Variable(_, _name, _expr) | Alias(_, _name, _expr) => {}
        }
    }
    if !locals.contains_key(&current_global) {
        locals.insert(current_global.clone(), local);
    }
    Ok(NamesBuilder { globals, locals })
}

fn compute_names(statements: &Statements) -> Result<(usize, Names),AssemblyError> {
    let labels = compute_labels(statements)?;
    let mut globals = labels.globals;
    let locals = labels.locals;
    let mut pos: i32 = 0;
    let mut max_pos = 0;
    for statement in statements.statements.iter() {
        use ast::Statement::*;
        match statement {
            Directive(_, dir) => {
                pos += dir.size(pos)?
            },
            Label(_, name) => (),
            Instruction(_, instr) => {
                pos += instr.size() as i32;
            },
            Variable(_, name, expr) | Alias(_, name, expr) => {
                let val = {
                    let env = MapEnv::new("Name", &globals);
                    NameValue { span: statement.span(), value: expr.eval(&env)? }
                };
                add_name(name.to_lowercase(), val, &mut globals)?;
            }
        }
        if pos > max_pos {
            max_pos = pos;
        }
    }
    let mut new_globals = HashMap::new();
    for key in globals.keys() {
        new_globals.insert(key.to_owned(), globals[key].value);
    }
    let mut new_locals = HashMap::new();
    for key in locals.keys() {
        let mut new_local = HashMap::new();
        for lkey in locals[key].keys() {
            new_local.insert(lkey.to_owned(), locals[key][lkey].value);
        }
        new_locals.insert(key.to_owned(), new_local);
    }
    Ok(((max_pos + 1) as usize, Names { globals: new_globals, locals: new_locals }))
}

// Env stuff

pub struct MapEnv<'a,'b> {
    name: &'a str,
    map: &'b HashMap<String,NameValue>
}

impl<'a,'b> MapEnv<'a,'b> {
    fn new<'c,'d>(name: &'c str, map: &'d HashMap<String,NameValue>) -> MapEnv<'c,'d> {
        MapEnv { name: name, map: map }
    }
}

impl<'a,'b> Env<i32> for MapEnv<'a,'b> {
    fn name(&self) -> &str {
        self.name
    }

    fn get(&self, name: &str) -> Option<i32> {
        self.map.get(name).map(|v| v.value)
    }
}

struct LabelEnv<'a,'b,'c> {
    name: &'a str,
    globals: &'b HashMap<String,NameValue>,
    locals: &'c HashMap<String,NameValue>
}

impl<'a,'b,'c> LabelEnv<'a,'b,'c> {
    fn new<'d,'e,'f>(name: &'d str, globals: &'e HashMap<String,NameValue>, locals: &'f HashMap<String,NameValue>) -> LabelEnv<'d,'e,'f> {
        LabelEnv { name, globals, locals }
    }
}

impl<'a,'b,'c> Env<i32> for LabelEnv<'a,'b,'c> {
    fn name(&self) -> &str {
        self.name
    }

    fn get(&self, name: &str) -> Option<i32> {
        if name.starts_with(".") {
            self.locals.get(name).map(|v| v.value.clone())
        } else {
            self.globals.get(name).map(|v| v.value.clone())
        }
    }
}
