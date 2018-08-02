
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
use env::Env;

// pub fn assemble_file(filename: &str) -> Result<Vec<u8>,EvaluationError> {
//     let statements = parser::parse_file(filename)?;
//     assemble(&statements)
// }

// pub fn assemble(statements: &Statements) -> Result<Vec<u8>,EvaluationError> {
//     let (max_pos, names) = compute_names(statements)?;
//     let mut output = vec![0; max_pos];
//     generate_bytes(statements, &names, &mut output)?;
//     Ok(output)
// }

// fn generate_bytes(statements: &Statements, names: &HashMap<String,i32>, output: &mut Vec<u8>) -> Result<(),EvaluationError> {
//     let mut pos: usize = 0;
//     for statement in statements.statements.iter() {
//         println!("{} - {:?}", pos, statement);
//         match statement {
//             &Statement::Directive(ref dir) => {
//                 match dir {
//                     &Directive::Byte(ref bytes) => {
//                         for expr in bytes.iter() {
//                             let b: u8 = expr.to(names)?;
//                             output[pos] = b;
//                             pos = pos + 1;
//                         }
//                     },
//                     &Directive::ByteString(ref bytes) => {
//                         for b in bytes.iter() {
//                             output[pos] = *b;
//                             pos = pos + 1;
//                         }
//                     },
//                     &Directive::Org(ref location) => {
//                         pos = *location;
//                     },
//                     &Directive::Word(ref words) => {
//                         for expr in words.iter() {
//                             let w: u16 = expr.to(names)?;;
//                             output[pos] = (w & 0xFF) as u8;
//                             pos = pos + 1;
//                             output[pos] = ((w >> 8) & 0xFF) as u8;
//                             pos = pos + 1;
//                         }
//                     },
//                     &Directive::Include(_) => panic!("There should be no .include directives left at this point".to_string()),
//                     &Directive::Cnop(ref add, ref multiple) => {
//                         let add = add.eval(&names)? as usize;
//                         let multiple = multiple.eval(&names)? as usize;
//                         let mut mult = 0;
//                         loop {
//                             if pos < mult {
//                                 break;
//                             }
//                             mult = mult + multiple;
//                         }
//                         pos = mult + add;
//                     }
//                 }
//             },
//             &Statement::Label(_) => {},
//             &Statement::Instruction(ref instr) => {
//                 let next_pos = pos + instr.size();
//                 let bytes = instr.reduce(next_pos, names)?.to_bytes();
//                 for b in bytes.iter() {
//                     output[pos] = *b;
//                     pos = pos + 1;
//                 }
//             },
//             &Statement::Variable(_, _) => {},
//             &Statement::Alias(_, _) => {}
//         }
//     }
//     Ok(())
// }

#[derive(Debug)]
pub enum AssemblyError {
    NameNotFound(Span,String),
    DivideByZero(Span,String),
    MustBeLiteralNumber(Span),
    NameAlreadyExists(Span,Span,String)
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

fn add_name(name: String, value: NameValue, names: &mut HashMap<String,NameValue>) -> Result<(), AssemblyError> {
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
struct Names {
    globals: HashMap<String,NameValue>,
    locals: HashMap<String,HashMap<String,NameValue>>,
    variables: HashMap<String,NameValue>
}

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

fn compute_names(statements: &Statements) -> Result<Names,AssemblyError> {
    let mut globals = HashMap::new();
    let mut locals = HashMap::new();
    let mut local = HashMap::new();
    let mut variables = HashMap::new();
    let mut current_global = "".to_owned();
    let mut pos: i32 = 0;
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
            Variable(_, name, expr) | Alias(_, name, expr) => {
                let env = MapEnv::new("Variable/Alias name", &variables);
                let val = NameValue { span: statement.span(), value: expr.eval(&env)? };
                add_name(name.to_lowercase(), val, &mut local)?;
            }
        }
    }
    if !locals.contains_key(&current_global) {
        locals.insert(current_global.clone(), local);
    }
    Ok(Names { globals, locals, variables })
}

// fn compute_names(statements: &Statements) -> Result<(usize, HashMap<String,i32>),EvaluationError> {
//     let mut names = HashMap::new();
//     let mut max_pos: usize = 0;
//     let mut pos: i32 = 0;
//     for statement in statements.statements.iter() {
//         match statement {
//             &Statement::Directive(ref dir) => {
//                 match dir {
//                     &Directive::Byte(ref bytes) => pos = pos + (bytes.len() as i32),
//                     &Directive::ByteString(ref bytes) => pos = pos + (bytes.len() as i32),
//                     &Directive::Org(ref location) => pos = *location as i32,
//                     &Directive::Word(ref words) => pos = pos + ((words.len() * 2) as i32),
//                     &Directive::Include(_) => panic!("There should be no .include directives left at this point".to_string()),
//                     &Directive::Cnop(ref add, ref multiple) => {
//                         let add = add.eval(&names)?;
//                         let multiple = multiple.eval(&names)?;
//                         let mut mult = 0;
//                         loop {
//                             if pos < mult {
//                                 break;
//                             }
//                             mult = mult + multiple;
//                         }
//                         pos = mult + add;
//                     }
//                 }
//             },
//             &Statement::Label(ref name) => {
//                 add_name(pos, name.to_lowercase(), pos, &mut names)?;
//             },
//             &Statement::Instruction(ref instr) => {
//                 pos = pos + (instr.size() as i32);
//             },
//             &Statement::Variable(ref name, ref expr) => {
//                 let value = expr.eval(&names)?;
//                 add_name(pos, name.to_lowercase(), value, &mut names)?;
//             },
//             &Statement::Alias(ref name, ref expr) => {
//                 let value = expr.eval(&names)?;
//                 add_name(pos, name.to_lowercase(), value, &mut names)?;
//             }
//         }
//         if max_pos < (pos as usize) {
//             max_pos = pos as usize;
//         }
//     }
//     Ok((max_pos, names))
// }

