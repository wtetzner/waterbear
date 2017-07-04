
pub mod instruction;
pub mod parser;
pub mod ast;
pub mod expression;
pub mod num;

use asm::ast::{Statements,Statement,Directive};
use asm::expression::EvaluationError;
use instruction::ToVal;
use std::collections::HashMap;

pub fn assemble_file(filename: &str) -> Result<Vec<u8>,EvaluationError> {
    let statements = parser::parse_file(filename)?;
    assemble(&statements)
}

pub fn assemble(statements: &Statements) -> Result<Vec<u8>,EvaluationError> {
    let (max_pos, names) = compute_names(statements)?;
    let mut output = vec![0; max_pos];
    generate_bytes(statements, &names, &mut output)?;
    Ok(output)
}

fn generate_bytes(statements: &Statements, names: &HashMap<String,i32>, output: &mut Vec<u8>) -> Result<(),EvaluationError> {
    let mut pos: usize = 0;
    for statement in statements.statements.iter() {
        println!("{} - {:?}", pos, statement);
        match statement {
            &Statement::Directive(ref dir) => {
                match dir {
                    &Directive::Byte(ref bytes) => {
                        for expr in bytes.iter() {
                            let b: u8 = expr.to(names)?;
                            output[pos] = b;
                            pos = pos + 1;
                        }
                    },
                    &Directive::ByteString(ref bytes) => {
                        for b in bytes.iter() {
                            output[pos] = *b;
                            pos = pos + 1;
                        }
                    },
                    &Directive::Org(ref location) => {
                        pos = *location;
                    },
                    &Directive::Word(ref words) => {
                        for expr in words.iter() {
                            let w: u16 = expr.to(names)?;;
                            output[pos] = (w & 0xFF) as u8;
                            pos = pos + 1;
                            output[pos] = ((w >> 8) & 0xFF) as u8;
                            pos = pos + 1;
                        }
                    },
                    &Directive::Include(_) => panic!("There should be no .include directives left at this point".to_string()),
                    &Directive::Cnop(ref add, ref multiple) => {
                        let add = add.eval(&names)? as usize;
                        let multiple = multiple.eval(&names)? as usize;
                        let mut mult = 0;
                        loop {
                            if pos < mult {
                                break;
                            }
                            mult = mult + multiple;
                        }
                        pos = mult + add;
                    }
                }
            },
            &Statement::Label(_) => {},
            &Statement::Instruction(ref instr) => {
                let next_pos = pos + instr.size();
                let bytes = instr.reduce(next_pos, names)?.to_bytes();
                for b in bytes.iter() {
                    output[pos] = *b;
                    pos = pos + 1;
                }
            },
            &Statement::Variable(_, _) => {},
            &Statement::Alias(_, _) => {}
        }
    }
    Ok(())
}

fn add_name(pos: i32, name: String, value: i32, names: &mut HashMap<String,i32>) -> Result<(), EvaluationError> {
    if names.contains_key(&name) {
        Err(EvaluationError::Failure(format!("{} - Name {} already exists at {}", pos, name, names.get(&name).unwrap())))
    } else {
        names.insert(name, value);
        Ok(())
    }
}

fn compute_names(statements: &Statements) -> Result<(usize, HashMap<String,i32>),EvaluationError> {
    let mut names = HashMap::new();
    let mut max_pos: usize = 0;
    let mut pos: i32 = 0;
    for statement in statements.statements.iter() {
        match statement {
            &Statement::Directive(ref dir) => {
                match dir {
                    &Directive::Byte(ref bytes) => pos = pos + (bytes.len() as i32),
                    &Directive::ByteString(ref bytes) => pos = pos + (bytes.len() as i32),
                    &Directive::Org(ref location) => pos = *location as i32,
                    &Directive::Word(ref words) => pos = pos + ((words.len() * 2) as i32),
                    &Directive::Include(_) => panic!("There should be no .include directives left at this point".to_string()),
                    &Directive::Cnop(ref add, ref multiple) => {
                        let add = add.eval(&names)?;
                        let multiple = multiple.eval(&names)?;
                        let mut mult = 0;
                        loop {
                            if pos < mult {
                                break;
                            }
                            mult = mult + multiple;
                        }
                        pos = mult + add;
                    }
                }
            },
            &Statement::Label(ref name) => {
                add_name(pos, name.to_lowercase(), pos, &mut names)?;
            },
            &Statement::Instruction(ref instr) => {
                pos = pos + (instr.size() as i32);
            },
            &Statement::Variable(ref name, ref expr) => {
                let value = expr.eval(&names)?;
                add_name(pos, name.to_lowercase(), value, &mut names)?;
            },
            &Statement::Alias(ref name, ref expr) => {
                let value = expr.eval(&names)?;
                add_name(pos, name.to_lowercase(), value, &mut names)?;
            }
        }
        if max_pos < (pos as usize) {
            max_pos = pos as usize;
        }
    }
    Ok((max_pos, names))
}

