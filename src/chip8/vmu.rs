use std::{collections::{BTreeMap, BTreeSet}, sync::atomic::{AtomicI32, AtomicUsize}};

use crate::{ast::Statements, expression::Expr};

use super::ast::{Chip8Arg, Chip8Key, Chip8Register, Chip8Statement};

#[derive(Debug, Clone)]
pub enum Chip8ToVmuError {
    UnsupportedInsruction {
        line: usize,
        instruction: String,
    },
}

fn var_name(key: usize) -> String {
    format!("var_{key:03X}")
}

fn label_name(address: usize) -> String {
    format!("label_{address:04X}")
}

fn register_name(reg: Chip8Register) -> String {
    format!("REGISTER_{reg}")
}

fn key_name(reg: Chip8Key) -> String {
    format!("KEY_{reg}")
}

const REGISTER_I: &str = "REGISTER_I";
const REGISTER_DT: &str = "REGISTER_DT";
const REGISTER_ST: &str = "REGISTER_ST";

struct CurrentVar(i32);

impl CurrentVar {
    pub fn next(&mut self) -> i32 {
        let value = self.0;
        self.0 += 1;
        value
    }
}

pub fn to_vmu_assembly(instructions: &[Chip8Statement], start: usize) -> Result<Statements, Chip8ToVmuError> {
    let mut current_var = CurrentVar(0);
    let mut statements = Statements::empty();
    let ram_mapping = compute_ram_mapping(instructions);
    let labels_and_vars = labels_and_vars(instructions);

    for register in Chip8Register::values() {
        statements.push_var(&register_name(*register), Expr::num(current_var.next()));
    }

    statements.push_var(REGISTER_I, Expr::num(current_var.next()));
    statements.push_var(REGISTER_DT, Expr::num(current_var.next()));
    statements.push_var(REGISTER_ST, Expr::num(current_var.next()));

    let ram_start = current_var.0;

    for var in labels_and_vars.vars {
        statements.push_var(&var_name(var), Expr::num((ram_mapping[&var] as i32) + ram_start));
    }

    panic!("chip8 -> vmu is not yet implemented.");

    let mut index = 0;
    for (line, statement) in instructions.iter().enumerate() {
        match statement {
            Chip8Statement::Instruction(instr) => {
                match instr {
                    super::ast::Chip8Instruction::Sys(_) => {
                        return Err(Chip8ToVmuError::UnsupportedInsruction { line, instruction: instr.to_string() });
                    },
                    super::ast::Chip8Instruction::Cls => todo!(),
                    super::ast::Chip8Instruction::Ret => todo!(),
                    super::ast::Chip8Instruction::Jp(_, _) => todo!(),
                    super::ast::Chip8Instruction::Call(_) => todo!(),
                    super::ast::Chip8Instruction::Se(_, _) => todo!(),
                    super::ast::Chip8Instruction::Sne(_, _) => todo!(),
                    super::ast::Chip8Instruction::Ld_byte(_, _) => todo!(),
                    super::ast::Chip8Instruction::Add(_, _) => todo!(),
                    super::ast::Chip8Instruction::Ld_reg(_, _) => todo!(),
                    super::ast::Chip8Instruction::Or(_, _) => todo!(),
                    super::ast::Chip8Instruction::And(_, _) => todo!(),
                    super::ast::Chip8Instruction::Xor(_, _) => todo!(),
                    super::ast::Chip8Instruction::Sub(_, _) => todo!(),
                    super::ast::Chip8Instruction::Shr(_, _) => todo!(),
                    super::ast::Chip8Instruction::Subn(_, _) => todo!(),
                    super::ast::Chip8Instruction::Shl(_, _) => todo!(),
                    super::ast::Chip8Instruction::Ld_I(_) => todo!(),
                    super::ast::Chip8Instruction::Rnd(_, _) => todo!(),
                    super::ast::Chip8Instruction::Drw(_, _, _) => todo!(),
                    super::ast::Chip8Instruction::Skp(_) => todo!(),
                    super::ast::Chip8Instruction::Sknp(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_from_delay(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_key(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_to_delay(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_to_sound(_) => todo!(),
                    super::ast::Chip8Instruction::Add_I(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_font(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_bcd(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_store_reg(_) => todo!(),
                    super::ast::Chip8Instruction::Ld_read_reg(_) => todo!(),
                }
                index += 2;
            },
            Chip8Statement::Byte(byte) => {
                index += 1;
            },
        }
    }

    Ok(statements)
}

#[derive(Debug)]
struct LabelsAndVars {
    vars: BTreeSet<usize>,
    labels: BTreeSet<usize>,
}

fn labels_and_vars(instructions: &[Chip8Statement]) -> LabelsAndVars {
    let mut vars = BTreeSet::new();
    let mut labels = BTreeSet::new();

    let mut index = 0;
    for instruction in instructions {
        if let Chip8Statement::Instruction(instr) = instruction {
            match instr {
                super::ast::Chip8Instruction::Jp(_, addr) => {
                    labels.insert(addr.get());
                },
                super::ast::Chip8Instruction::Call(addr) => {
                    labels.insert(addr.get());
                },
                super::ast::Chip8Instruction::Skp(..) |
                super::ast::Chip8Instruction::Sknp(..) |
                super::ast::Chip8Instruction::Se(..) |
                super::ast::Chip8Instruction::Sne(..) => {
                    labels.insert(index + 2);
                },
                super::ast::Chip8Instruction::Ld_I(addr) => {
                    vars.insert(addr.get());
                },
                _ => {},
            }
            index += 2;
        } else {
            index += 1;
        }
    }

    LabelsAndVars { labels, vars }
}

fn compute_ram_mapping(instructions: &[Chip8Statement]) -> BTreeMap<usize, usize> {
    let mut mapping = BTreeMap::new();

    let mut index = 0;
    let mut new_index = 0;
    while index < instructions.len() {
        match &instructions[index] {
            Chip8Statement::Instruction(..) => {
                index += 2;
            },
            Chip8Statement::Byte(..) => {
                mapping.insert(index, new_index);
                index += 1;
                new_index += 1;
            },
        }
    }

    mapping
}
