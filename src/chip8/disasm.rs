use std::collections::{BTreeMap, BTreeSet};

use super::ast::{Chip8DecodeError, Chip8Instruction, Chip8Statement};

#[derive(Debug, Clone)]
pub enum Chip8DisassemblyError {
    DecodeError(Chip8DecodeError),
    AddressOutOfBounds(usize),
}

pub fn disassemble(bytes: &[u8], start: usize) -> Result<Vec<Chip8Statement>, Chip8DisassemblyError> {
    let mut decoded = BTreeMap::new();
    let mut to_visit = BTreeSet::new();
    to_visit.insert(start);

    while let Some(address) = to_visit.pop_first() {
        let addr = address - start;
        if addr > (bytes.len() - 2) {
            eprintln!("[WARN] Address out of bounds: {address:#x}");
            continue;
        }
        if decoded.contains_key(&address) {
            continue;
        }
        let instruction = Chip8Instruction::decode(&bytes[addr..])?;
        decoded.insert(address, instruction.clone());
        for target in targets(address, &instruction) {
            if !decoded.contains_key(&target) {
                to_visit.insert(target);
            }
        }
    }

    let mut statements = vec![];
    let mut addr = 0;
    while addr < bytes.len() {
        let address = addr + start;
        if decoded.contains_key(&address) {
            statements.push(Chip8Statement::Instruction(decoded[&address].clone()));
            addr += 2;
        } else {
            statements.push(Chip8Statement::Byte(bytes[addr]));
            addr += 1;
        }
    }
    Ok(statements)
}

impl From<Chip8DecodeError> for Chip8DisassemblyError {
    fn from(value: Chip8DecodeError) -> Self {
        Chip8DisassemblyError::DecodeError(value)
    }
}

fn targets(address: usize, instr: &Chip8Instruction) -> Vec<usize> {
    let next = address + 2;
    match instr {
        Chip8Instruction::Sys(_) => vec![next],
        Chip8Instruction::Cls => vec![next],
        Chip8Instruction::Ret => vec![next],
        Chip8Instruction::Jp(_, addr) => vec![addr.get()],
        Chip8Instruction::Call(addr) => vec![addr.get()],
        Chip8Instruction::Se(..) | Chip8Instruction::Sne(..) => vec![next, next + 2],
        Chip8Instruction::Ld_byte(..) => vec![next],
        Chip8Instruction::Add(..) => vec![next],
        Chip8Instruction::Ld_reg(..) => vec![next],
        Chip8Instruction::Or(..) => vec![next],
        Chip8Instruction::And(_, _) => vec![next],
        Chip8Instruction::Xor(_, _) => vec![next],
        Chip8Instruction::Sub(_, _) => vec![next],
        Chip8Instruction::Shr(_, _) => vec![next],
        Chip8Instruction::Subn(_, _) => vec![next],
        Chip8Instruction::Shl(_, _) => vec![next],
        Chip8Instruction::Ld_I(_) => vec![next],
        Chip8Instruction::Rnd(_, _) => vec![next],
        Chip8Instruction::Drw(_, _, _) => vec![next],
        Chip8Instruction::Skp(_) => vec![next, next + 2],
        Chip8Instruction::Sknp(_) => vec![next, next + 2],
        Chip8Instruction::Ld_from_delay(_) => vec![next],
        Chip8Instruction::Ld_key(_) => vec![next],
        Chip8Instruction::Ld_to_delay(_) => vec![next],
        Chip8Instruction::Ld_to_sound(_) => vec![next],
        Chip8Instruction::Add_I(_) => vec![next],
        Chip8Instruction::Ld_font(_) => vec![next],
        Chip8Instruction::Ld_bcd(_) => vec![next],
        Chip8Instruction::Ld_store_reg(_) => vec![next],
        Chip8Instruction::Ld_read_reg(_) => vec![next],
    }
}

impl core::fmt::Display for Chip8DisassemblyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Chip8DisassemblyError::DecodeError(err) => write!(f, "{err}"),
            Chip8DisassemblyError::AddressOutOfBounds(addr) => write!(f, "Address out of bounds: {addr:#x}"),
        }
    }
}
