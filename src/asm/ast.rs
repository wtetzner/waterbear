
use instruction;

pub enum Instruction {
    
}

pub enum Directive {
    Byte(Vec<u8>),
    Org(usize),
    Word(Vec<u16>),
    Include(String)
}

pub enum Statement {
    Directive(Directive),
    Label(String),
    Instruction(instruction::Instruction)
}


