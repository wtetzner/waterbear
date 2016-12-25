
use instruction;
use instruction::{ToVal,B3,D9,A12};

pub enum Instruction {
    
}

pub enum Directive {
    Byte(Vec<u8>),
    Org(usize),
    Word(Vec<u16>),
    Include(String)
}

pub enum Statement<S3: ToVal<B3>,S8: ToVal<i8>,S9: ToVal<D9>,S12: ToVal<A12>,S16: ToVal<u16>> {
    Directive(Directive),
    Label(String),
    Instruction(instruction::Instruction<S3,S8,S9,S12,S16>)
}


