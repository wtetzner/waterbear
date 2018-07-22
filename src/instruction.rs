
use std;
use expression::{Expr,EvaluationError};
use std::collections::HashMap;
use location::{Positioned,Span};

#[derive(Debug)]
pub enum EncodingError {
    NumOutOfRange {
        span: Span,
        bits: u32,
        value: i32
    },
    SignedNumOutOfRange {
        span: Span,
        bits: u32,
        value: i32
    },
    InvalidAddress {
        span: Span,
        value: i32
    },
    EvalError(EvaluationError)
}

impl EncodingError {
    pub fn out_of_range(span: Span, bits: u32, value: i32) -> EncodingError {
        EncodingError::NumOutOfRange { span, bits, value }
    }

    pub fn signed_out_of_range(span: Span, bits: u32, value: i32) -> EncodingError {
        EncodingError::SignedNumOutOfRange { span, bits, value }
    }

    pub fn invalid_addr(span: Span, value: i32) -> EncodingError {
        EncodingError::InvalidAddress { span, value }
    }
}

impl From<EvaluationError> for EncodingError {
    fn from(error: EvaluationError) -> Self {
        EncodingError::EvalError(error)
    }
}

#[derive(Debug,Clone,Copy)]
pub enum IndirectionMode {
    R0,
    R1,
    R2,
    R3
}

impl IndirectionMode {
    pub fn index(&self) -> u8 {
        match self {
            IndirectionMode::R0 => 0,
            IndirectionMode::R1 => 1,
            IndirectionMode::R2 => 2,
            IndirectionMode::R3 => 3
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug,Instruction)]
pub enum Instruction<Ex,IM> {
    #[instr(add(bits="10000001 [a7][a6][a5][a4][a3][a2][a1][a0]"))]
    Add_i8(Ex),
    #[instr(add(bits="1000001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"))]
    Add_d9(Ex),
    #[instr(add(bits="100001[a1][a0]"))]
    Add_Ri(IM),

    #[instr(addc(bits="10010001 [a7][a6][a5][a4][a3][a2][a1][a0]"))]
    Addc_i8(Ex),
    #[instr(addc(bits="1001001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"))]
    Addc_d9(Ex),
    #[instr(addc(bits="100101[a1][a0]"))]
    Addc_Ri(IM),

    Sub_i8(Ex),
    Sub_d9(Ex),
    Sub_Ri(IM),

    Subc_i8(Ex),
    Subc_d9(Ex),
    Subc_Ri(IM),

    Inc_d9(Ex),
    Inc_Ri(IM),

    Dec_d9(Ex),
    Dec_Ri(IM),

    Mul,
    Div,

    And_i8(Ex),
    And_d9(Ex),
    And_Ri(IM),

    Or_i8(Ex),
    Or_d9(Ex),
    Or_Ri(IM),

    Xor_i8(Ex),
    Xor_d9(Ex),
    Xor_Ri(IM),

    Rol,
    Rolc,

    Ror,
    Rorc,

    Ld_d9(Ex),
    Ld_Ri(IM),

    St_d9(Ex),
    St_Ri(IM),

    Mov_d9(Ex, Ex),
    Mov_Rj(Ex, IM),

    Ldc,

    Push(Ex),
    Pop(Ex),

    Xch_d9(Ex),
    Xch_Ri(IM),

    Jmp(Ex),
    Jmpf(Ex),

    Br(Ex),
    Brf(Ex),
    Bz(Ex),
    Bnz(Ex),
    Bp(Ex, Ex, Ex),
    Bpc(Ex, Ex, Ex),
    Bn(Ex, Ex, Ex),
    Dbnz_d9(Ex, Ex),
    Dbnz_Ri(IM, Ex),
    Be_i8(Ex, Ex),
    Be_d9(Ex, Ex),
    Be_Rj(IM, Ex, Ex),
    Bne_i8(Ex, Ex),
    Bne_d9(Ex, Ex),
    Bne_Rj(IM, Ex, Ex),

    Call(Ex),
    Callf(Ex),
    Callr(Ex),

    Ret,
    Reti,

    Clr1(Ex, Ex),
    Set1(Ex, Ex),
    Not1(Ex, Ex),

    Nop
}

struct Bits {
    current: usize,
    bytes: Vec<u8>
}

fn bit_at(data: i32, bit: usize) -> bool {
    ((data >> bit) & 0x1) != 0
}

fn set_bit(bytes: &mut [u8], bit: usize, value: bool) {
    let mask = 1 << (bit % 8);
    if value {
        
    }
}

// bits!(1, 0, 1, 1, 0, d@1, d@9)
impl Bits {
    fn new() -> Bits {
        Bits {
            current: 0,
            bytes: vec![]
        }
    }

    fn push(&mut self, value: i32, bits: usize) {
        {
            #[derive(Debug)]
            struct Foo {}

            println!("foo: {:?}", Foo {});
        }
        {
            #[derive(Debug)]
            struct Foo { x: usize }

            println!("foo: {:?}", Foo { x: 0 });
        }
        // if bits == 0 {
        //     return;
        // }
        // let mut free = (self.bytes.len() * 8) - self.current;
        // let mut remaining_bits = bits;
        
        // let mut data = value;
        // while remaining_bits > 0 {
        //     if free == 0 {
        //         self.bytes.push(0u8);
        //         free = free + 8;
        //     }
            
        // }

        // while free < bits {
        //     self.bytes.push(0u8);
        //     free = free + 8;
        // }
        // let mut byte = self.bytes[self.bytes.len() - 1];
        
    }

    fn render(self) -> Vec<u8> {
        self.bytes
    }
}

type EncResult<T> = Result<T,EncodingError>;
type Env = HashMap<String,i32>;

fn eval(expr: &Expr, env: &Env, bits: usize) -> EncResult<i32> {
    let value = expr.eval(env)?;
    if (value >> bits) != 0 {
        Err(EncodingError::out_of_range(expr.span(), bits, value))
    } else {
        Ok(value)
    }
}

fn eval3(expr: &Expr, env: &Env) -> EncResult<i32> {
    eval(expr, env, 3)
}

fn eval8(expr: &Expr, env: &Env) -> EncResult<i32> {
    eval(expr, env, 8)
}

fn eval9(expr: &Expr, env: &Env) -> EncResult<i32> {
    eval(expr, env, 9)
}

fn eval16(expr: &Expr, env: &Env) -> EncResult<i32> {
    eval(expr, env, 16)
}

fn rel8(expr: &Expr, pos: usize, env: &Env) -> EncResult<i32> {
    let address = expr.eval(env)?;
    if address >= 0 && address <= 65535 {
        let value = address - (pos as i32);
        if value >= -128 && value <= 127 {
            Ok(value)
        } else {
            Err(EncodingError::signed_out_of_range(expr.span(), 8, value))
        }
    } else {
        Err(EncodingError::invalid_addr(expr.span(), address))
    }
}

fn rel16(expr: &Expr, pos: usize, env: &Env) -> EncResult<i32> {
    let address = expr.eval(env)?;
    if address >= 0 && address <= 65535 {
        let value = address - (pos as i32);
        if value >= -32768 && value <= 32767 {
            Ok(value)
        } else {
            Err(EncodingError::signed_out_of_range(expr.span(), 16, value))
        }
    } else {
        Err(EncodingError::invalid_addr(expr.span(), address))
    }
}

type EvalResult = EncResult<Instruction<i32,u8>>;

impl Instruction<Expr,IndirectionMode> {
    pub fn eval(&self, pos: usize, env: &Env) -> EvalResult {
        let val = match self {
            use Instruction::*;
            Add_i8(imm) => Add_i8(eval8(imm, env)?),
            Add_d9(dir) => Add_d9(eval9(dir, env)?),
            Add_Ri(ind) => Add_Ri(ind.index()),

            Addc_i8(imm) => Addc_i8(eval8(imm, env)?),
            Addc_d9(dir) => Addc_d9(eval9(dir, env)?),
            Addc_Ri(ind) => Addc_Ri(ind.index()),

            Sub_i8(imm) => Sub_i8(eval8(imm, env)?),
            Sub_d9(dir) => Sub_d9(eval9(dir, env)?),
            Sub_Ri(ind) => Sub_Ri(ind.index()),

            Subc_i8(imm) => Subc_i8(eval8(imm, env)?),
            Subc_d9(dir) => Subc_d9(eval9(dir, env)?),
            Subc_Ri(ind) => Subc_Ri(ind.index()),

            Inc_d9(dir) => Inc_d9(eval9(dir, env)?),
            Inc_Ri(ind) => Inc_Ri(ind.index()),

            Dec_d9(dir) => Dec_d9(eval9(dir, env)?),
            Dec_Ri(ind) => Dec_Ri(ind.index()),

            Mul => Mul,
            Div => Div,

            And_i8(imm) => And_i8(eval8(imm, env)?),
            And_d9(dir) => And_d9(eval9(dir, env)?),
            And_Ri(ind) => And_Ri(ind.index()),

            Or_i8(imm) => Or_i8(eval8(imm, env)?),
            Or_d9(dir) => Or_d9(eval9(dir, env)?),
            Or_Ri(ind) => Or_Ri(ind.index()),

            Xor_i8(imm) => Xor_i8(eval8(imm, env)?),
            Xor_d9(dir) => Xor_d9(eval9(dir, env)?),
            Xor_Ri(ind) => Xor_Ri(ind.index()),

            Rol => Rol,
            Rolc => Rolc,

            Ror => Ror,
            Rorc => Rorc,

            Ld_d9(dir) => Ld_d9(eval9(dir, env)?),
            Ld_Ri(ind) => Ld_Ri(ind.index()),

            St_d9(dir) => St_d9(eval9(dir, env)?),
            St_Ri(ind) => St_Ri(ind.index()),

            Mov_d9(imm, dir) => Mov_d9(eval8(imm, env)?, eval9(dir, env)?),
            Mov_Rj(imm, ind) => Mov_Rj(eval8(imm, env)?, ind.index()),

            Ldc => Ldc,

            Push(dir) => Push(eval9(dir, env)?),
            Pop(dir) => Pop(eval9(dir, env)?),

            Xch_d9(dir) => Xch_d9(eval9(dir, env)?),
            Xch_Ri(ind) => Xch_Ri(ind.index()),

            Jmp(Ex),
            Jmpf(Ex),

            Br(Ex),
            Brf(Ex),
            Bz(Ex),
            Bnz(Ex),
            Bp(Ex, Ex, Ex),
            Bpc(Ex, Ex, Ex),
            Bn(Ex, Ex, Ex),
            Dbnz_d9(Ex, Ex),
            Dbnz_Ri(IM, Ex),
            Be_i8(Ex, Ex),
            Be_d9(Ex, Ex),
            Be_Rj(IM, Ex, Ex),
            Bne_i8(Ex, Ex),
            Bne_d9(Ex, Ex),
            Bne_Rj(IM, Ex, Ex),

            Call(Ex),
            Callf(Ex),
            Callr(Ex),

            Ret => Ret,
            Reti => Reti,

            Clr1(Ex, Ex),
            Set1(Ex, Ex),
            Not1(Ex, Ex),

            Nop => Nop
        };
        val
    }
    // pub fn encode(&self, pos: u32, env: &Env) -> EncResult<Vec<u8>> {
    //     self.to_bytes();
    //     Ok(vec![])
    // }

    // /// The size in bytes of the instruction
    // #[inline]
    // pub fn size(&self) -> usize {
    //     match self {
    //         Instruction::Add_i8(_) => 2,
    //         Instruction::Add_d9(_) => 2,
    //         Instruction::Add_Ri(_) => 1,

    //         Instruction::Addc_i8(_) => 2,
    //         Instruction::Addc_d9(_) => 2,
    //         Instruction::Addc_Ri(_) => 1,

    //         Instruction::Sub_i8(_) => 2,
    //         Instruction::Sub_d9(_) => 2,
    //         Instruction::Sub_Ri(_) => 1,

    //         Instruction::Subc_i8(_) => 2,
    //         Instruction::Subc_d9(_) => 2,
    //         Instruction::Subc_Ri(_) => 1,

    //         Instruction::Inc_d9(_) => 2,
    //         Instruction::Inc_Ri(_) => 1,

    //         Instruction::Dec_d9(_) => 2,
    //         Instruction::Dec_Ri(_) => 1,

    //         Instruction::Mul => 1,
    //         Instruction::Div => 1,

    //         Instruction::And_i8(_) => 2,
    //         Instruction::And_d9(_) => 2,
    //         Instruction::And_Ri(_) => 1,

    //         Instruction::Or_i8(_) => 2,
    //         Instruction::Or_d9(_) => 2,
    //         Instruction::Or_Ri(_) => 1,

    //         Instruction::Xor_i8(_) => 2,
    //         Instruction::Xor_d9(_) => 2,
    //         Instruction::Xor_Ri(_) => 1,

    //         Instruction::Rol => 1,
    //         Instruction::Rolc => 1,

    //         Instruction::Ror => 1,
    //         Instruction::Rorc => 1,

    //         Instruction::Ld_d9(_) => 2,
    //         Instruction::Ld_Ri(_) => 1,

    //         Instruction::St_d9(_) => 2,
    //         Instruction::St_Ri(_) => 1,

    //         Instruction::Mov_d9(_, _) => 3,
    //         Instruction::Mov_Rj(_, _) => 2,

    //         Instruction::Ldc => 1,

    //         Instruction::Push(_) => 2,
    //         Instruction::Pop(_) => 2,

    //         Instruction::Xch_d9(_) => 2,
    //         Instruction::Xch_Ri(_) => 1,

    //         Instruction::Jmp(_) => 2,
    //         Instruction::Jmpf(_) => 3,

    //         Instruction::Br(_) => 2,
    //         Instruction::Brf(_) => 3,
    //         Instruction::Bz(_) => 2,
    //         Instruction::Bnz(_) => 2,
    //         Instruction::Bp(_, _, _) => 3,
    //         Instruction::Bpc(_, _, _) => 3,
    //         Instruction::Bn(_, _, _) => 3,
    //         Instruction::Dbnz_d9(_, _) => 3,
    //         Instruction::Dbnz_Ri(_, _) => 2,
    //         Instruction::Be_i8(_,_) => 3,
    //         Instruction::Be_d9(_,_) => 3,
    //         Instruction::Be_Rj(_, _, _) => 3,
    //         Instruction::Bne_i8(_,_) => 3,
    //         Instruction::Bne_d9(_,_) => 3,
    //         Instruction::Bne_Rj(_, _, _) => 3,

    //         Instruction::Call(_) => 2,
    //         Instruction::Callf(_) => 3,
    //         Instruction::Callr(_) => 3,

    //         Instruction::Ret => 1,
    //         Instruction::Reti => 1,

    //         Instruction::Clr1(_,_) => 2,
    //         Instruction::Set1(_,_) => 2,
    //         Instruction::Not1(_,_) => 2,

    //         Instruction::Nop => 1
    //     }
    // }
}

#[cfg(test)]
mod Test {

    #[test]
    fn test_bit_at() {
    }
}
