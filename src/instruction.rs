
use expression::{Expr,EvaluationError};
use std::collections::HashMap;
use location::{Positioned,Span};
use env::{Names,Env};

#[derive(Debug)]
pub enum EncodingError {
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
    },
    EvalError(EvaluationError)
}

impl EncodingError {
    pub fn mismatch_top_bits(span: Span, pos: usize, value: i32, pos_top: u8, value_top: u8) -> EncodingError {
        EncodingError::AddrBitsDontMatch { span, pos, value, pos_top, value_top }
    }

    pub fn out_of_range(span: Span, bits: usize, value: i32) -> EncodingError {
        EncodingError::NumOutOfRange { span, bits, value }
    }

    pub fn signed_out_of_range(span: Span, bits: usize, value: i32) -> EncodingError {
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
    #[instr="10000001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Add_i8(Ex),
    #[instr="1000001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Add_d9(Ex),
    #[instr="100001[a1][a0]"]
    Add_Ri(IM),

    #[instr="10010001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Addc_i8(Ex),
    #[instr="1001001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Addc_d9(Ex),
    #[instr="100101[a1][a0]"]
    Addc_Ri(IM),

    #[instr="10100001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Sub_i8(Ex),
    #[instr="1010001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Sub_d9(Ex),
    #[instr="101001[a1][a0]"]
    Sub_Ri(IM),

    #[instr="10110001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Subc_i8(Ex),
    #[instr="1011001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Subc_d9(Ex),
    #[instr="101101[a1][a0]"]
    Subc_Ri(IM),

    #[instr="0110001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Inc_d9(Ex),
    #[instr="011001[a1][a0]"]
    Inc_Ri(IM),

    #[instr="0111001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Dec_d9(Ex),
    #[instr="011101[a1][a0]"]
    Dec_Ri(IM),

    #[instr="00110000"]
    Mul,
    #[instr="01000000"]
    Div,

    #[instr="11100001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    And_i8(Ex),
    #[instr="1110001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    And_d9(Ex),
    #[instr="111001[a1][a0]"]
    And_Ri(IM),

    #[instr="11010001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Or_i8(Ex),
    #[instr="1101001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Or_d9(Ex),
    #[instr="110101[a1][a0]"]
    Or_Ri(IM),

    #[instr="11110001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Xor_i8(Ex),
    #[instr="1111001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Xor_d9(Ex),
    #[instr="111101[a1][a0]"]
    Xor_Ri(IM),

    #[instr="11100000"]
    Rol,
    #[instr="11110000"]
    Rolc,

    #[instr="11000000"]
    Ror,
    #[instr="11010000"]
    Rorc,

    #[instr="0000001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Ld_d9(Ex),
    #[instr="000001[a1][a0]"]
    Ld_Ri(IM),

    #[instr="0001001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    St_d9(Ex),
    #[instr="000101[a1][a0]"]
    St_Ri(IM),

    #[instr="0010001[b8] [b7][b6][b5][b4][b3][b2][b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Mov_d9(Ex, Ex),
    #[instr="001001[b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Mov_Rj(Ex, IM),

    #[instr="11000001"]
    Ldc,

    #[instr="0110000[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Push(Ex),
    #[instr="0111000[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Pop(Ex),

    #[instr="1100001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Xch_d9(Ex),
    #[instr="110001[a1][a0]"]
    Xch_Ri(IM),

    #[instr="001[a11]1[a10][a9][a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Jmp(Ex),
    #[instr="00100001 [a15][a14][a13][a12][a11][a10][a9][a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Jmpf(Ex),

    #[instr="00000001 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Br(Ex),
    #[instr="00010001 [a7][a6][a5][a4][a3][a2][a1][a0] [a15][a14][a13][a12][a11][a10][a9][a8]"]
    Brf(Ex),
    #[instr="10000000 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Bz(Ex),
    #[instr="10010000 [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Bnz(Ex),
    #[instr="011[a8]1[b2][b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0] [c7][c6][c5][c4][c3][c2][c1][c0]"]
    Bp(Ex, Ex, Ex),
    #[instr="010[a8]1[b2][b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0] [c7][c6][c5][c4][c3][c2][c1][c0]"]
    Bpc(Ex, Ex, Ex),
    #[instr="100[a8]1[b2][b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0] [c7][c6][c5][c4][c3][c2][c1][c0]"]
    Bn(Ex, Ex, Ex),
    #[instr="0101001[a8] [a7][a6][a5][a4][a3][a2][a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0]"]
    Dbnz_d9(Ex, Ex),
    #[instr="010101[a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0]"]
    Dbnz_Ri(IM, Ex),
    #[instr="00110001 [a7][a6][a5][a4][a3][a2][a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0]"]
    Be_i8(Ex, Ex),
    #[instr="0011001[a8] [a7][a6][a5][a4][a3][a2][a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0]"]
    Be_d9(Ex, Ex),
    #[instr="001101[a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0] [c7][c6][c5][c4][c3][c2][c1][c0]"]
    Be_Rj(IM, Ex, Ex),
    #[instr="01000001 [a7][a6][a5][a4][a3][a2][a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0]"]
    Bne_i8(Ex, Ex),
    #[instr="0100001[a8] [a7][a6][a5][a4][a3][a2][a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0]"]
    Bne_d9(Ex, Ex),
    #[instr="010001[a1][a0] [b7][b6][b5][b4][b3][b2][b1][b0] [c7][c6][c5][c4][c3][c2][c1][c0]"]
    Bne_Rj(IM, Ex, Ex),

    #[instr="000[a11]1[a10][a9][a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Call(Ex),
    #[instr="00100000 [a15][a14][a13][a12][a11][a10][a9][a8] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Callf(Ex),
    #[instr="00010000 [a7][a6][a5][a4][a3][a2][a1][a0] [a15][a14][a13][a12][a11][a10][a9][a8]"]
    Callr(Ex),

    #[instr="10100000"]
    Ret,
    #[instr="10110000"]
    Reti,

    #[instr="110[a8]1[b2][b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Clr1(Ex, Ex),
    #[instr="111[a8]1[b2][b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Set1(Ex, Ex),
    #[instr="101[a8]1[b2][b1][b0] [a7][a6][a5][a4][a3][a2][a1][a0]"]
    Not1(Ex, Ex),

    #[instr="00000000"]
    Nop
}

type EncResult<T> = Result<T,EncodingError>;

fn eval<E: Env<i32>>(expr: &Expr, env: &E, bits: usize) -> EncResult<i32> {
    let value = expr.eval(env)?;
    if (value >> bits) != 0 {
        Err(EncodingError::out_of_range(expr.span(), bits, value))
    } else {
        Ok(value)
    }
}

fn eval3<E: Env<i32>>(expr: &Expr, env: &E) -> EncResult<i32> {
    eval(expr, env, 3)
}

fn eval8<E: Env<i32>>(expr: &Expr, env: &E) -> EncResult<i32> {
    eval(expr, env, 8)
}

fn eval9<E: Env<i32>>(expr: &Expr, env: &E) -> EncResult<i32> {
    eval(expr, env, 9)
}

fn eval16<E: Env<i32>>(expr: &Expr, env: &E) -> EncResult<i32> {
    eval(expr, env, 16)
}

fn rel8<E: Env<i32>>(expr: &Expr, pos: usize, env: &E) -> EncResult<i32> {
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

fn eval12<E: Env<i32>>(expr: &Expr, pos: usize, env: &E) -> EncResult<i32> {
    let addr = eval16(expr, env)?;
    let val_top = (addr as usize) & 0b1111000000000000;
    let pos_top = pos & 0b1111000000000000;
    let value = addr & 0b0000111111111111;
    if val_top != pos_top {
        return Err(EncodingError::mismatch_top_bits(
            expr.span(),
            pos,
            value,
            ((pos_top >> 12) & 0xFF) as u8,
            ((val_top >> 12) & 0xFF) as u8
        ));
    } else {
        Ok(value)
    }
}

fn rel16<E: Env<i32>>(expr: &Expr, pos: usize, env: &E) -> EncResult<i32> {
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
    pub fn eval(&self, pos: usize, label: &str, names: &Names) -> EvalResult {
        use instruction::Instruction::*;
        let nenv = names.as_env("Name", label);
        let venv = names.as_env("Variable", label);
        let lenv = names.as_env("Label", label);
        let val = match self {
            Add_i8(imm) => Add_i8(eval8(imm, &nenv)?),
            Add_d9(dir) => Add_d9(eval9(dir, &venv)?),
            Add_Ri(ind) => Add_Ri(ind.index()),

            Addc_i8(imm) => Addc_i8(eval8(imm, &nenv)?),
            Addc_d9(dir) => Addc_d9(eval9(dir, &venv)?),
            Addc_Ri(ind) => Addc_Ri(ind.index()),

            Sub_i8(imm) => Sub_i8(eval8(imm, &nenv)?),
            Sub_d9(dir) => Sub_d9(eval9(dir, &venv)?),
            Sub_Ri(ind) => Sub_Ri(ind.index()),

            Subc_i8(imm) => Subc_i8(eval8(imm, &nenv)?),
            Subc_d9(dir) => Subc_d9(eval9(dir, &venv)?),
            Subc_Ri(ind) => Subc_Ri(ind.index()),

            Inc_d9(dir) => Inc_d9(eval9(dir, &venv)?),
            Inc_Ri(ind) => Inc_Ri(ind.index()),

            Dec_d9(dir) => Dec_d9(eval9(dir, &venv)?),
            Dec_Ri(ind) => Dec_Ri(ind.index()),

            Mul => Mul,
            Div => Div,

            And_i8(imm) => And_i8(eval8(imm, &nenv)?),
            And_d9(dir) => And_d9(eval9(dir, &venv)?),
            And_Ri(ind) => And_Ri(ind.index()),

            Or_i8(imm) => Or_i8(eval8(imm, &nenv)?),
            Or_d9(dir) => Or_d9(eval9(dir, &venv)?),
            Or_Ri(ind) => Or_Ri(ind.index()),

            Xor_i8(imm) => Xor_i8(eval8(imm, &nenv)?),
            Xor_d9(dir) => Xor_d9(eval9(dir, &venv)?),
            Xor_Ri(ind) => Xor_Ri(ind.index()),

            Rol => Rol,
            Rolc => Rolc,

            Ror => Ror,
            Rorc => Rorc,

            Ld_d9(dir) => Ld_d9(eval9(dir, &venv)?),
            Ld_Ri(ind) => Ld_Ri(ind.index()),

            St_d9(dir) => St_d9(eval9(dir, &venv)?),
            St_Ri(ind) => St_Ri(ind.index()),

            Mov_d9(imm, dir) => Mov_d9(eval8(imm, &nenv)?, eval9(dir, &venv)?),
            Mov_Rj(imm, ind) => Mov_Rj(eval8(imm, &nenv)?, ind.index()),

            Ldc => Ldc,

            Push(dir) => Push(eval9(dir, &venv)?),
            Pop(dir) => Pop(eval9(dir, &venv)?),

            Xch_d9(dir) => Xch_d9(eval9(dir, &venv)?),
            Xch_Ri(ind) => Xch_Ri(ind.index()),

            Jmp(abs) => Jmp(eval12(abs, pos, &lenv)?),
            Jmpf(abs) => Jmpf(eval16(abs, &lenv)?),

            Br(rel) => Br(rel8(rel, pos, &lenv)?),
            Brf(rel) => Brf(rel16(rel, pos, &lenv)?),
            Bz(rel) => Bz(rel8(rel, pos, &lenv)?),
            Bnz(rel) => Bnz(rel8(rel, pos, &lenv)?),
            Bp(dir, b3, rel) => Bp(
                eval9(dir, &venv)?,
                eval3(b3, &nenv)?,
                rel8(rel, pos, &lenv)?
            ),
            Bpc(dir, b3, rel) => Bpc(
                eval9(dir, &venv)?,
                eval3(b3, &nenv)?,
                rel8(rel, pos, &lenv)?
            ),
            Bn(dir, b3, rel) => Bn(
                eval9(dir, &venv)?,
                eval3(b3, &nenv)?,
                rel8(rel, pos, &lenv)?
            ),
            Dbnz_d9(dir, rel) => Dbnz_d9(
                eval9(dir, &venv)?,
                rel8(rel, pos, &lenv)?
            ),
            Dbnz_Ri(ind, rel) => Dbnz_Ri(ind.index(), rel8(rel, pos, &lenv)?),
            Be_i8(imm, rel) => Be_i8(eval8(imm, &nenv)?, rel8(rel, pos, &lenv)?),
            Be_d9(dir, rel) => Be_d9(eval9(dir, &venv)?, rel8(rel, pos, &lenv)?),
            Be_Rj(ind, imm, rel) => Be_Rj(
                ind.index(),
                eval8(imm, &nenv)?,
                rel8(rel, pos, &lenv)?
            ),
            Bne_i8(imm, rel) => Bne_i8(
                eval8(imm, &nenv)?,
                rel8(rel, pos, &lenv)?
            ),
            Bne_d9(dir, rel) => Bne_d9(
                eval9(dir, &venv)?,
                rel8(rel, pos, &lenv)?
            ),
            Bne_Rj(ind, imm, rel) => Bne_Rj(
                ind.index(),
                eval8(imm, &nenv)?,
                rel8(rel, pos, &lenv)?
            ),

            Call(a12) => Call(eval12(a12, pos, &lenv)?),
            Callf(a16) => Callf(eval16(a16, &lenv)?),
            Callr(r16) => Callr(rel16(r16, pos, &lenv)?),

            Ret => Ret,
            Reti => Reti,

            Clr1(dir, b3) => Clr1(eval9(dir, &venv)?, eval3(b3, &nenv)?),
            Set1(dir, b3) => Set1(eval9(dir, &venv)?, eval3(b3, &nenv)?),
            Not1(dir, b3) => Not1(eval9(dir, &venv)?, eval3(b3, &nenv)?),

            Nop => Nop
        };
        Ok(val)
    }
}

#[cfg(test)]
mod test {
    use instruction::Instruction;

    #[test]
    fn test_encode() {
        test(Instruction::Add_i8(0xf3), vec![0x81, 0xf3], "Add_i8");
        test(Instruction::Add_d9(0x1F4), vec![0x83, 0xf4], "Add_d9");
    }

    fn test(instr: Instruction<i32,u8>, bytes: Vec<u8>, message: &str) {
        assert_eq!(instr.encode(), bytes, "{}", message);
    }
}
