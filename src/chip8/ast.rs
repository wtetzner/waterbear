
#[derive(Debug, Clone)]
pub enum Chip8DecodeError {
    UnknownInstruction(u8, u8),
    InvalidRegister(u8),
    InvalidKey(u8),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Chip8Address(u16);

impl Chip8Address {
    pub fn from_nybbles(n1: u8, n2: u8, n3: u8) -> Chip8Address {
        let n1: u16 = (n1 & 0x0Fu8).into();
        let n2: u16 = (n2 & 0x0Fu8).into();
        let n3: u16 = (n3 & 0x0Fu8).into();
        Chip8Address((n1 << 8) | (n2 << 4) | n3)
    }

    pub fn get(&self) -> usize {
        self.0.into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Chip8Register {
    V0 = 0,
    V1 = 1,
    V2 = 2,
    V3 = 3,
    V4 = 4,
    V5 = 5,
    V6 = 6,
    V7 = 7,
    V8 = 8,
    V9 = 9,
    VA = 10,
    VB = 11,
    VC = 12,
    VD = 13,
    VE = 14,
    VF = 15,
}

impl Chip8Register {
    pub fn new(value: u8) -> Result<Chip8Register, Chip8DecodeError> {
        use Chip8Register::*;
        let reg = match value {
            0 => V0,
            1 => V1,
            2 => V2,
            3 => V3,
            4 => V4,
            5 => V5,
            6 => V6,
            7 => V7,
            8 => V8,
            9 => V9,
            10 => VA,
            11 => VB,
            12 => VC,
            13 => VD,
            14 => VE,
            15 => VF,
            _ => return Err(Chip8DecodeError::InvalidRegister(value)),
        };
        Ok(reg)
    }

    pub fn num(&self) -> u8 {
        *self as u8
    }

    pub fn name(&self) -> &'static str {
        match self {
            Chip8Register::V0 => "V0",
            Chip8Register::V1 => "V1",
            Chip8Register::V2 => "V2",
            Chip8Register::V3 => "V3",
            Chip8Register::V4 => "V4",
            Chip8Register::V5 => "V5",
            Chip8Register::V6 => "V6",
            Chip8Register::V7 => "V7",
            Chip8Register::V8 => "V8",
            Chip8Register::V9 => "V9",
            Chip8Register::VA => "VA",
            Chip8Register::VB => "VB",
            Chip8Register::VC => "VC",
            Chip8Register::VD => "VD",
            Chip8Register::VE => "VE",
            Chip8Register::VF => "VF",
        }
    }

    pub fn values() -> &'static [Chip8Register] {
        static REGISTERS: &[Chip8Register] = &[
            Chip8Register::V0,
            Chip8Register::V1,
            Chip8Register::V2,
            Chip8Register::V3,
            Chip8Register::V4,
            Chip8Register::V5,
            Chip8Register::V6,
            Chip8Register::V7,
            Chip8Register::V8,
            Chip8Register::V9,
            Chip8Register::VA,
            Chip8Register::VB,
            Chip8Register::VC,
            Chip8Register::VD,
            Chip8Register::VE,
            Chip8Register::VF,
        ];
        REGISTERS
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Chip8Key {
    K0 = 0,
    K1 = 1,
    K2 = 2,
    K3 = 3,
    K4 = 4,
    K5 = 5,
    K6 = 6,
    K7 = 7,
    K8 = 8,
    K9 = 9,
    KA = 10,
    KB = 11,
    KC = 12,
    KD = 13,
    KE = 14,
    KF = 15,
}

impl Chip8Key {
    pub fn new(value: u8) -> Result<Chip8Key, Chip8DecodeError> {
        use Chip8Key::*;
        let key = match value {
            0 => K0,
            1 => K1,
            2 => K2,
            3 => K3,
            4 => K4,
            5 => K5,
            6 => K6,
            7 => K7,
            8 => K8,
            9 => K9,
            10 => KA,
            11 => KB,
            12 => KC,
            13 => KD,
            14 => KE,
            15 => KF,
            _ => return Err(Chip8DecodeError::InvalidKey(value)),
        };
        Ok(key)
    }

    pub fn num(&self) -> u8 {
        *self as u8
    }

    pub fn name(&self) -> &'static str {
        match self {
            Chip8Key::K0 => "K0",
            Chip8Key::K1 => "K1",
            Chip8Key::K2 => "K2",
            Chip8Key::K3 => "K3",
            Chip8Key::K4 => "K4",
            Chip8Key::K5 => "K5",
            Chip8Key::K6 => "K6",
            Chip8Key::K7 => "K7",
            Chip8Key::K8 => "K8",
            Chip8Key::K9 => "K9",
            Chip8Key::KA => "KA",
            Chip8Key::KB => "KB",
            Chip8Key::KC => "KC",
            Chip8Key::KD => "KD",
            Chip8Key::KE => "KE",
            Chip8Key::KF => "KF",
        }
    }

    pub fn values() -> &'static [Chip8Key] {
        static KEYS: &[Chip8Key] = &[
            Chip8Key::K0,
            Chip8Key::K1,
            Chip8Key::K2,
            Chip8Key::K3,
            Chip8Key::K4,
            Chip8Key::K5,
            Chip8Key::K6,
            Chip8Key::K7,
            Chip8Key::K8,
            Chip8Key::K9,
            Chip8Key::KA,
            Chip8Key::KB,
            Chip8Key::KC,
            Chip8Key::KD,
            Chip8Key::KE,
            Chip8Key::KF,
        ];
        KEYS
    }
}

fn byte(n1: u8, n2: u8) -> u8 {
    ((n1 & 0xF) << 4) | (n2 & 0xF)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Chip8Arg {
    Byte(u8),
    Reg(Chip8Register),
}

impl Chip8Arg {
    pub fn byte(n1: u8, n2: u8) -> Chip8Arg {
        Chip8Arg::Byte(byte(n1, n2))
    }

    pub fn reg(value: u8) -> Result<Chip8Arg, Chip8DecodeError> {
        Ok(Chip8Arg::Reg(Chip8Register::new(value)?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Chip8Instruction {
    /// 0nnn - SYS addr
    Sys(u16),

    /// 00E0 - CLS
    Cls,

    /// 00EE - RET
    Ret,

    /// 1nnn - JP addr
    /// Bnnn - JP V0, addr
    Jp(Option<Chip8Register>, Chip8Address),

    /// 2nnn - CALL addr
    Call(Chip8Address),

    /// 3xkk - SE Vx, byte
    /// 5xy0 - SE Vx, Vy
    Se(Chip8Register, Chip8Arg),

    /// 4xkk - SNE Vx, byte
    /// 9xy0 - SNE Vx, Vy
    Sne(Chip8Register, Chip8Arg),

    /// 6xkk - LD Vx, byte
    Ld_byte(Chip8Register, u8),

    /// 7xkk - ADD Vx, byte
    /// 8xy4 - ADD Vx, Vy
    Add(Chip8Register, Chip8Arg),

    /// 8xy0 - LD Vx, Vy
    Ld_reg(Chip8Register, Chip8Register),

    /// 8xy1 - OR Vx, Vy
    Or(Chip8Register, Chip8Register),

    /// 8xy2 - AND Vx, Vy
    And(Chip8Register, Chip8Register),

    /// 8xy3 - XOR Vx, Vy
    Xor(Chip8Register, Chip8Register),

    /// 8xy5 - SUB Vx, Vy
    Sub(Chip8Register, Chip8Register),

    /// 8xy6 - SHR Vx {, Vy}
    Shr(Chip8Register, Chip8Register),

    /// 8xy7 - SUBN Vx, Vy
    Subn(Chip8Register, Chip8Register),

    /// 8xyE - SHL Vx {, Vy}
    Shl(Chip8Register, Chip8Register),

    /// Annn - LD I, addr
    Ld_I(Chip8Address),

    /// Cxkk - RND Vx, byte
    Rnd(Chip8Register, u8),

    /// Dxyn - DRW Vx, Vy, nibble
    Drw(Chip8Register, Chip8Register, u8),

    /// Ex9E - SKP Vx
    Skp(Chip8Register),

    /// ExA1 - SKNP Vx
    Sknp(Chip8Register),

    /// Fx07 - LD Vx, DT
    Ld_from_delay(Chip8Register),

    /// Fx0A - LD Vx, K
    Ld_key(Chip8Register),

    /// Fx15 - LD DT, Vx
    Ld_to_delay(Chip8Register),

    /// Fx18 - LD ST, Vx
    Ld_to_sound(Chip8Register),

    /// Fx1E - ADD I, Vx
    Add_I(Chip8Register),

    /// Fx29 - LD F, Vx
    Ld_font(Chip8Register),

    /// Fx33 - LD B, Vx
    Ld_bcd(Chip8Register),

    /// Fx55 - LD [I], Vx
    Ld_store_reg(Chip8Register),

    /// Fx65 - LD Vx, [I]
    Ld_read_reg(Chip8Register),
}

#[derive(Debug, Clone)]
pub enum Chip8Statement {
    Instruction(Chip8Instruction),
    Byte(u8),
}

impl Chip8Instruction {
    pub fn decode(bytes: &[u8]) -> Result<Chip8Instruction, Chip8DecodeError> {
        let part1 = (bytes[0] & 0xF0) >> 4;
        let part2 = bytes[0] & 0x0F;
        let part3 = (bytes[1] & 0xF0) >> 4;
        let part4 = bytes[1] & 0x0F;

        let instr = match (part1, part2, part3, part4) {
            (0, 0, 0xE, 0) => Chip8Instruction::Cls,
            (0, 0, 0xE, 0xE) => Chip8Instruction::Ret,
            (0x1, n1, n2, n3) => Chip8Instruction::Jp(None, Chip8Address::from_nybbles(n1, n2, n3)),
            (0xB, n1, n2, n3) => Chip8Instruction::Jp(Some(Chip8Register::V0), Chip8Address::from_nybbles(n1, n2, n3)),
            (0x2, n1, n2, n3) => Chip8Instruction::Call(Chip8Address::from_nybbles(n1, n2, n3)),
            (0x3, x, k1, k2) => Chip8Instruction::Se(Chip8Register::new(x)?, Chip8Arg::byte(k1, k2)),
            (0x5, x, y, 0) => Chip8Instruction::Se(Chip8Register::new(x)?, Chip8Arg::reg(y)?),
            (0x4, x, k1, k2) => Chip8Instruction::Sne(Chip8Register::new(x)?, Chip8Arg::byte(k1, k2)),
            (0x9, x, y, 0) => Chip8Instruction::Sne(Chip8Register::new(x)?, Chip8Arg::reg(y)?),

            (0x6, x, k1, k2) => Chip8Instruction::Ld_byte(Chip8Register::new(x)?, byte(k1, k2)),
            (0x7, x, k1, k2) => Chip8Instruction::Add(Chip8Register::new(x)?, Chip8Arg::byte(k1, k2)),
            (0x8, x, y, 0x4) => Chip8Instruction::Add(Chip8Register::new(x)?, Chip8Arg::reg(y)?),
            (0x8, x, y, 0) => Chip8Instruction::Ld_reg(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0x8, x, y, 0x1) => Chip8Instruction::Or(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0x8, x, y, 0x2) => Chip8Instruction::And(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0x8, x, y, 0x3) => Chip8Instruction::Xor(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0x8, x, y, 0x5) => Chip8Instruction::Sub(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0x8, x, y, 0x6) => Chip8Instruction::Shr(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0x8, x, y, 0x7) => Chip8Instruction::Subn(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0x8, x, y, 0xE) => Chip8Instruction::Shl(Chip8Register::new(x)?, Chip8Register::new(y)?),
            (0xA, n1, n2, n3) => Chip8Instruction::Ld_I(Chip8Address::from_nybbles(n1, n2, n3)),
            (0xC, x, k1, k2) => Chip8Instruction::Rnd(Chip8Register::new(x)?, byte(k1, k2)),
            (0xD, x, y, n) => Chip8Instruction::Drw(Chip8Register::new(x)?, Chip8Register::new(y)?, n),
            (0xE, x, 0x9, 0xE) => Chip8Instruction::Skp(Chip8Register::new(x)?),
            (0xE, x, 0xA, 0x1) => Chip8Instruction::Sknp(Chip8Register::new(x)?),
            (0xF, x, 0, 0x7) => Chip8Instruction::Ld_from_delay(Chip8Register::new(x)?),
            (0xF, x, 0, 0xA) => Chip8Instruction::Ld_key(Chip8Register::new(x)?),
            (0xF, x, 0x1, 0x5) => Chip8Instruction::Ld_to_delay(Chip8Register::new(x)?),
            (0xF, x, 0x1, 0x8) => Chip8Instruction::Ld_to_sound(Chip8Register::new(x)?),
            (0xF, x, 0x1, 0xE) => Chip8Instruction::Add_I(Chip8Register::new(x)?),
            (0xF, x, 0x2, 0x9) => Chip8Instruction::Ld_font(Chip8Register::new(x)?),
            (0xF, x, 0x3, 0x3) => Chip8Instruction::Ld_bcd(Chip8Register::new(x)?),
            (0xF, x, 0x5, 0x5) => Chip8Instruction::Ld_store_reg(Chip8Register::new(x)?),
            (0xF, x, 0x6, 0x5) => Chip8Instruction::Ld_read_reg(Chip8Register::new(x)?),

            (0, n1, n2, n3) => Chip8Instruction::Sys(Chip8Address::from_nybbles(n1, n2, n3).0),
            _ => {
                return Err(Chip8DecodeError::UnknownInstruction(bytes[0], bytes[1]));
            }
        };
        Ok(instr)
    }
}

impl core::fmt::Display for Chip8Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Chip8Statement::Instruction(instr) => write!(f, "{instr}"),
            Chip8Statement::Byte(byte) => write!(f, "BT {byte:#x}"),
        }
    }
}

impl core::fmt::Display for Chip8Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Chip8Instruction::Sys(addr) => write!(f, "SYS {addr:#X}"),
            Chip8Instruction::Cls => write!(f, "CLS"),
            Chip8Instruction::Ret => write!(f, "RET"),
            Chip8Instruction::Jp(None, addr) => write!(f, "JP {addr}"),
            Chip8Instruction::Jp(Some(reg), addr) => write!(f, "JP {reg}, {addr}"),
            Chip8Instruction::Call(addr) => write!(f, "CALL {addr}"),
            Chip8Instruction::Se(reg, arg) => write!(f, "SE {reg}, {arg}"),
            Chip8Instruction::Sne(reg, arg) => write!(f, "SNE {reg}, {arg}"),
            Chip8Instruction::Ld_byte(reg, byte) => write!(f, "LD {reg}, {byte:#04X} // Load byte into {reg}"),
            Chip8Instruction::Add(reg, arg) => write!(f, "ADD {reg}, {arg}"),
            Chip8Instruction::Ld_reg(reg1, reg2) => write!(f, "LD {reg1}, {reg2}"),
            Chip8Instruction::Or(reg1, reg2) => write!(f, "OR {reg1}, {reg2}"),
            Chip8Instruction::And(reg1, reg2) => write!(f, "AND {reg1}, {reg2}"),
            Chip8Instruction::Xor(reg1, reg2) => write!(f, "XOR {reg1}, {reg2}"),
            Chip8Instruction::Sub(reg1, reg2) => write!(f, "SUB {reg1}, {reg2}"),
            Chip8Instruction::Shr(reg1, reg2) => if reg1 == reg2 {
                write!(f, "SHR {reg1}")
            } else {
                write!(f, "SHR {reg1}, {reg2}")
            },
            Chip8Instruction::Subn(reg1, reg2) => write!(f ,"SUBN {reg1}, {reg2}"),
            Chip8Instruction::Shl(reg1, reg2) => if reg1 == reg2 {
                write!(f, "SHL {reg1}")
            } else {
                write!(f, "SHL {reg1}, {reg2}")
            },
            Chip8Instruction::Ld_I(addr) => write!(f, "LD I, {addr}"),
            Chip8Instruction::Rnd(reg, byte) => write!(f, "RND {reg}, {byte:#04X}"),
            Chip8Instruction::Drw(reg1, reg2, byte) => write!(f, "DRW {reg1}, {reg2}, {byte:#03X}"),
            Chip8Instruction::Skp(reg) => write!(f, "SKP {reg}"),
            Chip8Instruction::Sknp(reg) => write!(f, "SKNP {reg}"),
            Chip8Instruction::Ld_from_delay(reg) => write!(f, "LD {reg}, DT"),
            Chip8Instruction::Ld_key(reg) => write!(f, "LD {reg}"),
            Chip8Instruction::Ld_to_delay(reg) => write!(f, "LD DT, {reg}"),
            Chip8Instruction::Ld_to_sound(reg) => write!(f, "LD ST, {reg}"),
            Chip8Instruction::Add_I(reg) => write!(f, "ADD I, {reg}"),
            Chip8Instruction::Ld_font(reg) => write!(f, "LD F, {reg}"),
            Chip8Instruction::Ld_bcd(reg) => write!(f, "LD B, {reg}"),
            Chip8Instruction::Ld_store_reg(reg) => write!(f, "LD [I], {reg}"),
            Chip8Instruction::Ld_read_reg(reg) => write!(f, "LD {reg}, [I]"),
        }
    }
}

impl core::fmt::Display for Chip8Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#05X}", self.0)
    }
}

impl core::fmt::Display for Chip8Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value =  self.name();
        write!(f, "{value}")
    }
}

impl core::fmt::Display for Chip8Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = self.name();
        write!(f, "{value}")
    }
}

impl core::fmt::Display for Chip8Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Chip8Arg::Byte(value) => write!(f, "{value:#x}"),
            Chip8Arg::Reg(reg) => write!(f, "{reg}"),
        }
    }
}

impl core::fmt::Display for Chip8DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Chip8DecodeError::UnknownInstruction(byte1, byte2) => {
                write!(f, "Unknown instruction: {:#06X}", ((*byte1 as u16) << 8) | (*byte2 as u16))
            },
            Chip8DecodeError::InvalidRegister(reg) => write!(f, "Invalid register: V{reg:x}"),
            Chip8DecodeError::InvalidKey(key) => write!(f, "K{key:x}"),
        }
    }
}
