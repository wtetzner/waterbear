use std::convert::Infallible;

use walrus::{
    ValType,
    ir::{BinaryOp, ExtendedLoad, Load, LoadKind, UnaryOp, Value},
};

use super::{
    domain::AbstractDomain,
    forward::{ForwardAnalysis, TransferContext},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Extension {
    Unknown,
    ZeroExtendedFrom(u8),
    SignExtendedFrom(u8),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntegerBits {
    pub width: u8,
    pub known_zero: u128,
    pub known_one: u128,
    pub extension: Extension,
}

impl IntegerBits {
    pub fn unknown(width: u8) -> Self {
        Self {
            width,
            known_zero: 0,
            known_one: 0,
            extension: Extension::Unknown,
        }
    }

    pub fn constant(width: u8, value: u128) -> Self {
        let mask = low_mask(width);
        let value = value & mask;
        Self {
            width,
            known_zero: !value & mask,
            known_one: value,
            extension: smallest_extension(width, value),
        }
    }

    pub fn exact_value(&self) -> Option<u128> {
        let mask = low_mask(self.width);
        ((self.known_zero | self.known_one) == mask).then_some(self.known_one)
    }

    pub fn minimum_unsigned_width(&self) -> u8 {
        let possible_one = !self.known_zero & low_mask(self.width);
        (u128::BITS - possible_one.leading_zeros()).max(1) as u8
    }

    fn normalized(mut self) -> Self {
        let mask = low_mask(self.width);
        self.known_zero &= mask;
        self.known_one &= mask;
        debug_assert_eq!(self.known_zero & self.known_one, 0);
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntegerFact {
    Bottom(Option<ValType>),
    Bits(IntegerBits),
    Other(ValType),
}

impl IntegerFact {
    pub fn bits(&self) -> Option<&IntegerBits> {
        match self {
            Self::Bits(bits) => Some(bits),
            _ => None,
        }
    }

    pub fn exact_value(&self) -> Option<u128> {
        self.bits()?.exact_value()
    }
}

impl AbstractDomain for IntegerFact {
    fn bottom(ty: Option<ValType>) -> Self {
        Self::Bottom(ty)
    }

    fn top(ty: ValType) -> Self {
        match integer_width(ty) {
            Some(width) => Self::Bits(IntegerBits::unknown(width)),
            None => Self::Other(ty),
        }
    }

    fn is_bottom(&self) -> bool {
        matches!(self, Self::Bottom(_))
    }

    fn join_assign(&mut self, other: &Self) -> bool {
        let previous = self.clone();
        match (&mut *self, other) {
            (left @ Self::Bottom(_), right) => *left = right.clone(),
            (_, Self::Bottom(_)) => {}
            (Self::Bits(left), Self::Bits(right)) if left.width == right.width => {
                left.known_zero &= right.known_zero;
                left.known_one &= right.known_one;
                if left.extension != right.extension {
                    left.extension = Extension::Unknown;
                }
            }
            (Self::Other(left), Self::Other(right)) if left == right => {}
            (left, right) => {
                let ty = fact_type(left).or_else(|| fact_type(right));
                *left = ty.map(Self::top).unwrap_or(Self::Bottom(None));
            }
        }
        *self != previous
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct IntegerValueAnalysis;

impl ForwardAnalysis for IntegerValueAnalysis {
    type Fact = IntegerFact;
    type Error = Infallible;

    fn constant(
        &mut self,
        _cx: &TransferContext<'_>,
        value: Value,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(match value {
            Value::I32(value) => IntegerFact::Bits(IntegerBits::constant(32, value as u32 as u128)),
            Value::I64(value) => IntegerFact::Bits(IntegerBits::constant(64, value as u64 as u128)),
            Value::V128(value) => IntegerFact::Bits(IntegerBits::constant(128, value)),
            Value::F32(_) => IntegerFact::Other(ValType::F32),
            Value::F64(_) => IntegerFact::Other(ValType::F64),
        })
    }

    fn unary(
        &mut self,
        _cx: &TransferContext<'_>,
        op: UnaryOp,
        input: &Self::Fact,
        output: ValType,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(analyze_unary(op, input, output))
    }

    fn binary(
        &mut self,
        _cx: &TransferContext<'_>,
        op: BinaryOp,
        left: &Self::Fact,
        right: &Self::Fact,
        output: ValType,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(analyze_binary(op, left, right, output))
    }

    fn load(
        &mut self,
        _cx: &TransferContext<'_>,
        load: &Load,
        _address: &Self::Fact,
        output: ValType,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(analyze_load(load.kind, output))
    }
}

fn analyze_unary(op: UnaryOp, input: &IntegerFact, output: ValType) -> IntegerFact {
    use UnaryOp::*;

    let Some(input_bits) = input.bits() else {
        return IntegerFact::top(output);
    };
    let output_width = integer_width(output);

    let result = match op {
        I32WrapI64 => truncate(input_bits, 32),
        I64ExtendUI32 => zero_extend(input_bits, 32, 64),
        I64ExtendSI32 => sign_extend(input_bits, 32, 64),
        I32Extend8S => sign_extend(input_bits, 8, 32),
        I32Extend16S => sign_extend(input_bits, 16, 32),
        I64Extend8S => sign_extend(input_bits, 8, 64),
        I64Extend16S => sign_extend(input_bits, 16, 64),
        I64Extend32S => sign_extend(input_bits, 32, 64),
        I32Eqz | I64Eqz => match input_bits.exact_value() {
            Some(value) => IntegerBits::constant(32, u128::from(value == 0)),
            None => boolean_bits(),
        },
        I32Clz | I64Clz | I32Ctz | I64Ctz | I32Popcnt | I64Popcnt => {
            match input_bits.exact_value() {
                Some(value) => {
                    let value = evaluate_integer_unary(op, input_bits.width, value).unwrap();
                    IntegerBits::constant(output_width.unwrap(), value)
                }
                None => IntegerBits::unknown(output_width.unwrap()),
            }
        }
        _ => return IntegerFact::top(output),
    };
    IntegerFact::Bits(result.normalized())
}

fn analyze_binary(
    op: BinaryOp,
    left: &IntegerFact,
    right: &IntegerFact,
    output: ValType,
) -> IntegerFact {
    use BinaryOp::*;

    let (Some(left), Some(right)) = (left.bits(), right.bits()) else {
        return IntegerFact::top(output);
    };
    let output_width = integer_width(output).unwrap_or(32);

    if let (Some(left_value), Some(right_value)) = (left.exact_value(), right.exact_value())
        && let Some(value) = evaluate_integer_binary(op, left.width, left_value, right_value)
    {
        return IntegerFact::Bits(IntegerBits::constant(output_width, value));
    }

    let bits = match op {
        I32And | I64And | V128And => IntegerBits {
            width: output_width,
            known_zero: left.known_zero | right.known_zero,
            known_one: left.known_one & right.known_one,
            extension: Extension::Unknown,
        },
        I32Or | I64Or | V128Or => IntegerBits {
            width: output_width,
            known_zero: left.known_zero & right.known_zero,
            known_one: left.known_one | right.known_one,
            extension: Extension::Unknown,
        },
        I32Xor | I64Xor | V128Xor => IntegerBits {
            width: output_width,
            known_zero: (left.known_zero & right.known_zero) | (left.known_one & right.known_one),
            known_one: (left.known_zero & right.known_one) | (left.known_one & right.known_zero),
            extension: Extension::Unknown,
        },
        I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU
        | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS | I64GeU => {
            boolean_bits()
        }
        _ => IntegerBits::unknown(output_width),
    };
    IntegerFact::Bits(bits.normalized())
}

fn analyze_load(kind: LoadKind, output: ValType) -> IntegerFact {
    use LoadKind::*;

    let (source_width, extension) = match kind {
        I32_8 { kind } | I64_8 { kind } => (8, kind),
        I32_16 { kind } | I64_16 { kind } => (16, kind),
        I64_32 { kind } => (32, kind),
        _ => return IntegerFact::top(output),
    };
    let output_width = integer_width(output).expect("extended integer load");
    let bits = match extension {
        ExtendedLoad::SignExtend => sign_extend(
            &IntegerBits::unknown(source_width),
            source_width,
            output_width,
        ),
        ExtendedLoad::ZeroExtend | ExtendedLoad::ZeroExtendAtomic => zero_extend(
            &IntegerBits::unknown(source_width),
            source_width,
            output_width,
        ),
    };
    IntegerFact::Bits(bits)
}

fn truncate(input: &IntegerBits, width: u8) -> IntegerBits {
    IntegerBits {
        width,
        known_zero: input.known_zero & low_mask(width),
        known_one: input.known_one & low_mask(width),
        extension: Extension::Unknown,
    }
}

fn zero_extend(input: &IntegerBits, source_width: u8, output_width: u8) -> IntegerBits {
    let source_mask = low_mask(source_width);
    IntegerBits {
        width: output_width,
        known_zero: (input.known_zero & source_mask) | (low_mask(output_width) & !source_mask),
        known_one: input.known_one & source_mask,
        extension: Extension::ZeroExtendedFrom(source_width),
    }
}

fn sign_extend(input: &IntegerBits, source_width: u8, output_width: u8) -> IntegerBits {
    let source_mask = low_mask(source_width);
    let upper = low_mask(output_width) & !source_mask;
    let sign = 1u128 << (source_width - 1);
    let (known_zero_upper, known_one_upper) = if input.known_zero & sign != 0 {
        (upper, 0)
    } else if input.known_one & sign != 0 {
        (0, upper)
    } else {
        (0, 0)
    };
    IntegerBits {
        width: output_width,
        known_zero: (input.known_zero & source_mask) | known_zero_upper,
        known_one: (input.known_one & source_mask) | known_one_upper,
        extension: Extension::SignExtendedFrom(source_width),
    }
}

fn boolean_bits() -> IntegerBits {
    IntegerBits {
        width: 32,
        known_zero: u32::MAX as u128 & !1,
        known_one: 0,
        extension: Extension::ZeroExtendedFrom(1),
    }
}

fn evaluate_integer_unary(op: UnaryOp, width: u8, value: u128) -> Option<u128> {
    use UnaryOp::*;
    Some(match (op, width) {
        (I32Clz, 32) => (value as u32).leading_zeros() as u128,
        (I32Ctz, 32) => (value as u32).trailing_zeros() as u128,
        (I32Popcnt, 32) => (value as u32).count_ones() as u128,
        (I64Clz, 64) => (value as u64).leading_zeros() as u128,
        (I64Ctz, 64) => (value as u64).trailing_zeros() as u128,
        (I64Popcnt, 64) => (value as u64).count_ones() as u128,
        _ => return None,
    })
}

fn evaluate_integer_binary(op: BinaryOp, width: u8, left: u128, right: u128) -> Option<u128> {
    use BinaryOp::*;

    let mask = low_mask(width);
    let shift = (right & (width as u128 - 1)) as u32;
    let unsigned = match op {
        I32Add | I64Add => left.wrapping_add(right) & mask,
        I32Sub | I64Sub => left.wrapping_sub(right) & mask,
        I32Mul | I64Mul => left.wrapping_mul(right) & mask,
        I32And | I64And | V128And => left & right,
        I32Or | I64Or | V128Or => left | right,
        I32Xor | I64Xor | V128Xor => left ^ right,
        I32Shl | I64Shl => left.wrapping_shl(shift) & mask,
        I32ShrU | I64ShrU => left >> shift,
        I32Rotl | I64Rotl => rotate_left(left, shift, width),
        I32Rotr | I64Rotr => rotate_right(left, shift, width),
        I32Eq | I64Eq => u128::from(left == right),
        I32Ne | I64Ne => u128::from(left != right),
        I32LtU | I64LtU => u128::from(left < right),
        I32GtU | I64GtU => u128::from(left > right),
        I32LeU | I64LeU => u128::from(left <= right),
        I32GeU | I64GeU => u128::from(left >= right),
        I32LtS | I64LtS => u128::from(as_signed(left, width) < as_signed(right, width)),
        I32GtS | I64GtS => u128::from(as_signed(left, width) > as_signed(right, width)),
        I32LeS | I64LeS => u128::from(as_signed(left, width) <= as_signed(right, width)),
        I32GeS | I64GeS => u128::from(as_signed(left, width) >= as_signed(right, width)),
        _ => return None,
    };
    Some(unsigned)
}

fn rotate_left(value: u128, amount: u32, width: u8) -> u128 {
    if amount == 0 {
        value & low_mask(width)
    } else {
        ((value << amount) | (value >> (width as u32 - amount))) & low_mask(width)
    }
}

fn rotate_right(value: u128, amount: u32, width: u8) -> u128 {
    if amount == 0 {
        value & low_mask(width)
    } else {
        ((value >> amount) | (value << (width as u32 - amount))) & low_mask(width)
    }
}

fn as_signed(value: u128, width: u8) -> i128 {
    let shift = u128::BITS - u32::from(width);
    ((value << shift) as i128) >> shift
}

fn fact_type(fact: &IntegerFact) -> Option<ValType> {
    match fact {
        IntegerFact::Bottom(ty) => *ty,
        IntegerFact::Bits(bits) => match bits.width {
            32 => Some(ValType::I32),
            64 => Some(ValType::I64),
            128 => Some(ValType::V128),
            _ => None,
        },
        IntegerFact::Other(ty) => Some(*ty),
    }
}

fn integer_width(ty: ValType) -> Option<u8> {
    match ty {
        ValType::I32 => Some(32),
        ValType::I64 => Some(64),
        ValType::V128 => Some(128),
        ValType::F32 | ValType::F64 | ValType::Ref(_) => None,
    }
}

fn low_mask(width: u8) -> u128 {
    match width {
        128 => u128::MAX,
        width => (1u128 << width) - 1,
    }
}

fn smallest_extension(width: u8, value: u128) -> Extension {
    for source in [1, 8, 16, 32, 64] {
        if source >= width {
            break;
        }
        if value & !low_mask(source) == 0 {
            return Extension::ZeroExtendedFrom(source);
        }
    }
    Extension::Unknown
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn join_keeps_only_bits_known_on_both_paths() {
        let mut left = IntegerFact::Bits(IntegerBits::constant(32, 0b0101));
        let right = IntegerFact::Bits(IntegerBits::constant(32, 0b0111));

        left.join_assign(&right);

        let bits = left.bits().unwrap();
        assert_eq!(bits.known_one & 0b1111, 0b0101);
        assert_eq!(bits.known_zero & 0b1111, 0b1000);
    }

    #[test]
    fn zero_extension_records_source_width() {
        let result = analyze_load(
            LoadKind::I32_8 {
                kind: ExtendedLoad::ZeroExtend,
            },
            ValType::I32,
        );

        let bits = result.bits().unwrap();
        assert_eq!(bits.extension, Extension::ZeroExtendedFrom(8));
        assert_eq!(bits.known_zero, u32::MAX as u128 & !(u8::MAX as u128));
    }
}
