use std::{
    collections::BTreeMap,
    fmt::{Debug, Write},
};

use walrus::{
    FunctionId, LocalFunction, RefType, ValType,
    ir::{
        AtomicOp, AtomicWidth, BinaryOp, ExtendedLoad, Instr, InstrSeqId, LoadKind, LoadSimdKind,
        MemArg, StoreKind, UnaryOp, Value,
    },
};

use super::names::Names;
use crate::wasm::analysis::{
    address::{AddressBase, AddressFact},
    backward::BitDemand,
    frame::{FrameAccess, FrameAccessKind, FrameOffset},
    integer::{Extension, IntegerFact},
};

pub(super) fn sequence_labels(body: &LocalFunction) -> BTreeMap<InstrSeqId, String> {
    let mut labels = BTreeMap::from([(body.entry_block(), "$return".to_owned())]);
    let mut pending = vec![body.entry_block()];
    while let Some(sequence) = pending.pop() {
        for (instruction, _) in &body.block(sequence).instrs {
            match instruction {
                Instr::Block(op) => {
                    labels.insert(op.seq, format!("$block{}", op.seq.index()));
                    pending.push(op.seq);
                }
                Instr::Loop(op) => {
                    labels.insert(op.seq, format!("$loop{}", op.seq.index()));
                    pending.push(op.seq);
                }
                Instr::IfElse(op) => {
                    let label = format!("$if{}", op.consequent.index());
                    labels.insert(op.consequent, label.clone());
                    labels.insert(op.alternative, label);
                    pending.push(op.alternative);
                    pending.push(op.consequent);
                }
                _ => {}
            }
        }
    }
    labels
}

pub(super) fn label(labels: &BTreeMap<InstrSeqId, String>, id: InstrSeqId) -> String {
    labels
        .get(&id)
        .cloned()
        .unwrap_or_else(|| format!("$label{}", id.index()))
}

pub(super) fn signature(parameters: &[ValType], results: &[ValType]) -> String {
    let mut output = String::new();
    for parameter in parameters {
        write!(output, " (param {parameter})").unwrap();
    }
    for result in results {
        write!(output, " (result {result})").unwrap();
    }
    output
}

pub(super) fn global_type(ty: ValType, mutable: bool, shared: bool) -> String {
    let ty = if mutable {
        format!("(mut {ty})")
    } else {
        ty.to_string()
    };
    if shared { format!("(shared {ty})") } else { ty }
}

pub(super) fn limits(initial: u64, maximum: Option<u64>, shared: bool, memory64: bool) -> String {
    let mut output = format!(" {initial}");
    if let Some(maximum) = maximum {
        write!(output, " {maximum}").unwrap();
    }
    if shared {
        output.push_str(" shared");
    }
    if memory64 {
        output.push_str(" i64");
    }
    output
}

pub(super) fn ref_type(ty: RefType) -> &'static str {
    match ty {
        RefType::Funcref => "funcref",
        RefType::Externref => "externref",
        _ => "ref",
    }
}

pub(super) fn value_instruction(value: Value) -> String {
    match value {
        Value::I32(value) => format!("i32.const {value}"),
        Value::I64(value) => format!("i64.const {value}"),
        Value::F32(value) => format!("f32.const {value}"),
        Value::F64(value) => format!("f64.const {value}"),
        Value::V128(value) => format!("v128.const 0x{value:032x}"),
    }
}

pub(super) fn binary_op(op: BinaryOp) -> String {
    match op {
        BinaryOp::I8x16ReplaceLane { idx } => format!("i8x16.replace_lane {idx}"),
        BinaryOp::I16x8ReplaceLane { idx } => format!("i16x8.replace_lane {idx}"),
        BinaryOp::I32x4ReplaceLane { idx } => format!("i32x4.replace_lane {idx}"),
        BinaryOp::I64x2ReplaceLane { idx } => format!("i64x2.replace_lane {idx}"),
        BinaryOp::F32x4ReplaceLane { idx } => format!("f32x4.replace_lane {idx}"),
        BinaryOp::F64x2ReplaceLane { idx } => format!("f64x2.replace_lane {idx}"),
        _ => operation_name(op),
    }
}

pub(super) fn unary_op(op: UnaryOp) -> String {
    match op {
        UnaryOp::I8x16ExtractLaneS { idx } => format!("i8x16.extract_lane_s {idx}"),
        UnaryOp::I8x16ExtractLaneU { idx } => format!("i8x16.extract_lane_u {idx}"),
        UnaryOp::I16x8ExtractLaneS { idx } => format!("i16x8.extract_lane_s {idx}"),
        UnaryOp::I16x8ExtractLaneU { idx } => format!("i16x8.extract_lane_u {idx}"),
        UnaryOp::I32x4ExtractLane { idx } => format!("i32x4.extract_lane {idx}"),
        UnaryOp::I64x2ExtractLane { idx } => format!("i64x2.extract_lane {idx}"),
        UnaryOp::F32x4ExtractLane { idx } => format!("f32x4.extract_lane {idx}"),
        UnaryOp::F64x2ExtractLane { idx } => format!("f64x2.extract_lane {idx}"),
        UnaryOp::I32WrapI64 => "i32.wrap_i64".to_owned(),
        UnaryOp::I32TruncSF32 => "i32.trunc_f32_s".to_owned(),
        UnaryOp::I32TruncUF32 => "i32.trunc_f32_u".to_owned(),
        UnaryOp::I32TruncSF64 => "i32.trunc_f64_s".to_owned(),
        UnaryOp::I32TruncUF64 => "i32.trunc_f64_u".to_owned(),
        UnaryOp::I64ExtendSI32 => "i64.extend_i32_s".to_owned(),
        UnaryOp::I64ExtendUI32 => "i64.extend_i32_u".to_owned(),
        UnaryOp::I64TruncSF32 => "i64.trunc_f32_s".to_owned(),
        UnaryOp::I64TruncUF32 => "i64.trunc_f32_u".to_owned(),
        UnaryOp::I64TruncSF64 => "i64.trunc_f64_s".to_owned(),
        UnaryOp::I64TruncUF64 => "i64.trunc_f64_u".to_owned(),
        UnaryOp::F32ConvertSI32 => "f32.convert_i32_s".to_owned(),
        UnaryOp::F32ConvertUI32 => "f32.convert_i32_u".to_owned(),
        UnaryOp::F32ConvertSI64 => "f32.convert_i64_s".to_owned(),
        UnaryOp::F32ConvertUI64 => "f32.convert_i64_u".to_owned(),
        UnaryOp::F64ConvertSI32 => "f64.convert_i32_s".to_owned(),
        UnaryOp::F64ConvertUI32 => "f64.convert_i32_u".to_owned(),
        UnaryOp::F64ConvertSI64 => "f64.convert_i64_s".to_owned(),
        UnaryOp::F64ConvertUI64 => "f64.convert_i64_u".to_owned(),
        UnaryOp::I32TruncSSatF32 => "i32.trunc_sat_f32_s".to_owned(),
        UnaryOp::I32TruncUSatF32 => "i32.trunc_sat_f32_u".to_owned(),
        UnaryOp::I32TruncSSatF64 => "i32.trunc_sat_f64_s".to_owned(),
        UnaryOp::I32TruncUSatF64 => "i32.trunc_sat_f64_u".to_owned(),
        UnaryOp::I64TruncSSatF32 => "i64.trunc_sat_f32_s".to_owned(),
        UnaryOp::I64TruncUSatF32 => "i64.trunc_sat_f32_u".to_owned(),
        UnaryOp::I64TruncSSatF64 => "i64.trunc_sat_f64_s".to_owned(),
        UnaryOp::I64TruncUSatF64 => "i64.trunc_sat_f64_u".to_owned(),
        _ => operation_name(op),
    }
}

pub(super) fn operation_name(operation: impl Debug) -> String {
    let name = format!("{operation:?}");
    const PREFIXES: [&str; 11] = [
        "I8x16", "I16x8", "I32x4", "I64x2", "F32x4", "F64x2", "V128", "I32", "I64", "F32", "F64",
    ];
    let (prefix, operation) = PREFIXES
        .iter()
        .find_map(|prefix| name.strip_prefix(prefix).map(|rest| (*prefix, rest)))
        .unwrap_or(("wasm", name.as_str()));
    format!(
        "{}.{}",
        prefix.to_ascii_lowercase(),
        camel_to_snake(operation)
    )
}

pub(super) fn camel_to_snake(value: &str) -> String {
    let mut output = String::new();
    for (index, character) in value.char_indices() {
        if character.is_ascii_uppercase()
            && index > 0
            && value[..index]
                .chars()
                .next_back()
                .is_some_and(|previous| previous.is_ascii_lowercase() || previous.is_ascii_digit())
        {
            output.push('_');
        }
        output.push(character.to_ascii_lowercase());
    }
    output
}

pub(super) fn load_kind(kind: LoadKind) -> &'static str {
    match kind {
        LoadKind::I32 { atomic: false } => "i32.load",
        LoadKind::I32 { atomic: true } => "i32.atomic.load",
        LoadKind::I64 { atomic: false } => "i64.load",
        LoadKind::I64 { atomic: true } => "i64.atomic.load",
        LoadKind::F32 => "f32.load",
        LoadKind::F64 => "f64.load",
        LoadKind::V128 => "v128.load",
        LoadKind::I32_8 { kind } => extended_load("i32", "8", kind),
        LoadKind::I32_16 { kind } => extended_load("i32", "16", kind),
        LoadKind::I64_8 { kind } => extended_load("i64", "8", kind),
        LoadKind::I64_16 { kind } => extended_load("i64", "16", kind),
        LoadKind::I64_32 { kind } => extended_load("i64", "32", kind),
    }
}

pub(super) fn extended_load(ty: &str, width: &str, kind: ExtendedLoad) -> &'static str {
    match (ty, width, kind) {
        ("i32", "8", ExtendedLoad::SignExtend) => "i32.load8_s",
        ("i32", "8", ExtendedLoad::ZeroExtend) => "i32.load8_u",
        ("i32", "8", ExtendedLoad::ZeroExtendAtomic) => "i32.atomic.load8_u",
        ("i32", "16", ExtendedLoad::SignExtend) => "i32.load16_s",
        ("i32", "16", ExtendedLoad::ZeroExtend) => "i32.load16_u",
        ("i32", "16", ExtendedLoad::ZeroExtendAtomic) => "i32.atomic.load16_u",
        ("i64", "8", ExtendedLoad::SignExtend) => "i64.load8_s",
        ("i64", "8", ExtendedLoad::ZeroExtend) => "i64.load8_u",
        ("i64", "8", ExtendedLoad::ZeroExtendAtomic) => "i64.atomic.load8_u",
        ("i64", "16", ExtendedLoad::SignExtend) => "i64.load16_s",
        ("i64", "16", ExtendedLoad::ZeroExtend) => "i64.load16_u",
        ("i64", "16", ExtendedLoad::ZeroExtendAtomic) => "i64.atomic.load16_u",
        ("i64", "32", ExtendedLoad::SignExtend) => "i64.load32_s",
        ("i64", "32", ExtendedLoad::ZeroExtend) => "i64.load32_u",
        ("i64", "32", ExtendedLoad::ZeroExtendAtomic) => "i64.atomic.load32_u",
        _ => unreachable!(),
    }
}

pub(super) fn store_kind(kind: StoreKind) -> &'static str {
    match kind {
        StoreKind::I32 { atomic: false } => "i32.store",
        StoreKind::I32 { atomic: true } => "i32.atomic.store",
        StoreKind::I64 { atomic: false } => "i64.store",
        StoreKind::I64 { atomic: true } => "i64.atomic.store",
        StoreKind::F32 => "f32.store",
        StoreKind::F64 => "f64.store",
        StoreKind::V128 => "v128.store",
        StoreKind::I32_8 { atomic: false } => "i32.store8",
        StoreKind::I32_8 { atomic: true } => "i32.atomic.store8",
        StoreKind::I32_16 { atomic: false } => "i32.store16",
        StoreKind::I32_16 { atomic: true } => "i32.atomic.store16",
        StoreKind::I64_8 { atomic: false } => "i64.store8",
        StoreKind::I64_8 { atomic: true } => "i64.atomic.store8",
        StoreKind::I64_16 { atomic: false } => "i64.store16",
        StoreKind::I64_16 { atomic: true } => "i64.atomic.store16",
        StoreKind::I64_32 { atomic: false } => "i64.store32",
        StoreKind::I64_32 { atomic: true } => "i64.atomic.store32",
    }
}

pub(super) fn load_simd_kind(kind: LoadSimdKind) -> String {
    match kind {
        LoadSimdKind::Splat8 => "v128.load8_splat".to_owned(),
        LoadSimdKind::Splat16 => "v128.load16_splat".to_owned(),
        LoadSimdKind::Splat32 => "v128.load32_splat".to_owned(),
        LoadSimdKind::Splat64 => "v128.load64_splat".to_owned(),
        LoadSimdKind::V128Load8x8S => "v128.load8x8_s".to_owned(),
        LoadSimdKind::V128Load8x8U => "v128.load8x8_u".to_owned(),
        LoadSimdKind::V128Load16x4S => "v128.load16x4_s".to_owned(),
        LoadSimdKind::V128Load16x4U => "v128.load16x4_u".to_owned(),
        LoadSimdKind::V128Load32x2S => "v128.load32x2_s".to_owned(),
        LoadSimdKind::V128Load32x2U => "v128.load32x2_u".to_owned(),
        LoadSimdKind::V128Load32Zero => "v128.load32_zero".to_owned(),
        LoadSimdKind::V128Load64Zero => "v128.load64_zero".to_owned(),
        LoadSimdKind::V128Load8Lane(lane) => format!("v128.load8_lane {lane}"),
        LoadSimdKind::V128Load16Lane(lane) => format!("v128.load16_lane {lane}"),
        LoadSimdKind::V128Load32Lane(lane) => format!("v128.load32_lane {lane}"),
        LoadSimdKind::V128Load64Lane(lane) => format!("v128.load64_lane {lane}"),
        LoadSimdKind::V128Store8Lane(lane) => format!("v128.store8_lane {lane}"),
        LoadSimdKind::V128Store16Lane(lane) => format!("v128.store16_lane {lane}"),
        LoadSimdKind::V128Store32Lane(lane) => format!("v128.store32_lane {lane}"),
        LoadSimdKind::V128Store64Lane(lane) => format!("v128.store64_lane {lane}"),
    }
}

pub(super) fn atomic_rmw(width: AtomicWidth, op: AtomicOp) -> String {
    let (ty, width) = atomic_width(width);
    let op = match op {
        AtomicOp::Add => "add",
        AtomicOp::Sub => "sub",
        AtomicOp::And => "and",
        AtomicOp::Or => "or",
        AtomicOp::Xor => "xor",
        AtomicOp::Xchg => "xchg",
    };
    let unsigned = if width.is_empty() { "" } else { "_u" };
    format!("{ty}.atomic.rmw{width}.{op}{unsigned}")
}

pub(super) fn atomic_cmpxchg(width: AtomicWidth) -> String {
    let (ty, width) = atomic_width(width);
    let unsigned = if width.is_empty() { "" } else { "_u" };
    format!("{ty}.atomic.rmw{width}.cmpxchg{unsigned}")
}

pub(super) fn atomic_width(width: AtomicWidth) -> (&'static str, &'static str) {
    match width {
        AtomicWidth::I32 => ("i32", ""),
        AtomicWidth::I32_8 => ("i32", "8"),
        AtomicWidth::I32_16 => ("i32", "16"),
        AtomicWidth::I64 => ("i64", ""),
        AtomicWidth::I64_8 => ("i64", "8"),
        AtomicWidth::I64_16 => ("i64", "16"),
        AtomicWidth::I64_32 => ("i64", "32"),
    }
}

pub(super) fn memarg(arg: MemArg) -> String {
    let mut output = String::new();
    if arg.offset != 0 {
        write!(output, " offset={}", arg.offset).unwrap();
    }
    write!(output, " align={}", arg.align).unwrap();
    output
}

pub(super) fn integer_fact(fact: &IntegerFact) -> Option<String> {
    let IntegerFact::Bits(bits) = fact else {
        return None;
    };
    if let Some(value) = bits.exact_value() {
        return Some(if value < 10 {
            format!("constant={value}")
        } else {
            format!("constant=0x{value:x}")
        });
    }

    let mut facts = Vec::new();
    let minimum_width = bits.minimum_unsigned_width();
    if minimum_width < bits.width {
        facts.push(format!("fits u{minimum_width}"));
    }
    match bits.extension {
        Extension::Unknown => {}
        Extension::ZeroExtendedFrom(width) => {
            facts.push(format!("zero-extended from {width} bits"))
        }
        Extension::SignExtendedFrom(width) => {
            facts.push(format!("sign-extended from {width} bits"))
        }
    }
    if bits.known_one != 0 {
        facts.push(format!("known-one=0x{:x}", bits.known_one));
    }
    if facts.is_empty() {
        None
    } else {
        Some(facts.join(", "))
    }
}

pub(super) fn address_fact(
    fact: &AddressFact,
    function: FunctionId,
    names: &Names,
) -> Option<String> {
    match fact {
        AddressFact::Affine {
            base: AddressBase::ShadowStack(owner),
            minimum_offset,
            maximum_offset,
            ..
        } => {
            let owner = if *owner == function {
                String::new()
            } else {
                format!("({})", names.function(*owner))
            };
            Some(if minimum_offset == maximum_offset {
                format!("shadow-stack{owner}[{minimum_offset}]")
            } else {
                format!("shadow-stack{owner}[{minimum_offset}..{maximum_offset}]")
            })
        }
        _ => None,
    }
}

pub(super) fn demand_fact(demand: &BitDemand) -> Option<String> {
    if demand.demanded == 0 {
        return demand.opaque.then(|| "opaque value demanded".to_owned());
    }
    let width = demand.ty.map(type_width).unwrap_or(128);
    let full = low_mask(width);
    let mut result = if demand.demanded == full {
        format!("{width} bits demanded")
    } else if demand.demanded == low_mask(demand.demanded.count_ones()) {
        format!("low {} bits demanded", demand.demanded.count_ones())
    } else {
        format!("bits 0x{:x} demanded", demand.demanded)
    };
    if demand.opaque {
        result.push_str(", opaque use");
    }
    Some(result)
}

pub(super) fn frame_access(access: &FrameAccess) -> String {
    let operation = match access.kind {
        FrameAccessKind::Load => "load from",
        FrameAccessKind::Store => "store to",
    };
    let offset = match access.offset {
        FrameOffset::Fixed(offset) => offset.to_string(),
        FrameOffset::Range { minimum, maximum } => format!("{minimum}..{maximum}"),
    };
    let object = access
        .object
        .map(|object| format!(" ($stack{})", object.0))
        .unwrap_or_default();
    format!(
        "{operation} shadow-stack[{offset}]{object}, {} bytes, align={}",
        access.width, access.alignment
    )
}

pub(super) fn extend_fact(facts: &mut Vec<String>, fact: Option<String>) {
    if let Some(fact) = fact {
        facts.push(fact);
    }
}

pub(super) fn type_width(ty: ValType) -> u32 {
    match ty {
        ValType::I32 | ValType::F32 => 32,
        ValType::I64 | ValType::F64 => 64,
        ValType::V128 => 128,
        ValType::Ref(_) => 0,
    }
}

pub(super) fn low_mask(width: u32) -> u128 {
    if width >= 128 {
        u128::MAX
    } else if width == 0 {
        0
    } else {
        (1u128 << width) - 1
    }
}
pub(super) fn bare(symbol: &str) -> &str {
    symbol.strip_prefix('$').unwrap_or(symbol)
}

pub(super) fn quoted(value: &str) -> String {
    let mut output = String::from("\"");
    for character in value.chars() {
        match character {
            '\\' => output.push_str("\\\\"),
            '"' => output.push_str("\\\""),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            character if character.is_control() => {
                write!(output, "\\u{{{:x}}}", character as u32).unwrap();
            }
            character => output.push(character),
        }
    }
    output.push('"');
    output
}

pub(super) fn byte_preview(bytes: &[u8]) -> String {
    const LIMIT: usize = 32;
    let mut output = String::from("\"");
    for &byte in bytes.iter().take(LIMIT) {
        match byte {
            b'\\' => output.push_str("\\\\"),
            b'"' => output.push_str("\\\""),
            0x20..=0x7e => output.push(char::from(byte)),
            byte => write!(output, "\\{byte:02x}").unwrap(),
        }
    }
    if bytes.len() > LIMIT {
        output.push_str("...");
    }
    output.push('"');
    output
}
