//! VMU operations exposed to WebAssembly programs as imported C symbols.
//!
//! This registry is the single source of truth for import recognition, IR
//! validation, side-effect information, and generated language headers.

use walrus::{FunctionId, FunctionKind, Module};

use super::analysis::calls::{
    ArgumentSummary, CallResolver, CallSummary, MemoryEffects, PointerBehavior,
    ResultPointerSummary,
};

pub mod header;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Intrinsic {
    SfrRead(Sfr),
    SfrWrite(Sfr),
    SfrBitRead(Sfr),
    SfrBitWrite(Sfr),
    SfrBitToggle(Sfr),
    XramRead,
    XramWrite,
    XramCopyFromMain,
    XramCopyToMain,
}

impl Intrinsic {
    pub fn definition(self) -> Option<IntrinsicDefinition> {
        definitions()
            .into_iter()
            .find(|definition| definition.operation == self)
    }

    pub fn display_name(self) -> String {
        self.definition()
            .map(|definition| definition.symbol)
            .unwrap_or_else(|| format!("<unregistered {self:?}>"))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sfr {
    InterruptEnable,
    InterruptPriority,
    OscillationControl,
    Timer0Control,
    Timer0Prescaler,
    Timer0Low,
    Timer0LowReload,
    Timer0High,
    Timer0HighReload,
    LcdModeControl,
    LcdStartAddress,
    LcdContrastControl,
    Port3,
    Port7,
    BaseTimerControl,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SfrAccess {
    pub byte_read: bool,
    pub byte_write: bool,
    pub bit_read: bool,
    pub bit_write: bool,
}

impl SfrAccess {
    pub const READ_ONLY: Self = Self {
        byte_read: true,
        byte_write: false,
        bit_read: true,
        bit_write: false,
    };

    pub const WRITE_ONLY: Self = Self {
        byte_read: false,
        byte_write: true,
        bit_read: false,
        bit_write: false,
    };

    pub const READ_WRITE: Self = Self {
        byte_read: true,
        byte_write: true,
        bit_read: true,
        bit_write: true,
    };

    pub const READ_WRITE_BYTES: Self = Self {
        byte_read: true,
        byte_write: true,
        bit_read: false,
        bit_write: false,
    };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SfrDefinition {
    pub register: Sfr,
    pub symbol: &'static str,
    pub description: &'static str,
    pub access: SfrAccess,
}

/// Application-visible SFRs audited for the initial VMU API.
///
/// Compiler-owned registers and the XRAM/WRAM control registers deliberately
/// do not appear here.
pub const SFR_DEFINITIONS: &[SfrDefinition] = &[
    SfrDefinition {
        register: Sfr::InterruptEnable,
        symbol: "ie",
        description: "master interrupt enable control register",
        access: SfrAccess::READ_WRITE,
    },
    SfrDefinition {
        register: Sfr::InterruptPriority,
        symbol: "ip",
        description: "interrupt priority control register",
        access: SfrAccess::READ_WRITE,
    },
    SfrDefinition {
        register: Sfr::OscillationControl,
        symbol: "ocr",
        description: "oscillation control register",
        access: SfrAccess::READ_WRITE,
    },
    SfrDefinition {
        register: Sfr::Timer0Control,
        symbol: "t0cnt",
        description: "timer 0 control register",
        access: SfrAccess::READ_WRITE,
    },
    SfrDefinition {
        register: Sfr::Timer0Prescaler,
        symbol: "t0prr",
        description: "timer 0 prescaler data register",
        access: SfrAccess::READ_WRITE_BYTES,
    },
    SfrDefinition {
        register: Sfr::Timer0Low,
        symbol: "t0l",
        description: "timer 0 low counter",
        access: SfrAccess::READ_ONLY,
    },
    SfrDefinition {
        register: Sfr::Timer0LowReload,
        symbol: "t0lr",
        description: "timer 0 low reload register",
        access: SfrAccess::READ_WRITE_BYTES,
    },
    SfrDefinition {
        register: Sfr::Timer0High,
        symbol: "t0h",
        description: "timer 0 high counter",
        access: SfrAccess::READ_ONLY,
    },
    SfrDefinition {
        register: Sfr::Timer0HighReload,
        symbol: "t0hr",
        description: "timer 0 high reload register",
        access: SfrAccess::READ_WRITE_BYTES,
    },
    SfrDefinition {
        register: Sfr::LcdModeControl,
        symbol: "mcr",
        description: "LCD mode control register",
        access: SfrAccess::WRITE_ONLY,
    },
    SfrDefinition {
        register: Sfr::LcdStartAddress,
        symbol: "stad",
        description: "LCD display start address register",
        access: SfrAccess::READ_WRITE_BYTES,
    },
    SfrDefinition {
        register: Sfr::LcdContrastControl,
        symbol: "vccr",
        description: "LCD contrast control register",
        access: SfrAccess::WRITE_ONLY,
    },
    SfrDefinition {
        register: Sfr::Port3,
        symbol: "p3",
        description: "port 3 button input latch",
        access: SfrAccess::READ_WRITE,
    },
    SfrDefinition {
        register: Sfr::Port7,
        symbol: "p7",
        description: "port 7 input latch",
        access: SfrAccess::READ_ONLY,
    },
    SfrDefinition {
        register: Sfr::BaseTimerControl,
        symbol: "btcr",
        description: "base timer control register",
        access: SfrAccess::READ_WRITE,
    },
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AbiType {
    U8,
    ConstU8Pointer,
    MutU8Pointer,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntrinsicParameter {
    pub name: &'static str,
    pub ty: AbiType,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntrinsicEffect {
    ReadSfr(Sfr),
    WriteSfr(Sfr),
    ReadMainMemory,
    WriteMainMemory,
    ReadXram,
    WriteXram,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntrinsicDefinition {
    pub operation: Intrinsic,
    pub symbol: String,
    pub description: String,
    pub parameters: Vec<IntrinsicParameter>,
    pub result: Option<AbiType>,
    pub effects: Vec<IntrinsicEffect>,
    /// Volatile operations cannot be removed or combined merely because their
    /// explicit inputs and outputs appear redundant.
    pub volatile: bool,
}

/// Reflects over every intrinsic currently accepted by the VMU backend.
pub fn definitions() -> Vec<IntrinsicDefinition> {
    let mut definitions = Vec::new();

    for sfr in SFR_DEFINITIONS {
        if sfr.access.byte_read {
            definitions.push(sfr_definition(sfr, SfrOperation::Read));
        }
        if sfr.access.byte_write {
            definitions.push(sfr_definition(sfr, SfrOperation::Write));
        }
        if sfr.access.bit_read {
            definitions.push(sfr_definition(sfr, SfrOperation::BitRead));
        }
        if sfr.access.bit_write {
            definitions.push(sfr_definition(sfr, SfrOperation::BitWrite));
            definitions.push(sfr_definition(sfr, SfrOperation::BitToggle));
        }
    }

    definitions.extend([
        IntrinsicDefinition {
            operation: Intrinsic::XramRead,
            symbol: "waterbear_vmu_xram_read".into(),
            description: "Read one byte from XRAM. Banks 0 and 1 implement offsets 0x00 through 0x7b except those ending in 0xc through 0xf; bank 2 implements 0x00 through 0x05.".into(),
            parameters: vec![
                parameter("bank", AbiType::U8),
                parameter("offset", AbiType::U8),
            ],
            result: Some(AbiType::U8),
            effects: vec![IntrinsicEffect::ReadXram],
            volatile: true,
        },
        IntrinsicDefinition {
            operation: Intrinsic::XramWrite,
            symbol: "waterbear_vmu_xram_write".into(),
            description: "Write one byte to XRAM. Banks 0 and 1 implement offsets 0x00 through 0x7b except those ending in 0xc through 0xf; bank 2 implements 0x00 through 0x05.".into(),
            parameters: vec![
                parameter("bank", AbiType::U8),
                parameter("offset", AbiType::U8),
                parameter("value", AbiType::U8),
            ],
            result: None,
            effects: vec![IntrinsicEffect::WriteXram],
            volatile: true,
        },
        IntrinsicDefinition {
            operation: Intrinsic::XramCopyFromMain,
            symbol: "waterbear_vmu_xram_copy_from_main".into(),
            description: "Copy bytes from WebAssembly memory into one contiguous implemented XRAM range; the range must not cross an unimplemented address.".into(),
            parameters: vec![
                parameter("bank", AbiType::U8),
                parameter("offset", AbiType::U8),
                parameter("source", AbiType::ConstU8Pointer),
                parameter("length", AbiType::U8),
            ],
            result: None,
            effects: vec![IntrinsicEffect::ReadMainMemory, IntrinsicEffect::WriteXram],
            volatile: true,
        },
        IntrinsicDefinition {
            operation: Intrinsic::XramCopyToMain,
            symbol: "waterbear_vmu_xram_copy_to_main".into(),
            description: "Copy bytes from one contiguous implemented XRAM range into WebAssembly memory; the range must not cross an unimplemented address.".into(),
            parameters: vec![
                parameter("destination", AbiType::MutU8Pointer),
                parameter("bank", AbiType::U8),
                parameter("offset", AbiType::U8),
                parameter("length", AbiType::U8),
            ],
            result: None,
            effects: vec![IntrinsicEffect::ReadXram, IntrinsicEffect::WriteMainMemory],
            volatile: true,
        },
    ]);

    definitions
}

pub fn resolve(symbol: &str) -> Option<Intrinsic> {
    definitions()
        .into_iter()
        .find(|definition| definition.symbol == symbol)
        .map(|definition| definition.operation)
}

/// Supplies abstract-analysis semantics for registered VMU imports.
#[derive(Clone, Copy, Debug, Default)]
pub struct IntrinsicCallResolver;

impl CallResolver for IntrinsicCallResolver {
    fn resolve(&self, module: &Module, function: FunctionId) -> CallSummary {
        let FunctionKind::Import(imported) = &module.funcs.get(function).kind else {
            return CallSummary::conservative(module, function);
        };
        let import = module.imports.get(imported.import);
        let Some(definition) = resolve(&import.name).and_then(Intrinsic::definition) else {
            return CallSummary::conservative(module, function);
        };
        let ty = module.funcs.get(function).ty();
        let parameters = module.types.params(ty).to_vec();
        let results = module.types.results(ty).to_vec();
        if parameters.len() != definition.parameters.len() {
            return CallSummary::conservative(module, function);
        }

        let arguments = definition
            .parameters
            .iter()
            .map(|parameter| match parameter.ty {
                AbiType::U8 => ArgumentSummary {
                    observed_bits: Some(u8::MAX.into()),
                    pointer: PointerBehavior::NotAPointer,
                },
                AbiType::ConstU8Pointer => ArgumentSummary {
                    observed_bits: None,
                    pointer: PointerBehavior::ReadOnly,
                },
                AbiType::MutU8Pointer => ArgumentSummary {
                    observed_bits: None,
                    pointer: PointerBehavior::ReadWrite,
                },
            })
            .collect();

        CallSummary {
            parameters,
            results,
            arguments,
            result_pointers: definition
                .result
                .iter()
                .map(|_| ResultPointerSummary::default())
                .collect(),
            memory: intrinsic_memory_effects(&definition.effects),
        }
    }
}

fn intrinsic_memory_effects(effects: &[IntrinsicEffect]) -> MemoryEffects {
    let reads = effects
        .iter()
        .any(|effect| matches!(effect, IntrinsicEffect::ReadMainMemory));
    let writes = effects
        .iter()
        .any(|effect| matches!(effect, IntrinsicEffect::WriteMainMemory));
    match (reads, writes) {
        (false, false) => MemoryEffects::None,
        (true, false) => MemoryEffects::Read,
        (false, true) => MemoryEffects::Write,
        (true, true) => MemoryEffects::ReadWrite,
    }
}

#[derive(Clone, Copy)]
enum SfrOperation {
    Read,
    Write,
    BitRead,
    BitWrite,
    BitToggle,
}

fn sfr_definition(sfr: &SfrDefinition, operation: SfrOperation) -> IntrinsicDefinition {
    let (intrinsic, suffix, parameters, result, effect, action) = match operation {
        SfrOperation::Read => (
            Intrinsic::SfrRead(sfr.register),
            "read",
            Vec::new(),
            Some(AbiType::U8),
            IntrinsicEffect::ReadSfr(sfr.register),
            "Read",
        ),
        SfrOperation::Write => (
            Intrinsic::SfrWrite(sfr.register),
            "write",
            vec![parameter("value", AbiType::U8)],
            None,
            IntrinsicEffect::WriteSfr(sfr.register),
            "Write",
        ),
        SfrOperation::BitRead => (
            Intrinsic::SfrBitRead(sfr.register),
            "bit_read",
            vec![parameter("bit", AbiType::U8)],
            Some(AbiType::U8),
            IntrinsicEffect::ReadSfr(sfr.register),
            "Read a bit from",
        ),
        SfrOperation::BitWrite => (
            Intrinsic::SfrBitWrite(sfr.register),
            "bit_write",
            vec![
                parameter("bit", AbiType::U8),
                parameter("value", AbiType::U8),
            ],
            None,
            IntrinsicEffect::WriteSfr(sfr.register),
            "Write a bit in",
        ),
        SfrOperation::BitToggle => (
            Intrinsic::SfrBitToggle(sfr.register),
            "bit_toggle",
            vec![parameter("bit", AbiType::U8)],
            None,
            IntrinsicEffect::WriteSfr(sfr.register),
            "Toggle a bit in",
        ),
    };

    IntrinsicDefinition {
        operation: intrinsic,
        symbol: format!("waterbear_vmu_sfr_{}_{suffix}", sfr.symbol),
        description: format!("{action} the VMU {}.", sfr.description),
        parameters,
        result,
        effects: vec![effect],
        volatile: true,
    }
}

fn parameter(name: &'static str, ty: AbiType) -> IntrinsicParameter {
    IntrinsicParameter { name, ty }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use walrus::{Module, ValType};

    use super::{Intrinsic, IntrinsicCallResolver, Sfr, definitions, resolve};
    use crate::wasm::analysis::calls::{CallResolver, MemoryEffects, PointerBehavior};

    #[test]
    fn registered_symbols_are_unique_valid_c_identifiers() {
        let definitions = definitions();
        let symbols = definitions
            .iter()
            .map(|definition| definition.symbol.as_str())
            .collect::<BTreeSet<_>>();

        assert_eq!(symbols.len(), definitions.len());
        assert!(symbols.iter().all(|symbol| is_c_identifier(symbol)));

        let operations = definitions
            .iter()
            .map(|definition| definition.operation)
            .collect::<BTreeSet<_>>();
        assert_eq!(operations.len(), definitions.len());
    }

    #[test]
    fn symbols_resolve_to_the_registered_operation() {
        assert_eq!(
            resolve("waterbear_vmu_sfr_p7_read"),
            Some(Intrinsic::SfrRead(Sfr::Port7))
        );
        assert_eq!(
            resolve("waterbear_vmu_xram_write"),
            Some(Intrinsic::XramWrite)
        );
        assert_eq!(
            resolve("waterbear_vmu_sfr_p3_write"),
            Some(Intrinsic::SfrWrite(Sfr::Port3))
        );
        assert_eq!(resolve("waterbear_vmu_missing"), None);
    }

    #[test]
    fn supplies_pointer_and_memory_semantics_for_analysis() {
        let mut module = Module::default();
        let ty = module.types.add(
            &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            &[],
        );
        let (function, _) = module.add_import_func("env", "waterbear_vmu_xram_copy_to_main", ty);

        let summary = IntrinsicCallResolver.resolve(&module, function);

        assert_eq!(summary.arguments[0].pointer, PointerBehavior::ReadWrite);
        assert!(
            summary.arguments[1..]
                .iter()
                .all(|argument| argument.pointer == PointerBehavior::NotAPointer)
        );
        assert_eq!(summary.memory, MemoryEffects::Write);
    }

    fn is_c_identifier(value: &str) -> bool {
        let mut characters = value.bytes();
        characters
            .next()
            .is_some_and(|character| character == b'_' || character.is_ascii_alphabetic())
            && characters.all(|character| character == b'_' || character.is_ascii_alphanumeric())
    }
}
