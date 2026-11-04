//! Translation from analyzed WebAssembly into the VMU high-level IR.
//!
//! Walrus keeps WebAssembly's structured control flow, so this pass can lower
//! it directly without first building a control-flow graph. The operand stack
//! exists only while translating a function; values that cross statement
//! boundaries are named explicitly in the resulting tree.

use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt,
};

use walrus::{
    ConstExpr, DataKind, ExportItem, FunctionId, FunctionKind, GlobalId, GlobalKind, LocalFunction,
    LocalId, Module, ValType,
    ir::{BinaryOp, ExtendedLoad, Instr, InstrSeqId, LoadKind, StoreKind, UnaryOp, Value},
};

use super::{
    analysis::{
        AnalysisDb,
        backward::BitDemand,
        calls::PointerBehavior,
        frame::{FrameObject, FrameObjectId, FrameOffset},
        index::{InstructionId, InstructionKey, ValueId},
        shape::sequence_signature,
    },
    config::{InterruptConfig, InterruptSlot},
    hir::{
        self, BitWidth, Block, CompareOp, Constant, ConversionOp, Expression, ExpressionKind,
        FloatBinaryOp, FloatUnaryOp, FloatWidth, IntegerBinaryOp, IntegerConstant, IntegerUnaryOp,
        MemoryAccess, Parameter, Place, Signedness, Statement, Static, StaticInitializer,
        SwitchCase, Type,
    },
    hir_tail_calls,
    intrinsics::{self, AbiType, Intrinsic},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LowerError {
    EntryNotFound(String),
    EntryIsNotLocal(String),
    InterruptNotFound {
        slot: InterruptSlot,
        function: String,
    },
    InterruptIsNotLocal {
        slot: InterruptSlot,
        function: String,
    },
    InterruptIsEntry {
        slot: InterruptSlot,
        function: String,
    },
    InvalidInterruptSignature {
        slot: InterruptSlot,
        function: String,
    },
    UnsupportedGlobal(String),
    UnresolvedCall {
        function: String,
        callee: String,
    },
    InvalidFunction {
        function: String,
        message: String,
    },
    UnsupportedInstruction {
        function: String,
        instruction: String,
    },
}

impl fmt::Display for LowerError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EntryNotFound(entry) => {
                write!(formatter, "entry function {entry:?} was not found")
            }
            Self::EntryIsNotLocal(entry) => {
                write!(
                    formatter,
                    "entry function {entry:?} is imported, not defined locally"
                )
            }
            Self::InterruptNotFound { slot, function } => write!(
                formatter,
                "interrupt {} refers to missing exported function {function:?}",
                slot.name()
            ),
            Self::InterruptIsNotLocal { slot, function } => write!(
                formatter,
                "interrupt {} refers to imported function {function:?}, not a local definition",
                slot.name()
            ),
            Self::InterruptIsEntry { slot, function } => write!(
                formatter,
                "interrupt {} cannot use entry function {function:?}",
                slot.name()
            ),
            Self::InvalidInterruptSignature { slot, function } => write!(
                formatter,
                "interrupt {} function {function:?} must have signature () -> ()",
                slot.name()
            ),
            Self::UnsupportedGlobal(global) => {
                write!(
                    formatter,
                    "imported global {global:?} cannot be represented in VMU HIR"
                )
            }
            Self::UnresolvedCall { function, callee } => write!(
                formatter,
                "function {function:?} calls {callee:?}, which has no definition or registered intrinsic"
            ),
            Self::InvalidFunction { function, message } => {
                write!(formatter, "cannot lower function {function:?}: {message}")
            }
            Self::UnsupportedInstruction {
                function,
                instruction,
            } => write!(
                formatter,
                "function {function:?} contains an instruction not yet supported by VMU HIR: {instruction}"
            ),
        }
    }
}

impl Error for LowerError {}

/// Lowers every locally defined function and selects `entry` as the VMU main
/// function. Every call must resolve to either another local function or a
/// registered VMU intrinsic.
pub fn lower(
    module: &Module,
    analyses: &AnalysisDb,
    entry: &str,
) -> Result<hir::Program, LowerError> {
    lower_with_interrupts(module, analyses, entry, &InterruptConfig::default())
}

pub fn lower_with_interrupts(
    module: &Module,
    analyses: &AnalysisDb,
    entry: &str,
    interrupts: &InterruptConfig,
) -> Result<hir::Program, LowerError> {
    let names = Names::new(module);
    let entry = find_entry(module, &names, entry)?;
    let interrupt_functions = resolve_interrupts(module, entry, interrupts)?;
    let signatures = Signatures::new(module, analyses);
    let data = StaticData::new(module, &names);

    let mut functions = BTreeMap::new();
    for function in module.funcs.iter() {
        if matches!(function.kind, FunctionKind::Local(_)) {
            let lowered =
                FunctionLowerer::new(module, analyses, &names, &signatures, &data, function.id())
                    .lower()?;
            functions.insert(function.id(), lowered);
        }
    }

    let main = functions
        .remove(&entry)
        .ok_or_else(|| LowerError::EntryIsNotLocal(names.function(entry).to_owned()))?;
    let functions = module
        .funcs
        .iter()
        .filter_map(|function| functions.remove(&function.id()))
        .collect();

    let omitted_stack_pointer = stack_pointer_can_be_omitted(module, analyses)
        .then_some(analyses.frames.stack_pointer)
        .flatten();
    let mut program = hir::Program {
        main,
        interrupt_handlers: hir::InterruptHandlers::default(),
        functions,
        statics: lower_statics(module, analyses, &names, &data, omitted_stack_pointer)?,
    };
    hir_tail_calls::canonicalize(&mut program);
    super::hir_rotate::canonicalize(&mut program);
    install_interrupt_wrappers(&mut program, &names, &interrupt_functions);
    Ok(program)
}

fn resolve_interrupts(
    module: &Module,
    entry: FunctionId,
    config: &InterruptConfig,
) -> Result<BTreeMap<InterruptSlot, FunctionId>, LowerError> {
    let mut functions = BTreeMap::new();
    for slot in InterruptSlot::ALL {
        let Some(name) = config.get(slot) else {
            continue;
        };
        let Some(function) = module.exports.iter().find_map(|export| {
            (export.name == name)
                .then_some(export.item)
                .and_then(|item| match item {
                    ExportItem::Function(function) => Some(function),
                    _ => None,
                })
        }) else {
            return Err(LowerError::InterruptNotFound {
                slot,
                function: name.to_owned(),
            });
        };
        if !matches!(module.funcs.get(function).kind, FunctionKind::Local(_)) {
            return Err(LowerError::InterruptIsNotLocal {
                slot,
                function: name.to_owned(),
            });
        }
        if function == entry {
            return Err(LowerError::InterruptIsEntry {
                slot,
                function: name.to_owned(),
            });
        }
        let (parameters, results) = module.types.params_results(module.funcs.get(function).ty());
        if !parameters.is_empty() || !results.is_empty() {
            return Err(LowerError::InvalidInterruptSignature {
                slot,
                function: name.to_owned(),
            });
        }
        functions.insert(slot, function);
    }
    Ok(functions)
}

fn install_interrupt_wrappers(
    program: &mut hir::Program,
    names: &Names,
    functions: &BTreeMap<InterruptSlot, FunctionId>,
) {
    let mut used = program
        .functions
        .iter()
        .map(|function| function.name.clone())
        .chain(std::iter::once(program.main.name.clone()))
        .collect();
    for (slot, function) in functions {
        let target = names.function(*function).to_owned();
        let wrapper_name =
            unique_name(&format!("__waterbear_interrupt_{}", slot.name()), &mut used);
        let wrapper = hir::Function {
            name: wrapper_name,
            parameters: Vec::new(),
            result: Type::Unit,
            body: Block {
                statements: vec![
                    Statement::Call {
                        destination: None,
                        function: target,
                        arguments: Vec::new(),
                    },
                    Statement::Return(None),
                ],
            },
        };
        set_interrupt_handler(&mut program.interrupt_handlers, *slot, wrapper);
    }
}

fn set_interrupt_handler(
    handlers: &mut hir::InterruptHandlers,
    slot: InterruptSlot,
    function: hir::Function,
) {
    let destination = match slot {
        InterruptSlot::External0 => &mut handlers.external_0,
        InterruptSlot::External1 => &mut handlers.external_1,
        InterruptSlot::External2OrTimer0Low => &mut handlers.external_2_or_timer_0_low,
        InterruptSlot::External3OrBaseTimer => &mut handlers.external_3_or_base_timer,
        InterruptSlot::Timer0High => &mut handlers.timer_0_high,
        InterruptSlot::Timer1 => &mut handlers.timer_1,
        InterruptSlot::Serial0 => &mut handlers.serial_0,
        InterruptSlot::Serial1 => &mut handlers.serial_1,
        InterruptSlot::DreamcastInterface => &mut handlers.dreamcast_interface,
        InterruptSlot::Port3 => &mut handlers.port_3,
    };
    *destination = Some(function);
}

fn find_entry(module: &Module, names: &Names, entry: &str) -> Result<FunctionId, LowerError> {
    if let Some(function) = module.exports.iter().find_map(|export| {
        (export.name == entry)
            .then_some(export.item)
            .and_then(|item| match item {
                ExportItem::Function(function) => Some(function),
                _ => None,
            })
    }) {
        return match module.funcs.get(function).kind {
            FunctionKind::Local(_) => Ok(function),
            _ => Err(LowerError::EntryIsNotLocal(entry.to_owned())),
        };
    }

    let mut matches = module.funcs.iter().filter(|function| {
        function.name.as_deref() == Some(entry) || names.function(function.id()) == entry
    });
    let Some(function) = matches.next() else {
        return Err(LowerError::EntryNotFound(entry.to_owned()));
    };
    if !matches!(function.kind, FunctionKind::Local(_)) {
        return Err(LowerError::EntryIsNotLocal(entry.to_owned()));
    }
    Ok(function.id())
}

#[derive(Clone)]
struct FunctionSignature {
    parameters: Vec<Type>,
    results: Vec<Type>,
}

struct Signatures {
    functions: BTreeMap<FunctionId, FunctionSignature>,
}

impl Signatures {
    fn new(module: &Module, analyses: &AnalysisDb) -> Self {
        let functions = module
            .funcs
            .iter()
            .map(|function| {
                let (parameters, results) = module.types.params_results(function.ty());
                let parameters = match analyses.index.functions.get(&function.id()) {
                    Some(info) => {
                        let summary = &analyses.calls[&function.id()];
                        info.parameters
                            .iter()
                            .copied()
                            .zip(&summary.arguments)
                            .map(|(value, argument)| {
                                if argument.pointer == PointerBehavior::NotAPointer {
                                    analyzed_type(analyses, value, false)
                                } else {
                                    Type::Pointer
                                }
                            })
                            .collect()
                    }
                    None => parameters.iter().copied().map(wasm_type).collect(),
                };
                let results = results
                    .iter()
                    .copied()
                    .enumerate()
                    .map(|(result, ty)| {
                        analyzed_function_result_type(analyses, function.id(), result, ty)
                    })
                    .collect();
                (
                    function.id(),
                    FunctionSignature {
                        parameters,
                        results,
                    },
                )
            })
            .collect();
        Self { functions }
    }

    fn get(&self, function: FunctionId) -> &FunctionSignature {
        &self.functions[&function]
    }
}

struct Names {
    functions: BTreeMap<FunctionId, String>,
    globals: BTreeMap<GlobalId, String>,
    data: BTreeMap<walrus::DataId, String>,
}

impl Names {
    fn new(module: &Module) -> Self {
        let mut used = BTreeSet::new();
        let functions = module
            .funcs
            .iter()
            .map(|function| {
                let preferred = function
                    .name
                    .clone()
                    .or_else(|| match &function.kind {
                        FunctionKind::Import(imported) => {
                            Some(module.imports.get(imported.import).name.clone())
                        }
                        _ => None,
                    })
                    .or_else(|| {
                        module
                            .exports
                            .get_exported_func(function.id())
                            .map(|export| export.name.clone())
                    })
                    .unwrap_or_else(|| format!("function{}", function.id().index()));
                (function.id(), unique_name(&preferred, &mut used))
            })
            .collect();

        let mut used = BTreeSet::new();
        let globals = module
            .globals
            .iter()
            .map(|global| {
                let preferred = global
                    .name
                    .clone()
                    .or_else(|| match global.kind {
                        GlobalKind::Import(import) => Some(module.imports.get(import).name.clone()),
                        GlobalKind::Local(_) => None,
                    })
                    .or_else(|| {
                        module
                            .exports
                            .get_exported_global(global.id())
                            .map(|export| export.name.clone())
                    })
                    .unwrap_or_else(|| format!("global{}", global.id().index()));
                (global.id(), unique_name(&preferred, &mut used))
            })
            .collect();

        let mut used = BTreeSet::new();
        let data = module
            .data
            .iter()
            .map(|data| {
                let preferred = data
                    .name
                    .clone()
                    .unwrap_or_else(|| format!("data{}", data.id().index()));
                (data.id(), unique_name(&preferred, &mut used))
            })
            .collect();

        Self {
            functions,
            globals,
            data,
        }
    }

    fn function(&self, function: FunctionId) -> &str {
        &self.functions[&function]
    }

    fn global(&self, global: GlobalId) -> &str {
        &self.globals[&global]
    }

    fn data(&self, data: walrus::DataId) -> &str {
        &self.data[&data]
    }
}

fn unique_name(preferred: &str, used: &mut BTreeSet<String>) -> String {
    let base = identifier(preferred);
    let mut candidate = base.clone();
    let mut suffix = 2;
    while !used.insert(candidate.clone()) {
        candidate = format!("{base}_{suffix}");
        suffix += 1;
    }
    candidate
}

fn identifier(value: &str) -> String {
    let mut result: String = value
        .trim_start_matches('$')
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || character == '_' {
                character
            } else {
                '_'
            }
        })
        .collect();
    if result.is_empty() {
        result.push_str("unnamed");
    }
    if result.starts_with(|character: char| character.is_ascii_digit()) {
        result.insert(0, '_');
    }
    result
}

#[derive(Clone)]
struct DataRange {
    start: u64,
    end: u64,
    name: String,
}

struct StaticData {
    ranges: Vec<DataRange>,
}

impl StaticData {
    fn new(module: &Module, names: &Names) -> Self {
        let ranges = module
            .data
            .iter()
            .filter_map(|data| {
                let DataKind::Active { offset, .. } = data.kind else {
                    return None;
                };
                let start = const_expr_u64(module, offset)?;
                Some(DataRange {
                    start,
                    end: start.saturating_add(data.value.len() as u64),
                    name: names.data(data.id()).to_owned(),
                })
            })
            .collect();
        Self { ranges }
    }

    fn address(&self, address: u64) -> Option<(String, usize)> {
        self.ranges.iter().find_map(|range| {
            (address >= range.start && address < range.end).then(|| {
                (
                    range.name.clone(),
                    address.saturating_sub(range.start) as usize,
                )
            })
        })
    }
}

fn lower_statics(
    module: &Module,
    analyses: &AnalysisDb,
    names: &Names,
    _data: &StaticData,
    omitted_global: Option<GlobalId>,
) -> Result<Vec<Static>, LowerError> {
    let byte = bits_type(8);
    let mut statics = module
        .data
        .iter()
        .map(|data| Static {
            name: names.data(data.id()).to_owned(),
            ty: Type::Array {
                element: Box::new(byte.clone()),
                length: data.value.len(),
            },
            mutable: analyses.data_mutability.is_mutable(data.id()),
            initializer: StaticInitializer::Bytes(data.value.clone()),
        })
        .collect::<Vec<_>>();

    for global in module.globals.iter() {
        if Some(global.id()) == omitted_global {
            continue;
        }
        let GlobalKind::Local(initializer) = global.kind else {
            return Err(LowerError::UnsupportedGlobal(
                names.global(global.id()).to_owned(),
            ));
        };
        statics.push(Static {
            name: names.global(global.id()).to_owned(),
            ty: wasm_type(global.ty),
            mutable: global.mutable,
            initializer: StaticInitializer::Constant(lower_const_expr(module, names, initializer)?),
        });
    }
    Ok(statics)
}

fn lower_const_expr(
    module: &Module,
    names: &Names,
    expression: ConstExpr,
) -> Result<Constant, LowerError> {
    Ok(match expression {
        ConstExpr::Value(value) => full_width_constant(value),
        ConstExpr::Global(global) => {
            let global = module.globals.get(global);
            let GlobalKind::Local(initializer) = global.kind else {
                return Err(LowerError::UnsupportedGlobal(
                    names.global(global.id()).to_owned(),
                ));
            };
            return lower_const_expr(module, names, initializer);
        }
        ConstExpr::RefNull(_) => Constant::NullPointer,
        ConstExpr::RefFunc(function) => {
            Constant::FunctionAddress(names.function(function).to_owned())
        }
    })
}

fn const_expr_u64(module: &Module, expression: ConstExpr) -> Option<u64> {
    match expression {
        ConstExpr::Value(Value::I32(value)) => Some(value as u32 as u64),
        ConstExpr::Value(Value::I64(value)) => Some(value as u64),
        ConstExpr::Global(global) => match module.globals.get(global).kind {
            GlobalKind::Local(expression) => const_expr_u64(module, expression),
            GlobalKind::Import(_) => None,
        },
        _ => None,
    }
}

#[derive(Clone)]
struct FrameLayout {
    name: String,
    ty: Type,
    start: i64,
    size: u64,
    scalar: bool,
}

#[derive(Clone)]
enum ControlKind {
    Function,
    Block {
        label: String,
        destinations: Vec<(Place, Type)>,
    },
    Loop {
        label: String,
        destinations: Vec<(Place, Type)>,
    },
}

#[derive(Clone)]
struct ControlTarget {
    sequences: Vec<InstrSeqId>,
    types: Vec<Type>,
    kind: ControlKind,
}

#[derive(Default)]
struct Flow {
    fallthrough: bool,
    branches: BTreeSet<InstrSeqId>,
}

impl Flow {
    fn continuing() -> Self {
        Self {
            fallthrough: true,
            branches: BTreeSet::new(),
        }
    }

    fn stopped() -> Self {
        Self::default()
    }

    fn branch(target: InstrSeqId, conditional: bool) -> Self {
        Self {
            fallthrough: conditional,
            branches: [target].into_iter().collect(),
        }
    }
}

struct FunctionLowerer<'a> {
    module: &'a Module,
    analyses: &'a AnalysisDb,
    names: &'a Names,
    signatures: &'a Signatures,
    data: &'a StaticData,
    function: FunctionId,
    body: &'a LocalFunction,
    function_name: String,
    local_names: BTreeMap<LocalId, String>,
    local_types: BTreeMap<LocalId, Type>,
    frames: BTreeMap<FrameObjectId, FrameLayout>,
    frame_lifted: bool,
    shadow_locals: BTreeSet<LocalId>,
    used_names: BTreeSet<String>,
    next_temporary: usize,
    next_label: usize,
}

impl<'a> FunctionLowerer<'a> {
    fn new(
        module: &'a Module,
        analyses: &'a AnalysisDb,
        names: &'a Names,
        signatures: &'a Signatures,
        data: &'a StaticData,
        function: FunctionId,
    ) -> Self {
        let body = module.funcs.get(function).kind.unwrap_local();
        let signature = signatures.get(function);
        let mut used_names = BTreeSet::new();
        let mut local_names = BTreeMap::new();
        let mut local_types = BTreeMap::new();
        let parameter_types = body
            .args
            .iter()
            .copied()
            .zip(signature.parameters.iter().cloned())
            .collect::<BTreeMap<_, _>>();

        for local in analyses.index.functions[&function].locals.keys().copied() {
            let preferred = module
                .locals
                .get(local)
                .name
                .clone()
                .unwrap_or_else(|| format!("local{}", local.index()));
            local_names.insert(local, unique_name(&preferred, &mut used_names));
            local_types.insert(
                local,
                parameter_types
                    .get(&local)
                    .cloned()
                    .unwrap_or_else(|| analyzed_local_type(module, analyses, function, local)),
            );
        }

        let frame_lifted = frame_can_be_fully_lifted(module, analyses, function);
        let frames = frame_layouts(module, analyses, function, &mut used_names);
        let shadow_locals = if frame_lifted {
            shadow_address_locals(module, analyses, function)
        } else {
            BTreeSet::new()
        };
        Self {
            module,
            analyses,
            names,
            signatures,
            data,
            function,
            body,
            function_name: names.function(function).to_owned(),
            local_names,
            local_types,
            frames,
            frame_lifted,
            shadow_locals,
            used_names,
            next_temporary: 0,
            next_label: 0,
        }
    }

    fn lower(mut self) -> Result<hir::Function, LowerError> {
        if !self.frame_lifted && self.uses_wasm_stack_pointer() {
            let frame = &self.analyses.frames.functions[&self.function];
            let reason = if frame.address_escapes {
                "a frame address escapes"
            } else if !frame.dynamic_accesses.is_empty() {
                "it contains dynamic accesses that have not been assigned to an abstract object"
            } else {
                "its stack-pointer operations could not be represented as abstract objects"
            };
            return self.invalid(format!(
                "WebAssembly shadow-stack frame could not be abstracted into HIR because {reason}"
            ));
        }

        let signature = self.signatures.get(self.function).clone();
        let parameters = self
            .body
            .args
            .iter()
            .copied()
            .zip(signature.parameters.iter().cloned())
            .map(|(local, ty)| Parameter {
                name: self.local_name(local).to_owned(),
                ty,
            })
            .collect();

        let mut statements = Vec::new();
        for (&local, name) in &self.local_names {
            if self.body.args.contains(&local) || self.shadow_locals.contains(&local) {
                continue;
            }
            let ty = self.local_types[&local].clone();
            statements.push(Statement::Declare {
                name: name.clone(),
                ty: ty.clone(),
                mutable: true,
                initializer: zero_expression(&ty),
            });
        }
        for frame in self.frames.values() {
            statements.push(Statement::Declare {
                name: frame.name.clone(),
                ty: frame.ty.clone(),
                mutable: true,
                initializer: None,
            });
        }

        let entry = self.body.entry_block();
        let controls = vec![ControlTarget {
            sequences: vec![entry],
            types: signature.results.clone(),
            kind: ControlKind::Function,
        }];
        let mut stack = Vec::new();
        let flow = self.lower_sequence(entry, 0, &mut stack, &mut statements, &controls)?;
        if flow.fallthrough {
            let results = self.sequence_results(&mut stack, 0, signature.results.len())?;
            statements.push(Statement::Return(return_value(results, &signature.results)));
        }

        Ok(hir::Function {
            name: self.function_name.clone(),
            parameters,
            result: result_type(&signature.results),
            body: Block { statements },
        })
    }

    fn lower_sequence(
        &mut self,
        sequence: InstrSeqId,
        base: usize,
        stack: &mut Vec<Expression>,
        statements: &mut Vec<Statement>,
        controls: &[ControlTarget],
    ) -> Result<Flow, LowerError> {
        let mut flow = Flow::continuing();
        let instructions = &self.body.block(sequence).instrs;
        for (index, (instruction, _)) in instructions.iter().enumerate() {
            if !flow.fallthrough {
                break;
            }
            let instruction_id = self.analyses.index.instruction_id(InstructionKey {
                function: self.function,
                sequence,
                index,
            });
            let step =
                self.lower_instruction(instruction_id, instruction, stack, statements, controls)?;
            flow.branches.extend(step.branches);
            flow.fallthrough = step.fallthrough;
        }

        if flow.fallthrough {
            let (_, results) = sequence_signature(self.module, self.body, sequence);
            if stack.len() != base + results.len() {
                return self.invalid(format!(
                    "sequence {:?} ended with {} stack values, expected {}",
                    sequence,
                    stack.len().saturating_sub(base),
                    results.len()
                ));
            }
        }
        Ok(flow)
    }

    fn lower_instruction(
        &mut self,
        instruction_id: InstructionId,
        instruction: &Instr,
        stack: &mut Vec<Expression>,
        statements: &mut Vec<Statement>,
        controls: &[ControlTarget],
    ) -> Result<Flow, LowerError> {
        use Instr::*;

        match instruction {
            Block(operation) => {
                self.lower_block(instruction_id, operation.seq, stack, statements, controls)
            }
            Loop(operation) => {
                self.lower_loop(instruction_id, operation.seq, stack, statements, controls)
            }
            IfElse(operation) => self.lower_if(
                instruction_id,
                operation.consequent,
                operation.alternative,
                stack,
                statements,
                controls,
            ),
            Const(operation) => {
                let output = self.output_value(instruction_id, 0)?;
                stack.push(self.lower_constant(operation.value, output));
                Ok(Flow::continuing())
            }
            LocalGet(operation) => {
                let output = self.output_value(instruction_id, 0)?;
                if self.frame_lifted && self.value_is_shadow_address(output) {
                    stack.push(self.shadow_address_expression(output));
                    return Ok(Flow::continuing());
                }
                let storage_ty = self.local_types[&operation.local].clone();
                let analyzed = if storage_ty == Type::Pointer {
                    Type::Pointer
                } else {
                    analyzed_type(self.analyses, output, self.value_is_boolean(instruction_id))
                };
                let expression = coerce(
                    Expression {
                        ty: storage_ty,
                        kind: ExpressionKind::Read(Place::Variable(
                            self.local_name(operation.local).to_owned(),
                        )),
                    },
                    &analyzed,
                );
                stack.push(self.bind_expression(statements, "value", expression));
                Ok(Flow::continuing())
            }
            LocalSet(operation) => {
                let value = self.pop(stack, instruction_id)?;
                if self.frame_lifted && self.input_is_shadow_address(instruction_id, 0) {
                    return Ok(Flow::continuing());
                }
                let ty = self.local_types[&operation.local].clone();
                statements.push(Statement::Assign {
                    destination: Place::Variable(self.local_name(operation.local).to_owned()),
                    value: coerce(value, &ty),
                });
                Ok(Flow::continuing())
            }
            LocalTee(operation) => {
                let value = self.pop(stack, instruction_id)?;
                let output = self.output_value(instruction_id, 0)?;
                if self.frame_lifted && self.value_is_shadow_address(output) {
                    stack.push(self.shadow_address_expression(output));
                    return Ok(Flow::continuing());
                }
                let local_ty = self.local_types[&operation.local].clone();
                let output_ty = analyzed_type(self.analyses, output, false);
                let bound_ty = common_storage_type(local_ty.clone(), output_ty.clone(), &value.ty);
                let value = self.bind_expression(statements, "value", coerce(value, &bound_ty));
                statements.push(Statement::Assign {
                    destination: Place::Variable(self.local_name(operation.local).to_owned()),
                    value: coerce(value.clone(), &local_ty),
                });
                stack.push(coerce(value, &output_ty));
                Ok(Flow::continuing())
            }
            GlobalGet(operation) => {
                let output = self.output_value(instruction_id, 0)?;
                if self.frame_lifted && Some(operation.global) == self.analyses.frames.stack_pointer
                {
                    stack.push(self.shadow_address_expression(output));
                    return Ok(Flow::continuing());
                }
                let storage_ty = wasm_type(self.module.globals.get(operation.global).ty);
                let expression = coerce(
                    Expression {
                        ty: storage_ty,
                        kind: ExpressionKind::Read(Place::Static(
                            self.names.global(operation.global).to_owned(),
                        )),
                    },
                    &analyzed_type(self.analyses, output, false),
                );
                stack.push(self.bind_expression(statements, "value", expression));
                Ok(Flow::continuing())
            }
            GlobalSet(operation) => {
                let value = self.pop(stack, instruction_id)?;
                if self.frame_lifted
                    && Some(operation.global) == self.analyses.frames.stack_pointer
                    && self.input_is_shadow_address(instruction_id, 0)
                {
                    return Ok(Flow::continuing());
                }
                let ty = wasm_type(self.module.globals.get(operation.global).ty);
                statements.push(Statement::Assign {
                    destination: Place::Static(self.names.global(operation.global).to_owned()),
                    value: coerce(value, &ty),
                });
                Ok(Flow::continuing())
            }
            Binop(operation) => {
                let [left, right] = self.pop_array(stack, instruction_id)?;
                let output = self.output_value(instruction_id, 0)?;
                if self.frame_lifted && self.value_is_shadow_address(output) {
                    stack.push(self.shadow_address_expression(output));
                    return Ok(Flow::continuing());
                }
                let expression = self.lower_binary(operation.op, left, right, output)?;
                stack.push(if binary_can_trap(operation.op) {
                    self.bind_expression(statements, "value", expression)
                } else {
                    expression
                });
                Ok(Flow::continuing())
            }
            Unop(operation) => {
                let operand = self.pop(stack, instruction_id)?;
                let output = self.output_value(instruction_id, 0)?;
                let expression = self.lower_unary(operation.op, operand, output)?;
                stack.push(if unary_can_trap(operation.op) {
                    self.bind_expression(statements, "value", expression)
                } else {
                    expression
                });
                Ok(Flow::continuing())
            }
            Select(_) => {
                let [when_true, when_false, condition] = self.pop_array(stack, instruction_id)?;
                let output = self.output_value(instruction_id, 0)?;
                let ty = analyzed_type(self.analyses, output, false);
                stack.push(Expression {
                    ty: ty.clone(),
                    kind: ExpressionKind::Select {
                        when_true: Box::new(coerce(when_true, &ty)),
                        when_false: Box::new(coerce(when_false, &ty)),
                        condition: Box::new(as_condition(condition)),
                    },
                });
                Ok(Flow::continuing())
            }
            Load(operation) => {
                let address = self.pop(stack, instruction_id)?;
                let output = self.output_value(instruction_id, 0)?;
                let storage_ty = load_storage_type(operation.kind);
                let loaded = if let Some(place) = self.frame_place(instruction_id) {
                    let expression = Expression {
                        ty: storage_ty.clone(),
                        kind: ExpressionKind::Read(place),
                    };
                    self.bind_expression(statements, "load", expression)
                } else {
                    let address =
                        self.require_abstract_pointer(address, instruction_id, "load address")?;
                    let name = self.declare_temporary(statements, "load", storage_ty.clone());
                    statements.push(Statement::Load {
                        destination: Some(Place::Variable(name.clone())),
                        address: add_memory_offset(address, operation.arg.offset),
                        ty: storage_ty.clone(),
                        access: memory_access(operation.kind.atomic()),
                    });
                    read_variable(name, storage_ty.clone())
                };
                stack.push(finish_load(
                    loaded,
                    operation.kind,
                    analyzed_type(self.analyses, output, false),
                ));
                Ok(Flow::continuing())
            }
            Store(operation) => {
                let [address, value] = self.pop_array(stack, instruction_id)?;
                let ty = store_storage_type(operation.kind);
                let value = coerce(value, &ty);
                if let Some(destination) = self.frame_place(instruction_id) {
                    statements.push(Statement::Assign { destination, value });
                } else {
                    let address =
                        self.require_abstract_pointer(address, instruction_id, "store address")?;
                    statements.push(Statement::Store {
                        address: add_memory_offset(address, operation.arg.offset),
                        value,
                        ty,
                        access: memory_access(operation.kind.atomic()),
                    });
                }
                Ok(Flow::continuing())
            }
            Call(operation) => {
                self.lower_call(instruction_id, operation.func, stack, statements)?;
                Ok(Flow::continuing())
            }
            Br(operation) => {
                statements.extend(self.lower_branch(operation.block, stack, controls)?);
                Ok(Flow::branch(operation.block, false))
            }
            BrIf(operation) => {
                let condition = self.pop(stack, instruction_id)?;
                let body = hir::Block {
                    statements: self.lower_branch(operation.block, stack, controls)?,
                };
                statements.push(Statement::If {
                    condition: as_condition(condition),
                    then_body: body,
                    else_body: None,
                });
                Ok(Flow::branch(operation.block, true))
            }
            BrTable(operation) => {
                let selector = self.pop(stack, instruction_id)?;
                let width = expression_bit_width(&selector).unwrap_or(32);
                let cases = operation
                    .blocks
                    .iter()
                    .enumerate()
                    .map(|(value, target)| {
                        Ok(SwitchCase {
                            value: integer_constant(width, value as u128),
                            body: hir::Block {
                                statements: self.lower_branch(*target, stack, controls)?,
                            },
                        })
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                let default = hir::Block {
                    statements: self.lower_branch(operation.default, stack, controls)?,
                };
                statements.push(Statement::Switch {
                    value: selector,
                    cases,
                    default,
                });
                let mut flow = Flow::stopped();
                flow.branches.extend(operation.blocks.iter().copied());
                flow.branches.insert(operation.default);
                Ok(flow)
            }
            Return(_) => {
                let result_types = self.signatures.get(self.function).results.clone();
                let values = self.pop_values(stack, result_types.len(), instruction_id)?;
                statements.push(Statement::Return(return_value(values, &result_types)));
                Ok(Flow::stopped())
            }
            Drop(_) => {
                let value = self.pop(stack, instruction_id)?;
                let _ = self.bind_expression(statements, "discard", value);
                Ok(Flow::continuing())
            }
            Unreachable(_) => {
                statements.push(Statement::Trap);
                Ok(Flow::stopped())
            }
            RefNull(_) => {
                stack.push(Expression {
                    ty: Type::Pointer,
                    kind: ExpressionKind::Constant(Constant::NullPointer),
                });
                Ok(Flow::continuing())
            }
            RefFunc(operation) => {
                stack.push(Expression {
                    ty: Type::Pointer,
                    kind: ExpressionKind::Constant(Constant::FunctionAddress(
                        self.names.function(operation.func).to_owned(),
                    )),
                });
                Ok(Flow::continuing())
            }
            RefIsNull(_) => {
                let value = self.pop(stack, instruction_id)?;
                stack.push(Expression {
                    ty: Type::Bool,
                    kind: ExpressionKind::Compare {
                        op: CompareOp::Equal,
                        left: Box::new(value),
                        right: Box::new(Expression {
                            ty: Type::Pointer,
                            kind: ExpressionKind::Constant(Constant::NullPointer),
                        }),
                    },
                });
                Ok(Flow::continuing())
            }
            ReturnCall(operation) => {
                self.lower_return_call(instruction_id, operation.func, stack, statements)?;
                Ok(Flow::stopped())
            }
            ReturnCallIndirect(_) => self.unsupported(instruction),
            _ => self.unsupported(instruction),
        }
    }

    fn lower_block(
        &mut self,
        instruction: InstructionId,
        sequence: InstrSeqId,
        stack: &mut Vec<Expression>,
        statements: &mut Vec<Statement>,
        controls: &[ControlTarget],
    ) -> Result<Flow, LowerError> {
        let (parameters, results) = sequence_signature(self.module, self.body, sequence);
        let parameters = self.pop_values(stack, parameters.len(), instruction)?;
        let base = stack.len();
        let outputs = self.output_types(instruction, results.len())?;
        let destinations = self.declare_results(statements, "block_result", &outputs);
        let label = self.fresh_label("block");
        let target = ControlTarget {
            sequences: vec![sequence],
            types: outputs.clone(),
            kind: ControlKind::Block {
                label: label.clone(),
                destinations: destinations.clone(),
            },
        };
        let mut nested_controls = controls.to_vec();
        nested_controls.push(target);
        let mut nested_stack = stack.clone();
        nested_stack.extend(parameters);
        let mut body = Vec::new();
        let mut flow = self.lower_sequence(
            sequence,
            base,
            &mut nested_stack,
            &mut body,
            &nested_controls,
        )?;
        if flow.fallthrough {
            let values = self.sequence_results(&mut nested_stack, base, results.len())?;
            assign_results(&mut body, &destinations, values);
        }
        let exited = flow.branches.remove(&sequence);
        statements.push(Statement::Block {
            label: Some(label),
            body: Block { statements: body },
        });
        stack.extend(result_reads(&destinations));
        flow.fallthrough |= exited;
        Ok(flow)
    }

    fn lower_loop(
        &mut self,
        instruction: InstructionId,
        sequence: InstrSeqId,
        stack: &mut Vec<Expression>,
        statements: &mut Vec<Statement>,
        controls: &[ControlTarget],
    ) -> Result<Flow, LowerError> {
        let (parameter_types, result_types) = sequence_signature(self.module, self.body, sequence);
        let parameters = self.pop_values(stack, parameter_types.len(), instruction)?;
        let base = stack.len();
        let loop_types = parameter_types
            .into_iter()
            .map(wasm_type)
            .collect::<Vec<_>>();
        let loop_variables = loop_types
            .iter()
            .cloned()
            .zip(parameters)
            .map(|(ty, initializer)| {
                let name = self.fresh_name("loop_value");
                statements.push(Statement::Declare {
                    name: name.clone(),
                    ty: ty.clone(),
                    mutable: true,
                    initializer: Some(coerce(initializer, &ty)),
                });
                (Place::Variable(name), ty)
            })
            .collect::<Vec<_>>();
        let outputs = self.output_types(instruction, result_types.len())?;
        let results = self.declare_results(statements, "loop_result", &outputs);
        let label = self.fresh_label("loop");
        let target = ControlTarget {
            sequences: vec![sequence],
            types: loop_types,
            kind: ControlKind::Loop {
                label: label.clone(),
                destinations: loop_variables.clone(),
            },
        };
        let mut nested_controls = controls.to_vec();
        nested_controls.push(target);
        let mut nested_stack = stack.clone();
        nested_stack.extend(result_reads(&loop_variables));
        let mut body = Vec::new();
        let mut flow = self.lower_sequence(
            sequence,
            base,
            &mut nested_stack,
            &mut body,
            &nested_controls,
        )?;
        if flow.fallthrough {
            let values = self.sequence_results(&mut nested_stack, base, result_types.len())?;
            assign_results(&mut body, &results, values);
        }
        flow.branches.remove(&sequence);
        statements.push(Statement::Loop {
            label: Some(label),
            body: Block { statements: body },
        });
        stack.extend(result_reads(&results));
        Ok(flow)
    }

    fn lower_if(
        &mut self,
        instruction: InstructionId,
        consequent: InstrSeqId,
        alternative: InstrSeqId,
        stack: &mut Vec<Expression>,
        statements: &mut Vec<Statement>,
        controls: &[ControlTarget],
    ) -> Result<Flow, LowerError> {
        let condition = self.pop(stack, instruction)?;
        let (parameter_types, result_types) =
            sequence_signature(self.module, self.body, consequent);
        let parameters = self.pop_values(stack, parameter_types.len(), instruction)?;
        let base = stack.len();
        let outputs = self.output_types(instruction, result_types.len())?;
        let results = self.declare_results(statements, "if_result", &outputs);
        let label = self.fresh_label("if");
        let target = ControlTarget {
            sequences: vec![consequent, alternative],
            types: outputs.clone(),
            kind: ControlKind::Block {
                label: label.clone(),
                destinations: results.clone(),
            },
        };
        let mut nested_controls = controls.to_vec();
        nested_controls.push(target);

        let mut then_stack = stack.clone();
        then_stack.extend(parameters.iter().cloned());
        let mut then_statements = Vec::new();
        let mut then_flow = self.lower_sequence(
            consequent,
            base,
            &mut then_stack,
            &mut then_statements,
            &nested_controls,
        )?;
        if then_flow.fallthrough {
            let values = self.sequence_results(&mut then_stack, base, result_types.len())?;
            assign_results(&mut then_statements, &results, values);
        }

        let mut else_stack = stack.clone();
        else_stack.extend(parameters);
        let mut else_statements = Vec::new();
        let mut else_flow = self.lower_sequence(
            alternative,
            base,
            &mut else_stack,
            &mut else_statements,
            &nested_controls,
        )?;
        if else_flow.fallthrough {
            let values = self.sequence_results(&mut else_stack, base, result_types.len())?;
            assign_results(&mut else_statements, &results, values);
        }

        let then_exited_consequent = then_flow.branches.remove(&consequent);
        let then_exited_alternative = then_flow.branches.remove(&alternative);
        let else_exited_consequent = else_flow.branches.remove(&consequent);
        let else_exited_alternative = else_flow.branches.remove(&alternative);
        let then_exited = then_exited_consequent || then_exited_alternative;
        let else_exited = else_exited_consequent || else_exited_alternative;
        let fallthrough =
            then_flow.fallthrough || else_flow.fallthrough || then_exited || else_exited;
        let mut branches = then_flow.branches;
        branches.extend(else_flow.branches);
        let conditional = Statement::If {
            condition: as_condition(condition),
            then_body: Block {
                statements: then_statements,
            },
            else_body: Some(Block {
                statements: else_statements,
            }),
        };
        statements.push(Statement::Block {
            label: Some(label),
            body: Block {
                statements: vec![conditional],
            },
        });
        stack.extend(result_reads(&results));
        Ok(Flow {
            fallthrough,
            branches,
        })
    }

    fn lower_call(
        &mut self,
        instruction: InstructionId,
        function: FunctionId,
        stack: &mut Vec<Expression>,
        statements: &mut Vec<Statement>,
    ) -> Result<(), LowerError> {
        let signature = self.signatures.get(function).clone();
        let intrinsic = self.resolve_intrinsic(function)?;
        let arguments = self.pop_values(stack, signature.parameters.len(), instruction)?;
        let arguments = self.recover_frame_arguments(instruction, arguments);

        if let Some(operation) = intrinsic {
            let definition = operation.definition().expect("registered intrinsic");
            self.validate_intrinsic_signature(function, &definition)?;
            let parameter_types = definition
                .parameters
                .iter()
                .map(|parameter| abi_type(parameter.ty))
                .collect::<Vec<_>>();
            let arguments =
                self.coerce_call_arguments(arguments, &parameter_types, &definition.symbol)?;
            let result_types = definition
                .result
                .map(abi_type)
                .into_iter()
                .collect::<Vec<_>>();
            let destination = self.declare_call_result(statements, "intrinsic", &result_types);
            statements.push(Statement::Intrinsic {
                destination: destination.as_ref().map(|(place, _)| place.clone()),
                operation,
                arguments,
            });
            self.push_call_results(instruction, destination, result_types, stack)?;
            return Ok(());
        }

        let arguments = self.coerce_call_arguments(
            arguments,
            &signature.parameters,
            self.names.function(function),
        )?;
        let destination = self.declare_call_result(statements, "call", &signature.results);
        statements.push(Statement::Call {
            destination: destination.as_ref().map(|(place, _)| place.clone()),
            function: self.names.function(function).to_owned(),
            arguments,
        });
        self.push_call_results(instruction, destination, signature.results, stack)
    }

    fn lower_return_call(
        &mut self,
        instruction: InstructionId,
        function: FunctionId,
        stack: &mut Vec<Expression>,
        statements: &mut Vec<Statement>,
    ) -> Result<(), LowerError> {
        let signature = self.signatures.get(function).clone();
        let caller_results = self.signatures.get(self.function).results.clone();
        if signature.results != caller_results {
            return self.invalid(format!(
                "tail call to {:?} returns {:?}, but the caller returns {:?}",
                self.names.function(function),
                signature.results,
                caller_results,
            ));
        }

        let intrinsic = self.resolve_intrinsic(function)?;
        let arguments = self.pop_values(stack, signature.parameters.len(), instruction)?;
        let arguments = self.recover_frame_arguments(instruction, arguments);

        if let Some(operation) = intrinsic {
            let definition = operation.definition().expect("registered intrinsic");
            self.validate_intrinsic_signature(function, &definition)?;
            let parameter_types = definition
                .parameters
                .iter()
                .map(|parameter| abi_type(parameter.ty))
                .collect::<Vec<_>>();
            let arguments =
                self.coerce_call_arguments(arguments, &parameter_types, &definition.symbol)?;
            let result_types = definition
                .result
                .map(abi_type)
                .into_iter()
                .collect::<Vec<_>>();
            let destination = self.declare_call_result(statements, "intrinsic", &result_types);
            statements.push(Statement::Intrinsic {
                destination: destination.as_ref().map(|(place, _)| place.clone()),
                operation,
                arguments,
            });
            let values = destination
                .map(|(place, ty)| {
                    vec![Expression {
                        ty,
                        kind: ExpressionKind::Read(place),
                    }]
                })
                .unwrap_or_default();
            statements.push(Statement::Return(return_value(values, &caller_results)));
            return Ok(());
        }

        statements.push(Statement::TailCall {
            function: self.names.function(function).to_owned(),
            arguments: self.coerce_call_arguments(
                arguments,
                &signature.parameters,
                self.names.function(function),
            )?,
        });
        Ok(())
    }

    fn resolve_intrinsic(&self, function: FunctionId) -> Result<Option<Intrinsic>, LowerError> {
        match &self.module.funcs.get(function).kind {
            FunctionKind::Import(imported) => {
                let import = self.module.imports.get(imported.import);
                let callee = format!("{}.{}", import.module, import.name);
                intrinsics::resolve(&import.name).map(Some).ok_or_else(|| {
                    LowerError::UnresolvedCall {
                        function: self.function_name.clone(),
                        callee,
                    }
                })
            }
            FunctionKind::Local(_) => Ok(None),
            FunctionKind::Uninitialized(_) => Err(LowerError::UnresolvedCall {
                function: self.function_name.clone(),
                callee: self.names.function(function).to_owned(),
            }),
        }
    }

    fn validate_intrinsic_signature(
        &self,
        function: FunctionId,
        definition: &intrinsics::IntrinsicDefinition,
    ) -> Result<(), LowerError> {
        let (parameters, results) = self
            .module
            .types
            .params_results(self.module.funcs.get(function).ty());
        let expected_parameters = definition
            .parameters
            .iter()
            .map(|parameter| intrinsic_wasm_type(parameter.ty))
            .collect::<Vec<_>>();
        let expected_results = definition
            .result
            .map(intrinsic_wasm_type)
            .into_iter()
            .collect::<Vec<_>>();

        if parameters == expected_parameters && results == expected_results {
            Ok(())
        } else {
            self.invalid(format!(
                "imported intrinsic {:?} has WebAssembly signature {:?} -> {:?}, expected {:?} -> {:?}",
                definition.symbol,
                parameters,
                results,
                expected_parameters,
                expected_results,
            ))
        }
    }

    fn declare_call_result(
        &mut self,
        statements: &mut Vec<Statement>,
        prefix: &str,
        results: &[Type],
    ) -> Option<(Place, Type)> {
        if results.is_empty() {
            return None;
        }
        let ty = result_type(results);
        let name = self.declare_temporary(statements, prefix, ty.clone());
        Some((Place::Variable(name), ty))
    }

    fn push_call_results(
        &self,
        instruction: InstructionId,
        destination: Option<(Place, Type)>,
        physical_types: Vec<Type>,
        stack: &mut Vec<Expression>,
    ) -> Result<(), LowerError> {
        let outputs = &self.analyses.index.instructions[instruction.0].outputs;
        if outputs.len() != physical_types.len() {
            return self.invalid(format!(
                "call has {} HIR results but {} WebAssembly results",
                physical_types.len(),
                outputs.len()
            ));
        }
        let Some((place, aggregate_ty)) = destination else {
            return Ok(());
        };
        let aggregate = Expression {
            ty: aggregate_ty,
            kind: ExpressionKind::Read(place),
        };
        for (index, (&output, physical_ty)) in outputs.iter().zip(physical_types).enumerate() {
            let value = if outputs.len() == 1 {
                aggregate.clone()
            } else {
                Expression {
                    ty: physical_ty,
                    kind: ExpressionKind::ExtractElement {
                        aggregate: Box::new(aggregate.clone()),
                        index,
                    },
                }
            };
            stack.push(coerce(value, &analyzed_type(self.analyses, output, false)));
        }
        Ok(())
    }

    fn lower_branch(
        &mut self,
        target: InstrSeqId,
        stack: &[Expression],
        controls: &[ControlTarget],
    ) -> Result<Vec<Statement>, LowerError> {
        let control = controls
            .iter()
            .rev()
            .find(|control| control.sequences.contains(&target))
            .ok_or_else(|| LowerError::InvalidFunction {
                function: self.function_name.clone(),
                message: format!("branch target {target:?} is not an enclosing control"),
            })?;
        if stack.len() < control.types.len() {
            return self.invalid("branch operands exceed stack height".into());
        }
        let values = stack[stack.len() - control.types.len()..].to_vec();
        Ok(match &control.kind {
            ControlKind::Function => vec![Statement::Return(return_value(values, &control.types))],
            ControlKind::Block {
                label,
                destinations,
            } => {
                let mut statements = Vec::new();
                assign_results(&mut statements, destinations, values);
                statements.push(Statement::Break {
                    label: Some(label.clone()),
                });
                statements
            }
            ControlKind::Loop {
                label,
                destinations,
            } => {
                let mut statements = Vec::new();
                let values = values
                    .into_iter()
                    .map(|value| self.bind_expression(&mut statements, "branch_value", value))
                    .collect();
                assign_results(&mut statements, destinations, values);
                statements.push(Statement::Continue {
                    label: Some(label.clone()),
                });
                statements
            }
        })
    }

    fn lower_constant(&self, value: Value, output: ValueId) -> Expression {
        if let Value::I32(value) = value
            && let Some((name, offset)) = self.data.address(value as u32 as u64)
        {
            return Expression {
                ty: Type::Pointer,
                kind: ExpressionKind::Constant(Constant::StaticAddress { name, offset }),
            };
        }
        if let Value::I64(value) = value
            && let Some((name, offset)) = self.data.address(value as u64)
        {
            return Expression {
                ty: Type::Pointer,
                kind: ExpressionKind::Constant(Constant::StaticAddress { name, offset }),
            };
        }

        let ty = analyzed_type(self.analyses, output, false);
        let constant = match (&ty, value) {
            (Type::Bool, Value::I32(value)) => Constant::Bool(value != 0),
            (Type::Bool, Value::I64(value)) => Constant::Bool(value != 0),
            (Type::Bits(width), Value::I32(value)) => Constant::Integer(
                IntegerConstant::new(*width, truncate_bits(value as u32 as u128, width.get()))
                    .unwrap(),
            ),
            (Type::Bits(width), Value::I64(value)) => Constant::Integer(
                IntegerConstant::new(*width, truncate_bits(value as u64 as u128, width.get()))
                    .unwrap(),
            ),
            (Type::Bits(width), Value::V128(value)) => Constant::Integer(
                IntegerConstant::new(*width, truncate_bits(value, width.get())).unwrap(),
            ),
            (_, Value::F32(value)) => Constant::Float32(value.to_bits()),
            (_, Value::F64(value)) => Constant::Float64(value.to_bits()),
            _ => full_width_constant(value),
        };
        Expression {
            ty,
            kind: ExpressionKind::Constant(constant),
        }
    }

    fn lower_binary(
        &self,
        operation: BinaryOp,
        left: Expression,
        right: Expression,
        output: ValueId,
    ) -> Result<Expression, LowerError> {
        use BinaryOp::*;
        let integer_width = binary_integer_width(operation);
        let target = analyzed_type(
            self.analyses,
            output,
            matches!(
                operation,
                I32Eq
                    | I32Ne
                    | I32LtS
                    | I32LtU
                    | I32GtS
                    | I32GtU
                    | I32LeS
                    | I32LeU
                    | I32GeS
                    | I32GeU
                    | I64Eq
                    | I64Ne
                    | I64LtS
                    | I64LtU
                    | I64GtS
                    | I64GtU
                    | I64LeS
                    | I64LeU
                    | I64GeS
                    | I64GeU
                    | F32Eq
                    | F32Ne
                    | F32Lt
                    | F32Gt
                    | F32Le
                    | F32Ge
                    | F64Eq
                    | F64Ne
                    | F64Lt
                    | F64Gt
                    | F64Le
                    | F64Ge
            ),
        );

        let compare = match operation {
            I32Eq | I64Eq => Some(CompareOp::Equal),
            I32Ne | I64Ne => Some(CompareOp::NotEqual),
            I32LtS | I64LtS => Some(CompareOp::LessThan(Signedness::Signed)),
            I32LtU | I64LtU => Some(CompareOp::LessThan(Signedness::Unsigned)),
            I32GtS | I64GtS => Some(CompareOp::GreaterThan(Signedness::Signed)),
            I32GtU | I64GtU => Some(CompareOp::GreaterThan(Signedness::Unsigned)),
            I32LeS | I64LeS => Some(CompareOp::LessThanOrEqual(Signedness::Signed)),
            I32LeU | I64LeU => Some(CompareOp::LessThanOrEqual(Signedness::Unsigned)),
            I32GeS | I64GeS => Some(CompareOp::GreaterThanOrEqual(Signedness::Signed)),
            I32GeU | I64GeU => Some(CompareOp::GreaterThanOrEqual(Signedness::Unsigned)),
            F32Eq | F64Eq => Some(CompareOp::FloatEqual),
            F32Ne | F64Ne => Some(CompareOp::FloatNotEqual),
            F32Lt | F64Lt => Some(CompareOp::FloatLessThan),
            F32Gt | F64Gt => Some(CompareOp::FloatGreaterThan),
            F32Le | F64Le => Some(CompareOp::FloatLessThanOrEqual),
            F32Ge | F64Ge => Some(CompareOp::FloatGreaterThanOrEqual),
            _ => None,
        };
        if let Some(operation) = compare {
            return Ok(Expression {
                ty: Type::Bool,
                kind: ExpressionKind::Compare {
                    op: operation,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            });
        }

        let integer = match operation {
            I32Add | I64Add => Some(IntegerBinaryOp::Add),
            I32Sub | I64Sub => Some(IntegerBinaryOp::Subtract),
            I32Mul | I64Mul => Some(IntegerBinaryOp::Multiply),
            I32DivS | I64DivS => Some(IntegerBinaryOp::Divide(Signedness::Signed)),
            I32DivU | I64DivU => Some(IntegerBinaryOp::Divide(Signedness::Unsigned)),
            I32RemS | I64RemS => Some(IntegerBinaryOp::Remainder(Signedness::Signed)),
            I32RemU | I64RemU => Some(IntegerBinaryOp::Remainder(Signedness::Unsigned)),
            I32And | I64And => Some(IntegerBinaryOp::And),
            I32Or | I64Or => Some(IntegerBinaryOp::Or),
            I32Xor | I64Xor => Some(IntegerBinaryOp::Xor),
            I32Shl | I64Shl => Some(IntegerBinaryOp::ShiftLeft),
            I32ShrS | I64ShrS => Some(IntegerBinaryOp::ShiftRight(Signedness::Signed)),
            I32ShrU | I64ShrU => Some(IntegerBinaryOp::ShiftRight(Signedness::Unsigned)),
            I32Rotl | I64Rotl => Some(IntegerBinaryOp::RotateLeft),
            I32Rotr | I64Rotr => Some(IntegerBinaryOp::RotateRight),
            _ => None,
        };
        if let Some(operation) = integer {
            if matches!(operation, IntegerBinaryOp::Add) {
                if left.ty == Type::Pointer {
                    return Ok(pointer_offset(left, right));
                }
                if right.ty == Type::Pointer {
                    return Ok(pointer_offset(right, left));
                }
            }
            if matches!(operation, IntegerBinaryOp::Subtract) && left.ty == Type::Pointer {
                let offset = Expression {
                    ty: right.ty.clone(),
                    kind: ExpressionKind::IntegerUnary {
                        op: IntegerUnaryOp::Negate,
                        operand: Box::new(right),
                    },
                };
                return Ok(pointer_offset(left, offset));
            }
            let full_ty = bits_type(integer_width);
            let operation_ty =
                narrowed_integer_operation_type(operation, &target, &left, &right, &full_ty);
            let expression = Expression {
                ty: operation_ty.clone(),
                kind: ExpressionKind::IntegerBinary {
                    op: operation,
                    left: Box::new(coerce_integer_operand(left, &operation_ty, operation)),
                    right: Box::new(coerce(right, &operation_ty)),
                },
            };
            return Ok(coerce(expression, &target));
        }

        let float = match operation {
            F32Add | F64Add => Some(FloatBinaryOp::Add),
            F32Sub | F64Sub => Some(FloatBinaryOp::Subtract),
            F32Mul | F64Mul => Some(FloatBinaryOp::Multiply),
            F32Div | F64Div => Some(FloatBinaryOp::Divide),
            F32Min | F64Min => Some(FloatBinaryOp::Minimum),
            F32Max | F64Max => Some(FloatBinaryOp::Maximum),
            F32Copysign | F64Copysign => Some(FloatBinaryOp::CopySign),
            _ => None,
        };
        if let Some(operation) = float {
            return Ok(Expression {
                ty: target,
                kind: ExpressionKind::FloatBinary {
                    op: operation,
                    left: Box::new(left),
                    right: Box::new(right),
                },
            });
        }

        self.unsupported(&Instr::Binop(walrus::ir::Binop { op: operation }))
    }

    fn lower_unary(
        &self,
        operation: UnaryOp,
        operand: Expression,
        output: ValueId,
    ) -> Result<Expression, LowerError> {
        use UnaryOp::*;
        let target = analyzed_type(self.analyses, output, matches!(operation, I32Eqz | I64Eqz));
        if matches!(operation, I32Eqz | I64Eqz) {
            return Ok(Expression {
                ty: Type::Bool,
                kind: ExpressionKind::Compare {
                    op: CompareOp::Equal,
                    right: Box::new(integer_expression(
                        expression_bit_width(&operand).unwrap_or(32),
                        0,
                    )),
                    left: Box::new(operand),
                },
            });
        }

        let integer = match operation {
            I32Clz | I64Clz => Some(IntegerUnaryOp::CountLeadingZeros),
            I32Ctz | I64Ctz => Some(IntegerUnaryOp::CountTrailingZeros),
            I32Popcnt | I64Popcnt => Some(IntegerUnaryOp::PopulationCount),
            _ => None,
        };
        if let Some(operation) = integer {
            let full_ty = if matches!(
                operation,
                IntegerUnaryOp::CountLeadingZeros
                    | IntegerUnaryOp::CountTrailingZeros
                    | IntegerUnaryOp::PopulationCount
            ) && matches!(target, Type::Bits(width) if width.get() > 32)
            {
                bits_type(64)
            } else {
                wasm_type(self.analyses.index.values[output.0].ty.unwrap())
            };
            return Ok(coerce(
                Expression {
                    ty: full_ty,
                    kind: ExpressionKind::IntegerUnary {
                        op: operation,
                        operand: Box::new(operand),
                    },
                },
                &target,
            ));
        }

        let float = match operation {
            F32Abs | F64Abs => Some(FloatUnaryOp::AbsoluteValue),
            F32Neg | F64Neg => Some(FloatUnaryOp::Negate),
            F32Ceil | F64Ceil => Some(FloatUnaryOp::Ceiling),
            F32Floor | F64Floor => Some(FloatUnaryOp::Floor),
            F32Trunc | F64Trunc => Some(FloatUnaryOp::Truncate),
            F32Nearest | F64Nearest => Some(FloatUnaryOp::Nearest),
            F32Sqrt | F64Sqrt => Some(FloatUnaryOp::SquareRoot),
            _ => None,
        };
        if let Some(operation) = float {
            return Ok(Expression {
                ty: target,
                kind: ExpressionKind::FloatUnary {
                    op: operation,
                    operand: Box::new(operand),
                },
            });
        }

        let conversion = match operation {
            I32WrapI64 => Some(ConversionOp::Truncate),
            I64ExtendSI32 => Some(ConversionOp::SignExtend),
            I64ExtendUI32 => Some(ConversionOp::ZeroExtend),
            I32TruncSF32 | I32TruncSF64 | I64TruncSF32 | I64TruncSF64 => {
                Some(ConversionOp::FloatToInteger {
                    signedness: Signedness::Signed,
                    saturating: false,
                })
            }
            I32TruncUF32 | I32TruncUF64 | I64TruncUF32 | I64TruncUF64 => {
                Some(ConversionOp::FloatToInteger {
                    signedness: Signedness::Unsigned,
                    saturating: false,
                })
            }
            I32TruncSSatF32 | I32TruncSSatF64 | I64TruncSSatF32 | I64TruncSSatF64 => {
                Some(ConversionOp::FloatToInteger {
                    signedness: Signedness::Signed,
                    saturating: true,
                })
            }
            I32TruncUSatF32 | I32TruncUSatF64 | I64TruncUSatF32 | I64TruncUSatF64 => {
                Some(ConversionOp::FloatToInteger {
                    signedness: Signedness::Unsigned,
                    saturating: true,
                })
            }
            F32ConvertSI32 | F32ConvertSI64 | F64ConvertSI32 | F64ConvertSI64 => {
                Some(ConversionOp::IntegerToFloat(Signedness::Signed))
            }
            F32ConvertUI32 | F32ConvertUI64 | F64ConvertUI32 | F64ConvertUI64 => {
                Some(ConversionOp::IntegerToFloat(Signedness::Unsigned))
            }
            F32DemoteF64 => Some(ConversionOp::DemoteFloat),
            F64PromoteF32 => Some(ConversionOp::PromoteFloat),
            I32ReinterpretF32 | I64ReinterpretF64 | F32ReinterpretI32 | F64ReinterpretI64 => {
                Some(ConversionOp::Reinterpret)
            }
            _ => None,
        };
        if let Some(operation) = conversion {
            return Ok(Expression {
                ty: target,
                kind: ExpressionKind::Convert {
                    op: operation,
                    operand: Box::new(operand),
                },
            });
        }

        let signed_source_width = match operation {
            I32Extend8S | I64Extend8S => Some(8),
            I32Extend16S | I64Extend16S => Some(16),
            I64Extend32S => Some(32),
            _ => None,
        };
        if let Some(width) = signed_source_width {
            let truncated = coerce(operand, &bits_type(width));
            return Ok(Expression {
                ty: target,
                kind: ExpressionKind::Convert {
                    op: ConversionOp::SignExtend,
                    operand: Box::new(truncated),
                },
            });
        }

        self.unsupported(&Instr::Unop(walrus::ir::Unop { op: operation }))
    }

    fn output_value(
        &self,
        instruction: InstructionId,
        index: usize,
    ) -> Result<ValueId, LowerError> {
        self.analyses.index.instructions[instruction.0]
            .outputs
            .get(index)
            .copied()
            .ok_or_else(|| LowerError::InvalidFunction {
                function: self.function_name.clone(),
                message: format!("instruction {instruction:?} has no result {index}"),
            })
    }

    fn output_types(
        &self,
        instruction: InstructionId,
        count: usize,
    ) -> Result<Vec<Type>, LowerError> {
        let outputs = &self.analyses.index.instructions[instruction.0].outputs;
        if outputs.len() != count {
            return self.invalid(format!(
                "instruction {instruction:?} has {} results, expected {count}",
                outputs.len()
            ));
        }
        Ok(outputs
            .iter()
            .copied()
            .map(|value| analyzed_type(self.analyses, value, false))
            .collect())
    }

    fn value_is_boolean(&self, instruction: InstructionId) -> bool {
        matches!(
            self.analyses.index.instruction(self.module, instruction),
            Instr::RefIsNull(_)
                | Instr::Unop(walrus::ir::Unop {
                    op: UnaryOp::I32Eqz | UnaryOp::I64Eqz,
                })
                | Instr::Binop(walrus::ir::Binop {
                    op: BinaryOp::I32Eq
                        | BinaryOp::I32Ne
                        | BinaryOp::I32LtS
                        | BinaryOp::I32LtU
                        | BinaryOp::I32GtS
                        | BinaryOp::I32GtU
                        | BinaryOp::I32LeS
                        | BinaryOp::I32LeU
                        | BinaryOp::I32GeS
                        | BinaryOp::I32GeU
                        | BinaryOp::I64Eq
                        | BinaryOp::I64Ne
                        | BinaryOp::I64LtS
                        | BinaryOp::I64LtU
                        | BinaryOp::I64GtS
                        | BinaryOp::I64GtU
                        | BinaryOp::I64LeS
                        | BinaryOp::I64LeU
                        | BinaryOp::I64GeS
                        | BinaryOp::I64GeU
                        | BinaryOp::F32Eq
                        | BinaryOp::F32Ne
                        | BinaryOp::F32Lt
                        | BinaryOp::F32Gt
                        | BinaryOp::F32Le
                        | BinaryOp::F32Ge
                        | BinaryOp::F64Eq
                        | BinaryOp::F64Ne
                        | BinaryOp::F64Lt
                        | BinaryOp::F64Gt
                        | BinaryOp::F64Le
                        | BinaryOp::F64Ge,
                })
        )
    }

    fn frame_place(&self, instruction: InstructionId) -> Option<Place> {
        let access = self.analyses.frames.accesses.get(&instruction)?;
        let object = access.object?;
        let layout = self.frames.get(&object)?;
        let FrameOffset::Fixed(offset) = access.offset else {
            return None;
        };
        let relative = offset.saturating_sub(layout.start);
        if layout.scalar && relative == 0 {
            return Some(Place::Variable(layout.name.clone()));
        }
        Some(Place::ByteOffset {
            base: Box::new(Place::Variable(layout.name.clone())),
            offset: Box::new(integer_expression(32, relative as u64 as u128)),
        })
    }

    fn recover_frame_arguments(
        &self,
        instruction: InstructionId,
        arguments: Vec<Expression>,
    ) -> Vec<Expression> {
        if !self.frame_lifted {
            return arguments;
        }
        let inputs = self.analyses.addresses.instruction_inputs[instruction.0]
            .as_deref()
            .unwrap_or(&[]);
        let pointer_arguments = match self.analyses.index.instruction(self.module, instruction) {
            Instr::Call(call) => Some(&self.analyses.calls[&call.func].arguments),
            Instr::ReturnCall(call) => Some(&self.analyses.calls[&call.func].arguments),
            _ => None,
        };
        arguments
            .into_iter()
            .enumerate()
            .map(|(index, argument)| {
                if !pointer_arguments.is_some_and(|arguments| {
                    arguments.get(index).is_some_and(|argument| {
                        matches!(
                            argument.pointer,
                            PointerBehavior::ReadOnly | PointerBehavior::ReadWrite
                        )
                    })
                }) {
                    return argument;
                }
                inputs
                    .get(index)
                    .and_then(|input| input.stack_range(self.function))
                    .and_then(|(minimum, maximum)| {
                        (minimum == maximum)
                            .then(|| self.frame_address_expression(minimum))
                            .flatten()
                    })
                    .unwrap_or(argument)
            })
            .collect()
    }

    fn frame_address_expression(&self, offset: i64) -> Option<Expression> {
        let layout = self.frames.values().find(|layout| {
            let end = layout.start.saturating_add(layout.size as i64);
            offset >= layout.start && offset < end
        })?;
        let relative = offset.saturating_sub(layout.start);
        let place = if relative == 0 {
            Place::Variable(layout.name.clone())
        } else {
            Place::ByteOffset {
                base: Box::new(Place::Variable(layout.name.clone())),
                offset: Box::new(integer_expression(32, relative as u64 as u128)),
            }
        };
        Some(Expression {
            ty: Type::Pointer,
            kind: ExpressionKind::AddressOf(place),
        })
    }

    fn shadow_address_expression(&self, value: ValueId) -> Expression {
        self.analyses.addresses.values[value.0]
            .stack_range(self.function)
            .and_then(|(minimum, maximum)| {
                (minimum == maximum)
                    .then(|| self.frame_address_expression(minimum))
                    .flatten()
            })
            .unwrap_or(Expression {
                ty: Type::Pointer,
                kind: ExpressionKind::Constant(Constant::NullPointer),
            })
    }

    fn value_is_shadow_address(&self, value: ValueId) -> bool {
        self.analyses.addresses.values[value.0]
            .stack_range(self.function)
            .is_some()
    }

    fn input_is_shadow_address(&self, instruction: InstructionId, index: usize) -> bool {
        self.analyses.addresses.instruction_inputs[instruction.0]
            .as_deref()
            .and_then(|inputs| inputs.get(index))
            .and_then(|input| input.stack_range(self.function))
            .is_some()
    }

    fn uses_wasm_stack_pointer(&self) -> bool {
        let Some(stack_pointer) = self.analyses.frames.stack_pointer else {
            return false;
        };
        self.analyses.index.functions[&self.function]
            .instructions
            .iter()
            .any(|&instruction| {
                matches!(
                    self.analyses.index.instruction(self.module, instruction),
                    Instr::GlobalGet(operation) if operation.global == stack_pointer
                ) || matches!(
                    self.analyses.index.instruction(self.module, instruction),
                    Instr::GlobalSet(operation) if operation.global == stack_pointer
                )
            })
    }

    fn require_abstract_pointer(
        &self,
        expression: Expression,
        instruction: InstructionId,
        use_name: &str,
    ) -> Result<Expression, LowerError> {
        if expression.ty == Type::Pointer {
            return Ok(expression);
        }
        self.invalid(format!(
            "{use_name} at {instruction:?} is an unresolved WebAssembly linear-memory address ({:?}); addresses must resolve to an HIR object and offset",
            expression.ty,
        ))
    }

    fn coerce_call_arguments(
        &self,
        arguments: Vec<Expression>,
        parameters: &[Type],
        callee: &str,
    ) -> Result<Vec<Expression>, LowerError> {
        arguments
            .into_iter()
            .zip(parameters)
            .enumerate()
            .map(|(index, (argument, parameter))| {
                if argument.ty == Type::Pointer && *parameter != Type::Pointer {
                    return self.invalid(format!(
                        "argument {index} to {callee:?} converts an abstract pointer into {:?}",
                        parameter,
                    ));
                }
                if argument.ty != Type::Pointer && *parameter == Type::Pointer {
                    return self.invalid(format!(
                        "argument {index} to {callee:?} is an unresolved WebAssembly linear-memory address ({:?}); addresses must resolve to an HIR object and offset",
                        argument.ty,
                    ));
                }
                let argument = coerce(argument, parameter);
                if argument.ty != *parameter {
                    return self.invalid(format!(
                        "argument {index} to {callee:?} has type {:?}, expected {:?}",
                        argument.ty, parameter,
                    ));
                }
                Ok(argument)
            })
            .collect()
    }

    fn declare_results(
        &mut self,
        statements: &mut Vec<Statement>,
        prefix: &str,
        types: &[Type],
    ) -> Vec<(Place, Type)> {
        types
            .iter()
            .cloned()
            .map(|ty| {
                let name = self.declare_temporary(statements, prefix, ty.clone());
                (Place::Variable(name), ty)
            })
            .collect()
    }

    fn declare_temporary(
        &mut self,
        statements: &mut Vec<Statement>,
        prefix: &str,
        ty: Type,
    ) -> String {
        let name = self.fresh_name(prefix);
        statements.push(Statement::Declare {
            name: name.clone(),
            ty,
            mutable: true,
            initializer: None,
        });
        name
    }

    fn bind_expression(
        &mut self,
        statements: &mut Vec<Statement>,
        prefix: &str,
        expression: Expression,
    ) -> Expression {
        let name = self.fresh_name(prefix);
        let ty = expression.ty.clone();
        statements.push(Statement::Declare {
            name: name.clone(),
            ty: ty.clone(),
            mutable: false,
            initializer: Some(expression),
        });
        read_variable(name, ty)
    }

    fn fresh_name(&mut self, prefix: &str) -> String {
        loop {
            let candidate = format!("{prefix}{}", self.next_temporary);
            self.next_temporary += 1;
            if self.used_names.insert(candidate.clone()) {
                return candidate;
            }
        }
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!("{prefix}{}", self.next_label);
        self.next_label += 1;
        label
    }

    fn local_name(&self, local: LocalId) -> &str {
        &self.local_names[&local]
    }

    fn pop(
        &self,
        stack: &mut Vec<Expression>,
        instruction: InstructionId,
    ) -> Result<Expression, LowerError> {
        stack.pop().ok_or_else(|| LowerError::InvalidFunction {
            function: self.function_name.clone(),
            message: format!("operand stack underflow at {instruction:?}"),
        })
    }

    fn pop_array<const N: usize>(
        &self,
        stack: &mut Vec<Expression>,
        instruction: InstructionId,
    ) -> Result<[Expression; N], LowerError> {
        self.pop_values(stack, N, instruction)?
            .try_into()
            .map_err(|_| unreachable!())
    }

    fn pop_values(
        &self,
        stack: &mut Vec<Expression>,
        count: usize,
        instruction: InstructionId,
    ) -> Result<Vec<Expression>, LowerError> {
        let Some(start) = stack.len().checked_sub(count) else {
            return self.invalid(format!("operand stack underflow at {instruction:?}"));
        };
        Ok(stack.split_off(start))
    }

    fn sequence_results(
        &self,
        stack: &mut Vec<Expression>,
        base: usize,
        count: usize,
    ) -> Result<Vec<Expression>, LowerError> {
        if stack.len() != base + count {
            return self.invalid(format!(
                "control ended with {} stack values above its base, expected {count}",
                stack.len().saturating_sub(base)
            ));
        }
        Ok(stack.split_off(base))
    }

    fn invalid<T>(&self, message: String) -> Result<T, LowerError> {
        Err(LowerError::InvalidFunction {
            function: self.function_name.clone(),
            message,
        })
    }

    fn unsupported<T>(&self, instruction: &Instr) -> Result<T, LowerError> {
        Err(LowerError::UnsupportedInstruction {
            function: self.function_name.clone(),
            instruction: format!("{instruction:?}"),
        })
    }
}

fn frame_layouts(
    module: &Module,
    analyses: &AnalysisDb,
    function: FunctionId,
    used_names: &mut BTreeSet<String>,
) -> BTreeMap<FrameObjectId, FrameLayout> {
    let Some(frame) = analyses.frames.functions.get(&function) else {
        return BTreeMap::new();
    };
    frame
        .objects
        .iter()
        .filter(|object| !object.address_escapes)
        .map(|object| {
            let scalar_type = (!frame_object_address_is_taken(module, analyses, function, object))
                .then(|| frame_object_scalar_type(module, analyses, object))
                .flatten();
            let name = unique_name(&format!("stack{}", object.id.0), used_names);
            (
                object.id,
                FrameLayout {
                    name,
                    ty: scalar_type
                        .clone()
                        .unwrap_or(Type::OpaqueBytes(object.size as usize)),
                    start: object.start,
                    size: object.size,
                    scalar: scalar_type.is_some(),
                },
            )
        })
        .collect()
}

fn frame_object_address_is_taken(
    module: &Module,
    analyses: &AnalysisDb,
    function: FunctionId,
    object: &FrameObject,
) -> bool {
    let Some(info) = analyses.index.functions.get(&function) else {
        return false;
    };
    info.instructions.iter().copied().any(|instruction| {
        if !matches!(
            analyses.index.instruction(module, instruction),
            Instr::Call(_)
                | Instr::ReturnCall(_)
                | Instr::CallIndirect(_)
                | Instr::ReturnCallIndirect(_)
        ) {
            return false;
        }
        analyses.addresses.instruction_inputs[instruction.0]
            .as_deref()
            .unwrap_or(&[])
            .iter()
            .filter_map(|input| input.stack_range(function))
            .any(|(minimum, maximum)| {
                minimum == maximum
                    && minimum >= object.start
                    && minimum < object.start.saturating_add(object.size as i64)
            })
    })
}

fn frame_can_be_fully_lifted(module: &Module, analyses: &AnalysisDb, function: FunctionId) -> bool {
    let Some(frame) = analyses.frames.functions.get(&function) else {
        return false;
    };
    if frame.address_escapes || !frame.dynamic_accesses.is_empty() {
        return false;
    }

    let Some(info) = analyses.index.functions.get(&function) else {
        return false;
    };
    for &instruction_id in &info.instructions {
        let instruction = analyses.index.instruction(module, instruction_id);
        let inputs = analyses.addresses.instruction_inputs[instruction_id.0]
            .as_deref()
            .unwrap_or(&[]);
        if matches!(instruction, Instr::GlobalSet(set) if Some(set.global) == analyses.frames.stack_pointer)
            && !inputs
                .iter()
                .any(|input| input.stack_range(function).is_some())
        {
            return false;
        }
        for (input_index, input) in inputs.iter().enumerate() {
            let Some((minimum, maximum)) = input.stack_range(function) else {
                continue;
            };
            let allowed = match instruction {
                Instr::Load(_) | Instr::Store(_) if input_index == 0 => analyses
                    .frames
                    .accesses
                    .get(&instruction_id)
                    .is_some_and(|access| access.object.is_some()),
                Instr::LocalSet(_) | Instr::LocalTee(_) | Instr::Drop(_) => true,
                Instr::GlobalSet(set) => Some(set.global) == analyses.frames.stack_pointer,
                Instr::Binop(operation)
                    if matches!(operation.op, BinaryOp::I32Add | BinaryOp::I32Sub) =>
                {
                    analyses.index.instructions[instruction_id.0]
                        .outputs
                        .iter()
                        .any(|output| {
                            analyses.addresses.values[output.0]
                                .stack_range(function)
                                .is_some()
                        })
                }
                Instr::Call(call) if minimum == maximum => analyses.calls[&call.func]
                    .arguments
                    .get(input_index)
                    .is_some_and(|argument| {
                        matches!(
                            argument.pointer,
                            PointerBehavior::ReadOnly | PointerBehavior::ReadWrite
                        ) && frame_object_at_offset(frame, minimum).is_some()
                    }),
                _ => false,
            };
            if !allowed {
                return false;
            }
        }
    }

    true
}

fn frame_object_at_offset(
    frame: &super::analysis::frame::FunctionFrame,
    offset: i64,
) -> Option<FrameObjectId> {
    frame.objects.iter().find_map(|object| {
        let end = object.start.saturating_add(object.size as i64);
        (offset >= object.start && offset < end).then_some(object.id)
    })
}

fn shadow_address_locals(
    module: &Module,
    analyses: &AnalysisDb,
    function: FunctionId,
) -> BTreeSet<LocalId> {
    let Some(info) = analyses.index.functions.get(&function) else {
        return BTreeSet::new();
    };
    let mut locals = BTreeSet::new();
    for &instruction_id in &info.instructions {
        match analyses.index.instruction(module, instruction_id) {
            Instr::LocalGet(operation) => {
                if analyses.index.instructions[instruction_id.0]
                    .outputs
                    .iter()
                    .any(|output| {
                        analyses.addresses.values[output.0]
                            .stack_range(function)
                            .is_some()
                    })
                {
                    locals.insert(operation.local);
                }
            }
            Instr::LocalTee(operation) => {
                if analyses.index.instructions[instruction_id.0]
                    .outputs
                    .iter()
                    .any(|output| {
                        analyses.addresses.values[output.0]
                            .stack_range(function)
                            .is_some()
                    })
                {
                    locals.insert(operation.local);
                }
            }
            Instr::LocalSet(operation)
                if analyses.addresses.instruction_inputs[instruction_id.0]
                    .as_deref()
                    .and_then(|inputs| inputs.first())
                    .and_then(|input| input.stack_range(function))
                    .is_some() =>
            {
                locals.insert(operation.local);
            }
            _ => {}
        }
    }
    locals
}

fn stack_pointer_can_be_omitted(module: &Module, analyses: &AnalysisDb) -> bool {
    let Some(stack_pointer) = analyses.frames.stack_pointer else {
        return false;
    };
    if module
        .exports
        .iter()
        .any(|export| matches!(export.item, ExportItem::Global(global) if global == stack_pointer))
    {
        return false;
    }

    analyses.index.functions.iter().all(|(&function, info)| {
        let uses_stack_pointer = info.instructions.iter().any(|&instruction| {
            match analyses.index.instruction(module, instruction) {
                Instr::GlobalGet(operation) => operation.global == stack_pointer,
                Instr::GlobalSet(operation) => operation.global == stack_pointer,
                _ => false,
            }
        });
        !uses_stack_pointer || frame_can_be_fully_lifted(module, analyses, function)
    })
}

fn frame_object_scalar_type(
    module: &Module,
    analyses: &AnalysisDb,
    object: &FrameObject,
) -> Option<Type> {
    let mut types = object.accesses.iter().map(|instruction| {
        match analyses.index.instruction(module, *instruction) {
            Instr::Load(load) => load_storage_type(load.kind),
            Instr::Store(store) => store_storage_type(store.kind),
            _ => unreachable!("frame objects contain only memory accesses"),
        }
    });
    let first = types.next()?;
    let width = object.size as u32;
    (types.all(|ty| ty == first) && storage_bytes(&first) == Some(width)).then_some(first)
}

fn assign_results(
    statements: &mut Vec<Statement>,
    destinations: &[(Place, Type)],
    values: Vec<Expression>,
) {
    for ((destination, ty), value) in destinations.iter().zip(values) {
        statements.push(Statement::Assign {
            destination: destination.clone(),
            value: coerce(value, ty),
        });
    }
}

fn result_reads(results: &[(Place, Type)]) -> Vec<Expression> {
    results
        .iter()
        .map(|(place, ty)| Expression {
            ty: ty.clone(),
            kind: ExpressionKind::Read(place.clone()),
        })
        .collect()
}

fn return_value(values: Vec<Expression>, types: &[Type]) -> Option<Expression> {
    match values.len() {
        0 => None,
        1 => Some(coerce(values.into_iter().next().unwrap(), &types[0])),
        _ => Some(Expression {
            ty: Type::Tuple(types.to_vec()),
            kind: ExpressionKind::Aggregate(coerce_values(values, types)),
        }),
    }
}

fn result_type(results: &[Type]) -> Type {
    match results {
        [] => Type::Unit,
        [result] => result.clone(),
        results => Type::Tuple(results.to_vec()),
    }
}

fn analyzed_type(analyses: &AnalysisDb, value: ValueId, boolean: bool) -> Type {
    let Some(ty) = analyses.index.values[value.0].ty else {
        return Type::Unit;
    };
    let proven_width = analyses.integers.values[value.0]
        .bits()
        .map(|bits| bits.minimum_unsigned_width());
    analyzed_integer_type(
        ty,
        Some(&analyses.demanded_bits.values[value.0]),
        proven_width,
        boolean,
    )
}

fn analyzed_function_result_type(
    analyses: &AnalysisDb,
    function: FunctionId,
    result: usize,
    ty: ValType,
) -> Type {
    let demand = analyses
        .demanded_bits
        .function_results
        .get(&function)
        .and_then(|results| results.get(result));
    analyzed_integer_type(ty, demand, None, false)
}

fn analyzed_integer_type(
    ty: ValType,
    demand: Option<&BitDemand>,
    proven_width: Option<u8>,
    boolean: bool,
) -> Type {
    let original_width = match ty {
        ValType::I32 => 32,
        ValType::I64 => 64,
        ValType::V128 => 128,
        _ => return wasm_type(ty),
    };
    let demanded_width = if demand.is_none_or(|demand| demand.demanded == 0) {
        original_width
    } else {
        let demand = demand.unwrap();
        (u128::BITS - demand.demanded.leading_zeros()) as u8
    };
    let proven_width = proven_width.unwrap_or(original_width);
    let width = original_width.min(demanded_width).min(proven_width).max(1);
    if boolean && width == 1 {
        Type::Bool
    } else {
        bits_type(width)
    }
}

fn analyzed_local_type(
    module: &Module,
    analyses: &AnalysisDb,
    function: FunctionId,
    local: LocalId,
) -> Type {
    let original = wasm_type(module.locals.get(local).ty());
    let mut inferred = None;
    for &instruction in &analyses.index.functions[&function].instructions {
        let Instr::LocalGet(operation) = analyses.index.instruction(module, instruction) else {
            continue;
        };
        if operation.local != local {
            continue;
        }
        let Some(&output) = analyses.index.instructions[instruction.0].outputs.first() else {
            continue;
        };
        let ty = analyzed_type(analyses, output, false);
        inferred = Some(match inferred {
            None => ty,
            Some(previous) => common_storage_type(previous, ty, &original),
        });
    }
    inferred.unwrap_or(original)
}

fn common_storage_type(left: Type, right: Type, fallback: &Type) -> Type {
    match (left, right) {
        (Type::Bits(left), Type::Bits(right)) => Type::Bits(left.max(right)),
        (left, right) if left == right => left,
        _ => fallback.clone(),
    }
}

fn wasm_type(ty: ValType) -> Type {
    match ty {
        ValType::I32 => bits_type(32),
        ValType::I64 => bits_type(64),
        ValType::F32 => Type::Float(FloatWidth::Bits32),
        ValType::F64 => Type::Float(FloatWidth::Bits64),
        ValType::V128 => bits_type(128),
        ValType::Ref(_) => Type::Pointer,
    }
}

fn abi_type(ty: AbiType) -> Type {
    match ty {
        AbiType::U8 => bits_type(8),
        AbiType::ConstU8Pointer | AbiType::MutU8Pointer => Type::Pointer,
    }
}

fn intrinsic_wasm_type(_ty: AbiType) -> ValType {
    // All current intrinsic ABI values are represented by WebAssembly i32.
    ValType::I32
}

fn bits_type(width: u8) -> Type {
    Type::Bits(BitWidth::new(width).unwrap())
}

fn zero_expression(ty: &Type) -> Option<Expression> {
    let constant = match ty {
        Type::Bool => Constant::Bool(false),
        Type::Bits(width) => Constant::Integer(IntegerConstant::new(*width, 0).unwrap()),
        Type::Float(FloatWidth::Bits32) => Constant::Float32(0),
        Type::Float(FloatWidth::Bits64) => Constant::Float64(0),
        Type::Pointer => Constant::NullPointer,
        _ => return None,
    };
    Some(Expression {
        ty: ty.clone(),
        kind: ExpressionKind::Constant(constant),
    })
}

fn full_width_constant(value: Value) -> Constant {
    match value {
        Value::I32(value) => Constant::Integer(integer_constant(32, value as u32 as u128)),
        Value::I64(value) => Constant::Integer(integer_constant(64, value as u64 as u128)),
        Value::F32(value) => Constant::Float32(value.to_bits()),
        Value::F64(value) => Constant::Float64(value.to_bits()),
        Value::V128(value) => Constant::Integer(integer_constant(128, value)),
    }
}

fn integer_constant(width: u8, bits: u128) -> IntegerConstant {
    IntegerConstant::new(BitWidth::new(width).unwrap(), truncate_bits(bits, width)).unwrap()
}

fn integer_expression(width: u8, bits: u128) -> Expression {
    Expression {
        ty: bits_type(width),
        kind: ExpressionKind::Constant(Constant::Integer(integer_constant(width, bits))),
    }
}

fn truncate_bits(bits: u128, width: u8) -> u128 {
    if width == 128 {
        bits
    } else {
        bits & ((1_u128 << width) - 1)
    }
}

fn read_variable(name: String, ty: Type) -> Expression {
    Expression {
        ty,
        kind: ExpressionKind::Read(Place::Variable(name)),
    }
}

fn coerce_values(values: Vec<Expression>, types: &[Type]) -> Vec<Expression> {
    values
        .into_iter()
        .zip(types)
        .map(|(value, ty)| coerce(value, ty))
        .collect()
}

fn coerce(value: Expression, target: &Type) -> Expression {
    if &value.ty == target {
        return value;
    }
    let operation = match (&value.ty, target) {
        (Type::Bool, Type::Bits(_)) => Some(ConversionOp::BooleanToInteger),
        (Type::Bits(_), Type::Bool) => Some(ConversionOp::IntegerToBoolean),
        (Type::Bits(source), Type::Bits(target)) if source > target => Some(ConversionOp::Truncate),
        (Type::Bits(_), Type::Bits(_)) => Some(ConversionOp::ZeroExtend),
        _ => None,
    };
    match operation {
        Some(operation) => Expression {
            ty: target.clone(),
            kind: ExpressionKind::Convert {
                op: operation,
                operand: Box::new(value),
            },
        },
        None => value,
    }
}

fn as_condition(value: Expression) -> Expression {
    coerce(value, &Type::Bool)
}

fn pointer_offset(base: Expression, offset: Expression) -> Expression {
    Expression {
        ty: Type::Pointer,
        kind: ExpressionKind::PointerOffset {
            base: Box::new(base),
            offset: Box::new(offset),
        },
    }
}

fn add_memory_offset(address: Expression, offset: u32) -> Expression {
    if offset == 0 {
        return address;
    }
    if address.ty == Type::Pointer {
        return pointer_offset(address, integer_expression(32, u128::from(offset)));
    }
    let ty = address.ty.clone();
    let width = expression_bit_width(&address).unwrap_or(32);
    Expression {
        ty,
        kind: ExpressionKind::IntegerBinary {
            op: IntegerBinaryOp::Add,
            left: Box::new(address),
            right: Box::new(integer_expression(width, u128::from(offset))),
        },
    }
}

fn expression_bit_width(expression: &Expression) -> Option<u8> {
    match expression.ty {
        Type::Bool => Some(1),
        Type::Bits(width) => Some(width.get()),
        _ => None,
    }
}

fn binary_integer_width(operation: BinaryOp) -> u8 {
    use BinaryOp::*;
    match operation {
        I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or
        | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotl | I64Rotr => 64,
        _ => 32,
    }
}

fn binary_can_trap(operation: BinaryOp) -> bool {
    use BinaryOp::*;
    matches!(
        operation,
        I32DivS | I32DivU | I32RemS | I32RemU | I64DivS | I64DivU | I64RemS | I64RemU
    )
}

fn unary_can_trap(operation: UnaryOp) -> bool {
    use UnaryOp::*;
    matches!(
        operation,
        I32TruncSF32
            | I32TruncUF32
            | I32TruncSF64
            | I32TruncUF64
            | I64TruncSF32
            | I64TruncUF32
            | I64TruncSF64
            | I64TruncUF64
    )
}

fn coerce_integer_operand(
    value: Expression,
    target: &Type,
    operation: IntegerBinaryOp,
) -> Expression {
    let signed = matches!(
        operation,
        IntegerBinaryOp::Divide(Signedness::Signed)
            | IntegerBinaryOp::Remainder(Signedness::Signed)
            | IntegerBinaryOp::ShiftRight(Signedness::Signed)
    );
    if signed
        && matches!((&value.ty, target), (Type::Bits(source), Type::Bits(target)) if source < target)
    {
        Expression {
            ty: target.clone(),
            kind: ExpressionKind::Convert {
                op: ConversionOp::SignExtend,
                operand: Box::new(value),
            },
        }
    } else {
        coerce(value, target)
    }
}

fn narrowed_integer_operation_type(
    operation: IntegerBinaryOp,
    target: &Type,
    left: &Expression,
    right: &Expression,
    full: &Type,
) -> Type {
    let (Type::Bits(target_width), Type::Bits(full_width)) = (target, full) else {
        return full.clone();
    };
    if target_width >= full_width {
        return full.clone();
    }

    let closed_over_low_bits = matches!(
        operation,
        IntegerBinaryOp::Add
            | IntegerBinaryOp::Subtract
            | IntegerBinaryOp::Multiply
            | IntegerBinaryOp::And
            | IntegerBinaryOp::Or
            | IntegerBinaryOp::Xor
    );
    if closed_over_low_bits {
        return target.clone();
    }

    let shift = integer_constant_value(right).and_then(|value| u8::try_from(value).ok());
    match operation {
        IntegerBinaryOp::ShiftLeft if shift.is_some_and(|shift| shift < target_width.get()) => {
            target.clone()
        }
        IntegerBinaryOp::ShiftRight(Signedness::Unsigned)
            if shift.is_some_and(|shift| shift < target_width.get())
                && expression_unsigned_width(left)
                    .is_some_and(|width| width <= target_width.get()) =>
        {
            target.clone()
        }
        _ => full.clone(),
    }
}

fn expression_unsigned_width(expression: &Expression) -> Option<u8> {
    match expression.ty {
        Type::Bool => Some(1),
        Type::Bits(width) => Some(width.get()),
        _ => None,
    }
}

fn integer_constant_value(expression: &Expression) -> Option<u128> {
    match &expression.kind {
        ExpressionKind::Constant(Constant::Bool(value)) => Some(u128::from(*value)),
        ExpressionKind::Constant(Constant::Integer(value)) => Some(value.bits()),
        ExpressionKind::Convert {
            op: ConversionOp::Truncate | ConversionOp::ZeroExtend | ConversionOp::BooleanToInteger,
            operand,
        } => integer_constant_value(operand).map(|value| match expression.ty {
            Type::Bits(width) if width.get() < 128 => value & ((1_u128 << width.get()) - 1),
            _ => value,
        }),
        _ => None,
    }
}

fn load_storage_type(kind: LoadKind) -> Type {
    use LoadKind::*;
    match kind {
        I32 { .. } | I64_32 { .. } => bits_type(32),
        I64 { .. } => bits_type(64),
        F32 => Type::Float(FloatWidth::Bits32),
        F64 => Type::Float(FloatWidth::Bits64),
        V128 => bits_type(128),
        I32_8 { .. } | I64_8 { .. } => bits_type(8),
        I32_16 { .. } | I64_16 { .. } => bits_type(16),
    }
}

fn store_storage_type(kind: StoreKind) -> Type {
    use StoreKind::*;
    match kind {
        I32 { .. } | I64_32 { .. } => bits_type(32),
        I64 { .. } => bits_type(64),
        F32 => Type::Float(FloatWidth::Bits32),
        F64 => Type::Float(FloatWidth::Bits64),
        V128 => bits_type(128),
        I32_8 { .. } | I64_8 { .. } => bits_type(8),
        I32_16 { .. } | I64_16 { .. } => bits_type(16),
    }
}

fn storage_bytes(ty: &Type) -> Option<u32> {
    match ty {
        Type::Bool => Some(1),
        Type::Bits(width) => Some(u32::from(width.get()).div_ceil(8)),
        Type::Float(FloatWidth::Bits32) => Some(4),
        Type::Float(FloatWidth::Bits64) => Some(8),
        Type::Pointer => Some(4),
        _ => None,
    }
}

fn finish_load(value: Expression, kind: LoadKind, target: Type) -> Expression {
    use LoadKind::*;
    let extension = match kind {
        I32_8 { kind } | I32_16 { kind } | I64_8 { kind } | I64_16 { kind } | I64_32 { kind } => {
            Some(kind)
        }
        _ => None,
    };
    let Some(extension) = extension else {
        return coerce(value, &target);
    };
    if matches!((&value.ty, &target), (Type::Bits(source), Type::Bits(target)) if source >= target)
    {
        return coerce(value, &target);
    }
    let operation = match extension {
        ExtendedLoad::SignExtend => ConversionOp::SignExtend,
        ExtendedLoad::ZeroExtend | ExtendedLoad::ZeroExtendAtomic => ConversionOp::ZeroExtend,
    };
    Expression {
        ty: target,
        kind: ExpressionKind::Convert {
            op: operation,
            operand: Box::new(value),
        },
    }
}

fn memory_access(volatile: bool) -> MemoryAccess {
    MemoryAccess { volatile }
}

#[cfg(test)]
mod tests {
    use walrus::{
        ConstExpr, DataKind, FunctionBuilder, Module, ValType,
        ir::{
            BinaryOp, Binop, BrIf, Call, Drop, ExtendedLoad, GlobalGet, GlobalSet, IfElse, Load,
            LoadKind, LocalGet, LocalTee, Loop, MemArg, ReturnCall, Store, StoreKind, Value,
        },
    };

    use super::{LowerError, lower, lower_with_interrupts};
    use crate::wasm::{
        analysis::{analyze, analyze_with_imports},
        asm_lower,
        config::{InterruptConfig, PackageConfig},
        hir, hir_printer,
        intrinsics::IntrinsicCallResolver,
        lir_allocate, lir_lower, package,
    };

    #[test]
    fn lowers_a_scalar_function_and_selects_its_export_as_main() {
        let mut module = Module::default();
        let left = module.locals.add(ValType::I32);
        let right = module.locals.add(ValType::I32);
        let mut builder = FunctionBuilder::new(
            &mut module.types,
            &[ValType::I32, ValType::I32],
            &[ValType::I32],
        );
        builder
            .func_body()
            .instr(LocalGet { local: left })
            .instr(LocalGet { local: right })
            .instr(Binop {
                op: BinaryOp::I32Add,
            });
        let function = builder.finish(vec![left, right], &mut module.funcs);
        module.exports.add("main", function);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("main fn"));
        assert!(rendered.contains("add("));
        assert!(rendered.contains("return"));
    }

    #[test]
    fn narrows_internal_results_locals_and_low_bit_operations() {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);
        module.data.add(
            DataKind::Active {
                memory,
                offset: ConstExpr::Value(Value::I32(0)),
            },
            vec![0],
        );
        let accumulator = module.locals.add(ValType::I32);
        module.locals.get_mut(accumulator).name = Some("accumulator".into());

        let mut rotate = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);
        rotate
            .func_body()
            .i32_const(0xb4)
            .instr(LocalTee { local: accumulator })
            .i32_const(1)
            .instr(Binop {
                op: BinaryOp::I32Shl,
            })
            .instr(LocalGet { local: accumulator })
            .i32_const(128)
            .instr(Binop {
                op: BinaryOp::I32And,
            })
            .i32_const(7)
            .instr(Binop {
                op: BinaryOp::I32ShrU,
            })
            .instr(Binop {
                op: BinaryOp::I32Or,
            });
        let rotate = rotate.finish(Vec::new(), &mut module.funcs);
        module.funcs.get_mut(rotate).name = Some("rotate".into());

        let mut main = FunctionBuilder::new(&mut module.types, &[], &[]);
        main.func_body()
            .i32_const(0)
            .instr(Call { func: rotate })
            .instr(Store {
                memory,
                kind: StoreKind::I32_8 { atomic: false },
                arg: MemArg {
                    align: 1,
                    offset: 0,
                },
            });
        let main = main.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);
        assert!(program.statics[0].mutable);
        let rotate = program
            .functions
            .iter()
            .find(|function| function.name == "rotate")
            .unwrap();
        let bits_8 = hir::Type::Bits(hir::BitWidth::new(8).unwrap());

        assert!(rendered.contains("fn rotate() -> bits<8>"));
        assert!(rendered.contains("let mut accumulator: bits<8>"));
        assert_eq!(rotate.result, bits_8);
        let Some(hir::Statement::Return(Some(result))) = rotate.body.statements.last() else {
            panic!("rotate should end by returning its computed value");
        };
        let hir::ExpressionKind::IntegerBinary {
            op: hir::IntegerBinaryOp::Or,
            left,
            right,
        } = &result.kind
        else {
            panic!("rotate should return the combined shifts");
        };
        assert_eq!(left.ty, bits_8);
        assert_eq!(right.ty, bits_8);
        lir_lower::lower(&program).unwrap();
    }

    #[test]
    fn configured_interrupts_call_exported_normal_functions() {
        let mut module = Module::default();
        let handler_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        let handler = handler_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("tick", handler);

        let main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let config = InterruptConfig {
            timer_0_high: Some("tick".into()),
            ..InterruptConfig::default()
        };
        let program = lower_with_interrupts(&module, &analyses, "main", &config).unwrap();

        let wrapper = program.interrupt_handlers.timer_0_high.as_ref().unwrap();
        assert_eq!(wrapper.name, "__waterbear_interrupt_timer_0_high");
        assert!(matches!(
            wrapper.body.statements.as_slice(),
            [
                hir::Statement::Call {
                    destination: None,
                    function,
                    arguments
                },
                hir::Statement::Return(None)
            ] if function == "tick" && arguments.is_empty()
        ));
        assert!(
            program
                .functions
                .iter()
                .any(|function| function.name == "tick")
        );

        let lir = lir_lower::lower(&program).unwrap();
        let allocated = lir_allocate::allocate(&lir).unwrap();
        let code = asm_lower::lower(&allocated).unwrap();
        let assembly = package::package(&PackageConfig::default(), &allocated, &code)
            .unwrap()
            .to_string();

        assert!(
            assembly.contains(
                ".org $0023\n  ;; timer 0 high\n  jmpf  __waterbear_interrupt_timer_0_high"
            )
        );
        assert!(assembly.contains("  callf tick"));
        assert!(assembly.contains("  reti"));
    }

    #[test]
    fn detects_ordinary_calls_in_tail_position() {
        let mut module = Module::default();
        let callee_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        let callee = callee_builder.finish(Vec::new(), &mut module.funcs);
        module.funcs.get_mut(callee).name = Some("tick".into());

        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        main_builder
            .func_body()
            .instr(Call { func: callee })
            .instr(Call { func: callee });
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("call tick();"));
        assert!(rendered.contains("tail call tick();"));
    }

    #[test]
    fn detects_returned_values_as_tail_calls() {
        let mut module = Module::default();
        let value = module.locals.add(ValType::I32);
        let mut callee_builder =
            FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        callee_builder.func_body().instr(LocalGet { local: value });
        let callee = callee_builder.finish(vec![value], &mut module.funcs);
        module.funcs.get_mut(callee).name = Some("identity".into());

        let argument = module.locals.add(ValType::I32);
        let mut main_builder =
            FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        main_builder
            .func_body()
            .instr(LocalGet { local: argument })
            .instr(Call { func: callee });
        let main = main_builder.finish(vec![argument], &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("tail call identity("));
        assert!(!rendered.contains("call0"));
    }

    #[test]
    fn detects_tail_calls_that_feed_a_structured_if_result() {
        let mut module = Module::default();
        let value = module.locals.add(ValType::I32);
        let mut callee_builder =
            FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        callee_builder.func_body().instr(LocalGet { local: value });
        let callee = callee_builder.finish(vec![value], &mut module.funcs);
        module.funcs.get_mut(callee).name = Some("identity".into());

        let condition = module.locals.add(ValType::I32);
        let argument = module.locals.add(ValType::I32);
        let mut main_builder = FunctionBuilder::new(
            &mut module.types,
            &[ValType::I32, ValType::I32],
            &[ValType::I32],
        );
        let consequent = {
            let mut sequence = main_builder.dangling_instr_seq(ValType::I32);
            sequence
                .instr(LocalGet { local: argument })
                .instr(Call { func: callee });
            sequence.id()
        };
        let alternative = {
            let mut sequence = main_builder.dangling_instr_seq(ValType::I32);
            sequence
                .instr(LocalGet { local: argument })
                .instr(Call { func: callee });
            sequence.id()
        };
        main_builder
            .func_body()
            .instr(LocalGet { local: condition })
            .instr(IfElse {
                consequent,
                alternative,
            });
        let main = main_builder.finish(vec![condition, argument], &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert_eq!(rendered.matches("tail call identity").count(), 2);
    }

    #[test]
    fn lowers_native_wasm_return_calls() {
        let mut module = Module::default();
        let value = module.locals.add(ValType::I32);
        let mut callee_builder =
            FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        callee_builder.func_body().instr(LocalGet { local: value });
        let callee = callee_builder.finish(vec![value], &mut module.funcs);
        module.funcs.get_mut(callee).name = Some("identity".into());

        let argument = module.locals.add(ValType::I32);
        let mut main_builder =
            FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        main_builder
            .func_body()
            .instr(LocalGet { local: argument })
            .instr(ReturnCall { func: callee });
        let main = main_builder.finish(vec![argument], &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("tail call identity("));
    }

    #[test]
    fn lowers_native_return_calls_to_intrinsics_as_intrinsic_returns() {
        let mut module = Module::default();
        let signature = module.types.add(&[], &[ValType::I32]);
        let (read_port, _) = module.add_import_func("env", "waterbear_vmu_sfr_p7_read", signature);

        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);
        main_builder
            .func_body()
            .instr(ReturnCall { func: read_port });
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("intrinsic waterbear_vmu_sfr_p7_read();"));
        assert!(rendered.contains("return"));
        assert!(!rendered.contains("tail call waterbear_vmu_sfr_p7_read"));
    }

    #[test]
    fn translates_registered_imports_to_intrinsics() {
        let mut module = Module::default();
        let signature = module.types.add(&[], &[ValType::I32]);
        let (read_port, _) = module.add_import_func("env", "waterbear_vmu_sfr_p7_read", signature);

        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        main_builder
            .func_body()
            .instr(Call { func: read_port })
            .instr(Drop {});
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("intrinsic waterbear_vmu_sfr_p7_read();"));
        assert!(!rendered.contains("call waterbear_vmu_sfr_p7_read"));
    }

    #[test]
    fn rejects_calls_to_unregistered_imports() {
        let mut module = Module::default();
        let signature = module.types.add(&[], &[]);
        let (missing, _) = module.add_import_func("env", "missing", signature);

        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        main_builder.func_body().instr(Call { func: missing });
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let error = lower(&module, &analyses, "main").unwrap_err();

        assert_eq!(
            error,
            LowerError::UnresolvedCall {
                function: "main".into(),
                callee: "env.missing".into(),
            }
        );
    }

    #[test]
    fn rejects_intrinsics_with_the_wrong_wasm_signature() {
        let mut module = Module::default();
        let signature = module.types.add(&[], &[]);
        let (read_port, _) = module.add_import_func("env", "waterbear_vmu_sfr_p7_read", signature);

        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        main_builder.func_body().instr(Call { func: read_port });
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let error = lower(&module, &analyses, "main").unwrap_err();

        assert!(matches!(
            &error,
            LowerError::InvalidFunction { function, message }
                if function == "main"
                    && message.contains("waterbear_vmu_sfr_p7_read")
                    && message.contains("signature")
        ));
    }

    #[test]
    fn replaces_recovered_shadow_stack_accesses_with_a_local_object() {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);
        let stack_pointer = module.globals.add_local(
            ValType::I32,
            true,
            false,
            ConstExpr::Value(Value::I32(1024)),
        );
        module.globals.get_mut(stack_pointer).name = Some("__stack_pointer".into());
        let frame_pointer = module.locals.add(ValType::I32);
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        builder
            .func_body()
            .instr(GlobalGet {
                global: stack_pointer,
            })
            .i32_const(16)
            .instr(Binop {
                op: BinaryOp::I32Sub,
            })
            .instr(LocalTee {
                local: frame_pointer,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(7)
            .instr(Store {
                memory,
                kind: StoreKind::I32_8 { atomic: false },
                arg: MemArg {
                    align: 1,
                    offset: 4,
                },
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .instr(Load {
                memory,
                kind: LoadKind::I32_8 {
                    kind: ExtendedLoad::ZeroExtend,
                },
                arg: MemArg {
                    align: 1,
                    offset: 4,
                },
            })
            .instr(Drop {})
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(16)
            .instr(Binop {
                op: BinaryOp::I32Add,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            });
        let main = builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("stack0: bits<8>"));
        assert!(rendered.contains("stack0 ="));
        assert!(rendered.contains("read(stack0)"));
        assert!(!rendered.contains("store<bits<8>>"));
        assert!(!rendered.contains("load<bits<8>>"));
        assert!(!rendered.contains("__stack_pointer"));
    }

    #[test]
    fn passes_recovered_frame_addresses_to_intrinsics() {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);
        let stack_pointer = module.globals.add_local(
            ValType::I32,
            true,
            false,
            ConstExpr::Value(Value::I32(1024)),
        );
        module.globals.get_mut(stack_pointer).name = Some("__stack_pointer".into());
        let frame_pointer = module.locals.add(ValType::I32);
        let intrinsic_ty = module.types.add(
            &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            &[],
        );
        let (copy_to_main, _) =
            module.add_import_func("env", "waterbear_vmu_xram_copy_to_main", intrinsic_ty);

        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        builder
            .func_body()
            .instr(GlobalGet {
                global: stack_pointer,
            })
            .i32_const(8)
            .instr(Binop {
                op: BinaryOp::I32Sub,
            })
            .instr(LocalTee {
                local: frame_pointer,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i64_const(0)
            .instr(Store {
                memory,
                kind: StoreKind::I64 { atomic: false },
                arg: MemArg {
                    align: 8,
                    offset: 0,
                },
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(1)
            .i32_const(24)
            .i32_const(8)
            .instr(Call { func: copy_to_main })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(8)
            .instr(Binop {
                op: BinaryOp::I32Add,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            });
        let main = builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses =
            analyze_with_imports(&module, &IntrinsicCallResolver).expect("analysis succeeds");
        let program = lower(&module, &analyses, "main").expect("lowering succeeds");
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("stack0: opaque_bytes<8>"));
        assert!(
            rendered.contains(
                "intrinsic waterbear_vmu_xram_copy_to_main((address_of(stack0) : pointer)"
            )
        );
        assert!(!rendered.contains("__stack_pointer"));
    }

    #[test]
    fn passes_recovered_frame_addresses_to_read_only_local_functions() {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);
        let stack_pointer = module.globals.add_local(
            ValType::I32,
            true,
            false,
            ConstExpr::Value(Value::I32(1024)),
        );
        module.globals.get_mut(stack_pointer).name = Some("__stack_pointer".into());

        let input = module.locals.add(ValType::I32);
        module.locals.get_mut(input).name = Some("input".into());
        let mut checksum_builder =
            FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        checksum_builder
            .func_body()
            .instr(LocalGet { local: input })
            .instr(Load {
                memory,
                kind: LoadKind::I32_8 {
                    kind: ExtendedLoad::ZeroExtend,
                },
                arg: MemArg {
                    align: 1,
                    offset: 0,
                },
            });
        let checksum = checksum_builder.finish(vec![input], &mut module.funcs);
        module.funcs.get_mut(checksum).name = Some("checksum".into());

        let frame_pointer = module.locals.add(ValType::I32);
        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        main_builder
            .func_body()
            .instr(GlobalGet {
                global: stack_pointer,
            })
            .i32_const(1)
            .instr(Binop {
                op: BinaryOp::I32Sub,
            })
            .instr(LocalTee {
                local: frame_pointer,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(7)
            .instr(Store {
                memory,
                kind: StoreKind::I32_8 { atomic: false },
                arg: MemArg {
                    align: 1,
                    offset: 0,
                },
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .instr(Call { func: checksum })
            .instr(Drop {})
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(1)
            .instr(Binop {
                op: BinaryOp::I32Add,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            });
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).expect("analysis succeeds");
        let program = lower(&module, &analyses, "main").expect("lowering succeeds");
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("stack0: opaque_bytes<1>"));
        assert!(rendered.contains("call checksum((address_of(stack0) : pointer))"));
        assert!(
            rendered.contains("fn checksum(input: pointer)"),
            "{rendered}"
        );
        assert!(!rendered.contains("__stack_pointer"));
        lir_lower::lower(&program).expect("LIR lowering succeeds");
    }

    #[test]
    fn rejects_a_shadow_stack_address_that_really_escapes() {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);
        module.data.add(
            DataKind::Active {
                memory,
                offset: ConstExpr::Value(Value::I32(0)),
            },
            vec![0; 4],
        );
        let stack_pointer = module.globals.add_local(
            ValType::I32,
            true,
            false,
            ConstExpr::Value(Value::I32(1024)),
        );
        module.globals.get_mut(stack_pointer).name = Some("__stack_pointer".into());

        let escaped_pointer = module.locals.add(ValType::I32);
        let mut escape_builder = FunctionBuilder::new(&mut module.types, &[ValType::I32], &[]);
        escape_builder
            .func_body()
            .i32_const(0)
            .instr(LocalGet {
                local: escaped_pointer,
            })
            .instr(Store {
                memory,
                kind: StoreKind::I32 { atomic: false },
                arg: MemArg {
                    align: 4,
                    offset: 0,
                },
            });
        let escape = escape_builder.finish(vec![escaped_pointer], &mut module.funcs);
        module.funcs.get_mut(escape).name = Some("escape".into());

        let frame_pointer = module.locals.add(ValType::I32);
        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        main_builder
            .func_body()
            .instr(GlobalGet {
                global: stack_pointer,
            })
            .i32_const(4)
            .instr(Binop {
                op: BinaryOp::I32Sub,
            })
            .instr(LocalTee {
                local: frame_pointer,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(7)
            .instr(Store {
                memory,
                kind: StoreKind::I32 { atomic: false },
                arg: MemArg {
                    align: 4,
                    offset: 0,
                },
            })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .instr(Call { func: escape })
            .instr(LocalGet {
                local: frame_pointer,
            })
            .i32_const(4)
            .instr(Binop {
                op: BinaryOp::I32Add,
            })
            .instr(GlobalSet {
                global: stack_pointer,
            });
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).expect("analysis succeeds");
        let error = lower(&module, &analyses, "main").expect_err("lowering must reject the leak");

        assert!(
            matches!(
                &error,
                LowerError::InvalidFunction { function, message }
                    if function == "main"
                        && message.contains("shadow-stack frame")
                        && message.contains("frame address escapes")
            ),
            "{error:?}"
        );
    }

    #[test]
    fn lowers_structured_if_results_and_loop_backedges() {
        let mut module = Module::default();

        let condition = module.locals.add(ValType::I32);
        let mut choose_builder =
            FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        let consequent = {
            let mut sequence = choose_builder.dangling_instr_seq(ValType::I32);
            sequence.i32_const(1);
            sequence.id()
        };
        let alternative = {
            let mut sequence = choose_builder.dangling_instr_seq(ValType::I32);
            sequence.i32_const(2);
            sequence.id()
        };
        choose_builder
            .func_body()
            .instr(LocalGet { local: condition })
            .instr(IfElse {
                consequent,
                alternative,
            });
        let choose = choose_builder.finish(vec![condition], &mut module.funcs);
        module.funcs.get_mut(choose).name = Some("choose".into());

        let counter = module.locals.add(ValType::I32);
        let mut main_builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        let loop_body = {
            let mut sequence = main_builder.dangling_instr_seq(None);
            let id = sequence.id();
            sequence
                .instr(LocalGet { local: counter })
                .i32_const(1)
                .instr(Binop {
                    op: BinaryOp::I32Sub,
                })
                .instr(LocalTee { local: counter })
                .instr(BrIf { block: id });
            id
        };
        main_builder
            .func_body()
            .i32_const(3)
            .instr(walrus::ir::LocalSet { local: counter })
            .instr(Loop { seq: loop_body });
        let main = main_builder.finish(Vec::new(), &mut module.funcs);
        module.exports.add("main", main);

        let analyses = analyze(&module).unwrap();
        let program = lower(&module, &analyses, "main").unwrap();
        let rendered = hir_printer::render(&program);

        assert!(rendered.contains("if_result"));
        assert!(rendered.contains("loop loop0"));
        assert!(rendered.contains("continue loop0;"));
    }
}
