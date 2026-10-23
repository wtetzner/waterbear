//! Human-readable WebAssembly annotated with abstract-analysis results.

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write,
};

use walrus::{
    ConstExpr, DataKind, ElementItems, ElementKind, ExportItem, FunctionId, FunctionKind,
    GlobalKind, LocalFunction, Module,
    ir::{Instr, InstrSeqId},
};

use super::analysis::{
    AnalysisDb,
    frame::FunctionFrame,
    index::{InstructionId, InstructionKey, ValueId, ValueProducer},
    value_flow::ValueSet,
};

mod format;
mod names;

use self::{
    format::*,
    names::{Names, identifier},
};

/// Renders a module in WAT-like syntax with analysis facts in comments.
pub fn render(module: &Module, analyses: &AnalysisDb) -> String {
    Printer::new(module, analyses).render()
}

struct Printer<'a> {
    module: &'a Module,
    analyses: &'a AnalysisDb,
    names: Names,
    output: String,
}

impl<'a> Printer<'a> {
    fn new(module: &'a Module, analyses: &'a AnalysisDb) -> Self {
        Self {
            module,
            analyses,
            names: Names::new(module),
            output: String::new(),
        }
    }

    fn render(mut self) -> String {
        let module_name = self
            .module
            .name
            .as_deref()
            .map(identifier)
            .unwrap_or_default();
        let suffix = if module_name.is_empty() {
            String::new()
        } else {
            format!(" ${module_name}")
        };
        self.line(0, format_args!("(module{suffix}"));
        self.line(
            1,
            format_args!(
                ";; analysis: {} functions, {} instructions, {} stack objects",
                self.analyses.index.functions.len(),
                self.analyses.index.instructions.len(),
                self.analyses
                    .frames
                    .functions
                    .values()
                    .map(|frame| frame.objects.len())
                    .sum::<usize>()
            ),
        );

        self.write_types();
        self.write_imports();
        self.write_memories();
        self.write_tables();
        self.write_globals();
        self.write_exports();
        self.write_start();
        self.write_elements();
        self.write_data();
        self.write_functions();
        self.line(0, format_args!(")"));
        self.output
    }

    fn write_types(&mut self) {
        let mut used = self
            .module
            .funcs
            .iter()
            .map(|function| function.ty())
            .collect::<BTreeSet<_>>();
        for instruction in &self.analyses.index.instructions {
            match self.analyses.index.instruction(self.module, instruction.id) {
                Instr::CallIndirect(op) => {
                    used.insert(op.ty);
                }
                Instr::ReturnCallIndirect(op) => {
                    used.insert(op.ty);
                }
                _ => {}
            }
        }

        for ty in self
            .module
            .types
            .iter()
            .filter(|ty| used.contains(&ty.id()))
        {
            let name = self.names.ty(ty.id());
            let signature = signature(ty.params(), ty.results());
            self.line(1, format_args!("(type {name} (func{signature}))"));
        }
    }

    fn write_imports(&mut self) {
        for import in self.module.imports.iter() {
            let module = quoted(&import.module);
            let field = quoted(&import.name);
            let description = match import.kind {
                walrus::ImportKind::Function(id) => {
                    let function = self.module.funcs.get(id);
                    let ty = self.module.types.get(function.ty());
                    format!(
                        "(func {}{})",
                        self.names.function(id),
                        signature(ty.params(), ty.results())
                    )
                }
                walrus::ImportKind::Global(id) => {
                    let global = self.module.globals.get(id);
                    format!(
                        "(global {} {})",
                        self.names.global(id),
                        global_type(global.ty, global.mutable, global.shared)
                    )
                }
                walrus::ImportKind::Memory(id) => {
                    let memory = self.module.memories.get(id);
                    format!(
                        "(memory {}{})",
                        self.names.memory(id),
                        limits(
                            memory.initial,
                            memory.maximum,
                            memory.shared,
                            memory.memory64
                        )
                    )
                }
                walrus::ImportKind::Table(id) => {
                    let table = self.module.tables.get(id);
                    format!(
                        "(table {}{} {})",
                        self.names.table(id),
                        limits(table.initial, table.maximum, false, table.table64),
                        ref_type(table.element_ty)
                    )
                }
            };
            self.line(1, format_args!("(import {module} {field} {description})"));
        }
    }

    fn write_memories(&mut self) {
        for memory in self
            .module
            .memories
            .iter()
            .filter(|memory| memory.import.is_none())
        {
            self.line(
                1,
                format_args!(
                    "(memory {}{})",
                    self.names.memory(memory.id()),
                    limits(
                        memory.initial,
                        memory.maximum,
                        memory.shared,
                        memory.memory64
                    )
                ),
            );
        }
    }

    fn write_tables(&mut self) {
        for table in self
            .module
            .tables
            .iter()
            .filter(|table| table.import.is_none())
        {
            self.line(
                1,
                format_args!(
                    "(table {}{} {})",
                    self.names.table(table.id()),
                    limits(table.initial, table.maximum, false, table.table64),
                    ref_type(table.element_ty)
                ),
            );
        }
    }

    fn write_globals(&mut self) {
        for global in self.module.globals.iter() {
            let GlobalKind::Local(initializer) = global.kind else {
                continue;
            };
            self.line(
                1,
                format_args!(
                    "(global {} {} {})",
                    self.names.global(global.id()),
                    global_type(global.ty, global.mutable, global.shared),
                    self.const_expr(initializer)
                ),
            );
            let value = self.analyses.index.globals[&global.id()];
            self.write_value_annotation(2, "value", value, None);
        }
    }

    fn write_exports(&mut self) {
        for export in self.module.exports.iter() {
            let item = match export.item {
                ExportItem::Function(id) => format!("func {}", self.names.function(id)),
                ExportItem::Global(id) => format!("global {}", self.names.global(id)),
                ExportItem::Memory(id) => format!("memory {}", self.names.memory(id)),
                ExportItem::Table(id) => format!("table {}", self.names.table(id)),
            };
            self.line(
                1,
                format_args!("(export {} ({item}))", quoted(&export.name)),
            );
        }
    }

    fn write_start(&mut self) {
        if let Some(function) = self.module.start {
            self.line(1, format_args!("(start {})", self.names.function(function)));
        }
    }

    fn write_elements(&mut self) {
        for element in self.module.elements.iter() {
            let name = self.names.element(element.id());
            let kind = match element.kind {
                ElementKind::Passive => "passive".to_owned(),
                ElementKind::Declared => "declare".to_owned(),
                ElementKind::Active { table, offset } => format!(
                    "table={} offset={}",
                    self.names.table(table),
                    self.const_expr(offset)
                ),
            };
            let items = match &element.items {
                ElementItems::Functions(functions) => functions
                    .iter()
                    .map(|id| self.names.function(*id))
                    .collect::<Vec<_>>()
                    .join(" "),
                ElementItems::Expressions(ty, expressions) => {
                    let expressions = expressions
                        .iter()
                        .map(|expression| self.const_expr(*expression))
                        .collect::<Vec<_>>()
                        .join(" ");
                    format!("{} {expressions}", ref_type(*ty))
                }
            };
            self.line(1, format_args!("(elem {name} {kind} {items})"));
        }
    }

    fn write_data(&mut self) {
        for data in self.module.data.iter() {
            let name = self.names.data(data.id());
            let placement = match data.kind {
                DataKind::Passive => "passive".to_owned(),
                DataKind::Active { memory, offset } => format!(
                    "memory={} offset={}",
                    self.names.memory(memory),
                    self.const_expr(offset)
                ),
            };
            self.line(
                1,
                format_args!(
                    "(data {name} {placement} {}) ;; {} bytes",
                    byte_preview(&data.value),
                    data.value.len()
                ),
            );
        }
    }

    fn write_functions(&mut self) {
        for function in self.module.funcs.iter() {
            let FunctionKind::Local(body) = &function.kind else {
                continue;
            };
            self.write_function(function.id(), body);
        }
    }

    fn write_function(&mut self, function: FunctionId, body: &LocalFunction) {
        let info = &self.analyses.index.functions[&function];
        let ty = self.module.types.get(body.ty());
        let mut header = format!("(func {}", self.names.function(function));
        for (&local, ty) in body.args.iter().zip(ty.params()) {
            write!(header, " (param {} {ty})", self.names.local(local)).unwrap();
        }
        for result in ty.results() {
            write!(header, " (result {result})").unwrap();
        }
        self.line(1, format_args!("{header}"));

        let parameters: BTreeSet<_> = body.args.iter().copied().collect();
        for &local in info
            .locals
            .keys()
            .filter(|local| !parameters.contains(local))
        {
            self.line(
                2,
                format_args!(
                    "(local {} {})",
                    self.names.local(local),
                    self.module.locals.get(local).ty()
                ),
            );
        }
        for (&local, &value) in body.args.iter().zip(&info.parameters) {
            self.write_value_annotation(
                2,
                &format!("parameter {}", self.names.local(local)),
                value,
                Some(function),
            );
        }
        if let Some(frame) = self.analyses.frames.functions.get(&function) {
            self.write_frame_summary(2, frame);
        }
        self.write_return_summary(2, function);

        let labels = sequence_labels(body);
        self.write_sequence(function, body, body.entry_block(), &labels, 2);
        self.line(1, format_args!(")"));
    }

    fn write_sequence(
        &mut self,
        function: FunctionId,
        body: &LocalFunction,
        sequence: InstrSeqId,
        labels: &BTreeMap<InstrSeqId, String>,
        indent: usize,
    ) {
        for (index, (instruction, _)) in body.block(sequence).instrs.iter().enumerate() {
            let instruction_id = self.analyses.index.instruction_id(InstructionKey {
                function,
                sequence,
                index,
            });
            match instruction {
                Instr::Block(op) => {
                    self.line(
                        indent,
                        format_args!(
                            "(block {}{}",
                            labels[&op.seq],
                            self.sequence_signature(body, op.seq)
                        ),
                    );
                    self.write_instruction_annotations(indent + 1, function, instruction_id);
                    self.write_sequence(function, body, op.seq, labels, indent + 1);
                    self.line(indent, format_args!(")"));
                }
                Instr::Loop(op) => {
                    self.line(
                        indent,
                        format_args!(
                            "(loop {}{}",
                            labels[&op.seq],
                            self.sequence_signature(body, op.seq)
                        ),
                    );
                    self.write_instruction_annotations(indent + 1, function, instruction_id);
                    self.write_sequence(function, body, op.seq, labels, indent + 1);
                    self.line(indent, format_args!(")"));
                }
                Instr::IfElse(op) => {
                    self.line(
                        indent,
                        format_args!(
                            "(if {}{}",
                            labels[&op.consequent],
                            self.sequence_signature(body, op.consequent)
                        ),
                    );
                    self.write_instruction_annotations(indent + 1, function, instruction_id);
                    self.line(indent + 1, format_args!("(then"));
                    self.write_sequence(function, body, op.consequent, labels, indent + 2);
                    self.line(indent + 1, format_args!(")"));
                    self.line(indent + 1, format_args!("(else"));
                    self.write_sequence(function, body, op.alternative, labels, indent + 2);
                    self.line(indent + 1, format_args!(")"));
                    self.line(indent, format_args!(")"));
                }
                _ => {
                    self.line(
                        indent,
                        format_args!("{}", self.instruction(instruction, labels)),
                    );
                    self.write_instruction_annotations(indent + 1, function, instruction_id);
                }
            }
        }
    }

    fn instruction(&self, instruction: &Instr, labels: &BTreeMap<InstrSeqId, String>) -> String {
        match instruction {
            Instr::Block(_) | Instr::Loop(_) | Instr::IfElse(_) => unreachable!(),
            Instr::Call(op) => format!("call {}", self.names.function(op.func)),
            Instr::CallIndirect(op) => format!(
                "call_indirect {} (type {})",
                self.names.table(op.table),
                self.names.ty(op.ty)
            ),
            Instr::LocalGet(op) => format!("local.get {}", self.names.local(op.local)),
            Instr::LocalSet(op) => format!("local.set {}", self.names.local(op.local)),
            Instr::LocalTee(op) => format!("local.tee {}", self.names.local(op.local)),
            Instr::GlobalGet(op) => format!("global.get {}", self.names.global(op.global)),
            Instr::GlobalSet(op) => format!("global.set {}", self.names.global(op.global)),
            Instr::Const(op) => value_instruction(op.value),
            Instr::Binop(op) => binary_op(op.op),
            Instr::Unop(op) => unary_op(op.op),
            Instr::Select(op) => op
                .ty
                .map(|ty| format!("select (result {ty})"))
                .unwrap_or_else(|| "select".to_owned()),
            Instr::Unreachable(_) => "unreachable".to_owned(),
            Instr::Br(op) => format!("br {}", label(labels, op.block)),
            Instr::BrIf(op) => format!("br_if {}", label(labels, op.block)),
            Instr::BrTable(op) => format!(
                "br_table {}",
                op.blocks
                    .iter()
                    .map(|target| label(labels, *target))
                    .chain(std::iter::once(label(labels, op.default)))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Instr::Drop(_) => "drop".to_owned(),
            Instr::Return(_) => "return".to_owned(),
            Instr::MemorySize(op) => format!("memory.size {}", self.names.memory(op.memory)),
            Instr::MemoryGrow(op) => format!("memory.grow {}", self.names.memory(op.memory)),
            Instr::MemoryInit(op) => format!(
                "memory.init {} {}",
                self.names.memory(op.memory),
                self.names.data(op.data)
            ),
            Instr::DataDrop(op) => format!("data.drop {}", self.names.data(op.data)),
            Instr::MemoryCopy(op) => format!(
                "memory.copy {} {}",
                self.names.memory(op.dst),
                self.names.memory(op.src)
            ),
            Instr::MemoryFill(op) => format!("memory.fill {}", self.names.memory(op.memory)),
            Instr::Load(op) => format!(
                "{} {}{}",
                load_kind(op.kind),
                self.names.memory(op.memory),
                memarg(op.arg)
            ),
            Instr::Store(op) => format!(
                "{} {}{}",
                store_kind(op.kind),
                self.names.memory(op.memory),
                memarg(op.arg)
            ),
            Instr::AtomicRmw(op) => format!(
                "{} {}{}",
                atomic_rmw(op.width, op.op),
                self.names.memory(op.memory),
                memarg(op.arg)
            ),
            Instr::Cmpxchg(op) => format!(
                "{} {}{}",
                atomic_cmpxchg(op.width),
                self.names.memory(op.memory),
                memarg(op.arg)
            ),
            Instr::AtomicNotify(op) => format!(
                "memory.atomic.notify {}{}",
                self.names.memory(op.memory),
                memarg(op.arg)
            ),
            Instr::AtomicWait(op) => format!(
                "memory.atomic.wait{} {}{}",
                if op.sixty_four { "64" } else { "32" },
                self.names.memory(op.memory),
                memarg(op.arg)
            ),
            Instr::AtomicFence(_) => "atomic.fence".to_owned(),
            Instr::TableGet(op) => format!("table.get {}", self.names.table(op.table)),
            Instr::TableSet(op) => format!("table.set {}", self.names.table(op.table)),
            Instr::TableGrow(op) => format!("table.grow {}", self.names.table(op.table)),
            Instr::TableSize(op) => format!("table.size {}", self.names.table(op.table)),
            Instr::TableFill(op) => format!("table.fill {}", self.names.table(op.table)),
            Instr::RefNull(op) => format!("ref.null {}", ref_type(op.ty)),
            Instr::RefIsNull(_) => "ref.is_null".to_owned(),
            Instr::RefFunc(op) => format!("ref.func {}", self.names.function(op.func)),
            Instr::V128Bitselect(_) => "v128.bitselect".to_owned(),
            Instr::I8x16Swizzle(_) => "i8x16.swizzle".to_owned(),
            Instr::I8x16Shuffle(op) => format!(
                "i8x16.shuffle {}",
                op.indices
                    .iter()
                    .map(u8::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Instr::LoadSimd(op) => format!(
                "{} {}{}",
                load_simd_kind(op.kind),
                self.names.memory(op.memory),
                memarg(op.arg)
            ),
            Instr::TableInit(op) => format!(
                "table.init {} {}",
                self.names.table(op.table),
                self.names.element(op.elem)
            ),
            Instr::ElemDrop(op) => format!("elem.drop {}", self.names.element(op.elem)),
            Instr::TableCopy(op) => format!(
                "table.copy {} {}",
                self.names.table(op.dst),
                self.names.table(op.src)
            ),
            Instr::ReturnCall(op) => format!("return_call {}", self.names.function(op.func)),
            Instr::ReturnCallIndirect(op) => format!(
                "return_call_indirect {} (type {})",
                self.names.table(op.table),
                self.names.ty(op.ty)
            ),
        }
    }

    fn write_instruction_annotations(
        &mut self,
        indent: usize,
        function: FunctionId,
        instruction: InstructionId,
    ) {
        let input_flow = self.analyses.value_flow.instruction_inputs[instruction.0].as_deref();
        let input_integers = self.analyses.integers.instruction_inputs[instruction.0].as_deref();
        let input_addresses = self.analyses.addresses.instruction_inputs[instruction.0].as_deref();
        if let (Some(flow), Some(integers), Some(addresses)) =
            (input_flow, input_integers, input_addresses)
        {
            for (operand, ((flow, integer), address)) in
                flow.iter().zip(integers).zip(addresses).enumerate()
            {
                let mut facts = vec![format!("from {}", self.value_set(flow))];
                extend_fact(&mut facts, integer_fact(integer));
                extend_fact(&mut facts, address_fact(address, function, &self.names));
                self.line(
                    indent,
                    format_args!(";; input {operand}: {}", facts.join(", ")),
                );
            }
        }

        let outputs = self.analyses.index.instructions[instruction.0]
            .outputs
            .clone();
        for value in outputs {
            self.write_value_annotation(indent, "output", value, Some(function));
        }
        if let Some(access) = self.analyses.frames.accesses.get(&instruction).cloned() {
            self.line(indent, format_args!(";; frame: {}", frame_access(&access)));
        }
    }

    fn write_value_annotation(
        &mut self,
        indent: usize,
        label: &str,
        value: ValueId,
        function: Option<FunctionId>,
    ) {
        let mut facts = Vec::new();
        extend_fact(
            &mut facts,
            integer_fact(&self.analyses.integers.values[value.0]),
        );
        if let Some(function) = function {
            extend_fact(
                &mut facts,
                address_fact(
                    &self.analyses.addresses.values[value.0],
                    function,
                    &self.names,
                ),
            );
        }
        extend_fact(
            &mut facts,
            demand_fact(&self.analyses.demanded_bits.values[value.0]),
        );
        let suffix = if facts.is_empty() {
            String::new()
        } else {
            format!(": {}", facts.join(", "))
        };
        self.line(
            indent,
            format_args!(";; {label}: {}{suffix}", self.value(value)),
        );
    }

    fn write_frame_summary(&mut self, indent: usize, frame: &FunctionFrame) {
        if frame.size_lower_bound == 0
            && frame.objects.is_empty()
            && frame.dynamic_accesses.is_empty()
            && !frame.address_escapes
        {
            return;
        }
        let escapes = if frame.address_escapes {
            ", address escapes"
        } else {
            ""
        };
        self.line(
            indent,
            format_args!(
                ";; frame: at least {} bytes{escapes}",
                frame.size_lower_bound
            ),
        );
        for object in &frame.objects {
            let end = i128::from(object.start) + i128::from(object.size);
            let escapes = if object.address_escapes {
                ", address escapes"
            } else {
                ""
            };
            self.line(
                indent,
                format_args!(
                    ";; stack object $stack{}: [{}..{}), align={}, {} accesses{escapes}",
                    object.id.0,
                    object.start,
                    end,
                    object.alignment,
                    object.accesses.len()
                ),
            );
        }
        if !frame.dynamic_accesses.is_empty() {
            self.line(
                indent,
                format_args!(
                    ";; dynamic stack accesses: {}",
                    frame.dynamic_accesses.len()
                ),
            );
        }
    }

    fn write_return_summary(&mut self, indent: usize, function: FunctionId) {
        let Some(flow) = self.analyses.value_flow.function_returns.get(&function) else {
            return;
        };
        let integers = &self.analyses.integers.function_returns[&function];
        let addresses = &self.analyses.addresses.function_returns[&function];
        for (result, ((flow, integer), address)) in
            flow.iter().zip(integers).zip(addresses).enumerate()
        {
            let mut facts = vec![format!("from {}", self.value_set(flow))];
            extend_fact(&mut facts, integer_fact(integer));
            extend_fact(&mut facts, address_fact(address, function, &self.names));
            self.line(
                indent,
                format_args!(";; result {result}: {}", facts.join(", ")),
            );
        }
    }

    fn sequence_signature(&self, body: &LocalFunction, sequence: InstrSeqId) -> String {
        match body.block(sequence).ty {
            walrus::ir::InstrSeqType::Simple(result) => {
                signature(&[], &result.into_iter().collect::<Vec<_>>())
            }
            walrus::ir::InstrSeqType::MultiValue(ty) => {
                let ty = self.module.types.get(ty);
                signature(ty.params(), ty.results())
            }
        }
    }

    fn const_expr(&self, expression: ConstExpr) -> String {
        match expression {
            ConstExpr::Value(value) => format!("({})", value_instruction(value)),
            ConstExpr::Global(global) => format!("(global.get {})", self.names.global(global)),
            ConstExpr::RefNull(ty) => format!("(ref.null {})", ref_type(ty)),
            ConstExpr::RefFunc(function) => {
                format!("(ref.func {})", self.names.function(function))
            }
        }
    }

    fn value_set(&self, flow: &ValueSet) -> String {
        let mut values = flow
            .values
            .iter()
            .map(|value| self.value(*value))
            .collect::<Vec<_>>();
        if flow.unknown {
            values.push("unknown".to_owned());
        }
        if values.is_empty() {
            "none".to_owned()
        } else {
            values.join(" or ")
        }
    }

    fn value(&self, value: ValueId) -> String {
        match self.analyses.index.values[value.0].producer {
            ValueProducer::Instruction { result, .. } if result > 0 => {
                format!("%v{}.{}", value.0, result)
            }
            ValueProducer::Instruction { .. } => format!("%v{}", value.0),
            ValueProducer::FunctionParameter {
                function,
                parameter,
                local,
            } => format!(
                "%{}.arg{parameter}({})",
                bare(&self.names.function(function)),
                self.names.local(local)
            ),
            ValueProducer::LocalInitial { function, local } => format!(
                "%{}.initial({})",
                bare(&self.names.function(function)),
                self.names.local(local)
            ),
            ValueProducer::GlobalInitial(global) => {
                format!("%initial({})", self.names.global(global))
            }
        }
    }

    fn line(&mut self, indent: usize, arguments: std::fmt::Arguments<'_>) {
        for _ in 0..indent {
            self.output.push_str("  ");
        }
        self.output.write_fmt(arguments).unwrap();
        self.output.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use walrus::{ConstExpr, FunctionBuilder, Module, ValType, ir::LocalGet};

    use super::render;
    use crate::wasm::analysis::analyze;

    #[test]
    fn renders_names_structure_and_analysis_facts() {
        let mut module = Module::default();
        let stack_pointer = module.globals.add_local(
            ValType::I32,
            true,
            false,
            ConstExpr::Value(walrus::ir::Value::I32(1024)),
        );
        module.globals.get_mut(stack_pointer).name = Some("__stack_pointer".into());

        let parameter = module.locals.add(ValType::I32);
        module.locals.get_mut(parameter).name = Some("value".into());
        let mut builder = FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        builder.func_body().instr(LocalGet { local: parameter });
        let function = builder.finish(vec![parameter], &mut module.funcs);
        module.funcs.get_mut(function).name = Some("main".into());
        module.exports.add("entry", function);

        let analyses = analyze(&module).unwrap();
        let output = render(&module, &analyses);

        assert!(output.contains("(global $__stack_pointer (mut i32) (i32.const 1024))"));
        assert!(output.contains("(export \"entry\" (func $main))"));
        assert!(output.contains("(func $main (param $value i32) (result i32)"));
        assert!(output.contains("local.get $value"));
        assert!(output.contains(";; parameter $value: %main.arg0($value): 32 bits demanded"));
        assert!(output.contains("32 bits demanded"));
        assert!(!output.contains("analysis: unreachable"));
        assert!(!output.contains("FunctionId"));
        assert!(!output.contains("LocalGet"));
    }
}
