use std::{collections::BTreeMap, fmt};

use walrus::{
    ConstExpr, FunctionId, GlobalKind, LocalId, Module, ValType,
    ir::{BinaryOp, Instr, InstrSeqId, Load, UnaryOp, Value},
};

use super::{
    calls::{CallResolver, CallSummary},
    domain::AbstractDomain,
    index::{InstructionId, InstructionKey, ProgramIndex, ValueId, ValueProducer},
    shape::{instruction_shape, sequence_signature, value_type, zero_value},
};

#[derive(Debug)]
pub enum ForwardError<E> {
    Analysis(E),
    InvalidProgram(String),
    DidNotConverge {
        function: FunctionId,
        loop_id: InstrSeqId,
    },
}

impl<E: fmt::Display> fmt::Display for ForwardError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Analysis(error) => error.fmt(f),
            Self::InvalidProgram(message) => message.fmt(f),
            Self::DidNotConverge { function, loop_id } => {
                write!(
                    f,
                    "analysis did not converge in {function:?}, loop {loop_id:?}"
                )
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum RootKind {
    FunctionParameter,
    LocalInitial,
    GlobalInitial,
}

pub struct TransferContext<'a> {
    pub module: &'a Module,
    pub index: &'a ProgramIndex,
    pub function: FunctionId,
    pub instruction: Option<InstructionId>,
}

/// Defines the interesting transfer functions for a forward analysis.
///
/// The interpreter owns WebAssembly stack, local, global, and structured
/// control-flow semantics. Unimplemented operations conservatively produce
/// `top`, so a pass only needs to describe operations that refine its domain.
pub trait ForwardAnalysis {
    type Fact: AbstractDomain;
    type Error;

    fn root(
        &mut self,
        _cx: &TransferContext<'_>,
        _kind: RootKind,
        _value: ValueId,
        default: Self::Fact,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(default)
    }

    fn constant(
        &mut self,
        _cx: &TransferContext<'_>,
        value: Value,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(Self::Fact::top(value_type(value)))
    }

    fn unary(
        &mut self,
        _cx: &TransferContext<'_>,
        _op: UnaryOp,
        _input: &Self::Fact,
        output: ValType,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(Self::Fact::top(output))
    }

    fn binary(
        &mut self,
        _cx: &TransferContext<'_>,
        _op: BinaryOp,
        _left: &Self::Fact,
        _right: &Self::Fact,
        output: ValType,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(Self::Fact::top(output))
    }

    fn load(
        &mut self,
        _cx: &TransferContext<'_>,
        _load: &Load,
        _address: &Self::Fact,
        output: ValType,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(Self::Fact::top(output))
    }

    fn call(
        &mut self,
        _cx: &TransferContext<'_>,
        summary: &CallSummary,
        _arguments: &[Self::Fact],
    ) -> Result<Vec<Self::Fact>, Self::Error> {
        Ok(summary
            .results
            .iter()
            .copied()
            .map(Self::Fact::top)
            .collect())
    }

    fn instruction(
        &mut self,
        cx: &TransferContext<'_>,
        instruction: &Instr,
        inputs: &[Self::Fact],
        output_types: &[Option<ValType>],
        call: Option<&CallSummary>,
    ) -> Result<Vec<Self::Fact>, Self::Error> {
        match instruction {
            Instr::Const(op) => Ok(vec![self.constant(cx, op.value)?]),
            Instr::Unop(op) => Ok(vec![self.unary(
                cx,
                op.op,
                &inputs[0],
                output_types[0].expect("unary operation result type"),
            )?]),
            Instr::Binop(op) => Ok(vec![self.binary(
                cx,
                op.op,
                &inputs[0],
                &inputs[1],
                output_types[0].expect("binary operation result type"),
            )?]),
            Instr::Load(op) => Ok(vec![self.load(
                cx,
                op,
                &inputs[0],
                output_types[0].expect("load result type"),
            )?]),
            Instr::Call(_) | Instr::CallIndirect(_) => {
                self.call(cx, call.expect("call summary"), inputs)
            }
            _ => Ok(output_types
                .iter()
                .map(|ty| match ty {
                    Some(ty) => Self::Fact::top(*ty),
                    None => inputs
                        .first()
                        .cloned()
                        .unwrap_or_else(|| Self::Fact::bottom(None)),
                })
                .collect()),
        }
    }

    /// Gives an instruction result a stable identity before it is consumed.
    /// Normal value analyses keep the fact unchanged; value-flow analysis uses
    /// this hook to replace it with the singleton `ValueId` it represents.
    fn materialize(
        &mut self,
        _cx: &TransferContext<'_>,
        _value: ValueId,
        fact: Self::Fact,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(fact)
    }
}

#[derive(Clone, Debug)]
pub struct ForwardResults<F> {
    pub values: Vec<F>,
    pub instruction_inputs: Vec<Option<Vec<F>>>,
    pub function_returns: BTreeMap<FunctionId, Vec<F>>,
}

impl<F: AbstractDomain> ForwardResults<F> {
    fn new(index: &ProgramIndex) -> Self {
        Self {
            values: index
                .values
                .iter()
                .map(|value| F::bottom(value.ty))
                .collect(),
            instruction_inputs: vec![None; index.instructions.len()],
            function_returns: BTreeMap::new(),
        }
    }
}

pub fn run_forward<A, R>(
    module: &Module,
    index: &ProgramIndex,
    calls: &R,
    analysis: &mut A,
) -> Result<ForwardResults<A::Fact>, ForwardError<A::Error>>
where
    A: ForwardAnalysis,
    R: CallResolver,
{
    Interpreter {
        module,
        index,
        calls,
        analysis,
        results: ForwardResults::new(index),
    }
    .run()
}

#[derive(Clone)]
struct MachineState<F> {
    stack: Vec<F>,
    locals: BTreeMap<LocalId, F>,
    globals: BTreeMap<walrus::GlobalId, F>,
}

impl<F: AbstractDomain> MachineState<F> {
    fn join_assign<E>(&mut self, other: &Self) -> Result<bool, ForwardError<E>> {
        self.merge_assign(other, false)
    }

    fn widen_assign<E>(&mut self, other: &Self) -> Result<bool, ForwardError<E>> {
        self.merge_assign(other, true)
    }

    fn merge_assign<E>(&mut self, other: &Self, widen: bool) -> Result<bool, ForwardError<E>> {
        if self.stack.len() != other.stack.len() {
            return Err(ForwardError::InvalidProgram(format!(
                "cannot join stacks of different heights: {} and {}",
                self.stack.len(),
                other.stack.len()
            )));
        }

        let mut changed = false;
        for (left, right) in self.stack.iter_mut().zip(&other.stack) {
            changed |= if widen {
                left.widen_assign(right)
            } else {
                left.join_assign(right)
            };
        }
        for (local, right) in &other.locals {
            let left = self.locals.get_mut(local).ok_or_else(|| {
                ForwardError::InvalidProgram(format!("missing local {local:?} at control join"))
            })?;
            changed |= if widen {
                left.widen_assign(right)
            } else {
                left.join_assign(right)
            };
        }
        for (global, right) in &other.globals {
            let left = self.globals.get_mut(global).ok_or_else(|| {
                ForwardError::InvalidProgram(format!("missing global {global:?} at control join"))
            })?;
            changed |= if widen {
                left.widen_assign(right)
            } else {
                left.join_assign(right)
            };
        }
        Ok(changed)
    }
}

struct Outcomes<F> {
    next: Option<MachineState<F>>,
    branches: BTreeMap<InstrSeqId, MachineState<F>>,
    returns: Option<MachineState<F>>,
}

impl<F> Outcomes<F> {
    fn continuing(state: MachineState<F>) -> Self {
        Self {
            next: Some(state),
            branches: BTreeMap::new(),
            returns: None,
        }
    }

    fn stopped() -> Self {
        Self {
            next: None,
            branches: BTreeMap::new(),
            returns: None,
        }
    }
}

#[derive(Clone, Copy)]
struct Label {
    sequence: InstrSeqId,
    base_height: usize,
    branch_arity: usize,
}

struct Interpreter<'a, A, R>
where
    A: ForwardAnalysis,
    R: CallResolver,
{
    module: &'a Module,
    index: &'a ProgramIndex,
    calls: &'a R,
    analysis: &'a mut A,
    results: ForwardResults<A::Fact>,
}

impl<'a, A, R> Interpreter<'a, A, R>
where
    A: ForwardAnalysis,
    R: CallResolver,
{
    fn run(mut self) -> Result<ForwardResults<A::Fact>, ForwardError<A::Error>> {
        for &function in self.index.functions.keys() {
            let body = self.module.funcs.get(function).kind.unwrap_local();
            let state = self.initial_state(function)?;
            let (_, results) = self.module.types.params_results(body.ty());
            let labels = vec![Label {
                sequence: body.entry_block(),
                base_height: 0,
                branch_arity: results.len(),
            }];

            let mut outcomes =
                self.execute_sequence(function, body.entry_block(), state.clone(), &labels)?;
            if let Some(branch) = outcomes.branches.remove(&body.entry_block()) {
                join_optional(&mut outcomes.returns, branch)?;
            }
            if let Some(mut next) = outcomes.next.take() {
                normalize_stack(&mut next, 0, results.len())?;
                join_optional(&mut outcomes.returns, next)?;
            }

            if let Some(returned) = outcomes.returns {
                self.results
                    .function_returns
                    .insert(function, returned.stack);
            } else {
                self.results.function_returns.insert(
                    function,
                    results
                        .iter()
                        .copied()
                        .map(|ty| A::Fact::bottom(Some(ty)))
                        .collect(),
                );
            }
        }

        Ok(self.results)
    }

    fn initial_state(
        &mut self,
        function: FunctionId,
    ) -> Result<MachineState<A::Fact>, ForwardError<A::Error>> {
        let info = &self.index.functions[&function];
        let mut locals = BTreeMap::new();
        let root_cx = TransferContext {
            module: self.module,
            index: self.index,
            function,
            instruction: None,
        };

        for (&local, &value) in &info.locals {
            let ty = self.module.locals.get(local).ty();
            let (kind, default) = match self.index.values[value.0].producer {
                ValueProducer::FunctionParameter { .. } => {
                    (RootKind::FunctionParameter, A::Fact::top(ty))
                }
                ValueProducer::LocalInitial { .. } => {
                    let default = match zero_value(ty) {
                        Some(value) => self
                            .analysis
                            .constant(&root_cx, value)
                            .map_err(ForwardError::Analysis)?,
                        None => A::Fact::top(ty),
                    };
                    (RootKind::LocalInitial, default)
                }
                _ => unreachable!("function local root"),
            };
            let rooted = self
                .analysis
                .root(&root_cx, kind, value, default)
                .map_err(ForwardError::Analysis)?;
            self.record_value(value, &rooted);
            locals.insert(local, rooted);
        }

        let mut globals = BTreeMap::new();
        for global in self.module.globals.iter() {
            let value = self.index.globals[&global.id()];
            let default = match &global.kind {
                GlobalKind::Import(_) => A::Fact::top(global.ty),
                GlobalKind::Local(ConstExpr::Value(value)) => self
                    .analysis
                    .constant(&root_cx, *value)
                    .map_err(ForwardError::Analysis)?,
                GlobalKind::Local(ConstExpr::Global(source)) => globals
                    .get(source)
                    .cloned()
                    .unwrap_or_else(|| A::Fact::top(global.ty)),
                GlobalKind::Local(ConstExpr::RefNull(_))
                | GlobalKind::Local(ConstExpr::RefFunc(_)) => A::Fact::top(global.ty),
            };
            let rooted = self
                .analysis
                .root(&root_cx, RootKind::GlobalInitial, value, default)
                .map_err(ForwardError::Analysis)?;
            self.record_value(value, &rooted);
            globals.insert(global.id(), rooted);
        }

        Ok(MachineState {
            stack: Vec::new(),
            locals,
            globals,
        })
    }

    fn execute_sequence(
        &mut self,
        function: FunctionId,
        sequence: InstrSeqId,
        state: MachineState<A::Fact>,
        labels: &[Label],
    ) -> Result<Outcomes<A::Fact>, ForwardError<A::Error>> {
        let body = self.module.funcs.get(function).kind.unwrap_local();
        let instruction_count = body.block(sequence).instrs.len();
        let mut outcomes = Outcomes::continuing(state);

        for instruction_index in 0..instruction_count {
            let Some(next) = outcomes.next.take() else {
                break;
            };
            let key = InstructionKey {
                function,
                sequence,
                index: instruction_index,
            };
            let instruction = body.block(sequence).instrs[instruction_index].0.clone();
            let current = self.execute_instruction(function, key, instruction, next, labels)?;
            merge_outcomes(&mut outcomes, current)?;
        }

        Ok(outcomes)
    }

    fn execute_instruction(
        &mut self,
        function: FunctionId,
        key: InstructionKey,
        instruction: Instr,
        mut state: MachineState<A::Fact>,
        labels: &[Label],
    ) -> Result<Outcomes<A::Fact>, ForwardError<A::Error>> {
        use Instr::*;

        let instruction_id = self.index.instruction_id(key);
        match &instruction {
            Block(op) => self.execute_block(function, instruction_id, op.seq, state, labels),
            Loop(op) => self.execute_loop(function, instruction_id, op.seq, state, labels),
            IfElse(op) => {
                let condition = pop(&mut state.stack, instruction_id)?;
                self.execute_if(
                    function,
                    instruction_id,
                    [op.consequent, op.alternative],
                    condition,
                    state,
                    labels,
                )
            }
            LocalGet(op) => {
                let fact = state.locals.get(&op.local).cloned().ok_or_else(|| {
                    ForwardError::InvalidProgram(format!("uninitialized local {:?}", op.local))
                })?;
                self.emit_outputs(function, instruction_id, vec![fact], &mut state.stack)?;
                Ok(Outcomes::continuing(state))
            }
            LocalSet(op) => {
                let fact = pop(&mut state.stack, instruction_id)?;
                self.record_inputs(instruction_id, std::slice::from_ref(&fact));
                state.locals.insert(op.local, fact);
                Ok(Outcomes::continuing(state))
            }
            LocalTee(op) => {
                let fact = pop(&mut state.stack, instruction_id)?;
                self.record_inputs(instruction_id, std::slice::from_ref(&fact));
                state.locals.insert(op.local, fact.clone());
                self.emit_outputs(function, instruction_id, vec![fact], &mut state.stack)?;
                Ok(Outcomes::continuing(state))
            }
            GlobalGet(op) => {
                let fact = state.globals.get(&op.global).cloned().ok_or_else(|| {
                    ForwardError::InvalidProgram(format!("uninitialized global {:?}", op.global))
                })?;
                self.emit_outputs(function, instruction_id, vec![fact], &mut state.stack)?;
                Ok(Outcomes::continuing(state))
            }
            GlobalSet(op) => {
                let fact = pop(&mut state.stack, instruction_id)?;
                self.record_inputs(instruction_id, std::slice::from_ref(&fact));
                state.globals.insert(op.global, fact);
                Ok(Outcomes::continuing(state))
            }
            Select(_) => {
                let inputs = pop_many(&mut state.stack, 3, instruction_id)?;
                self.record_inputs(instruction_id, &inputs);
                let mut selected = inputs[0].clone();
                selected.join_assign(&inputs[1]);
                self.emit_outputs(function, instruction_id, vec![selected], &mut state.stack)?;
                Ok(Outcomes::continuing(state))
            }
            Br(op) => {
                let branch = branch_state(&state, op.block, labels)?;
                self.record_branch_inputs(instruction_id, &branch, op.block, labels)?;
                let mut result = Outcomes::stopped();
                result.branches.insert(op.block, branch);
                Ok(result)
            }
            BrIf(op) => {
                let condition = pop(&mut state.stack, instruction_id)?;
                let branch = branch_state(&state, op.block, labels)?;
                let mut inputs = branch_values(&branch, op.block, labels)?;
                inputs.push(condition);
                self.record_inputs(instruction_id, &inputs);
                let mut result = Outcomes::continuing(state);
                result.branches.insert(op.block, branch);
                Ok(result)
            }
            BrTable(op) => {
                let selector = pop(&mut state.stack, instruction_id)?;
                let mut inputs = branch_values(&state, op.default, labels)?;
                inputs.push(selector);
                let mut result = Outcomes::stopped();
                let mut targets = op.blocks.to_vec();
                targets.push(op.default);
                for target in targets {
                    let branch = branch_state(&state, target, labels)?;
                    merge_branch(&mut result.branches, target, branch)?;
                }
                self.record_inputs(instruction_id, &inputs);
                Ok(result)
            }
            Return(_) => {
                let result_count = self
                    .module
                    .types
                    .results(self.module.funcs.get(function).ty())
                    .len();
                normalize_stack(&mut state, 0, result_count)?;
                self.record_inputs(instruction_id, &state.stack);
                let mut result = Outcomes::stopped();
                result.returns = Some(state);
                Ok(result)
            }
            Unreachable(_) => Ok(Outcomes::stopped()),
            ReturnCall(_) | ReturnCallIndirect(_) => {
                let shape = instruction_shape(
                    self.module,
                    self.module.funcs.get(function).kind.unwrap_local(),
                    &instruction,
                );
                let inputs = pop_many(&mut state.stack, shape.inputs, instruction_id)?;
                self.record_inputs(instruction_id, &inputs);
                let result_types = self
                    .module
                    .types
                    .results(self.module.funcs.get(function).ty());
                state.stack = result_types.iter().copied().map(A::Fact::top).collect();
                let mut result = Outcomes::stopped();
                result.returns = Some(state);
                Ok(result)
            }
            _ => {
                let body = self.module.funcs.get(function).kind.unwrap_local();
                let shape = instruction_shape(self.module, body, &instruction);
                let inputs = pop_many(&mut state.stack, shape.inputs, instruction_id)?;
                self.record_inputs(instruction_id, &inputs);
                let call_summary = match &instruction {
                    Call(op) => Some(self.calls.resolve(self.module, op.func)),
                    CallIndirect(op) => Some(CallSummary {
                        parameters: self.module.types.params(op.ty).to_vec(),
                        results: self.module.types.results(op.ty).to_vec(),
                        arguments: Vec::new(),
                        memory: super::calls::MemoryEffects::Unknown,
                    }),
                    _ => None,
                };
                let cx = TransferContext {
                    module: self.module,
                    index: self.index,
                    function,
                    instruction: Some(instruction_id),
                };
                let facts = self
                    .analysis
                    .instruction(
                        &cx,
                        &instruction,
                        &inputs,
                        &shape.outputs,
                        call_summary.as_ref(),
                    )
                    .map_err(ForwardError::Analysis)?;
                self.emit_outputs(function, instruction_id, facts, &mut state.stack)?;
                Ok(Outcomes::continuing(state))
            }
        }
    }

    fn execute_block(
        &mut self,
        function: FunctionId,
        instruction: InstructionId,
        sequence: InstrSeqId,
        state: MachineState<A::Fact>,
        labels: &[Label],
    ) -> Result<Outcomes<A::Fact>, ForwardError<A::Error>> {
        let body = self.module.funcs.get(function).kind.unwrap_local();
        let (parameters, results) = sequence_signature(self.module, body, sequence);
        let base = state
            .stack
            .len()
            .checked_sub(parameters.len())
            .ok_or_else(|| {
                ForwardError::InvalidProgram("block parameters exceed stack height".into())
            })?;
        let mut nested_labels = labels.to_vec();
        self.record_inputs(instruction, &state.stack[base..]);
        nested_labels.push(Label {
            sequence,
            base_height: base,
            branch_arity: results.len(),
        });
        let mut outcomes = self.execute_sequence(function, sequence, state, &nested_labels)?;
        let exit = take_control_exit(&mut outcomes, sequence)?;
        outcomes.next = self.finish_control(function, instruction, base, results.len(), exit)?;
        Ok(outcomes)
    }

    fn execute_if(
        &mut self,
        function: FunctionId,
        instruction: InstructionId,
        branches: [InstrSeqId; 2],
        condition: A::Fact,
        state: MachineState<A::Fact>,
        labels: &[Label],
    ) -> Result<Outcomes<A::Fact>, ForwardError<A::Error>> {
        let body = self.module.funcs.get(function).kind.unwrap_local();
        let (parameters, results) = sequence_signature(self.module, body, branches[0]);
        let base = state
            .stack
            .len()
            .checked_sub(parameters.len())
            .ok_or_else(|| {
                ForwardError::InvalidProgram("if parameters exceed stack height".into())
            })?;
        let mut inputs = state.stack[base..].to_vec();
        inputs.push(condition);
        self.record_inputs(instruction, &inputs);

        let mut combined = Outcomes::stopped();
        for sequence in branches {
            let mut nested_labels = labels.to_vec();
            nested_labels.push(Label {
                sequence,
                base_height: base,
                branch_arity: results.len(),
            });
            let mut branch =
                self.execute_sequence(function, sequence, state.clone(), &nested_labels)?;
            let exit = take_control_exit(&mut branch, sequence)?;
            if let Some(exit) = exit {
                join_optional(&mut combined.next, exit)?;
            }
            merge_non_fallthrough(&mut combined, branch)?;
        }

        combined.next = self.finish_control(
            function,
            instruction,
            base,
            results.len(),
            combined.next.take(),
        )?;
        Ok(combined)
    }

    fn execute_loop(
        &mut self,
        function: FunctionId,
        instruction: InstructionId,
        sequence: InstrSeqId,
        state: MachineState<A::Fact>,
        labels: &[Label],
    ) -> Result<Outcomes<A::Fact>, ForwardError<A::Error>> {
        const WIDEN_AFTER: usize = 8;
        const MAX_ITERATIONS: usize = 256;

        let body = self.module.funcs.get(function).kind.unwrap_local();
        let (parameters, results) = sequence_signature(self.module, body, sequence);
        let base = state
            .stack
            .len()
            .checked_sub(parameters.len())
            .ok_or_else(|| {
                ForwardError::InvalidProgram("loop parameters exceed stack height".into())
            })?;
        let mut header = state;
        let mut aggregate = Outcomes::stopped();

        for iteration in 0..MAX_ITERATIONS {
            self.record_inputs(instruction, &header.stack[base..]);
            let mut nested_labels = labels.to_vec();
            nested_labels.push(Label {
                sequence,
                base_height: base,
                branch_arity: parameters.len(),
            });
            let mut pass =
                self.execute_sequence(function, sequence, header.clone(), &nested_labels)?;
            let backedge = pass.branches.remove(&sequence);
            if let Some(exit) = pass.next.take() {
                join_optional(&mut aggregate.next, exit)?;
            }
            merge_non_fallthrough(&mut aggregate, pass)?;

            let Some(backedge) = backedge else {
                break;
            };
            let changed = if iteration >= WIDEN_AFTER {
                header.widen_assign(&backedge)?
            } else {
                header.join_assign(&backedge)?
            };
            if !changed {
                break;
            }
            if iteration + 1 == MAX_ITERATIONS {
                return Err(ForwardError::DidNotConverge {
                    function,
                    loop_id: sequence,
                });
            }
        }

        aggregate.next = self.finish_control(
            function,
            instruction,
            base,
            results.len(),
            aggregate.next.take(),
        )?;
        Ok(aggregate)
    }

    fn finish_control(
        &mut self,
        function: FunctionId,
        instruction: InstructionId,
        base: usize,
        result_count: usize,
        mut exit: Option<MachineState<A::Fact>>,
    ) -> Result<Option<MachineState<A::Fact>>, ForwardError<A::Error>> {
        let Some(mut exit_state) = exit.take() else {
            return Ok(None);
        };
        normalize_stack(&mut exit_state, base, result_count)?;
        let results = exit_state.stack.split_off(base);
        self.emit_outputs(function, instruction, results, &mut exit_state.stack)?;
        Ok(Some(exit_state))
    }

    fn emit_outputs(
        &mut self,
        function: FunctionId,
        instruction: InstructionId,
        facts: Vec<A::Fact>,
        stack: &mut Vec<A::Fact>,
    ) -> Result<(), ForwardError<A::Error>> {
        let outputs = self.index.instructions[instruction.0].outputs.clone();
        if facts.len() != outputs.len() {
            return Err(ForwardError::InvalidProgram(format!(
                "analysis produced {} results for an instruction with {} results",
                facts.len(),
                outputs.len()
            )));
        }
        for (value, fact) in outputs.into_iter().zip(facts) {
            self.record_value(value, &fact);
            let cx = TransferContext {
                module: self.module,
                index: self.index,
                function,
                instruction: Some(instruction),
            };
            let materialized = self
                .analysis
                .materialize(&cx, value, fact)
                .map_err(ForwardError::Analysis)?;
            stack.push(materialized);
        }
        Ok(())
    }

    fn record_value(&mut self, value: ValueId, fact: &A::Fact) {
        self.results.values[value.0].join_assign(fact);
    }

    fn record_inputs(&mut self, instruction: InstructionId, inputs: &[A::Fact]) {
        match &mut self.results.instruction_inputs[instruction.0] {
            Some(existing) => {
                debug_assert_eq!(existing.len(), inputs.len());
                for (left, right) in existing.iter_mut().zip(inputs) {
                    left.join_assign(right);
                }
            }
            slot @ None => *slot = Some(inputs.to_vec()),
        }
    }

    fn record_branch_inputs(
        &mut self,
        instruction: InstructionId,
        branch: &MachineState<A::Fact>,
        target: InstrSeqId,
        labels: &[Label],
    ) -> Result<(), ForwardError<A::Error>> {
        let values = branch_values(branch, target, labels)?;
        self.record_inputs(instruction, &values);
        Ok(())
    }
}

fn pop<F, E>(stack: &mut Vec<F>, instruction: InstructionId) -> Result<F, ForwardError<E>> {
    stack.pop().ok_or_else(|| {
        ForwardError::InvalidProgram(format!("stack underflow at instruction {instruction:?}"))
    })
}

fn pop_many<F, E>(
    stack: &mut Vec<F>,
    count: usize,
    instruction: InstructionId,
) -> Result<Vec<F>, ForwardError<E>> {
    if stack.len() < count {
        return Err(ForwardError::InvalidProgram(format!(
            "stack underflow at instruction {instruction:?}: need {count}, have {}",
            stack.len()
        )));
    }
    Ok(stack.split_off(stack.len() - count))
}

fn normalize_stack<F, E>(
    state: &mut MachineState<F>,
    base: usize,
    result_count: usize,
) -> Result<(), ForwardError<E>> {
    if state.stack.len() < base + result_count {
        return Err(ForwardError::InvalidProgram(format!(
            "control result requires {} stack values, found {}",
            base + result_count,
            state.stack.len()
        )));
    }
    let results = state.stack.split_off(state.stack.len() - result_count);
    state.stack.truncate(base);
    state.stack.extend(results);
    Ok(())
}

fn branch_state<F: Clone, E>(
    state: &MachineState<F>,
    target: InstrSeqId,
    labels: &[Label],
) -> Result<MachineState<F>, ForwardError<E>> {
    let label = labels
        .iter()
        .rev()
        .find(|label| label.sequence == target)
        .ok_or_else(|| ForwardError::InvalidProgram(format!("unknown branch target {target:?}")))?;
    let mut branch = state.clone();
    normalize_stack(&mut branch, label.base_height, label.branch_arity)?;
    Ok(branch)
}

fn branch_values<F: Clone, E>(
    state: &MachineState<F>,
    target: InstrSeqId,
    labels: &[Label],
) -> Result<Vec<F>, ForwardError<E>> {
    let label = labels
        .iter()
        .rev()
        .find(|label| label.sequence == target)
        .ok_or_else(|| ForwardError::InvalidProgram(format!("unknown branch target {target:?}")))?;
    let start = state
        .stack
        .len()
        .checked_sub(label.branch_arity)
        .ok_or_else(|| {
            ForwardError::InvalidProgram(format!(
                "branch to {target:?} requires {} values, found {}",
                label.branch_arity,
                state.stack.len()
            ))
        })?;
    Ok(state.stack[start..].to_vec())
}

fn take_control_exit<F: AbstractDomain, E>(
    outcomes: &mut Outcomes<F>,
    target: InstrSeqId,
) -> Result<Option<MachineState<F>>, ForwardError<E>> {
    let mut exit = outcomes.next.take();
    if let Some(branch) = outcomes.branches.remove(&target) {
        join_optional(&mut exit, branch)?;
    }
    Ok(exit)
}

fn merge_outcomes<F: AbstractDomain, E>(
    destination: &mut Outcomes<F>,
    mut source: Outcomes<F>,
) -> Result<(), ForwardError<E>> {
    destination.next = source.next.take();
    merge_non_fallthrough(destination, source)
}

fn merge_non_fallthrough<F: AbstractDomain, E>(
    destination: &mut Outcomes<F>,
    source: Outcomes<F>,
) -> Result<(), ForwardError<E>> {
    for (target, state) in source.branches {
        merge_branch(&mut destination.branches, target, state)?;
    }
    if let Some(returned) = source.returns {
        join_optional(&mut destination.returns, returned)?;
    }
    Ok(())
}

fn merge_branch<F: AbstractDomain, E>(
    branches: &mut BTreeMap<InstrSeqId, MachineState<F>>,
    target: InstrSeqId,
    state: MachineState<F>,
) -> Result<(), ForwardError<E>> {
    match branches.get_mut(&target) {
        Some(existing) => {
            existing.join_assign(&state)?;
        }
        None => {
            branches.insert(target, state);
        }
    }
    Ok(())
}

fn join_optional<F: AbstractDomain, E>(
    destination: &mut Option<MachineState<F>>,
    source: MachineState<F>,
) -> Result<(), ForwardError<E>> {
    match destination {
        Some(existing) => {
            existing.join_assign(&source)?;
        }
        None => *destination = Some(source),
    }
    Ok(())
}
