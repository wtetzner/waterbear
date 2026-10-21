use std::{collections::BTreeSet, convert::Infallible, fmt};

use walrus::{
    Module, ValType,
    ir::{BinaryOp, Instr, UnaryOp},
};

use super::{
    calls::{CallResolver, PointerBehavior},
    domain::AbstractDomain,
    forward::ForwardResults,
    index::{InstructionId, ProgramIndex},
    value_flow::ValueSet,
};

#[derive(Debug)]
pub enum BackwardError<E> {
    Analysis(E),
    InvalidProgram(String),
    DidNotConverge,
}

impl<E: fmt::Display> fmt::Display for BackwardError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Analysis(error) => error.fmt(f),
            Self::InvalidProgram(message) => message.fmt(f),
            Self::DidNotConverge => f.write_str("backward analysis did not converge"),
        }
    }
}

pub struct BackwardContext<'a> {
    pub module: &'a Module,
    pub index: &'a ProgramIndex,
    pub instruction: InstructionId,
}

/// Defines how demands on instruction results propagate to their operands.
pub trait BackwardAnalysis {
    type Demand: AbstractDomain;
    type Error;

    fn input_demands(
        &mut self,
        _cx: &BackwardContext<'_>,
        _instruction: &Instr,
        input_types: &[Option<ValType>],
        output_demands: &[Self::Demand],
    ) -> Result<Vec<Self::Demand>, Self::Error> {
        let observed = output_demands.iter().any(|demand| !demand.is_bottom());
        Ok(input_types
            .iter()
            .map(|ty| match (observed, ty) {
                (true, Some(ty)) => Self::Demand::top(*ty),
                _ => Self::Demand::bottom(*ty),
            })
            .collect())
    }

    fn observed_bits(&mut self, ty: ValType, _bits: u128) -> Self::Demand {
        Self::Demand::top(ty)
    }
}

#[derive(Clone, Debug)]
pub struct BackwardResults<D> {
    pub values: Vec<D>,
}

pub fn run_backward<A, R>(
    module: &Module,
    index: &ProgramIndex,
    calls: &R,
    flow: &ForwardResults<ValueSet>,
    analysis: &mut A,
) -> Result<BackwardResults<A::Demand>, BackwardError<A::Error>>
where
    A: BackwardAnalysis,
    R: CallResolver,
{
    let max_iterations = index.values.len().saturating_mul(129).max(128);

    let mut results = BackwardResults {
        values: index
            .values
            .iter()
            .map(|value| A::Demand::bottom(value.ty))
            .collect(),
    };

    // Function results are observable at the ABI boundary. Internal call
    // summaries can relax this later without changing the engine.
    for (&function, origins) in &flow.function_returns {
        let result_types = module.types.results(module.funcs.get(function).ty());
        for (origin, ty) in origins.iter().zip(result_types) {
            distribute(&mut results.values, origin, &A::Demand::top(*ty));
        }
    }

    for iteration in 0..max_iterations {
        let before = results.values.clone();

        for info in index.instructions.iter().rev() {
            let instruction = index.instruction(module, info.id);
            let output_demands: Vec<_> = info
                .outputs
                .iter()
                .map(|value| results.values[value.0].clone())
                .collect();

            if is_implicit_copy(instruction) {
                for (&output, demand) in info.outputs.iter().zip(&output_demands) {
                    distribute(&mut results.values, &flow.values[output.0], demand);
                }
            }

            let input_origins = flow.instruction_inputs[info.id.0].as_deref().unwrap_or(&[]);
            let input_types: Vec<_> = input_origins
                .iter()
                .map(|origins| source_type(index, flow, origins))
                .collect();
            let cx = BackwardContext {
                module,
                index,
                instruction: info.id,
            };
            let mut demands = analysis
                .input_demands(&cx, instruction, &input_types, &output_demands)
                .map_err(BackwardError::Analysis)?;
            apply_call_summary(
                module,
                calls,
                analysis,
                instruction,
                &input_types,
                &mut demands,
            );

            if demands.len() != input_origins.len() {
                return Err(BackwardError::InvalidProgram(format!(
                    "analysis produced {} operand demands for instruction {:?}, expected {}",
                    demands.len(),
                    info.id,
                    input_origins.len()
                )));
            }
            for (origins, demand) in input_origins.iter().zip(&demands) {
                distribute(&mut results.values, origins, demand);
            }
        }

        if results.values == before {
            return Ok(results);
        }
        if iteration + 1 == max_iterations {
            return Err(BackwardError::DidNotConverge);
        }
    }

    unreachable!()
}

fn distribute<D: AbstractDomain>(demands: &mut [D], origins: &ValueSet, demand: &D) {
    for origin in &origins.values {
        demands[origin.0].join_assign(demand);
    }
}

fn source_type(
    index: &ProgramIndex,
    flow: &ForwardResults<ValueSet>,
    origins: &ValueSet,
) -> Option<ValType> {
    fn visit(
        index: &ProgramIndex,
        flow: &ForwardResults<ValueSet>,
        value: super::index::ValueId,
        visited: &mut BTreeSet<super::index::ValueId>,
    ) -> Option<ValType> {
        if !visited.insert(value) {
            return None;
        }
        index.values[value.0].ty.or_else(|| {
            flow.values[value.0]
                .values
                .iter()
                .find_map(|origin| visit(index, flow, *origin, visited))
        })
    }

    let mut visited = BTreeSet::new();
    origins
        .values
        .iter()
        .find_map(|value| visit(index, flow, *value, &mut visited))
}

fn is_implicit_copy(instruction: &Instr) -> bool {
    matches!(
        instruction,
        Instr::Block(_)
            | Instr::Loop(_)
            | Instr::IfElse(_)
            | Instr::LocalGet(_)
            | Instr::GlobalGet(_)
    )
}

fn apply_call_summary<A: BackwardAnalysis, R: CallResolver>(
    module: &Module,
    calls: &R,
    analysis: &mut A,
    instruction: &Instr,
    input_types: &[Option<ValType>],
    demands: &mut [A::Demand],
) {
    let function = match instruction {
        Instr::Call(call) => call.func,
        Instr::ReturnCall(call) => call.func,
        _ => return,
    };
    let summary = calls.resolve(module, function);
    for (index, (demand, ty)) in demands.iter_mut().zip(input_types).enumerate() {
        let Some(ty) = ty else {
            continue;
        };
        let Some(argument) = summary.arguments.get(index) else {
            demand.join_assign(&A::Demand::top(*ty));
            continue;
        };
        if let Some(bits) = argument.observed_bits {
            demand.join_assign(&analysis.observed_bits(*ty, bits));
        } else if argument.pointer != PointerBehavior::NotAPointer {
            demand.join_assign(&A::Demand::top(*ty));
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BitDemand {
    pub ty: Option<ValType>,
    pub demanded: u128,
    pub opaque: bool,
}

impl BitDemand {
    pub fn bits(ty: ValType, demanded: u128) -> Self {
        Self {
            ty: Some(ty),
            demanded: demanded & type_mask(ty),
            opaque: false,
        }
    }

    fn highest_prefix(&self) -> u128 {
        if self.demanded == 0 {
            0
        } else {
            low_mask((128 - self.demanded.leading_zeros()).min(self.width()))
        }
    }

    fn width(&self) -> u32 {
        self.ty.map(type_width).unwrap_or(128)
    }
}

impl AbstractDomain for BitDemand {
    fn bottom(ty: Option<ValType>) -> Self {
        Self {
            ty,
            demanded: 0,
            opaque: false,
        }
    }

    fn top(ty: ValType) -> Self {
        match ty {
            ValType::Ref(_) => Self {
                ty: Some(ty),
                demanded: 0,
                opaque: true,
            },
            _ => Self::bits(ty, type_mask(ty)),
        }
    }

    fn is_bottom(&self) -> bool {
        self.demanded == 0 && !self.opaque
    }

    fn join_assign(&mut self, other: &Self) -> bool {
        let previous = self.clone();
        if self.ty.is_none() {
            self.ty = other.ty;
        }
        self.demanded |= other.demanded;
        self.opaque |= other.opaque;
        *self != previous
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DemandedBitsAnalysis;

impl BackwardAnalysis for DemandedBitsAnalysis {
    type Demand = BitDemand;
    type Error = Infallible;

    fn input_demands(
        &mut self,
        _cx: &BackwardContext<'_>,
        instruction: &Instr,
        input_types: &[Option<ValType>],
        output_demands: &[Self::Demand],
    ) -> Result<Vec<Self::Demand>, Self::Error> {
        let bottom = || {
            input_types
                .iter()
                .map(|ty| BitDemand::bottom(*ty))
                .collect::<Vec<_>>()
        };
        let top = || {
            input_types
                .iter()
                .map(|ty| match ty {
                    Some(ty) => BitDemand::top(*ty),
                    None => BitDemand::bottom(None),
                })
                .collect::<Vec<_>>()
        };
        let output = output_demands.first();

        let demands = match instruction {
            Instr::Const(_)
            | Instr::LocalGet(_)
            | Instr::GlobalGet(_)
            | Instr::RefNull(_)
            | Instr::RefFunc(_)
            | Instr::MemorySize(_)
            | Instr::TableSize(_) => bottom(),

            Instr::LocalSet(_) | Instr::Drop(_) => bottom(),
            Instr::LocalTee(_) | Instr::GlobalSet(_) => output
                .cloned()
                .map(|demand| vec![demand])
                .unwrap_or_else(top),

            Instr::Binop(op) => binary_demands(op.op, input_types, output),
            Instr::Unop(op) => unary_demands(op.op, input_types, output),
            Instr::Select(_) => {
                let value_demand = output.cloned().unwrap_or_else(|| BitDemand::bottom(None));
                vec![
                    value_demand.clone(),
                    value_demand,
                    input_types[2]
                        .map(BitDemand::top)
                        .unwrap_or_else(|| BitDemand::bottom(None)),
                ]
            }
            Instr::Load(_) | Instr::LoadSimd(_) => top(),
            Instr::Store(op) => vec![
                input_types[0]
                    .map(BitDemand::top)
                    .unwrap_or_else(|| BitDemand::bottom(None)),
                input_types[1]
                    .map(|ty| BitDemand::bits(ty, low_mask(op.kind.width() * 8)))
                    .unwrap_or_else(|| BitDemand::bottom(None)),
            ],
            Instr::BrIf(_) | Instr::BrTable(_) | Instr::IfElse(_) => {
                let mut demands = bottom();
                if let Some((last, Some(ty))) = demands.last_mut().zip(input_types.last()) {
                    *last = BitDemand::top(*ty);
                }
                demands
            }
            Instr::Return(_) => top(),
            Instr::Br(_) | Instr::Unreachable(_) => bottom(),
            Instr::Call(_) | Instr::ReturnCall(_) => bottom(),
            Instr::CallIndirect(_) | Instr::ReturnCallIndirect(_) => top(),
            Instr::Block(_) | Instr::Loop(_) => bottom(),
            _ => {
                if output.is_some_and(|demand| !demand.is_bottom())
                    || has_observable_effect(instruction)
                {
                    top()
                } else {
                    bottom()
                }
            }
        };
        Ok(demands)
    }

    fn observed_bits(&mut self, ty: ValType, bits: u128) -> Self::Demand {
        BitDemand::bits(ty, bits)
    }
}

fn binary_demands(
    op: BinaryOp,
    input_types: &[Option<ValType>],
    output: Option<&BitDemand>,
) -> Vec<BitDemand> {
    use BinaryOp::*;
    let Some(output) = output else {
        return input_types
            .iter()
            .map(|ty| BitDemand::bottom(*ty))
            .collect();
    };
    if output.is_bottom() {
        return input_types
            .iter()
            .map(|ty| BitDemand::bottom(*ty))
            .collect();
    }

    let same_bits = |index: usize| match input_types[index] {
        Some(ty) => BitDemand::bits(ty, output.demanded),
        None => BitDemand::bottom(None),
    };
    let prefix = |index: usize| match input_types[index] {
        Some(ty) => BitDemand::bits(ty, output.highest_prefix()),
        None => BitDemand::bottom(None),
    };
    let full = |index: usize| match input_types[index] {
        Some(ty) => BitDemand::top(ty),
        None => BitDemand::bottom(None),
    };

    match op {
        I32And | I32Or | I32Xor | I64And | I64Or | I64Xor | V128And | V128Or | V128Xor
        | V128AndNot => vec![same_bits(0), same_bits(1)],
        I32Add | I32Sub | I32Mul | I64Add | I64Sub | I64Mul => {
            vec![prefix(0), prefix(1)]
        }
        I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU
        | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS | I64GeU => {
            vec![full(0), full(1)]
        }
        _ => vec![full(0), full(1)],
    }
}

fn unary_demands(
    op: UnaryOp,
    input_types: &[Option<ValType>],
    output: Option<&BitDemand>,
) -> Vec<BitDemand> {
    use UnaryOp::*;
    let Some(input_ty) = input_types.first().copied().flatten() else {
        return vec![BitDemand::bottom(None)];
    };
    let Some(output) = output else {
        return vec![BitDemand::bottom(Some(input_ty))];
    };
    if output.is_bottom() {
        return vec![BitDemand::bottom(Some(input_ty))];
    }

    let demand = match op {
        I32WrapI64 => BitDemand::bits(input_ty, output.demanded & u32::MAX as u128),
        I64ExtendUI32 => BitDemand::bits(input_ty, output.demanded & u32::MAX as u128),
        I64ExtendSI32 => extension_demand(input_ty, output, 32),
        I32Extend8S | I64Extend8S => extension_demand(input_ty, output, 8),
        I32Extend16S | I64Extend16S => extension_demand(input_ty, output, 16),
        I64Extend32S => extension_demand(input_ty, output, 32),
        _ => BitDemand::top(input_ty),
    };
    vec![demand]
}

fn extension_demand(input_ty: ValType, output: &BitDemand, source_width: u32) -> BitDemand {
    let source_mask = low_mask(source_width);
    let sign_bit = 1u128 << (source_width - 1);
    let upper_is_demanded = output.demanded & !source_mask != 0;
    let demanded = (output.demanded & source_mask) | if upper_is_demanded { sign_bit } else { 0 };
    BitDemand::bits(input_ty, demanded)
}

fn has_observable_effect(instruction: &Instr) -> bool {
    matches!(
        instruction,
        Instr::MemoryGrow(_)
            | Instr::MemoryInit(_)
            | Instr::DataDrop(_)
            | Instr::MemoryCopy(_)
            | Instr::MemoryFill(_)
            | Instr::AtomicRmw(_)
            | Instr::Cmpxchg(_)
            | Instr::AtomicNotify(_)
            | Instr::AtomicWait(_)
            | Instr::TableSet(_)
            | Instr::TableGrow(_)
            | Instr::TableFill(_)
            | Instr::TableInit(_)
            | Instr::ElemDrop(_)
            | Instr::TableCopy(_)
    )
}

fn type_width(ty: ValType) -> u32 {
    match ty {
        ValType::I32 | ValType::F32 => 32,
        ValType::I64 | ValType::F64 => 64,
        ValType::V128 => 128,
        ValType::Ref(_) => 0,
    }
}

fn type_mask(ty: ValType) -> u128 {
    low_mask(type_width(ty))
}

fn low_mask(bits: u32) -> u128 {
    match bits {
        0 => 0,
        128.. => u128::MAX,
        bits => (1u128 << bits) - 1,
    }
}
