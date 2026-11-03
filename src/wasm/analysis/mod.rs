//! Target-independent facts attached to the original Walrus WebAssembly AST.
//!
//! The structured interpreter in [`forward`] handles WebAssembly execution
//! mechanics once. Individual analyses provide small transfer functions and
//! store their results in side tables keyed by stable instruction and value
//! identifiers. Backends can consume these facts directly or use them while
//! lifting the module into a target-oriented IR.

pub mod address;
pub mod backward;
pub mod calls;
pub mod domain;
pub mod forward;
pub mod frame;
pub mod index;
pub mod integer;
pub(crate) mod shape;
pub mod value_flow;

use std::{collections::BTreeMap, convert::Infallible, error::Error, fmt};

use walrus::{FunctionId, Module};

use self::{
    address::{AddressFact, AffineAddressAnalysis},
    backward::{BackwardError, BackwardResults, BitDemand, DemandedBitsAnalysis, run_backward},
    calls::{CallResolver, CallSummary, ConservativeCallResolver, InferredCallResolver},
    forward::{ForwardError, ForwardResults, run_forward},
    frame::{FrameResults, recover_frames},
    index::ProgramIndex,
    integer::{IntegerFact, IntegerValueAnalysis},
    value_flow::{ValueFlowAnalysis, ValueSet},
};

#[derive(Clone, Debug)]
pub struct AnalysisDb {
    pub index: ProgramIndex,
    pub value_flow: ForwardResults<ValueSet>,
    pub integers: ForwardResults<IntegerFact>,
    pub demanded_bits: BackwardResults<BitDemand>,
    pub addresses: ForwardResults<AddressFact>,
    pub frames: FrameResults,
    pub calls: BTreeMap<FunctionId, CallSummary>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AnalysisError {
    Forward(String),
    Backward(String),
}

impl fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Forward(message) => write!(f, "forward analysis failed: {message}"),
            Self::Backward(message) => write!(f, "backward analysis failed: {message}"),
        }
    }
}

impl Error for AnalysisError {}

pub fn analyze(module: &Module) -> Result<AnalysisDb, AnalysisError> {
    analyze_with_imports(module, &ConservativeCallResolver)
}

/// Infers summaries for local functions while asking `imports` for the
/// semantics of imported functions.
pub fn analyze_with_imports<R: CallResolver>(
    module: &Module,
    imports: &R,
) -> Result<AnalysisDb, AnalysisError> {
    let index = ProgramIndex::build(module);
    let value_flow = run_forward(
        module,
        &index,
        &ConservativeCallResolver,
        &mut ValueFlowAnalysis,
    )
    .map_err(forward_error)?;
    let calls = InferredCallResolver::infer(module, &index, &value_flow, imports);
    finish_analysis(module, index, value_flow, &calls)
}

pub fn analyze_with_calls<R: CallResolver>(
    module: &Module,
    calls: &R,
) -> Result<AnalysisDb, AnalysisError> {
    let index = ProgramIndex::build(module);
    let value_flow =
        run_forward(module, &index, calls, &mut ValueFlowAnalysis).map_err(forward_error)?;
    finish_analysis(module, index, value_flow, calls)
}

fn finish_analysis<R: CallResolver>(
    module: &Module,
    index: ProgramIndex,
    value_flow: ForwardResults<ValueSet>,
    calls: &R,
) -> Result<AnalysisDb, AnalysisError> {
    let integers =
        run_forward(module, &index, calls, &mut IntegerValueAnalysis).map_err(forward_error)?;

    let mut address_analysis = AffineAddressAnalysis::new(module);
    let addresses =
        run_forward(module, &index, calls, &mut address_analysis).map_err(forward_error)?;

    let demanded_bits = run_backward(
        module,
        &index,
        calls,
        &value_flow,
        &mut DemandedBitsAnalysis,
    )
    .map_err(backward_error)?;
    let frames = recover_frames(module, &index, calls, &addresses);
    let calls = module
        .funcs
        .iter()
        .map(|function| (function.id(), calls.resolve(module, function.id())))
        .collect();

    Ok(AnalysisDb {
        index,
        value_flow,
        integers,
        demanded_bits,
        addresses,
        frames,
        calls,
    })
}

fn forward_error(error: ForwardError<Infallible>) -> AnalysisError {
    AnalysisError::Forward(error.to_string())
}

fn backward_error(error: BackwardError<Infallible>) -> AnalysisError {
    AnalysisError::Backward(error.to_string())
}

#[cfg(test)]
mod tests {
    use walrus::{
        ConstExpr, FunctionBuilder, Module, ValType,
        ir::{
            BinaryOp, Binop, BrIf, Call, Drop, GlobalGet, GlobalSet, IfElse, Instr, Load, LoadKind,
            LocalGet, LocalTee, Loop, MemArg, Select, Store, StoreKind,
        },
    };

    use super::{
        analyze, analyze_with_calls,
        calls::{ArgumentSummary, CallResolver, CallSummary, MemoryEffects, PointerBehavior},
        domain::AbstractDomain,
        frame::{FrameAccessKind, FrameOffset},
        integer::IntegerFact,
    };

    #[test]
    fn recovers_a_compiler_style_shadow_stack_object() {
        let (module, function) = shadow_stack_module();

        let analyses = analyze(&module).unwrap();
        let frame = &analyses.frames.functions[&function];

        assert_eq!(frame.size_lower_bound, 16);
        assert!(!frame.address_escapes);
        assert_eq!(frame.objects.len(), 1);
        assert_eq!(frame.objects[0].start, -12);
        assert_eq!(frame.objects[0].size, 1);
        assert_eq!(frame.objects[0].accesses.len(), 2);

        let accesses: Vec<_> = frame.objects[0]
            .accesses
            .iter()
            .map(|instruction| &analyses.frames.accesses[instruction])
            .collect();
        assert!(accesses.iter().any(|access| {
            access.kind == FrameAccessKind::Store && access.offset == FrameOffset::Fixed(-12)
        }));
        assert!(accesses.iter().any(|access| {
            access.kind == FrameAccessKind::Load && access.offset == FrameOffset::Fixed(-12)
        }));

        let store = analyses
            .index
            .instructions
            .iter()
            .find(|info| {
                matches!(
                    analyses.index.instruction(&module, info.id),
                    Instr::Store(_)
                )
            })
            .unwrap();
        let stored_value = analyses.value_flow.instruction_inputs[store.id.0]
            .as_ref()
            .unwrap()[1]
            .values
            .iter()
            .next()
            .unwrap();
        assert_eq!(
            analyses.demanded_bits.values[stored_value.0].demanded,
            u8::MAX as u128
        );
        assert!(!analyses.integers.values[stored_value.0].is_bottom());
    }

    #[test]
    fn joins_structured_branches_without_a_cfg() {
        let mut module = Module::default();
        let condition = module.locals.add(ValType::I32);
        let mut builder = FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        let consequent = {
            let mut sequence = builder.dangling_instr_seq(ValType::I32);
            sequence.i32_const(1);
            sequence.id()
        };
        let alternative = {
            let mut sequence = builder.dangling_instr_seq(ValType::I32);
            sequence.i32_const(3);
            sequence.id()
        };
        builder
            .func_body()
            .instr(LocalGet { local: condition })
            .instr(IfElse {
                consequent,
                alternative,
            });
        let function = builder.finish(vec![condition], &mut module.funcs);

        let analyses = analyze(&module).unwrap();
        let returned = &analyses.integers.function_returns[&function][0];
        let IntegerFact::Bits(bits) = returned else {
            panic!("integer result should retain bit facts")
        };

        assert_eq!(bits.known_one & 0b11, 0b01);
        assert_eq!(bits.known_zero & 0b11, 0);
        let parameter = analyses.index.functions[&function].parameters[0];
        assert_eq!(
            analyses.demanded_bits.values[parameter.0].demanded,
            u32::MAX as u128
        );
    }

    #[test]
    fn call_summaries_can_observe_only_selected_bits() {
        let mut module = Module::default();
        let parameter = module.locals.add(ValType::I32);
        let callee = FunctionBuilder::new(&mut module.types, &[ValType::I32], &[])
            .finish(vec![parameter], &mut module.funcs);

        let mut caller = FunctionBuilder::new(&mut module.types, &[], &[]);
        caller
            .func_body()
            .i32_const(0x1234)
            .instr(Call { func: callee });
        let caller = caller.finish(Vec::new(), &mut module.funcs);

        let analyses = analyze_with_calls(&module, &LowByteCall).unwrap();
        let call = analyses.index.functions[&caller]
            .instructions
            .iter()
            .find(|&&id| matches!(analyses.index.instruction(&module, id), Instr::Call(_)))
            .unwrap();
        let argument = analyses.value_flow.instruction_inputs[call.0]
            .as_ref()
            .unwrap()[0]
            .values
            .iter()
            .next()
            .unwrap();

        assert_eq!(analyses.demanded_bits.values[argument.0].demanded, 0xff);
    }

    #[test]
    fn infers_local_pointer_behavior_from_memory_uses() {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);

        let read_pointer = module.locals.add(ValType::I32);
        let mut read_builder = FunctionBuilder::new(&mut module.types, &[ValType::I32], &[]);
        read_builder
            .func_body()
            .instr(LocalGet {
                local: read_pointer,
            })
            .instr(Load {
                memory,
                kind: LoadKind::I32_8 {
                    kind: walrus::ir::ExtendedLoad::ZeroExtend,
                },
                arg: MemArg {
                    align: 1,
                    offset: 0,
                },
            })
            .instr(Drop {});
        let reader = read_builder.finish(vec![read_pointer], &mut module.funcs);

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

        let analyses = analyze(&module).unwrap();

        assert_eq!(
            analyses.calls[&reader].arguments[0].pointer,
            PointerBehavior::ReadOnly
        );
        assert_eq!(
            analyses.calls[&escape].arguments[0].pointer,
            PointerBehavior::Escapes
        );
    }

    #[test]
    fn infers_demands_through_an_untyped_select() {
        let mut module = Module::default();
        let left = module.locals.add(ValType::I32);
        let right = module.locals.add(ValType::I32);
        let condition = module.locals.add(ValType::I32);
        let mut builder = FunctionBuilder::new(
            &mut module.types,
            &[ValType::I32, ValType::I32, ValType::I32],
            &[ValType::I32],
        );
        builder
            .func_body()
            .instr(LocalGet { local: left })
            .instr(LocalGet { local: right })
            .instr(LocalGet { local: condition })
            .instr(Select { ty: None });
        let function = builder.finish(vec![left, right, condition], &mut module.funcs);

        let analyses = analyze(&module).unwrap();

        for parameter in &analyses.index.functions[&function].parameters {
            assert_eq!(
                analyses.demanded_bits.values[parameter.0].demanded,
                u32::MAX as u128
            );
        }
    }

    #[test]
    fn converges_value_flow_across_a_loop_backedge() {
        let mut module = Module::default();
        let counter = module.locals.add(ValType::I32);
        let mut builder = FunctionBuilder::new(&mut module.types, &[ValType::I32], &[ValType::I32]);
        let loop_body = {
            let mut sequence = builder.dangling_instr_seq(None);
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
        builder
            .func_body()
            .instr(Loop { seq: loop_body })
            .instr(LocalGet { local: counter });
        let function = builder.finish(vec![counter], &mut module.funcs);

        let analyses = analyze(&module).unwrap();
        let parameter = analyses.index.functions[&function].parameters[0];

        assert_eq!(
            analyses.demanded_bits.values[parameter.0].demanded,
            u32::MAX as u128
        );
    }

    struct LowByteCall;

    impl CallResolver for LowByteCall {
        fn resolve(&self, module: &Module, function: walrus::FunctionId) -> CallSummary {
            let ty = module.funcs.get(function).ty();
            CallSummary {
                parameters: module.types.params(ty).to_vec(),
                results: module.types.results(ty).to_vec(),
                arguments: vec![ArgumentSummary {
                    observed_bits: Some(0xff),
                    pointer: PointerBehavior::NotAPointer,
                }],
                memory: MemoryEffects::None,
            }
        }
    }

    fn shadow_stack_module() -> (Module, walrus::FunctionId) {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);
        let stack_pointer = module.globals.add_local(
            ValType::I32,
            true,
            false,
            ConstExpr::Value(walrus::ir::Value::I32(1024)),
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
                    kind: walrus::ir::ExtendedLoad::ZeroExtend,
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

        let function = builder.finish(Vec::new(), &mut module.funcs);
        (module, function)
    }
}
