//! Conservative mutability facts for WebAssembly data segments.

use std::collections::{BTreeMap, BTreeSet};

use walrus::{DataId, DataKind, Function, FunctionId, FunctionKind, Module, ValType, ir::Instr};

use super::{
    address::AddressFact,
    calls::{CallSummary, MemoryEffects, PointerBehavior},
    forward::ForwardResults,
    index::ProgramIndex,
};

/// Active data segments that may be modified after WebAssembly instantiation.
#[derive(Clone, Debug, Default)]
pub struct DataMutability {
    mutable: BTreeSet<DataId>,
}

impl DataMutability {
    pub fn is_mutable(&self, data: DataId) -> bool {
        self.mutable.contains(&data)
    }
}

struct DataRange {
    id: DataId,
    start: u64,
    end: u64,
}

pub fn analyze(
    module: &Module,
    index: &ProgramIndex,
    addresses: &ForwardResults<AddressFact>,
    calls: &BTreeMap<FunctionId, CallSummary>,
) -> DataMutability {
    let mut result = DataMutability::default();
    let mut ranges = Vec::new();
    for data in module.data.iter() {
        let DataKind::Active { offset, .. } = data.kind else {
            continue;
        };
        let Some(start) = const_expr_address(module, offset) else {
            result.mutable.insert(data.id());
            continue;
        };
        ranges.push(DataRange {
            id: data.id(),
            start,
            end: start.saturating_add(data.value.len() as u64),
        });
    }

    for instruction in &index.instructions {
        let operation = index.instruction(module, instruction.id);
        let inputs = addresses.instruction_inputs[instruction.id.0]
            .as_deref()
            .unwrap_or(&[]);
        match operation {
            Instr::Store(store) => {
                mark_address(&mut result, &ranges, inputs.first(), store.arg.offset);
            }
            Instr::MemoryCopy(_) | Instr::MemoryFill(_) | Instr::MemoryInit(_) => {
                mark_address(&mut result, &ranges, inputs.first(), 0);
            }
            Instr::AtomicRmw(_)
            | Instr::Cmpxchg(_)
            | Instr::AtomicNotify(_)
            | Instr::AtomicWait(_) => {
                mark_address(&mut result, &ranges, inputs.first(), 0);
            }
            Instr::Call(call) => {
                mark_call(
                    &mut result,
                    &ranges,
                    module.funcs.get(call.func),
                    &calls[&call.func],
                    inputs,
                );
            }
            Instr::ReturnCall(call) => mark_call(
                &mut result,
                &ranges,
                module.funcs.get(call.func),
                &calls[&call.func],
                inputs,
            ),
            Instr::CallIndirect(_) | Instr::ReturnCallIndirect(_) => {
                mark_all(&mut result, &ranges);
            }
            _ => {}
        }
    }

    result
}

fn mark_call(
    result: &mut DataMutability,
    ranges: &[DataRange],
    function: &Function,
    summary: &CallSummary,
    inputs: &[AddressFact],
) {
    let mut has_write_pointer = false;
    for (argument, address) in summary.arguments.iter().zip(inputs) {
        if matches!(
            argument.pointer,
            PointerBehavior::ReadWrite | PointerBehavior::Escapes
        ) {
            has_write_pointer = true;
            mark_address(result, ranges, Some(address), 0);
        }
    }

    if matches!(function.kind, FunctionKind::Import(_))
        && (summary.memory == MemoryEffects::Unknown
            || (matches!(
                summary.memory,
                MemoryEffects::Write | MemoryEffects::ReadWrite
            ) && !has_write_pointer))
    {
        mark_all(result, ranges);
    }
}

fn mark_address(
    result: &mut DataMutability,
    ranges: &[DataRange],
    address: Option<&AddressFact>,
    offset: u32,
) {
    match address {
        Some(AddressFact::Constant { value, ty }) => {
            let address = integer_address(*value, *ty).wrapping_add(u64::from(offset));
            for range in ranges {
                if address >= range.start && address < range.end {
                    result.mutable.insert(range.id);
                }
            }
        }
        Some(AddressFact::Affine { .. }) | Some(AddressFact::Bottom(_)) => {}
        Some(AddressFact::Unknown(_)) | None => mark_all(result, ranges),
    }
}

fn mark_all(result: &mut DataMutability, ranges: &[DataRange]) {
    result.mutable.extend(ranges.iter().map(|range| range.id));
}

fn integer_address(value: i64, ty: ValType) -> u64 {
    match ty {
        ValType::I32 => value as i32 as u32 as u64,
        ValType::I64 => value as u64,
        _ => 0,
    }
}

fn const_expr_address(module: &Module, expression: walrus::ConstExpr) -> Option<u64> {
    match expression {
        walrus::ConstExpr::Value(walrus::ir::Value::I32(value)) => Some(value as u32 as u64),
        walrus::ConstExpr::Value(walrus::ir::Value::I64(value)) => Some(value as u64),
        walrus::ConstExpr::Global(global) => match module.globals.get(global).kind {
            walrus::GlobalKind::Local(expression) => const_expr_address(module, expression),
            walrus::GlobalKind::Import(_) => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use walrus::{
        ConstExpr, DataKind, FunctionBuilder, Module,
        ir::{Drop, Load, LoadKind, MemArg, Store, StoreKind, Value},
    };

    use super::super::analyze;

    #[test]
    fn data_read_only_when_only_loaded() {
        let (mut module, memory, data) = module_with_data();
        let mut main = FunctionBuilder::new(&mut module.types, &[], &[]);
        main.func_body()
            .i32_const(32)
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
        main.finish(Vec::new(), &mut module.funcs);

        let analyses = analyze(&module).unwrap();

        assert!(!analyses.data_mutability.is_mutable(data));
    }

    #[test]
    fn data_becomes_mutable_when_stored() {
        let (mut module, memory, data) = module_with_data();
        let mut main = FunctionBuilder::new(&mut module.types, &[], &[]);
        main.func_body().i32_const(32).i32_const(7).instr(Store {
            memory,
            kind: StoreKind::I32_8 { atomic: false },
            arg: MemArg {
                align: 1,
                offset: 0,
            },
        });
        main.finish(Vec::new(), &mut module.funcs);

        let analyses = analyze(&module).unwrap();

        assert!(analyses.data_mutability.is_mutable(data));
    }

    fn module_with_data() -> (Module, walrus::MemoryId, walrus::DataId) {
        let mut module = Module::default();
        let memory = module.memories.add_local(false, false, 1, None, None);
        let data = module.data.add(
            DataKind::Active {
                memory,
                offset: ConstExpr::Value(Value::I32(32)),
            },
            vec![1, 2, 3, 4],
        );
        (module, memory, data)
    }
}
