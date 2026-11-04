//! Consistency checks for the collection of analysis side tables.

use std::collections::BTreeSet;

use walrus::{FunctionId, FunctionKind, Module};

use super::{AnalysisDb, AnalysisError, forward::ForwardResults, index::ValueProducer};

pub(super) fn validate(module: &Module, analyses: &AnalysisDb) -> Result<(), AnalysisError> {
    let index = &analyses.index;
    for (position, value) in index.values.iter().enumerate() {
        if value.id.0 != position {
            return invalid(format!("value at index {position} has ID {:?}", value.id));
        }
        match &value.producer {
            ValueProducer::Instruction {
                instruction,
                result,
            } => {
                let Some(info) = index.instructions.get(instruction.0) else {
                    return invalid(format!(
                        "value {:?} refers to missing instruction {instruction:?}",
                        value.id
                    ));
                };
                if info.outputs.get(*result) != Some(&value.id) {
                    return invalid(format!(
                        "value {:?} is not result {result} of instruction {instruction:?}",
                        value.id
                    ));
                }
            }
            ValueProducer::FunctionParameter {
                function,
                parameter,
                local,
            } => {
                let Some(info) = index.functions.get(function) else {
                    return invalid(format!(
                        "value {:?} is a parameter of unindexed function {function:?}",
                        value.id
                    ));
                };
                if info.parameters.get(*parameter) != Some(&value.id)
                    || info.locals.get(local) != Some(&value.id)
                {
                    return invalid(format!(
                        "value {:?} is not parameter {parameter} and local {local:?} of {function:?}",
                        value.id
                    ));
                }
            }
            ValueProducer::LocalInitial { function, local } => {
                let Some(info) = index.functions.get(function) else {
                    return invalid(format!(
                        "value {:?} is a local of unindexed function {function:?}",
                        value.id
                    ));
                };
                if info.locals.get(local) != Some(&value.id) {
                    return invalid(format!(
                        "value {:?} is not initial value of local {local:?} in {function:?}",
                        value.id
                    ));
                }
            }
            ValueProducer::GlobalInitial(global) => {
                if index.globals.get(global) != Some(&value.id) {
                    return invalid(format!(
                        "value {:?} is not initial value of global {global:?}",
                        value.id
                    ));
                }
            }
        }
    }

    for (position, instruction) in index.instructions.iter().enumerate() {
        if instruction.id.0 != position {
            return invalid(format!(
                "instruction at index {position} has ID {:?}",
                instruction.id
            ));
        }
        if !index.functions.contains_key(&instruction.key.function) {
            return invalid(format!(
                "instruction {:?} belongs to unindexed function {:?}",
                instruction.id, instruction.key.function
            ));
        }
        for (result, output) in instruction.outputs.iter().enumerate() {
            let Some(value) = index.values.get(output.0) else {
                return invalid(format!(
                    "instruction {:?} has an out-of-range result",
                    instruction.id
                ));
            };
            if !matches!(
                value.producer,
                ValueProducer::Instruction {
                    instruction: producer,
                    result: producer_result,
                } if producer == instruction.id && producer_result == result
            ) {
                return invalid(format!(
                    "result {result} of instruction {:?} has a different producer",
                    instruction.id
                ));
            }
        }
    }

    let local_functions = module
        .funcs
        .iter()
        .filter_map(|function| {
            matches!(function.kind, FunctionKind::Local(_)).then_some(function.id())
        })
        .collect::<BTreeSet<_>>();
    let indexed_functions = index.functions.keys().copied().collect::<BTreeSet<_>>();
    if indexed_functions != local_functions {
        return invalid("program index does not contain exactly the local functions".into());
    }

    for (&function, info) in &index.functions {
        if info.function != function {
            return invalid(format!(
                "function index key {function:?} contains record for {:?}",
                info.function
            ));
        }
        let parameter_count = module.types.params(module.funcs.get(function).ty()).len();
        if info.parameters.len() != parameter_count {
            return invalid(format!(
                "function {function:?} has {} indexed parameters, expected {parameter_count}",
                info.parameters.len()
            ));
        }
        for instruction in &info.instructions {
            let Some(instruction) = index.instructions.get(instruction.0) else {
                return invalid(format!(
                    "function {function:?} refers to missing instruction {instruction:?}"
                ));
            };
            if instruction.key.function != function {
                return invalid(format!(
                    "function {function:?} contains instruction owned by {:?}",
                    instruction.key.function
                ));
            }
        }
        if info
            .locals
            .values()
            .chain(&info.parameters)
            .any(|value| value.0 >= index.values.len())
        {
            return invalid(format!("function {function:?} has an out-of-range value"));
        }
    }

    if index.globals.len() != module.globals.iter().count()
        || index
            .globals
            .values()
            .any(|value| value.0 >= index.values.len())
    {
        return invalid("program index global table is incomplete or out of range".into());
    }

    validate_forward(
        "value-flow",
        &analyses.value_flow,
        analyses,
        module,
        &local_functions,
    )?;
    validate_forward(
        "integer",
        &analyses.integers,
        analyses,
        module,
        &local_functions,
    )?;
    validate_forward(
        "pointer-origin",
        &analyses.pointer_origins,
        analyses,
        module,
        &local_functions,
    )?;
    validate_forward(
        "address",
        &analyses.addresses,
        analyses,
        module,
        &local_functions,
    )?;

    if analyses.demanded_bits.values.len() != index.values.len() {
        return invalid(format!(
            "demanded-bits analysis has {} values, expected {}",
            analyses.demanded_bits.values.len(),
            index.values.len()
        ));
    }
    let demanded_functions = analyses
        .demanded_bits
        .function_results
        .keys()
        .copied()
        .collect::<BTreeSet<_>>();
    if demanded_functions != local_functions {
        return invalid("demanded-bits result table does not match local functions".into());
    }
    for (&function, values) in &analyses.demanded_bits.function_results {
        let expected = module.types.results(module.funcs.get(function).ty()).len();
        if values.len() != expected {
            return invalid(format!(
                "demanded-bits analysis has {} results for {function:?}, expected {expected}",
                values.len()
            ));
        }
    }

    let all_functions = module
        .funcs
        .iter()
        .map(|function| function.id())
        .collect::<BTreeSet<_>>();
    let summarized_functions = analyses.calls.keys().copied().collect::<BTreeSet<_>>();
    if summarized_functions != all_functions {
        return invalid("call-summary table does not match module functions".into());
    }
    for (&function, summary) in &analyses.calls {
        let ty = module.funcs.get(function).ty();
        let parameters = module.types.params(ty);
        let results = module.types.results(ty);
        if summary.parameters != parameters
            || summary.results != results
            || summary.arguments.len() != parameters.len()
        {
            return invalid(format!(
                "call summary for {function:?} does not match its function type"
            ));
        }
    }

    let framed_functions = analyses
        .frames
        .functions
        .keys()
        .copied()
        .collect::<BTreeSet<_>>();
    if framed_functions != local_functions {
        return invalid("frame-analysis table does not match local functions".into());
    }
    if analyses
        .frames
        .accesses
        .iter()
        .any(|(id, access)| id != &access.instruction || id.0 >= index.instructions.len())
    {
        return invalid("frame-analysis access table is inconsistent".into());
    }
    for (&function, frame) in &analyses.frames.functions {
        let instructions = analyses.index.functions[&function]
            .instructions
            .iter()
            .copied()
            .collect::<BTreeSet<_>>();
        for (position, object) in frame.objects.iter().enumerate() {
            if object.id.0 != position {
                return invalid(format!(
                    "frame object at index {position} in {function:?} has ID {:?}",
                    object.id
                ));
            }
            for access in &object.accesses {
                if !instructions.contains(access)
                    || analyses
                        .frames
                        .accesses
                        .get(access)
                        .and_then(|access| access.object)
                        != Some(object.id)
                {
                    return invalid(format!(
                        "frame object {:?} in {function:?} has an inconsistent access {access:?}",
                        object.id
                    ));
                }
            }
        }
        for access in &frame.dynamic_accesses {
            if !instructions.contains(access)
                || analyses
                    .frames
                    .accesses
                    .get(access)
                    .is_none_or(|access| access.object.is_some())
            {
                return invalid(format!(
                    "function {function:?} has an inconsistent dynamic frame access {access:?}"
                ));
            }
        }
    }
    for (&instruction, access) in &analyses.frames.accesses {
        let function = index.instructions[instruction.0].key.function;
        let frame = &analyses.frames.functions[&function];
        let listed = match access.object {
            Some(object) => frame
                .objects
                .get(object.0)
                .is_some_and(|object| object.accesses.contains(&instruction)),
            None => frame.dynamic_accesses.contains(&instruction),
        };
        if !listed {
            return invalid(format!(
                "frame access {instruction:?} is not listed by function {function:?}"
            ));
        }
    }

    Ok(())
}

fn validate_forward<F>(
    name: &str,
    results: &ForwardResults<F>,
    analyses: &AnalysisDb,
    module: &Module,
    local_functions: &BTreeSet<FunctionId>,
) -> Result<(), AnalysisError> {
    if results.values.len() != analyses.index.values.len() {
        return invalid(format!(
            "{name} analysis has {} values, expected {}",
            results.values.len(),
            analyses.index.values.len()
        ));
    }
    if results.instruction_inputs.len() != analyses.index.instructions.len() {
        return invalid(format!(
            "{name} analysis has {} instruction inputs, expected {}",
            results.instruction_inputs.len(),
            analyses.index.instructions.len()
        ));
    }
    let functions = results
        .function_returns
        .keys()
        .copied()
        .collect::<BTreeSet<_>>();
    if &functions != local_functions {
        return invalid(format!(
            "{name} analysis return table does not match local functions"
        ));
    }
    for (&function, values) in &results.function_returns {
        let expected = module.types.results(module.funcs.get(function).ty()).len();
        if values.len() != expected {
            return invalid(format!(
                "{name} analysis has {} returns for {function:?}, expected {expected}",
                values.len()
            ));
        }
    }
    Ok(())
}

fn invalid<T>(message: String) -> Result<T, AnalysisError> {
    Err(AnalysisError::InvalidDatabase(message))
}
