use std::collections::BTreeMap;

use walrus::{FunctionKind, Module};

use super::WasmError;

pub fn wasm_to_ir(module: &Module) -> Result<Program, WasmError> {
    for export in module.exports.iter() {
        
    }

    for function in module.funcs.iter() {
        match &function.kind {
            FunctionKind::Import(_) | FunctionKind::Uninitialized(_) => {},
            FunctionKind::Local(body) => {
                if let Some(name) = &function.name {
                    println!("{name}:");
                } else {
                    println!("function_{:?}:", function.id());
                }

                let entry = body.entry_block();
                let block = body.block(entry);
                for (instr, instr_loc_id) in block.iter() {
                    println!("  {:?}", instr);
                }
            }
        }
    }

    Ok(Program {})
}

#[derive(Clone, Debug)]
pub struct Program {
}

#[derive(Clone, Debug)]
pub struct Var {
    pub name: String,
    pub ty: ValueType,
}

#[derive(Clone, Debug)]
pub struct Function {
    name: String,
    parameters: Vec<Var>,
    results: Vec<ValueType>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ValueType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    V128,
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    V128(u128),
}

pub trait NativeFunction {
    fn name(&self) -> &str;
    fn params(&self) -> &[ValueType];
    fn results(&self) -> &[ValueType];
}

#[derive(Clone, Debug)]
pub struct NativeModule<F: NativeFunction> {
    functions: BTreeMap<String, F>,
}

impl<F: NativeFunction> Default for NativeModule<F> {
    fn default() -> Self {
        let mut functions = BTreeMap::new();
        // functions.insert(key, value);
        Self { functions: functions }
    }
}

#[derive(Clone, Debug)]
pub struct ExportedFunctionInterface {
    name: String,
    params: Vec<ValueType>,
    results: Vec<ValueType>,
}

pub struct VmuModuleInterface {
    functions: BTreeMap<String, ExportedFunctionInterface>,
}
