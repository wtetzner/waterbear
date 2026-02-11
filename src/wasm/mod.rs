use std::path::Path;

use ir::wasm_to_ir;
use walrus::Module;

use crate::wasm::ai::analyze;
mod ai;
pub mod cli;
mod dwarf;
mod ir;

#[derive(Debug)]
pub enum WasmError {
    Anyhow(anyhow::Error),
    GimliError(gimli::Error),
}

pub fn build<O: AsRef<Path>>(path: impl AsRef<Path>, output: Option<O>) -> Result<(), WasmError> {
    // use gimli::read::Reader;

    let path = path.as_ref();
    let output = match &output {
        Some(value) => Some(value.as_ref()),
        None => None,
    };
    let module = load_module(path)?;

    let ir = wasm_to_ir(&module)?;

    let mut analyzer = ai::signedness::SignAnalyzer;
    let sign_state = analyze(&module, &mut analyzer).unwrap();
    println!("sign_state: {sign_state:#?}");

    // parse_dwarf_entries(&dwarf)?;

    Ok(())
}

pub fn load_module(path: impl AsRef<Path>) -> Result<Module, WasmError> {
    let path = path.as_ref();
    let module = walrus::Module::from_file(path)?;
    Ok(module)
}

impl From<anyhow::Error> for WasmError {
    fn from(value: anyhow::Error) -> Self {
        WasmError::Anyhow(value)
    }
}

impl From<gimli::Error> for WasmError {
    fn from(value: gimli::Error) -> Self {
        WasmError::GimliError(value)
    }
}
