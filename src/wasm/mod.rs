use std::{any::Any, borrow::Borrow, path::Path, rc::Rc, sync::Arc};

use dwarf::parse_dwarf_entries;
use gimli::{read::Dwarf, EndianSlice, LittleEndian};
use ir::wasm_to_ir;
// use gimli::{DebugAbbrev, DebugInfo, Dwarf, EndianSlice, LittleEndian, Section};
use walrus::{Module, ModuleDebugData};
mod ai;
pub mod cli;
mod dwarf;
mod ir;

#[derive(Debug)]
pub enum WasmError {
    Anyhow(anyhow::Error),
    GimliError(gimli::Error),
}

fn extract_dwarf<'a>(module: &'a Module) -> Dwarf<EndianSlice<'a, LittleEndian>> {
    module.debug.dwarf.borrow(|section| {
        EndianSlice::new(section.as_slice(), LittleEndian)
    })
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

    let dwarf = extract_dwarf(&module);

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
