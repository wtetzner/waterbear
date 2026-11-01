use std::{error::Error, fmt, path::Path};

use walrus::Module;

pub mod analysis;
pub mod cli;
pub mod hir;
pub mod hir_printer;
pub mod lir;
pub mod printer;

#[derive(Debug)]
pub enum WasmError {
    Anyhow(anyhow::Error),
    Analysis(analysis::AnalysisError),
}

impl fmt::Display for WasmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Anyhow(error) => error.fmt(f),
            Self::Analysis(error) => error.fmt(f),
        }
    }
}

impl Error for WasmError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Anyhow(error) => Some(error.as_ref()),
            Self::Analysis(error) => Some(error),
        }
    }
}

pub fn build<O: AsRef<Path>>(
    path: impl AsRef<Path>,
    _entry: &str,
    _output: Option<O>,
) -> Result<(), WasmError> {
    let path = path.as_ref();
    let module = load_module(path)?;
    let analyses = analysis::analyze(&module)?;

    print!("{}", printer::render(&module, &analyses));

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

impl From<analysis::AnalysisError> for WasmError {
    fn from(value: analysis::AnalysisError) -> Self {
        WasmError::Analysis(value)
    }
}
