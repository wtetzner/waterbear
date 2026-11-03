use std::{error::Error, fmt, fs, io::Write, path::Path};

use walrus::Module;

use crate::{AssembleOptions, files::SourceFiles};

pub mod analysis;
pub mod asm_lower;
pub mod cli;
pub mod config;
pub mod hir;
pub mod hir_lower;
pub mod hir_printer;
pub mod hir_tail_calls;
pub mod intrinsics;
pub mod lir;
pub mod lir_allocate;
pub mod lir_lower;
pub mod lir_printer;
pub mod package;
pub mod printer;

#[derive(Debug)]
pub enum WasmError {
    Anyhow(anyhow::Error),
    Analysis(analysis::AnalysisError),
    Lower(hir_lower::LowerError),
    LirLower(lir_lower::LowerError),
    LirAllocate(lir_allocate::AllocationError),
    AsmLower(asm_lower::LowerError),
    Assembly(Box<crate::asm::AssemblyError>),
    Config(config::ConfigError),
    Package(package::PackageError),
}

impl fmt::Display for WasmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Anyhow(error) => error.fmt(f),
            Self::Analysis(error) => error.fmt(f),
            Self::Lower(error) => error.fmt(f),
            Self::LirLower(error) => error.fmt(f),
            Self::LirAllocate(error) => error.fmt(f),
            Self::AsmLower(error) => error.fmt(f),
            Self::Assembly(error) => {
                write!(f, "could not assemble generated VMU program: {error:?}")
            }
            Self::Config(error) => error.fmt(f),
            Self::Package(error) => error.fmt(f),
        }
    }
}

impl Error for WasmError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Anyhow(error) => Some(error.as_ref()),
            Self::Analysis(error) => Some(error),
            Self::Lower(error) => Some(error),
            Self::LirLower(error) => Some(error),
            Self::LirAllocate(error) => Some(error),
            Self::AsmLower(error) => Some(error),
            Self::Assembly(_) => None,
            Self::Config(error) => Some(error),
            Self::Package(error) => Some(error),
        }
    }
}

pub fn build<O: AsRef<Path>>(
    path: impl AsRef<Path>,
    entry: Option<&str>,
    config_path: Option<impl AsRef<Path>>,
    output_type: cli::OutputType,
    out: Option<O>,
) -> Result<(), WasmError> {
    let path = path.as_ref();
    let out = out.as_ref().map(AsRef::as_ref);
    let config = config::BuildConfig::load(config_path.as_ref().map(AsRef::as_ref))?;
    let entry = config.entry(entry);
    let module = load_module(path)?;
    let analyses = analysis::analyze_with_imports(&module, &intrinsics::IntrinsicCallResolver)?;
    let hir = hir_lower::lower_with_interrupts(&module, &analyses, entry, &config.interrupts)?;

    if output_type == cli::OutputType::Hir {
        return write_output(out, text_bytes(hir_printer::render(&hir)));
    }

    let lir = lir_lower::lower(&hir)?;

    if output_type == cli::OutputType::Lir {
        return write_output(out, text_bytes(lir_printer::render(&lir)));
    }

    let allocated_lir = lir_allocate::allocate(&lir)?;
    let code = asm_lower::lower(&allocated_lir)?;
    let assembly = package::package(&config.package, &allocated_lir, &code)?;

    if output_type == cli::OutputType::Asm {
        return write_output(out, text_bytes(assembly.to_string()));
    }

    let mut files = SourceFiles::new();
    let vms = crate::assemble_statements_with_options(
        &mut files,
        &assembly,
        AssembleOptions {
            optimize_jumps: true,
            ..AssembleOptions::default()
        },
    )?
    .bytes;
    write_output(out, vms)
}

fn text_bytes(mut text: String) -> Vec<u8> {
    if !text.ends_with('\n') {
        text.push('\n');
    }
    text.into_bytes()
}

fn write_output(path: Option<&Path>, bytes: Vec<u8>) -> Result<(), WasmError> {
    if let Some(path) = path {
        fs::write(path, bytes).map_err(anyhow::Error::from)?;
    } else {
        std::io::stdout()
            .lock()
            .write_all(&bytes)
            .map_err(anyhow::Error::from)?;
    }
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

impl From<hir_lower::LowerError> for WasmError {
    fn from(value: hir_lower::LowerError) -> Self {
        WasmError::Lower(value)
    }
}

impl From<lir_lower::LowerError> for WasmError {
    fn from(value: lir_lower::LowerError) -> Self {
        WasmError::LirLower(value)
    }
}

impl From<lir_allocate::AllocationError> for WasmError {
    fn from(value: lir_allocate::AllocationError) -> Self {
        WasmError::LirAllocate(value)
    }
}

impl From<asm_lower::LowerError> for WasmError {
    fn from(value: asm_lower::LowerError) -> Self {
        WasmError::AsmLower(value)
    }
}

impl From<crate::asm::AssemblyError> for WasmError {
    fn from(value: crate::asm::AssemblyError) -> Self {
        WasmError::Assembly(Box::new(value))
    }
}

impl From<config::ConfigError> for WasmError {
    fn from(value: config::ConfigError) -> Self {
        WasmError::Config(value)
    }
}

impl From<package::PackageError> for WasmError {
    fn from(value: package::PackageError) -> Self {
        WasmError::Package(value)
    }
}
