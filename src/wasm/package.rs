//! Final VMU image layout around lowered program assembly.

use std::{error::Error, fmt, path::Path};

use crate::{
    ast::{ByteValue, Directive, IncludeType, Statement, Statements},
    expression::Expr,
    img,
    instruction::Instr,
    location::Span,
};

use super::{asm_lower, config::PackageConfig, lir::AllocatedProgram, target::VMS_HEADER_ADDRESS};

#[derive(Debug)]
pub enum PackageError {
    MissingFunctionLabel(String),
    Icon(img::IconError),
}

impl fmt::Display for PackageError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingFunctionLabel(function) => {
                write!(
                    formatter,
                    "missing assembly label for function {function:?}"
                )
            }
            Self::Icon(img::IconError::InvalidIconSize(path, width, height)) => write!(
                formatter,
                "image {path:?} has size {width} x {height}, expected 32 x 32 for an icon or 72 x 56 for an eyecatch"
            ),
            Self::Icon(img::IconError::InvalidPaletteSize(path, size, maximum)) => write!(
                formatter,
                "image {path:?} uses {size} colors, but this format permits at most {maximum}"
            ),
            Self::Icon(img::IconError::FileLoadFailure(path, source)) => {
                write!(formatter, "could not read image {path:?}: {source}")
            }
            Self::Icon(img::IconError::ImageParseError(path, source)) => {
                write!(formatter, "could not parse image {path:?}: {source}")
            }
        }
    }
}

impl Error for PackageError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::MissingFunctionLabel(_) => None,
            Self::Icon(img::IconError::FileLoadFailure(_, source)) => Some(source),
            Self::Icon(img::IconError::ImageParseError(_, source)) => Some(source),
            Self::Icon(
                img::IconError::InvalidIconSize(..) | img::IconError::InvalidPaletteSize(..),
            ) => None,
        }
    }
}

impl From<img::IconError> for PackageError {
    fn from(value: img::IconError) -> Self {
        Self::Icon(value)
    }
}

/// Places lowered code after the VMU executable and VMS file headers.
pub fn package(
    config: &PackageConfig,
    program: &AllocatedProgram,
    code: &Statements,
) -> Result<Statements, PackageError> {
    let labels = asm_lower::function_labels(program);
    let startup_label = unique_internal_label(&labels, "__waterbear_start");
    let finished_label = unique_internal_label(&labels, "__waterbear_finished");
    let mut output = Statements::empty();
    emit_vectors(&mut output, program, &labels, &startup_label)?;
    emit_vms_header(&mut output, config)?;
    emit_startup(
        &mut output,
        &startup_label,
        &finished_label,
        label_for(&labels, &program.main.name)?,
    );
    output.append(code.as_slice());
    Ok(output)
}

fn emit_vectors(
    output: &mut Statements,
    program: &AllocatedProgram,
    labels: &std::collections::BTreeMap<String, String>,
    startup_label: &str,
) -> Result<(), PackageError> {
    output.push_comment("reset and interrupt vectors");
    output.push_org(0);
    output.push_instr(Instr::Jmpf(Expr::name(startup_label)));

    let handlers = &program.interrupt_handlers;
    for (address, name, handler) in [
        (0x0003, "external interrupt 0", handlers.external_0.as_ref()),
        (0x000b, "external interrupt 1", handlers.external_1.as_ref()),
        (
            0x0013,
            "external interrupt 2 / timer 0 low",
            handlers.external_2_or_timer_0_low.as_ref(),
        ),
        (
            0x001b,
            "external interrupt 3 / base timer",
            handlers.external_3_or_base_timer.as_ref(),
        ),
        (0x0023, "timer 0 high", handlers.timer_0_high.as_ref()),
        (0x002b, "timer 1", handlers.timer_1.as_ref()),
        (0x0033, "serial 0", handlers.serial_0.as_ref()),
        (0x003b, "serial 1", handlers.serial_1.as_ref()),
        (
            0x0043,
            "Dreamcast interface",
            handlers.dreamcast_interface.as_ref(),
        ),
        (0x004b, "port 3", handlers.port_3.as_ref()),
    ] {
        output.push_org(address);
        output.push_comment(name);
        if let Some(handler) = handler {
            output.push_instr(Instr::Jmpf(Expr::name(label_for(labels, &handler.name)?)));
        } else {
            output.push_instr(Instr::Reti);
        }
    }
    output.push_comment("");
    Ok(())
}

fn emit_startup(output: &mut Statements, startup: &str, finished: &str, main: &str) {
    output.push_comment("program startup");
    output.push_label(startup);
    output.push_instr(Instr::Callf(Expr::name(main)));
    output.push_label(finished);
    output.push_instr(Instr::Br(Expr::name(finished)));
    output.push_comment("");
}

fn unique_internal_label(
    labels: &std::collections::BTreeMap<String, String>,
    base: &str,
) -> String {
    if !labels.values().any(|label| label == base) {
        return base.to_owned();
    }

    for suffix in 1.. {
        let candidate = format!("{base}_{suffix}");
        if !labels.values().any(|label| label == &candidate) {
            return candidate;
        }
    }

    unreachable!()
}

fn label_for<'a>(
    labels: &'a std::collections::BTreeMap<String, String>,
    function: &str,
) -> Result<&'a str, PackageError> {
    labels
        .get(function)
        .map(String::as_str)
        .ok_or_else(|| PackageError::MissingFunctionLabel(function.to_owned()))
}

fn emit_vms_header(output: &mut Statements, config: &PackageConfig) -> Result<(), PackageError> {
    output.push_comment("VMS file header");
    output.push_org(VMS_HEADER_ADDRESS);
    output.push(Statement::Directive(Directive::Text(
        Span::default(),
        16,
        config.vmu_description.as_bytes().to_vec(),
    )));
    output.push(Statement::Directive(Directive::Text(
        Span::default(),
        32,
        config.dreamcast_description.as_bytes().to_vec(),
    )));
    output.push(Statement::Directive(Directive::String(
        Span::default(),
        16,
        config.application_id.as_bytes().to_vec(),
    )));

    match config.icon.path.as_deref() {
        Some(path) => emit_configured_icon(output, config, path)?,
        None => emit_default_icon(output, config.icon.speed),
    }
    output.push_comment("");
    Ok(())
}

fn emit_configured_icon(
    output: &mut Statements,
    config: &PackageConfig,
    path: &Path,
) -> Result<(), PackageError> {
    let eyecatch = config.icon.eyecatch.as_deref();
    img::to_icon(path, config.icon.speed, eyecatch)?;
    let eyecatch = eyecatch
        .map(|path| ByteValue::String(Span::default(), path.to_string_lossy().as_bytes().to_vec()));
    output.push(Statement::Directive(Directive::Include(
        Span::default(),
        IncludeType::Icon(Some(Expr::num(config.icon.speed as i32)), eyecatch),
        path.to_owned(),
    )));
    Ok(())
}

fn emit_default_icon(output: &mut Statements, speed: u16) {
    output.push_comment("built-in blank icon");
    output.push(Statement::word(vec![Expr::num(1), Expr::num(speed as i32)]));
    output.push_comment("no eyecatch, CRC, or payload size");
    output.push(Statement::word(vec![
        Expr::num(0),
        Expr::num(0),
        Expr::num(0),
        Expr::num(0),
    ]));
    output.push_comment("reserved header bytes");
    output.push(Statement::byte(vec![ByteValue::Expr(Expr::num(0)); 20]));
    output.push_comment("transparent icon palette");
    output.push(Statement::word(vec![Expr::num(0); 16]));
    output.push_comment("32 x 32 four-bit icon bitmap");
    for _ in 0..32 {
        output.push(Statement::byte(vec![ByteValue::Expr(Expr::num(0)); 16]));
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::package;
    use crate::wasm::{
        asm_lower,
        config::{IconConfig, PackageConfig},
        lir::{
            Block, BlockId, Function, FunctionAbi, FunctionKind, InterruptHandlers,
            PhysicalPlacement, Program, Terminator, TerminatorKind,
        },
    };
    use crate::{AssembleOptions, assemble, assemble_statements_with_options, files::SourceFiles};

    fn returning_function(name: &str, kind: FunctionKind) -> Function {
        Function {
            name: name.into(),
            kind,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: Some("entry".into()),
                instructions: Vec::new(),
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        }
    }

    #[test]
    fn emits_a_complete_default_package_before_code() {
        let program = Program::<PhysicalPlacement> {
            main: returning_function("main", FunctionKind::Normal),
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: Vec::new(),
        };
        let code = asm_lower::lower(&program).unwrap();
        let output = package(&PackageConfig::default(), &program, &code)
            .unwrap()
            .to_string();

        assert!(output.contains(".org 0\n  jmpf  __waterbear_start"));
        assert!(output.contains("__waterbear_start:\n  callf main"));
        assert!(output.contains("__waterbear_finished:\n  br    __waterbear_finished"));
        assert!(output.contains(".org $0200"));
        assert!(output.contains(".text 16 \"Waterbear WASM\""));
        assert!(output.contains("built-in blank icon"));
        assert!(output.find("VMS file header").unwrap() < output.find("main:").unwrap());
    }

    #[test]
    fn default_package_assembles_to_a_complete_image() {
        let program = Program::<PhysicalPlacement> {
            main: returning_function("main", FunctionKind::Normal),
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: Vec::new(),
        };
        let code = asm_lower::lower(&program).unwrap();
        let output = package(&PackageConfig::default(), &program, &code).unwrap();

        let bytes = assemble(&output, false).unwrap().bytes;

        assert!(bytes.len() > 0x0480);
        assert_eq!(&bytes[0x0200..0x0210], b"Waterbear WASM  ");
        assert_eq!(&bytes[0x0240..0x0244], &[1, 0, 10, 0]);
        assert!(bytes[0x0244..0x0480].iter().all(|byte| *byte == 0));
    }

    #[test]
    fn configured_images_are_validated_and_printed_as_an_icon_include() {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let icon = root.join("test-projects/basic/images/icon.gif");
        let eyecatch = root.join("test-projects/basic/images/eyecatch.png");
        let config = PackageConfig {
            icon: IconConfig {
                path: Some(icon.clone()),
                speed: 12,
                eyecatch: Some(eyecatch.clone()),
            },
            ..PackageConfig::default()
        };
        let program = Program::<PhysicalPlacement> {
            main: returning_function("main", FunctionKind::Normal),
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: Vec::new(),
        };
        let code = asm_lower::lower(&program).unwrap();

        let output = package(&config, &program, &code).unwrap();
        let rendered = output.to_string();

        assert!(rendered.contains(&format!(
            ".include icon \"{}\" speed = $0C, eyecatch = \"{}\"",
            icon.display(),
            eyecatch.display()
        )));

        let mut files = SourceFiles::new();
        let assembled = assemble_statements_with_options(
            &mut files,
            &output,
            AssembleOptions {
                optimize_jumps: true,
                ..AssembleOptions::default()
            },
        )
        .unwrap();
        assert!(assembled.bytes.len() > 0x0480);
    }

    #[test]
    fn installs_interrupt_handlers_in_their_vector_slots() {
        let handler = returning_function("on_timer", FunctionKind::Interrupt);
        let program = Program::<PhysicalPlacement> {
            main: returning_function("main", FunctionKind::Normal),
            interrupt_handlers: InterruptHandlers {
                timer_0_high: Some(handler),
                ..InterruptHandlers::default()
            },
            functions: Vec::new(),
            statics: Vec::new(),
            storages: Vec::new(),
        };
        let code = asm_lower::lower(&program).unwrap();
        let output = package(&PackageConfig::default(), &program, &code)
            .unwrap()
            .to_string();

        assert!(output.contains(".org $0023\n  ;; timer 0 high\n  jmpf  on_timer"));
        assert!(output.contains("on_timer:"));
        assert!(output.contains("  reti"));
    }
}
