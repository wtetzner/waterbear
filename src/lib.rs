
#[macro_use]
extern crate clap;
extern crate termcolor;
extern crate atty;

extern crate unicode_segmentation;
extern crate unicode_categories;
extern crate regex;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate instruction_derive;

pub mod instruction;
pub mod parser;
pub mod ast;
pub mod expression;
pub mod input;
pub mod lexer;
pub mod location;
pub mod files;
mod asm;
mod env;

use std::fs::File;
use std::io::Write;
use std::fmt::Display;

use asm::AssemblyError;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use atty::Stream;

use std::path::Path;
use expression::{EvaluationError};
use files::{SourceFiles};

pub use asm::{assemble_file,assemble};

const DESCRIPTION: &'static str = env!("CARGO_PKG_DESCRIPTION");
const NAME: &'static str = env!("CARGO_PKG_NAME");
const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub fn run_command(args: &[String]) {
    let matches = clap_app!(
        waterbear =>
            (name: NAME)
            (version: VERSION)
            (about: DESCRIPTION)
            (@subcommand assemble =>
              (about: "Assembler for the Dreamcast VMU")
              (@arg INPUT: +required "Sets the input file to assemble")
              (@arg OUTPUT: -o --output +required +takes_value "Output file")
             )
    ).get_matches_from(args);

    let mut stdout = ColorWriter::new(StandardStream::stdout(if atty::is(Stream::Stdout) { ColorChoice::Auto } else { ColorChoice::Never }));

    if let Some(matches) = matches.subcommand_matches("assemble") {
        let input_file = matches.value_of("INPUT").unwrap();
        let path = Path::new(input_file);
        let dir = path.parent().unwrap_or(Path::new("."));
        let mut files = files::SourceFiles::new(dir.to_str().unwrap().to_owned());
        match assemble_cmd(&mut files, &mut stdout, matches) {
            Ok(_num_bytes) => {},
            Err(ref err) => {
                stdout.write("Failed to assemble ")
                    .bold()
                    .write(input_file)
                    .reset()
                    .writeln(":");
                print_error(&mut files, err, &mut stdout);
            }
        }
    } else {
        eprintln!("No subcommand specified");
        std::process::exit(1);
    }
}

fn assemble_cmd(mut files: &mut SourceFiles, stdout: &mut ColorWriter, matches: &clap::ArgMatches) -> Result<usize, AssemblyError> {
    let input_file = matches.value_of("INPUT").unwrap();
    let bytes = asm::assemble_file(&mut files, input_file)?;
    let output_file = matches.value_of("OUTPUT").unwrap();
    let mut outfile = File::create(output_file).unwrap();
    outfile.write_all(&bytes).unwrap();

    stdout.write_ok()
        .write(" Assembled ")
        .bold()
        .write(bytes.len())
        .reset()
        .write(" bytes to ")
        .bold()
        .write(output_file)
        .reset()
        .writeln(".");

    Ok(bytes.len())
}

fn print_error(files: &SourceFiles, err: &AssemblyError, stdout: &mut ColorWriter) {
    use asm::AssemblyError::*;
    match err {
        NameNotFound(span,msg) => {
            stdout.write_error().space().writeln(msg);
        },
        DivideByZero(span,msg) => {
            stdout.write_error().space().writeln(msg);
        },
        MustBeLiteralNumber(span) => {
            stdout.write_error()
                .writeln(" Must be a literal number");
        },
        NameAlreadyExists(current,existing,name) => {
            stdout.write_error().space()
                .write("Name already exists: ")
                .bold()
                .writeln(name)
                .reset();
        },
        InvalidCodeLocation(span,location) => {
            stdout.write_error().space()
                .write("Invalid code location: ")
                .bold()
                .writeln(format!("0x{:X}", location))
                .reset();
        },
        NumOutOfRange {
            span,
            bits,
            value
        } => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        SignedNumOutOfRange {
            span,
            bits,
            value
        } => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        InvalidAddress {
            span,
            value
        } => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        AddrBitsDontMatch {
            span,
            pos,
            value,
            pos_top,
            value_top
        } => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        UnexpectedChar(loc) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        UnexpectedToken(tok) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        InvalidInstruction(tok) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        ExpectedTokenNotFound(name, tok) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        InvalidExpression(loc) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        MissingBytes(span) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        MissingWords(span) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        UnknownDirective(loc) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        UnknownInstruction(loc) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        WrongInstructionArgs(span,name,arg_types) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        UnexpectedEof => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        FileLoadFailure(err) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        },
        FileUtf8Error(err) => {
            let msg = format!("{:?}", err);
            stdout.write_error().space().writeln(msg);
        }
    }
}

struct ColorWriter {
    stream: StandardStream
}

impl ColorWriter {
    pub fn new(stream: StandardStream) -> ColorWriter {
        ColorWriter { stream }
    }

    pub fn newline(&mut self) -> &mut ColorWriter {
        self.writeln("")
    }

    pub fn space(&mut self) -> &mut ColorWriter {
        self.write(" ")
    }

    pub fn write_error(&mut self) -> &mut ColorWriter {
        self.write("[")
            .red()
            .write("ERROR")
            .reset()
            .write("]")
    }

    pub fn write_ok(&mut self) -> &mut ColorWriter {
        self.write("[")
            .green()
            .write("OK")
            .reset()
            .write("]")
    }

    pub fn write<T: Display>(&mut self, value: T) -> &mut ColorWriter {
        write!(self.stream, "{}", value).ok();
        self
    }

    pub fn writeln<T: Display>(&mut self, value: T) -> &mut ColorWriter {
        writeln!(self.stream, "{}", value).ok();
        self
    }

    pub fn reset(&mut self) -> &mut ColorWriter {
        self.stream.reset().ok();
        self
    }

    pub fn intense(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_intense(true)).ok();
        self
    }

    pub fn bold(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_bold(true)).ok();
        self
    }

    pub fn green(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_fg(Some(Color::Green))).ok();
        self
    }

    pub fn red(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_fg(Some(Color::Red))).ok();
        self
    }
}
