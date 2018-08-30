
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
use std::string::ToString;
use std::fmt::Display;

use asm::AssemblyError;

use std::io::Read;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use atty::Stream;

use std::path::Path;
use ast::{Statements,Statement,Directive};
use expression::{EvaluationError};
use std::collections::HashMap;
use location::{Span, Positioned, Location};
use env::{Env,Names};
use instruction::{EncodingError};
use lexer::{Token,LexerError};
use parser::{ParseError,ArgType,Parser};
use files::{FileLoadError,SourceFiles};
use input::Input;

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
        match assemble_cmd(&mut stdout, matches) {
            Ok(_num_bytes) => {},
            Err(ref err) => {
                println!("Failed to assemble {}:", input_file);
                println!("{:?}", err);
            }
        }
    } else {
        eprintln!("No subcommand specified");
        std::process::exit(1);
    }
}

fn assemble_cmd(stdout: &mut ColorWriter, matches: &clap::ArgMatches) -> Result<usize, AssemblyError> {
    let input_file = matches.value_of("INPUT").unwrap();
    let bytes = asm::assemble_file(input_file)?;
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

// fn print_error(err: &AssemblyError) {
//     match err {

//     }
// }

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
