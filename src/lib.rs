
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

use location::{Span};
use unicode_segmentation::UnicodeSegmentation;
use regex::Regex;
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
        let dir = path.parent().unwrap_or(Path::new("")).to_str().unwrap();

        let mut files = files::SourceFiles::new(dir.to_owned());
        match assemble_cmd(&mut files, &mut stdout, matches) {
            Ok(_num_bytes) => {},
            Err(ref err) => {
                stdout.write_error().space()
                    .write("Failed to assemble ")
                    .cyan()
                    .write(input_file)
                    .reset()
                    .newline()
                    .newline()
                    .red()
                    .write("✘")
                    .reset()
                    .space();
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
            stdout.writeln(msg);
            stdout.newline();
            highlight_line(&span, "Unknown name", files, stdout);
        },
        DivideByZero(span,msg) => {
            stdout.writeln(msg);
            stdout.newline();
            highlight_line(&span, "", files, stdout);
        },
        MustBeLiteralNumber(span) => {
            stdout.writeln(" Must be a literal number")
                .newline();
            highlight_line(&span, "", files, stdout);
        },
        NameAlreadyExists(current,existing,name) => {
            stdout
                .write("Name already exists: ")
                .bold()
                .writeln(name)
                .newline()
                .reset();

            stdout.bold().writeln("The name found here...").reset();
            highlight_line(&current, "Duplicate", files, stdout);
            stdout.newline().bold().writeln("...was already declared here").reset();
            highlight_line(&existing, "Original", files, stdout);
        },
        InvalidCodeLocation(span,location) => {
            stdout
                .write("Invalid code location: ")
                .bold()
                .writeln(format!("0x{:X}", location))
                .reset()
                .newline();
            highlight_line(&span, "", files, stdout);
        },
        NumOutOfRange {
            span,
            bits,
            value
        } => {
            stdout
                .write("Value out of range: ")
                .bold()
                .write(value)
                .reset()
                .write(". Expected unsigned ")
                .bold()
                .write(bits)
                .reset()
                .writeln(" bit number.")
                .newline();
            highlight_line(&span, "", files, stdout);
        },
        SignedNumOutOfRange {
            span,
            bits,
            value
        } => {
            stdout
                .write("Value out of range: ")
                .bold()
                .write(value)
                .reset()
                .write(". Expected signed ")
                .bold()
                .write(bits)
                .reset()
                .writeln(" bit number.")
                .newline();
            highlight_line(&span, "", files, stdout);
        },
        InvalidAddress {
            span,
            value
        } => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        AddrBitsDontMatch {
            span,
            pos,
            value,
            pos_top,
            value_top
        } => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        UnexpectedChar(loc) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        UnexpectedToken(tok) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        InvalidInstruction(tok) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        ExpectedTokenNotFound(name, tok) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        InvalidExpression(loc) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        MissingBytes(span) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        MissingWords(span) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        UnknownDirective(loc) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        UnknownInstruction(loc) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        WrongInstructionArgs(span,name,arg_types) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        UnexpectedEof => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        },
        FileLoadFailure(file, err) => {
            stdout.write("Error loading file ")
                .cyan()
                .write(file)
                .reset()
                .write(": ")
                .writeln(err)
                .newline();
        },
        FileUtf8Error(err) => {
            let msg = format!("{:?}", err);
            stdout.writeln(msg);
        }
    }
}

fn highlight_line(span: &Span, msg: &str, files: &SourceFiles, stdout: &mut ColorWriter) {
    let file = files.for_id(span.start().file()).unwrap();
    let line = line_for_pos(span.start().pos(), file.contents());

    stdout.yellow().write(file.name()).reset()
        .write(":")
        .bold().write(span.start().line()).reset()
        .write(":")
        .bold().write(span.start().column());

    if span.start().column() != span.end().column() {
        stdout.write("-").write(span.end().column());
    }

    stdout.reset().newline();

    stdout.spaces(2).writeln(line);
    stdout.spaces(2).yellow().writeln(caret(&span, line, msg)).reset();
}

fn caret(span: &Span, line: &str, msg: &str) -> String {
    lazy_static! {
        static ref WS: Regex = Regex::new("^\\s$").unwrap();
    }
    let mut result = String::new();
    let graphemes = UnicodeSegmentation::graphemes(line, true);
    let start = span.start().column();
    let end = span.end().column();
    let mut col = 0;
    for grapheme in graphemes {
        if col < start {
            if WS.is_match(grapheme) {
                result.push_str(grapheme);
            } else {
                result.push_str(" ");
            }
        } else if col == start {
            result.push_str("^");
        } else if col > start && col < end {
            result.push_str("^");
        } else {
            break;
        }
        col += 1
    }
    if !msg.is_empty() {
        result.push_str(" ");
        result.push_str(msg);
    }
    result
}

fn line_for_pos(pos: usize, text: &str) -> &str {
    &text[start_of_line(pos, text)..end_of_line(pos, text)]
}

fn start_of_line(pos: usize, text: &str) -> usize {
    let bytes = text.as_bytes();
    let mut loc = pos;
    while loc > 0 && bytes[loc - 1] != b'\n' {
        loc -= 1;
    }
    loc
}

fn end_of_line(pos: usize, text: &str) -> usize {
    let bytes = text.as_bytes();
    let mut loc = pos;
    while loc < bytes.len()
        && !((loc < bytes.len() - 1 && bytes[loc] == b'\n') ||
             (loc < bytes.len() - 2 && bytes[loc] == b'\r' && bytes[loc + 1] == b'\n')) {
        loc += 1;
    }
    loc
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

    pub fn spaces(&mut self, count: usize) -> &mut ColorWriter {
        for _idx in 0..count {
            self.write(" ");
        }
        self
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

    pub fn yellow(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).ok();
        self
    }

    pub fn cyan(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_fg(Some(Color::Cyan))).ok();
        self
    }

    pub fn red(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_fg(Some(Color::Red))).ok();
        self
    }
}
