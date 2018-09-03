
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
mod disasm;
mod env;

use disasm::DisasmError;
use location::{Span};
use unicode_segmentation::UnicodeSegmentation;
use regex::Regex;
use std::fs::File;
use std::io::Write;
use std::fmt::Display;
use std::io::Read;

use std::path::Path;
use asm::AssemblyError;
use parser::ArgType;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use atty::Stream;

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
            (@subcommand disassemble =>
              (about: "Disassembler for the Dreamcast VMU")
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
    } else if let Some(matches) = matches.subcommand_matches("disassemble") {
        let input_file = matches.value_of("INPUT").unwrap();
        let output_file = matches.value_of("OUTPUT").unwrap();
        match disassemble_cmd(input_file, output_file) {
            Ok(_) => {},
            Err(ref err) => {
                println!("ERROR: {:?}", err);
            }
        }
    } else {
        eprintln!("No subcommand specified");
        std::process::exit(1);
    }
}

fn disassemble_cmd(filename: &str, output_file: &str) -> Result<(), DisasmError> {
    let bytes = {
        let mut file = File::open(filename).map_err(|e| DisasmError::NoSuchFile(filename.to_string(), e))?;
        let mut contents: Vec<u8> = vec![];
        file.read_to_end(&mut contents).map_err(|e| DisasmError::NoSuchFile(filename.to_string(), e))?;
        contents
    };
    let entry_points = vec![0, 0x3, 0xb, 0x13, 0x1b, 0x23, 0x2b, 0x33, 0x3b, 0x43, 0x4b, 0x130, 0x1f0];
    let statements = disasm::disassemble(&entry_points, &bytes)?;

    let mut outfile = File::create(output_file).unwrap();
    write!(outfile, "{}", statements);
    writeln!(outfile, "");
    Ok(())
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
            let hex = format!("0x{:X}", value);
            stdout.write("Invalid address: ")
                .bold()
                .writeln(hex)
                .reset()
                .newline();
            highlight_line(&span, "", files, stdout);
        },
        AddrBitsDontMatch {
            span,
            pos,
            value,
            pos_top,
            value_top
        } => {
            stdout.writeln("Target is a 12 bit absolution position, and the top 4 bits of the")
                .spaces(2)
                .write("target (")
                .yellow()
                .write(format!("0x{:04X}", *value as usize))
                .reset()
                .writeln(") don't match the top 4 bits of the current instruction")
                .write("  position (")
                .yellow()
                .write(format!("0x{:04X}", pos))
                .reset()
                .writeln(")")
                .newline()
                .spaces(2)
                .bold()
                .write("Top 4 bits of target:   ")
                .reset()
                .yellow()
                .writeln(format!("{:04b}", value_top))
                .reset()
                .spaces(2)
                .bold()
                .write("Top 4 bits of position: ")
                .reset()
                .yellow()
                .writeln(format!("{:04b}", pos_top))
                .reset()
                .newline();

            highlight_line(&span, "", files, stdout);
        },
        UnexpectedChar(loc) => {
            stdout.writeln("Unexpected Character").newline();
            highlight_line(&loc.to_span(), "", files, stdout);
        },
        UnexpectedToken(tok) => {
            stdout.writeln("Unexpected Token").newline();
            highlight_line(tok.span(), "", files, stdout);
        },
        InvalidInstruction(tok) => {
            stdout.writeln("Invalid Instruction").newline();
            highlight_line(tok.span(), "", files, stdout);
        },
        ExpectedTokenNotFound(name, tok) => {
            stdout.write("Expected to find ")
                .yellow()
                .writeln(name)
                .reset()
                .newline();
            highlight_line(tok.span(), "", files, stdout);
        },
        InvalidExpression(loc) => {
            stdout.writeln("Invalid Expression")
                .newline();
            highlight_line(&loc.to_span(), "", files, stdout);
        },
        MissingBytes(span) => {
            stdout.writeln("The ")
                .cyan()
                .write(".byte")
                .reset()
                .writeln(" directive requires at least one byte specified.")
                .newline();
            highlight_line(&span, "", files, stdout);
        },
        MissingWords(span) => {
            stdout.writeln("The ")
                .cyan()
                .write(".word")
                .reset()
                .writeln(" directive requires at least one word specified.")
                .newline();
            highlight_line(&span, "", files, stdout);
        },
        UnknownDirective(tok) => {
            stdout.writeln("Unknown Directive")
                .newline();
            highlight_line(tok.span(), "", files, stdout);
        },
        UnknownInstruction(tok) => {
            stdout.writeln("Unknown Instruction")
                .newline();
            highlight_line(tok.span(), "", files, stdout);
        },
        WrongInstructionArgs(span,name,arg_types) => {
            stdout.write("Wrong arguments for instruction ")
                .cyan()
                .write(name)
                .reset()
                .writeln(".")
                .newline();

            highlight_line(&span, "", files, stdout);

            stdout.newline();

            if arg_types.is_empty() || (arg_types.len() == 1 && arg_types[0].is_empty()) {
                stdout.cyan()
                    .write(name)
                    .reset()
                    .writeln(" expects 0 arguments.")
                    .newline();
            } else {
                stdout.write("Allowed form");
                if arg_types.len() > 1 {
                    stdout.write("s");
                }
                stdout.write(" for instruction ")
                    .cyan()
                    .write(name)
                    .reset()
                    .write(":")
                    .newline();

                for arglist in arg_types {
                    stdout.spaces(2)
                        .cyan()
                        .write(name)
                        .reset()
                        .space();
                    let mut first = true;
                    for arg in arglist {
                        if first {
                            first = false;
                        } else {
                            stdout.write(", ");
                        }
                        match arg {
                            ArgType::Imm => stdout.yellow()
                                .write(arg.to_str())
                                .reset(),
                            ArgType::IM => stdout.magenta()
                                .write(arg.to_str())
                                .reset(),
                            ArgType::B3 |
                            ArgType::A12 |
                            ArgType::A16 |
                            ArgType::R8 |
                            ArgType::R16 |
                            ArgType::D9 => stdout.write(arg.to_str())
                        };
                    }
                    stdout.newline();
                }
            }
            stdout.newline();
        },
        UnexpectedEof => {
            stdout.writeln("Unexpected EOF")
                .newline();
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
        FileUtf8Error(file, err) => {
            stdout.write("UTF-8 Error loading file ")
                .cyan()
                .write(file)
                .reset()
                .write(": ")
                .writeln(err)
                .newline();
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

    pub fn magenta(&mut self) -> &mut ColorWriter {
        self.stream.set_color(ColorSpec::new().set_fg(Some(Color::Magenta))).ok();
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
