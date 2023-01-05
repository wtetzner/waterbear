
extern crate clap;
extern crate termcolor;
extern crate atty;

extern crate unicode_segmentation;
extern crate regex;
extern crate lazy_static;
extern crate waterbear_instruction_derive;
extern crate uuid;
extern crate image;
extern crate serde;
extern crate enum_iterator;

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
mod cheader;
pub mod img;
mod metadata;

use crate::disasm::DisasmError;
use crate::location::{Span};
use unicode_segmentation::UnicodeSegmentation;
use regex::Regex;
use std::fs::File;
use std::io::Write;
use std::fmt::Display;
use std::io::Read;
use crate::ast::ArgType;

use std::path::Path;
use crate::asm::AssemblyError;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use atty::Stream;

use crate::expression::{EvaluationError};
use crate::files::{SourceFiles};
use clap::clap_app;
use lazy_static::lazy_static;

pub use crate::asm::{assemble_file,assemble};

const DESCRIPTION: &'static str = env!("CARGO_PKG_DESCRIPTION");
const NAME: &'static str = env!("CARGO_PKG_NAME");
const VERSION: &'static str = env!("CARGO_PKG_VERSION");
const RUSTC: &'static str = env!("RUSTC_VERSION");
const CARGO: &'static str = env!("CARGO_VERSION");
const GITSHA: &'static str = env!("GITSHA");

pub fn run_command(args: &[String]) {
    let matches = clap_app!(
        waterbear =>
            (name: NAME)
            (version: VERSION)
            (about: DESCRIPTION)
            (@subcommand assemble =>
              (version: VERSION)
              (about: "Assembler for the Dreamcast VMU")
              (@arg INPUT: +required "Sets the input file to assemble")
              (@arg OUTPUT: -o --output +required +takes_value "Output file")
             )
            (@subcommand expand =>
              (version: VERSION)
              (about: "Expand macros and output to stdout")
              (@arg INPUT: +required "Sets the input file to assemble")
             )
            (@subcommand disassemble =>
              (version: VERSION)
              (about: "Disassembler for the Dreamcast VMU")
              (@arg INPUT: +required "Sets the input file to assemble")
              (@arg OUTPUT: -o --output +required +takes_value "Output file")
              (@arg POSITIONS: -p --positions "Output byte positions")
              (@arg TYPE: -t --type +takes_value "The file type being disassembled: game, bios, file or raw. Defaults to raw.")
              (@arg ARRIVED_FROM: -a --arrived_from "Output instruction locations that target each instruction.")
             )
            (@subcommand icon =>
              (version: VERSION)
              (about: "Convert an image file into the assembly needed for an icon.")
              (@arg INPUT: +required "Input image file. Must be 32x32, and no more than 16 colors.")
              (@arg SPEED: -s --speed +takes_value "Animation speed. Default is 10.")
              (@arg EYECATCH: -e --eyecatch +takes_value "Eyecatch image. Must be 72×56.")
             )
            (@subcommand image =>
             (version: VERSION)
             (about: "Convert an image file into another format.")
             (@arg INPUT: +required "Input image file.")
             (@arg OUTPUT: -o --output +takes_value "Output file")
             (@arg format: -f --format +takes_value "Output format"))
            (@subcommand vmi =>
              (version: VERSION)
              (about: "Extract a VMI file from a given VMS file.")
              (@arg INPUT: +required "Input VMS file.")
              (@arg OUTPUT: -o --output +takes_value "Output file")
              (@arg copyright: -c --copyright +takes_value "Copyright notice")
              (@arg game: -g --game "Specify whether or not the VMS file is a game.")
             )
            (@subcommand version =>
              (version: VERSION)
              (about: "Version info about this build")
             )
    ).get_matches_from(args);

    let mut stdout = ColorWriter::new(StandardStream::stdout(if atty::is(Stream::Stdout) { ColorChoice::Auto } else { ColorChoice::Never }));
    let mut stderr = ColorWriter::new(StandardStream::stderr(if atty::is(Stream::Stderr) { ColorChoice::Auto } else { ColorChoice::Never }));

    if let Some(matches) = matches.subcommand_matches("assemble") {
        let input_file = matches.value_of("INPUT").unwrap();
        let path = Path::new(input_file);
        let dir = path.parent().unwrap_or(Path::new("")).to_str().unwrap();

        let mut files = files::SourceFiles::new(dir.to_owned());
        match assemble_cmd(&mut files, &mut stderr, matches) {
            Ok(_num_bytes) => {},
            Err(ref err) => {
                stderr.write_error().space()
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
                print_error(&mut files, err, &mut stderr);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("disassemble") {
        let input_file = matches.value_of("INPUT").unwrap();
        let output_file = matches.value_of("OUTPUT").unwrap();
        let positions = matches.occurrences_of("POSITIONS") > 0;
        let file_type = matches.value_of("TYPE").unwrap_or("raw");
        let arrived_from = matches.occurrences_of("ARRIVED_FROM") > 0;
        match disassemble_cmd(positions, arrived_from, file_type, input_file, output_file) {
            Ok(_) => {},
            Err(ref err) => {
                eprintln!("ERROR: {:?}", err);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("icon") {
        use img::IconError;
        let input_file = matches.value_of("INPUT").unwrap();
        let animation_speed = matches.value_of("SPEED").map(|s| s.parse::<u16>().unwrap());
        let eyecatch_file = matches.value_of("EYECATCH");

        match img::to_icon(input_file, animation_speed, eyecatch_file) {
            Ok(stmts) => {
                stdout.write(ast::Statement::comment("NOTE: This file was generated by the waterbear assembler:")).write("\n");
                stdout.write(ast::Statement::comment(&format!("{}", args.join(" ")))).write("\n");
                stdout.write(stmts).write("\n");
            },
            Err(IconError::InvalidPaletteSize(p, psize, expected)) => {
                stderr.write_error().space()
                    .write("Too many colors in ")
                    .cyan()
                    .write(p)
                    .reset()
                    .write(": ")
                    .write(psize)
                    .write(format!(". Must be {} or less.\n", expected));
                std::process::exit(1);
            },
            Err(IconError::InvalidIconSize(p, w, h)) => {
                stderr.write_error().space()
                    .write("Image ")
                    .cyan()
                    .write(p)
                    .reset()
                    .write(format!(" is the wrong size. It should be {}x{}.\n", w, h));
                std::process::exit(1);
            },
            Err(IconError::FileLoadFailure(filename, err)) => {
                stderr.write("Error loading file ")
                    .cyan()
                    .write(filename)
                    .reset()
                    .write(": ")
                    .writeln(err)
                    .newline();
                std::process::exit(1);
            },
            Err(IconError::ImageParseError(filename, err)) => {
                stderr.write("Error parsing image ")
                    .cyan()
                    .write(filename)
                    .reset()
                    .write(": ")
                    .writeln(err)
                    .newline();
                std::process::exit(1);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("image") {
        let input_file = matches.value_of("INPUT").unwrap();
        let format = img::ImageFormat::from(matches.value_of("format").unwrap_or(""))
            .unwrap_or(img::ImageFormat::Asm1Bit);
        let image = img::load_image(input_file).unwrap();

        use img::ImageFormat;
        let serialized = match format {
            ImageFormat::Json => serde_json::to_string(&image).unwrap(),
            ImageFormat::JsonPretty => serde_json::to_string_pretty(&image).unwrap(),
            ImageFormat::Asm1Bit => format!("{}", image.to_1bit_asm(false)),
            ImageFormat::Asm1BitMasked => format!("{}", image.to_1bit_asm(true)),
        };

        match matches.value_of("OUTPUT") {
            Some(output) => {
                let mut outfile = File::create(output).unwrap();
                outfile.write(serialized.as_bytes()).unwrap();
            },
            None => {
                println!("{}", serialized);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("vmi") {
        let input_file = matches.value_of("INPUT").unwrap();
        let copyright = matches.value_of("copyright").unwrap_or("");
        let filename_regex = Regex::new("\\.vms$").unwrap();
        let gen_output_file: &str = &(filename_regex.replace(input_file, "") + ".vmi");
        let output_file = matches.value_of("OUTPUT").unwrap_or(gen_output_file);
        let game = matches.is_present("game");

        let offset: usize = if game { 0x200 } else { 0 };

        match generate_vmi(input_file, output_file, copyright, offset) {
            Ok(_num_bytes) => {},
            Err(ref _err) => {
                stderr.write_error().space()
                    .write("Failed to generate VMI from ")
                    .cyan()
                    .write(input_file)
                    .reset()
                    .newline()
                    .newline()
                    .red()
                    .write("✘")
                    .reset()
                    .space();
                //print_error(&mut files, err, &mut stderr);
            }
        }

    } else if let Some(matches) = matches.subcommand_matches("expand") {
        let input_file = matches.value_of("INPUT").unwrap();
        let path = Path::new(input_file);
        let dir = path.parent().unwrap_or(Path::new("")).to_str().unwrap();

        let mut files = files::SourceFiles::new(dir.to_owned());
        match expand_cmd(&mut files, matches) {
            Ok(_) => {},
            Err(ref err) => {
                stderr.write_error().space()
                    .write("Failed to expand ")
                    .cyan()
                    .write(input_file)
                    .reset()
                    .newline()
                    .newline()
                    .red()
                    .write("✘")
                    .reset()
                    .space();
                print_error(&mut files, err, &mut stderr);
            }
        }
    } else if let Some(_) = matches.subcommand_matches("version") {
        stdout.magenta()
            .writeln("               _            _")
            .writeln("              | |          | |")
            .writeln("__      ____ _| |_ ___ _ __| |__   ___  __ _ _ __ ")
            .writeln("\\ \\ /\\ / / _` | __/ _ | '__| '_ \\ / _ \\/ _` | '__|")
            .writeln(" \\ V  V | (_| | ||  __| |  | |_) |  __| (_| | |")
            .writeln("  \\_/\\_/ \\__,_|\\__\\___|_|  |_.__/ \\___|\\__,_|_|")
            .reset()
            .newline()
            .yellow()
            .write(NAME)
            .reset()
            .space()
            .bold()
            .write("v")
            .writeln(VERSION)
            .reset()
            .writeln(DESCRIPTION)
            .newline()
            .bold()
            .writeln("Build Info")
            .reset()
            .cyan()
            .write("  Compiler").reset().write(": ")
            .writeln(RUSTC)
            .cyan()
            .write("  Cargo").reset().write(":    ")
            .writeln(CARGO)
            .cyan()
            .write("  git SHA").reset().write(":  ")
            .writeln(GITSHA)
            .reset()
            .newline();
    } else {
        eprintln!("No subcommand specified");
        std::process::exit(1);
    }
}

fn generate_vmi(input_file: &str, output_file: &str, copyright: &str, offset: usize) -> Result<(), DisasmError> {
    use chrono::prelude::*;

    let vms_bytes = {
        let mut file = File::open(input_file).map_err(|e| DisasmError::NoSuchFile(input_file.to_string(), e))?;
        let mut contents: Vec<u8> = vec![];
        file.read_to_end(&mut contents).map_err(|e| DisasmError::NoSuchFile(input_file.to_string(), e))?;
        contents
    };

    let basename = {
        let parts: Vec<&str> = input_file.split('/').collect();
        let filename = parts[parts.len() - 1];
        let filename_regex = Regex::new("\\.vms$").unwrap();
        let base = filename_regex.replace(filename, "").to_string();
        base
    };
    let basename_bytes = basename.as_bytes();

    let filename = {
        let parts: Vec<&str> = input_file.split('/').collect();
        let filename = parts[parts.len() - 1];
        filename.to_string()
    };
    let filename_bytes = filename.as_bytes();

    let mut bytes = [0u8; 108];

    // Copy description field from VMS file
    for idx in 0..32usize {
        bytes[0x4 + idx] = vms_bytes[offset + 0x10 + idx];
    }

    // Write copyright notice
    let copyright_bytes = copyright.as_bytes();
    for idx in 0..32usize {
        if copyright_bytes.len() > idx {
            bytes[0x24 + idx] = copyright_bytes[idx];
        } else {
            bytes[0x24 + idx] = b'\0';
        }
    }

    // Set file number to 1
    bytes[0x4E] = 1;
    bytes[0x4E + 1] = 0;

    // Write VMS resource name
    for idx in 0..8usize {
        if basename_bytes.len() > idx {
            bytes[0x50 + idx] = basename_bytes[idx];
        } else {
            bytes[0x50 + idx] = b'\0';
        }
    }

    // Write VMS filename
    for idx in 0..12usize {
        if filename_bytes.len() > idx {
            bytes[0x50 + idx] = filename_bytes[idx];
        } else {
            bytes[0x50 + idx] = b'\0';
        }
    }

    // Write game info
    if offset > 0 {
        bytes[0x64] = 0b00000010;
    }

    // Write checksum
    bytes[0] = bytes[0x50] & b'S';
    bytes[1] = bytes[0x50 + 1] & b'E';
    bytes[2] = bytes[0x50 + 2] & b'G';
    bytes[3] = bytes[0x50 + 3] & b'A';

    // Write file size
    {
        let le_bytes = vms_bytes.len().to_le_bytes();
        for idx in 0..4 {
            bytes[0x68 + idx] = le_bytes[idx];
        }
    }

    // *** Write current time ***
    let datetime: DateTime<Local> = Local::now();
    // Write year
    {
        let le_bytes = (datetime.year() as u16).to_le_bytes();
        for idx in 0..2 {
            bytes[0x44 + idx] = le_bytes[idx];
        }
    }
    // Write month
    bytes[0x46] = datetime.month() as u8;
    // Write day
    bytes[0x47] = datetime.day() as u8;
    // Write hour
    bytes[0x48] = datetime.hour() as u8;
    // Write minute
    bytes[0x49] = datetime.minute() as u8;
    // Write second
    bytes[0x4A] = datetime.second() as u8;

    // Write weekday
    bytes[0x4B] = match datetime.weekday() {
        Weekday::Sun => 0,
        Weekday::Mon => 1,
        Weekday::Tue => 2,
        Weekday::Wed => 3,
        Weekday::Thu => 4,
        Weekday::Fri => 5,
        Weekday::Sat => 6
    };

    let mut outfile = File::create(output_file).unwrap();
    outfile.write_all(&bytes).unwrap();

    Ok(())
}

fn disassemble_cmd(positions: bool, arrived_from: bool, file_type: &str, filename: &str, output_file: &str) -> Result<(), DisasmError> {
    let bytes = {
        let mut file = File::open(filename).map_err(|e| DisasmError::NoSuchFile(filename.to_string(), e))?;
        let mut contents: Vec<u8> = vec![];
        file.read_to_end(&mut contents).map_err(|e| DisasmError::NoSuchFile(filename.to_string(), e))?;
        contents
    };
    let entry_points = match file_type {
        "game" | "file" | "raw" =>
            vec![0, 0x3, 0xb, 0x13, 0x1b, 0x23, 0x2b, 0x33, 0x3b, 0x43, 0x4b, 0x130],
        "bios" => vec![
            0, 0x3, 0xb, 0x13, 0x1b, 0x23, 0x2b, 0x33, 0x3b, 0x43, 0x4b, 0x130,
            0x100, // BIOS_ADDR_FM_WRT_EX
            0x108, // BIOS_ADDR_FM_WRTA_EX
            0x110, // BIOS_ADDR_FM_VRF_EX
            0x120, // BIOS_ADDR_FM_PRD_EX
            0x130, // BIOS_ADDR_TIMER_EX: Timer update function used as ISR to populate date/time in system variables
            0x140, // BIOS_ADDR_SLEEP_EX
            0x1f0 // BIOS_ADDR_EXIT_EX: MODE button?
        ],
        _ => return Err(DisasmError::UnknownFileType(file_type.to_string()))
    };
    let statements = disasm::disassemble(arrived_from, &entry_points, &bytes)?;

    let mut outfile = File::create(output_file).unwrap();
    for dstmt in statements.to_vec() {
        if positions {
            if let Some(pos) = dstmt.pos() {
                write!(outfile, "{:04X}| ", pos).unwrap();
            } else {
                write!(outfile, "    | ").unwrap();
            }
        }
        write!(outfile, "{}", dstmt.statement()).unwrap();
        if let Some(comment) = dstmt.cmt() {
            write!(outfile, " ; {}", comment).unwrap();
        }
        writeln!(outfile, "").unwrap();
    }

    Ok(())
}

fn expand_cmd(mut files: &mut SourceFiles, matches: &clap::ArgMatches) -> Result<(),AssemblyError> {
    let input_file = matches.value_of("INPUT").unwrap();
    asm::expand_file(&mut files, input_file)
}

fn assemble_cmd(mut files: &mut SourceFiles, stderr: &mut ColorWriter, matches: &clap::ArgMatches) -> Result<usize, AssemblyError> {
    let input_file = matches.value_of("INPUT").unwrap();
    let bytes = asm::assemble_file(&mut files, input_file)?;
    let output_file = matches.value_of("OUTPUT").unwrap();
    let mut outfile = File::create(output_file).unwrap();
    outfile.write_all(&bytes).unwrap();

    stderr.write_ok()
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

fn print_error(files: &SourceFiles, err: &AssemblyError, stderr: &mut ColorWriter) {
    use crate::asm::AssemblyError::*;
    match err {
        NameNotFound(span,msg) => {
            stderr.writeln(msg);
            stderr.newline();
            highlight_line(&span, "Unknown name", files, stderr);
        },
        DivideByZero(span,msg) => {
            stderr.writeln(msg);
            stderr.newline();
            highlight_line(&span, "", files, stderr);
        },
        MustBeLiteralNumber(span) => {
            stderr.writeln(" Must be a literal number")
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        NumberOfBytesIsLongerThanLength(span, length, number_of_bytes) => {
            stderr.write("Number of bytes given (")
                .bold()
                .write(number_of_bytes)
                .reset()
                .write(") is longer than the specified length (")
                .bold()
                .write(length)
                .reset()
                .writeln(")")
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        NameAlreadyExists(current,existing,name) => {
            stderr
                .write("Name already exists: ")
                .bold()
                .writeln(name)
                .newline()
                .reset();

            stderr.bold().writeln("The name found here...").reset();
            highlight_line(&current, "Duplicate", files, stderr);
            stderr.newline().bold().writeln("...was already declared here").reset();
            highlight_line(&existing, "Original", files, stderr);
        },
        InvalidCodeLocation(span,location) => {
            stderr
                .write("Invalid code location: ")
                .bold()
                .writeln(format!("0x{:X}", location))
                .reset()
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        NumOutOfRange {
            span,
            bits,
            value
        } => {
            stderr
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
            highlight_line(&span, "", files, stderr);
        },
        ByteOutOfRange {
            span,
            value
        } => {
            stderr
                .write("Byte out of range: ")
                .bold()
                .writeln(value)
                .reset()
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        WordOutOfRange {
            span,
            value
        } => {
            stderr
                .write("Word out of range: ")
                .bold()
                .writeln(value)
                .reset()
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        SignedNumOutOfRange {
            span,
            bits,
            value
        } => {
            stderr
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
            highlight_line(&span, "", files, stderr);
        },
        InvalidAddress {
            span,
            value
        } => {
            let hex = format!("0x{:X}", value);
            stderr.write("Invalid address: ")
                .bold()
                .writeln(hex)
                .reset()
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        AddrBitsDontMatch {
            span,
            pos,
            value,
            pos_top,
            value_top
        } => {
            stderr.writeln("Target is a 12 bit absolution position, and the top 4 bits of the")
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

            highlight_line(&span, "", files, stderr);
            stderr.newline();
        },
        UnexpectedChar(loc) => {
            stderr.writeln("Unexpected Character").newline();
            highlight_line(&loc.to_span(), "", files, stderr);
        },
        UnexpectedToken(tok) => {
            stderr.writeln("Unexpected Token").newline();
            highlight_line(tok.span(), "", files, stderr);
        },
        InvalidInstruction(tok) => {
            stderr.writeln("Invalid Instruction").newline();
            highlight_line(tok.span(), "", files, stderr);
        },
        ExpectedTokenNotFound(name, tok) => {
            stderr.write("Expected to find ")
                .yellow()
                .writeln(name)
                .reset()
                .newline();
            highlight_line(tok.span(), "", files, stderr);
        },
        InvalidExpression(loc) => {
            stderr.writeln("Invalid Expression")
                .newline();
            highlight_line(&loc.to_span(), "", files, stderr);
        },
        MissingBytes(span) => {
            stderr.writeln("The ")
                .cyan()
                .write(".byte")
                .reset()
                .writeln(" directive requires at least one byte specified.")
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        MissingWords(span) => {
            stderr.writeln("The ")
                .cyan()
                .write(".word")
                .reset()
                .writeln(" directive requires at least one word specified.")
                .newline();
            highlight_line(&span, "", files, stderr);
        },
        UnknownDirective(tok) => {
            stderr.writeln("Unknown Directive")
                .newline();
            highlight_line(tok.span(), "", files, stderr);
        },
        UnknownInstruction(span) => {
            stderr.writeln("Unknown Instruction/Macro")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        WrongInstructionArgs(span,name,arg_types) => {
            stderr.write("Wrong arguments for instruction ")
                .cyan()
                .write(name)
                .reset()
                .writeln(".")
                .newline();

            highlight_line(&span, "", files, stderr);

            stderr.newline();

            if arg_types.is_empty() || (arg_types.len() == 1 && arg_types[0].is_empty()) {
                stderr.cyan()
                    .write(name)
                    .reset()
                    .writeln(" expects 0 arguments.")
                    .newline();
            } else {
                stderr.write("Allowed form");
                if arg_types.len() > 1 {
                    stderr.write("s");
                }
                stderr.write(" for instruction ")
                    .cyan()
                    .write(name)
                    .reset()
                    .write(":")
                    .newline();

                for arglist in arg_types {
                    stderr.spaces(2)
                        .cyan()
                        .write(name)
                        .reset()
                        .space();
                    let mut first = true;
                    for arg in arglist {
                        if first {
                            first = false;
                        } else {
                            stderr.write(", ");
                        }
                        match arg {
                            ArgType::Imm => stderr.yellow()
                                .write(arg.to_str())
                                .reset(),
                            ArgType::IM => stderr.magenta()
                                .write(arg.to_str())
                                .reset(),
                            ArgType::B3 |
                            ArgType::A12 |
                            ArgType::A16 |
                            ArgType::R8 |
                            ArgType::R16 |
                            ArgType::Macro |
                            ArgType::D9 => stderr.write(arg.to_str())
                        };
                    }
                    stderr.newline();
                }
            }
            stderr.newline();
        },
        UnexpectedEof => {
            stderr.writeln("Unexpected EOF")
                .newline();
        },
        FileLoadFailure(file, err) => {
            stderr.write("Error loading file ")
                .cyan()
                .write(file)
                .reset()
                .write(": ")
                .writeln(err)
                .newline();
        },
        FileUtf8Error(file, err) => {
            stderr.write("UTF-8 Error loading file ")
                .cyan()
                .write(file)
                .reset()
                .write(": ")
                .writeln(err)
                .newline();
        },
        MacroNameConflictsWithInstruction(span, name) => {
            stderr.write("Cannot name macro ")
                .cyan()
                .write(name)
                .reset()
                .writeln(": it conflicts with the instruction of the same name.")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        MacroAlreadyExists(new_span, existing_span, name, arity) => {
            stderr.write("Macro ")
                .cyan()
                .write(name)
                .reset()
                .write(" already exists for arity=")
                .write(arity)
                .writeln(".")
                .newline();
            stderr.writeln("This macro:");
            highlight_line(new_span, "", files, stderr);
            stderr.newline()
                .writeln("Already exists here:");
            highlight_line(existing_span, "Existing definition", files, stderr);
            stderr.newline();
        },
        DuplicateMacroArg(span) => {
            stderr.writeln("Duplicate macro arg.")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        InvalidMacroArg(span) => {
            stderr.writeln("No such macro argument name.")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        WrongNumberOfMacroArgs(inv_span, def_span, def_args, inv_args) => {
            stderr.write("Wrong number of arguments passed to macro. Expected ")
                .write(def_args)
                .write(", found ")
                .writeln(inv_args)
                .newline();
            highlight_line(inv_span, &format!("Found {} args", inv_args), files, stderr);
            stderr.newline()
                .bold()
                .writeln("Expected:")
                .reset()
                .newline();
            highlight_line(def_span, &format!("Expected {} args", def_args), files, stderr);
            stderr.newline();
        },
        DuplicateLabel(span) => {
            stderr.writeln("Found duplicate label")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        MacroLabelOutsideOfMacro(span) => {
            stderr.writeln("Macro label found outside of macro definition.")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        MacroArgOutsideOfMacro(span) => {
            stderr.writeln("Macro arg found outside of macro definition.")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        ImmediateValueNotAllowedHere(span) => {
            stderr.writeln("Immediate value not allowed within an expression or another immediate value.")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        IndirectionModeNotAllowedHere(span) => {
            stderr.writeln("Indirection mode not allowed within an expression or immediate value.")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        NoSuchMacro(span, name) => {
            stderr.write("No macro definition found for name ")
                .cyan()
                .write(name)
                .reset()
                .writeln(".")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        InvalidIconSize(path, w, h) => {
            stderr.write("Invalid icon size in ")
                .cyan()
                .write(path)
                .reset()
                .write(format!(". Expected {}x{}", w, h))
                .writeln(".")
                .newline();
        },
        InvalidPaletteSize(path, size, expected) => {
            stderr.write("Invalid palette size in ")
                .cyan()
                .write(path)
                .reset()
                .write(format!(". Expected {}, found {}", expected, size))
                .writeln(".")
                .newline();
        },
        ImageParseError(path, err) => {
            stderr.write("Error parsing image ")
                .cyan()
                .write(path)
                .reset()
                .write(": ")
                .writeln(err)
                .newline();
        },
        InvalidPath(span, expr) => {
            stderr.write("Invalid path ")
                .cyan()
                .write(expr)
                .reset()
                .writeln(".")
                .newline();
            highlight_line(span, "", files, stderr);
        },
        InvalidSpriteType(span, typ) => {
            stderr.write("Invalid sprite type: ")
                .cyan()
                .write(typ)
                .reset()
                .writeln(".")
                .newline();
            highlight_line(span, "", files, stderr);
        }
    }
}

fn highlight_line(span: &Span, msg: &str, files: &SourceFiles, stderr: &mut ColorWriter) {
    highlight_line_indented("", span, msg, files, stderr);
}

fn highlight_line_indented(indent: &str, span: &Span, msg: &str, files: &SourceFiles, stderr: &mut ColorWriter) {
    let file = files.for_id(span.start().file()).unwrap();
    let line = line_for_pos(span.start().pos(), file.contents());

    stderr.write(indent)
        .yellow().write(file.name()).reset()
        .write(":")
        .bold().write(span.start().line()).reset()
        .write(":")
        .bold().write(span.start().column());

    if span.start().column() != span.end().column() {
        stderr.write("-").write(span.end().column());
    }

    stderr.reset().newline();

    stderr.write(indent).spaces(2).writeln(line);
    stderr.write(indent).spaces(2).yellow().writeln(caret(&span, line, msg)).reset();

    let new_indent = if indent.len() >= 4 {
        indent.to_owned()
    } else {
        let mut new_indent = String::new();
        for _ in 0..indent.len() {
            new_indent.push_str(" ");
        }
        new_indent.push_str("  | ");
        new_indent
    };

    match span.parent() {
        Some(parent) => {
            stderr
                .write(&new_indent)
                .newline()
                .write(&new_indent)
                .cyan()
                .write("...expanded from")
                .reset()
                .newline()
                .write(&new_indent)
                .newline();
            highlight_line_indented(&new_indent, &parent, "", files, stderr);
        },
        None => {}
    }
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
    if col <= start {
        while col < start {
            result.push_str(" ");
            col += 1;
        }
        if start == end {
            result.push_str("^");
        } else {
            while col < end {
                result.push_str("^");
                col += 1;
            }
        }
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
