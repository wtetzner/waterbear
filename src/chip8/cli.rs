use std::{fs::File, io::{BufWriter, Write}};

use clap::{Parser, Subcommand, ValueEnum};

#[derive(Debug, Subcommand)]
pub enum Chip8Command {
    /// Disassemble a CHIP-8 binary
    Disassemble {
        /// Input file
        input: String,

        /// Output file
        #[arg(short = 'o', long)]
        output: Option<String>,

        /// Starting address. Default is 0x200.
        #[arg(short = 's', long, default_value = "512")]
        start: usize,
    },
}

pub fn chip8_command(command: &Chip8Command) {
    match command {
        Chip8Command::Disassemble { input, output, start } => {
            let bytes = std::fs::read(input).unwrap();
            eprintln!("Using start address {start:#x}");
            match super::disasm::disassemble(&bytes, *start) {
                Ok(statements) => {
                    let mut out_writer = BufWriter::new(match output {
                        Some(out) => {
                            Box::new(File::create(out).unwrap()) as Box<dyn Write>
                        }
                        None => Box::new(std::io::stdout()) as Box<dyn Write>,
                    });

                    for statement in &statements {
                        writeln!(&mut out_writer, "{statement}").unwrap();
                    }
                },
                Err(err) => {
                    eprintln!("[ERROR] {err}")
                },
            }
        },
    }
}
