use clap::{Subcommand};

#[derive(Debug, Subcommand)]
pub enum WasmCommand {
    /// Disassemble a CHIP-8 binary
    Build {
        /// Input file
        input: String,

        /// Output file
        #[arg(short = 'o', long)]
        output: Option<String>,
    },
}
