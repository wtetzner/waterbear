use clap::{Subcommand, ValueEnum};

use super::intrinsics::header::HeaderLanguage;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, ValueEnum)]
pub enum OutputType {
    Hir,
    Lir,
    Asm,
    #[default]
    Vms,
}

#[derive(Debug, Subcommand)]
pub enum WasmCommand {
    /// Build from WebAssembly.
    Build {
        /// Input file
        input: String,

        /// Entry point function name
        entry: Option<String>,

        /// VMU package and interrupt configuration
        #[arg(short = 'c', long)]
        config: Option<String>,

        /// Output file; writes to standard output when omitted
        #[arg(short = 'o', long)]
        out: Option<String>,

        /// Compiler stage to emit
        #[arg(long, value_enum, default_value_t)]
        output_type: OutputType,
    },

    /// Generate declarations for the VMU intrinsics available to WebAssembly.
    Header {
        /// Language to generate declarations for.
        #[arg(short = 'l', long, value_enum, default_value_t)]
        language: HeaderLanguage,

        /// Output file; writes to standard output when omitted.
        #[arg(short = 'o', long)]
        output: Option<String>,
    },
}
