use clap::Subcommand;

#[derive(Debug, Subcommand)]
pub enum WasmCommand {
    /// Build from WebAssembly.
    Build {
        /// Input file
        input: String,

        /// Entry point function name
        entry: Option<String>,

        /// Output file
        #[arg(short = 'o', long)]
        output: Option<String>,
    },
}
