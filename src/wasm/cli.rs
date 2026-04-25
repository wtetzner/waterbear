use clap::Subcommand;

#[derive(Debug, Subcommand)]
pub enum WasmCommand {
    /// Build from WebAssembly.
    Build {
        /// Input file
        input: String,

        /// Output file
        #[arg(short = 'o', long)]
        output: Option<String>,
    },
}
