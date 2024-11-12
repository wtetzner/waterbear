use clap::{Parser, Subcommand, ValueEnum};

use crate::{chip8::cli::Chip8Command, img::ImageFormat};

#[derive(Parser, Debug)]
#[command(name = "waterbear", version, about, long_about = None)]
pub struct Opt {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Assembler for the Dreamcast VMU.
    Assemble {
        /// The input file to assemble.
        input: String,

        /// The output file to generate. Usually ends in `.vms`.
        #[arg(short = 'o', long)]
        output: String,
    },

    /// Expand macros and output to stdout.
    Expand {
        /// The input file to expand.
        input: String,
    },

    /// Disassembler for the Dreamcast VMU.
    Disassemble {
        /// The input file to disassemble.
        input: String,

        /// The output file
        #[arg(short = 'o', long)]
        output: String,

        /// Output byte positions
        #[arg(short = 'p', long)]
        positions: bool,

        /// The file type being disassembled.
        #[arg(short = 't', long = "type", required = false)]
        file_type: FileType,

        /// Output instruction locations that target each instruction.
        #[arg(name = "arrived-from", short = 'a', long = "arrived-from", alias = "arrived_from")]
        arrived_from: bool,
    },

    /// Convert an image file into the assembly needed for an icon.
    Icon {
        /// Input image file. Must be 32x32, and no more than 16 colors.
        input: String,

        /// Animation speed. Default is 10.
        #[arg(short = 's', long, default_value = "10")]
        speed: u16,

        /// Eyecatch image. Must be 72×56.
        #[arg(short = 'e', long)]
        eyecatch: Option<String>,
    },

    /// Convert an image file into another format.
    Image {
        /// Input image file.
        input: String,

        /// Output file
        #[arg(short = 'o', long)]
        output: Option<String>,

        /// Output format
        format: ImageFormat,
    },

    /// Extract a VMI file from a given VMS file.
    Vmi {
        /// Input VMS file.
        input: String,

        /// Output file
        #[arg(short = 'o', long)]
        output: Option<String>,

        /// Copyright notice
        #[arg(short = 'c', long)]
        copyright: Option<String>,

        /// Specify whether or not the VMS file is a game.
        #[arg(short = 'g', long)]
        game: bool,
    },

    /// Version info about this build
    Version,

    /// CHIP-8 related subcommands
    #[clap(subcommand, hide = true)]
    Chip8(Chip8Command),
}

#[derive(Debug, Clone, Copy, ValueEnum, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[clap(rename_all = "kebab")]
pub enum FileType {
    Game,
    Bios,
    Raw,
}

impl FileType {
    pub fn as_str(&self) -> &'static str {
        match self {
            FileType::Game => "game",
            FileType::Bios => "bios",
            FileType::Raw => "raw",
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            FileType::Game => "A game",
            FileType::Bios => "A bios dump",
            FileType::Raw => "A raw, unknown file type",
        }
    }
}

impl AsRef<str> for FileType {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Default for FileType {
    fn default() -> Self {
        FileType::Raw
    }
}
