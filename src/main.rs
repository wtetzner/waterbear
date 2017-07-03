#![recursion_limit = "500"]

#[macro_use] extern crate clap;
#[macro_use] extern crate pest;
#[macro_use] extern crate lazy_static;
extern crate bit_vec;
extern crate hamt_rs;
extern crate unicode_segmentation;
extern crate regex;

use asm::parser;

use std::fs::File;
use std::io::Write;

mod module;
mod unique_id;
mod asm;
mod location;
mod lexer;

use asm::instruction;
use asm::expression::EvaluationError;
use location::{Filenames,FileID,Location,Span};
use lexer::LexerError;

use std::io::Read;

fn main() {
    let matches = clap_app!(
        wombat =>
            (version: "0.1.0")
            (author: "Walter Tetzner <walter@waltertetzner.net>")
            (about: "Compiler for the Wombat (Dreamcast VMU) programming language")
            // (@arg CONFIG: -c --config +takes_value "Sets a custom config file")
            // (@arg INPUT: +required "Sets the input file to use")
            (@arg verbose: -v ... "Sets the level of logging verbosity")
            (@subcommand assemble =>
             (about: "Assembler for the Dreamcast VMU")
             // (author: "Walter Tetzner <walter@waltertetzner.net>")
             (@arg INPUT: +required "Sets the input file to assemble")
             (@arg OUTPUT: -o --output +required +takes_value "Output file")
            )
            (@subcommand build =>
             (about: "Compiler for the Dreamcast VMU")
             (@arg INPUT: +required "Sets the input file to build")
             (@arg OUTPUT: -o --output +required +takes_value "Output file")
            )
    ).get_matches();

    if let Some(matches) = matches.subcommand_matches("assemble") {
        let input_file = matches.value_of("INPUT").unwrap();
        match assemble(matches) {
            Ok(_) => {}
            Err(ref err) => {
                println!("Failed to assemble {}:", input_file);
                println!("{}", err.to_string());
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("build") {
        let input_file = matches.value_of("INPUT").unwrap();
        let mut filenames = Filenames::with_capacity(1);
        match build(matches, &mut filenames) {
            Ok(_) => {}
            Err(ref err) => {
                println!("Failed to build {}:", input_file);
                println!("{}", err.to_string(&filenames));
            }
        }
    }

    let mut id_gen = unique_id::UniqueIdGenerator::new(0);
    println!("id: {:?}; id name: {}", module::Ident::new(&mut id_gen, "fred".to_string()), module::Ident::new(&mut id_gen, "fred".to_string()).to_string());
}

pub enum BuildError {
    FileError(String),
    LexerError(Location,String)
}

impl BuildError {
    pub fn to_string(&self, filenames: &Filenames) -> String {
        match self {
            &BuildError::FileError(ref msg) => msg.clone(),
            &BuildError::LexerError(loc, ref msg) => format!("{} {}", loc.to_string(filenames), msg)
        }
    }
}

impl std::convert::From<LexerError> for BuildError {
    fn from(error: LexerError) -> BuildError {
        BuildError::LexerError(error.location(), error.message().to_string())
    }
}

impl std::convert::From<std::io::Error> for BuildError {
    fn from(error: std::io::Error) -> BuildError {
        BuildError::FileError(format!("{}", error))
    }
}

fn build(matches: &clap::ArgMatches, filenames: &mut Filenames) -> Result<(),BuildError> {
    let input_file = matches.value_of("INPUT").unwrap();

    let mut file = File::open(input_file.to_string())?;
    let mut text = String::new();
    file.read_to_string(&mut text)?;

    let tokens = lexer::lex(filenames, input_file.to_string(), text)?;
    // println!("tokens: {:?}", tokens);
    for token in tokens.iter() {
        println!("{}", token.to_string(&filenames));
    }
    // let statements = parser::parse_file(input_file)?;
    // let bytes = asm::assemble(&statements)?;

    //let mut outfile = File::create(matches.value_of("OUTPUT").unwrap()).unwrap();
    // outfile.write_all(&bytes).unwrap();
    Ok(())
}

fn assemble(matches: &clap::ArgMatches) -> Result<(),EvaluationError> {
    let input_file = matches.value_of("INPUT").unwrap();
    let statements = parser::parse_file(input_file)?;
    let bytes = asm::assemble(&statements)?;
    let mut outfile = File::create(matches.value_of("OUTPUT").unwrap()).unwrap();
    outfile.write_all(&bytes).unwrap();
    Ok(())
}

