#![recursion_limit = "500"]

extern crate bit_vec;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate pest;

extern crate hamt_rs;
extern crate unicode_segmentation;

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
        match build(matches) {
            Ok(_) => {}
            Err(ref err) => {
                println!("Failed to build {}:", input_file);
                println!("{}", err.to_string());
            }
        }
    }

    let mut id_gen = unique_id::UniqueIdGenerator::new(0);
    println!("id: {:?}; id name: {}", module::Ident::new(&mut id_gen, "fred".to_string()), module::Ident::new(&mut id_gen, "fred".to_string()).to_string());
}

fn build(matches: &clap::ArgMatches) -> Result<(),LexerError> {
    let input_file = matches.value_of("INPUT").unwrap();
    let mut filenames = Filenames::with_capacity(1);
    let tokens = lexer::lex(&mut filenames, input_file.to_string(), "{{\r\n}}  ([,;:-]) -> => = > >= < <= ! != + / *  {{\n}}".to_string())?;
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

