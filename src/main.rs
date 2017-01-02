#![recursion_limit = "500"]

extern crate bit_vec;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate pest;

extern crate hamt_rs;

use asm::parser;

use bit_vec::BitVec;
use std::fs::File;
use std::io::Write;

mod module;
mod unique_id;
mod asm;

use asm::instruction;

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
             (version: "0.1.0")
             (author: "Walter Tetzner <walter@waltertetzner.net>")
             (@arg INPUT: +required "Sets the input file to compile")
             (@arg OUTPUT: -o --output +required +takes_value "Output file")
            )
    ).get_matches();

    if let Some(matches) = matches.subcommand_matches("assemble") {
        assemble(matches);
    }

    let mut id_gen = unique_id::UniqueIdGenerator::new(0);
    println!("id: {:?}; id name: {}", module::Ident::new(&mut id_gen, "fred".to_string()), module::Ident::new(&mut id_gen, "fred".to_string()).to_string());

}

fn assemble(matches: &clap::ArgMatches) {
    let input_file = matches.value_of("INPUT").unwrap();
    let statements = parser::parse_file(input_file).expect("Failed to load file");
    let bytes = asm::assemble(&statements).expect("Failed to assemble");
    let mut outfile = File::create(matches.value_of("OUTPUT").unwrap()).unwrap();
    outfile.write_all(&bytes).unwrap();
}

