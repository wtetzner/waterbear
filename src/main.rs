
extern crate bit_vec;

#[macro_use]
extern crate clap;

mod ast;
mod unique_id;
mod asm;

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
            )
    ).get_matches();


    let mut id_gen = unique_id::UniqueIdGenerator::new(0);
    println!("id: {:?}; id name: {}", ast::Ident::new(&mut id_gen, "fred".to_string()), ast::Ident::new(&mut id_gen, "fred".to_string()).to_string());

    println!("Hello, world!");
}


