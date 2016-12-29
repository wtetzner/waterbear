#![recursion_limit = "500"]

extern crate bit_vec;

#[macro_use]
extern crate clap;

#[macro_use]
extern crate pest;

use asm::parser;

use bit_vec::BitVec;
use std::fs::File;
use std::io::Write;

mod ast;
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
        let input_file = matches.value_of("INPUT").unwrap();
        println!("input: {:?}", input_file);
        let statements = parser::parse_file(input_file).expect("Failed to load file");
        let bytes = asm::assemble(&statements).expect("Failed to assemble");
        let mut outfile = File::create(matches.value_of("OUTPUT").unwrap()).unwrap();
        outfile.write_all(&bytes).unwrap();
    }

    let mut id_gen = unique_id::UniqueIdGenerator::new(0);
    println!("id: {:?}; id name: {}", ast::Ident::new(&mut id_gen, "fred".to_string()), ast::Ident::new(&mut id_gen, "fred".to_string()).to_string());

    println!("Hello, world!");

    println!("Encoded: {}", to_hex_string(encode()));

    println!("010: {}", i64::from_str_radix(&"010"[..], 8).unwrap());
    println!("0xFF: {}", i64::from_str_radix(&"0xFF"[2..], 16).unwrap());
    println!("1: {}", "1".parse::<i32>().unwrap());
}

fn encode() -> Vec<u8> {
    use instruction::Instruction::*;
    use instruction::IndirectionMode::*;
    use instruction::D9;

    let mut bits = BitVec::with_capacity(500);
    let instructions = vec![
        Add_i8(12),
        Add_d9(D9::new(45).expect("Invalid d9 value")),
        Add_Ri(R0)
    ];
    let mut instr_size = 0;
    for inst in instructions.iter() {
        instr_size = instr_size + inst.size();
    }
    println!("instr size: {}", instr_size);

    for inst in instructions {
        inst.encode(&mut bits);
    }

    println!("len: {}", bits.len());
    bits.to_bytes()
}

fn to_hex_string(bytes: Vec<u8>) -> String {
  let strs: Vec<String> = bytes.iter()
                               .map(|b| format!("{:08b}", b))
                               .collect();
  strs.join(" ")
}
