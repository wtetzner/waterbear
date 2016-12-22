
extern crate bit_vec;

#[macro_use]
extern crate clap;

use bit_vec::BitVec;

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

    println!("Encoded: {}", to_hex_string(encode()));
}

fn encode() -> Vec<u8> {
    use asm::Instruction::*;
    use asm::IndirectionMode::*;
    use asm::D9;

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
