extern crate wombat_vmu_asm;

#[macro_use]
extern crate clap;

use std::fs::File;
use std::io::Write;

use wombat_vmu_asm::instruction;
use wombat_vmu_asm::expression::EvaluationError;

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
            // (@subcommand build =>
            //   (about: "Compiler for the Dreamcast VMU")
            //   (@arg INPUT: +required "Sets the input file to build")
            //   (@arg OUTPUT: -o --output +required +takes_value "Output file")
            //  )
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
    } // else if let Some(matches) = matches.subcommand_matches("build") {
    //     let input_file = matches.value_of("INPUT").unwrap();
    //     let mut filenames = Filenames::with_capacity(1);
    //     match build(matches, &mut filenames) {
    //         Ok(_) => {}
    //         Err(ref err) => {
    //             println!("Failed to build {}:", input_file);
    //             println!("{}", err.to_string(&filenames));
    //         }
    //     }
    // }
}

fn assemble(matches: &clap::ArgMatches) -> Result<(), EvaluationError> {
    let input_file = matches.value_of("INPUT").unwrap();
    let bytes = wombat_vmu_asm::assemble_file(&input_file)?;
    let mut outfile = File::create(matches.value_of("OUTPUT").unwrap()).unwrap();
    outfile.write_all(&bytes).unwrap();
    Ok(())
}
