extern crate waterbear;

#[macro_use]
extern crate clap;
extern crate termcolor;

use std::fs::File;
use std::io::Write;
use std::string::ToString;

use waterbear::instruction;
use waterbear::expression::EvaluationError;

use std::io::Read;

const DESCRIPTION: &'static str = env!("CARGO_PKG_DESCRIPTION");
const NAME: &'static str = env!("CARGO_PKG_NAME");
const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    // We have to explicitly enable ANSI term support on Windows
    #[cfg(windows)]
    ansi_term::enable_ansi_support();

    let matches = clap_app!(
        waterbear =>
            (name: NAME)
            (version: VERSION)
            (about: DESCRIPTION)
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

    {
        use std::io::Write;
        use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

        let mut stdout = StandardStream::stdout(ColorChoice::Auto);
        writeln!(&mut stdout, "[");
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green))).unwrap();
        writeln!(&mut stdout, "OK");
        stdout.reset();
        writeln!(&mut stdout, "]");
        //writeln!(&mut stdout, "green text!")?;

        // println!("[{}] {} is {} cool",
        //          Green.paint("OK"),
        //          Style::new().bold().paint("Fred"),
        //          Red.paint("very")
        // );
    }
}

fn assemble(matches: &clap::ArgMatches) -> Result<(), EvaluationError> {
    // let input_file = matches.value_of("INPUT").unwrap();
    // let bytes = waterbear::assemble_file(&input_file)?;
    // let mut outfile = File::create(matches.value_of("OUTPUT").unwrap()).unwrap();
    // outfile.write_all(&bytes).unwrap();
    Ok(())
}
