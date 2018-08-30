extern crate waterbear;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    waterbear::run_command(&args);
}

