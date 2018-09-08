[![Build Status](https://travis-ci.org/wtetzner/waterbear.png?branch=master)](https://travis-ci.org/wtetzner/waterbear)

waterbear
=========

Waterbear is an assembler for the Dreamcast VMU. It can be used either as a library or as a command-line program.

Usage
=====

To assemble a file, use the `waterbear assemble` command:

    waterbear assemble example.s -o example.vms

For help, run `waterbear help`:

    % waterbear help
    waterbear 0.2.0
    Assembler for the Dreamcast VMU
    
    USAGE:
        waterbear [SUBCOMMAND]
    
    FLAGS:
        -h, --help       Prints help information
        -V, --version    Prints version information
    
    SUBCOMMANDS:
        assemble       Assembler for the Dreamcast VMU
        disassemble    Disassembler for the Dreamcast VMU
        help           Prints this message or the help of the given subcommand(s)

The `help` command can be used on subcommands as well:

    % waterbear help assemble
    waterbear.exe-assemble
    Assembler for the Dreamcast VMU
    
    USAGE:
        waterbear.exe assemble <INPUT> --output <OUTPUT>
    
    FLAGS:
        -h, --help       Prints help information
        -V, --version    Prints version information
    
    OPTIONS:
        -o, --output <OUTPUT>    Output file
    
    ARGS:
        <INPUT>    Sets the input file to assemble


Building
========

Make sure you have Rust/Cargo installed: https://rustup.rs/

Ensure you have the latest version of Rust:

    $ rustup update stable

Build using Cargo:

    $ cargo build --release

