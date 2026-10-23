[![Donate](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/wtetzner/donate)

waterbear
=========

Waterbear is an assembler for the Dreamcast VMU. It can be used either as a library or as a command-line program.

Documentation
=============

The documentation for waterbear can be viewed [here](https://wtetzner.github.io/waterbear/).

Usage
=====

To assemble a file, use the `waterbear assemble` command:

    waterbear assemble example.s -o example.vms

Add `--optimize-jumps` to select smaller jump and call encodings where possible
and fold the built-in long-range conditional branch macros when their targets
are in range:

    waterbear assemble example.s -o example.vms --optimize-jumps

The optimizer can replace `jmpf` with `br` or `jmp`, replace `callf` with
`call`, and fold recognized conditional-branch macro patterns. It leaves
`brf`, `callr`, and the `jmpf` in a `NOT1 EXT,0`/`JMPF` `CHANGE` sequence
untouched. Each tentative reduction is kept only when the entire program still
assembles successfully and every existing local control-flow target still
identifies the same source byte. The pass repeats until no more reductions are
possible.

For help, run `waterbear help`:

    % waterbear help
    waterbear 0.18.0
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

Credits
=======

These are people who have either directly or indirectly helped with
the development of waterbear.

| Name                                           | Contribution                                                                 |
|------------------------------------------------|------------------------------------------------------------------------------|
| [Falco Girgis](http://www.elysianshadows.com/) | [ElysianVMU](http://evmu.elysianshadows.com/) - VMU Emulator                 |
| [Kresna Susila](http://slum.online/dreamcast/) | VMU assembly programmer/[game developer](http://slum.online/dreamcast/nvmu/) |
| [Marcus Comstedt](http://mc.pp.se/dc/)         | Author of the [aslc86k assembler](http://mc.pp.se/dc/sw.html), [softvms emulator](http://mc.pp.se/dc/sw.html), [hardware documentation](http://mc.pp.se/dc/vms/), and [tetris](http://mc.pp.se/dc/files/tetris.s) VMU game |
| John Maushammer                                | Author of the [lcdis](http://mc.pp.se/dc/sw.html) disassembler               |

Copyright © 2015-2023 Walter Tetzner
