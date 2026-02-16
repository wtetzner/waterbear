waterbear
=========

`waterbear` is an assembler/disassembler for the Dreamcast VMU.

Download
========

You can download prebuilt binaries from here: [waterbear latest release](https://github.com/wtetzner/waterbear/releases/latest).

Usage
=====

To assemble a file, use the `waterbear assemble` command:

    waterbear assemble example.s -o example.vms


Reference
=========

* [Instructions](instructions.md)
* [Directives](directives.md)
* [Macros](macros.md)
* [Debug Info](debug.md)

Info
====

`waterbear` aims to be a robust, easy-to-use tool, and meant to make VMU
programming bit more approachable.

The error messages are designed to make it easy to find out what went
wrong where:

    % waterbear assemble example.s -o example.vms
    [ERROR] Failed to assemble example.s
    
    ✘ Name already exists: my_var
    
    The name found here...
    example.s:3:0-12
      my_var = $49
      ^^^^^^^^^^^^ Duplicate
    
    ...was already declared here
    example.s:2:0-12
      my_var = $48
      ^^^^^^^^^^^^ Original

Additionally, `waterbear` has a macro system.

    %macro  Safe_8bit_Add   %op
      ; Adds an 8bit number to acc and prevents overflow by maxing acc
      ; if detected
      add     %op
      bn      psw, 7, .no_carry%
      mov     #$ff, acc
    .no_carry%
    %end

The error message will help with macro expansion as well, walking you
through which macro expansions occured to generate the invalid code.

    % waterbear assemble example.s -o example.vms
    [ERROR] Failed to assemble example.s
    
    ✘ Wrong arguments for instruction add.
    
    example.s:9:8-22
              add     %op, 7
              ^^^^^^^^^^^^^^
      | 
      | ...expanded from
      | 
      | example.s:16:2-20
      |     Safe_8bit_Add %val
      |     ^^^^^^^^^^^^^^^^^^
      | 
      | ...expanded from
      | 
      | example.s:19:0-14
      |   Foo 100 + 1000
      |   ^^^^^^^^^^^^^^
    
    Allowed forms for instruction add:
      add #i8
      add d9
      add @Ri

In this example, you can see the `...expanded from` sections, showing
each step of multiple nested macro expansions.

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

Copyright © 2015-2019 Walter Tetzner
