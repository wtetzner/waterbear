
Macros
======

Macros are a way to introduce custom abstractions into an assembly
program. They are invoked using syntax that looks just like an
instruction, but the assembler automatically expands them into actual
instructions before assembling the final binary.

Syntax
------

The `%macro` keyword is used to define a new macro. It's followed by
the name of the macro, as well as a comma-separated list of argument
names, all of which start with `%`.

```
%macro double %arg1, %arg2, %arg3
```

The following lines make up the body of the macro, which can refer to
the arguments.

```
  ld %arg1
  add %arg2
```

The body can also use hygienic labels, which end with a `%`.

```
  my_label%:
```

Each time the macro is invoked, a hygienic label gets a completely
unique name, so it won't conflict with other similarly named labels in
the rest of the program.

Finally, the macro ends with `%end`.

Here's an example of a macro (courtesy of [Kresna Susila](http://slum.online/dreamcast/)).
It adds an 8-bit number to
`acc`, but if the addition overflows, `acc` is set to the maximum
value (`$ff`).

```
%macro  Safe_8bit_Add   %op
        ; Adds an 8bit number to acc and prevents overflow by maxing acc
        ; if detected
        add     %op
        bn      psw, 7, .no_carry%
        mov     #$ff, acc
.no_carry%
%end
```

The usage of a macro looks identical to an instruction. The name of
the macro is followed by a comma separated list of arguments.

```
  Safe_8bit_Add my_var
```

When assembling this code, the call to `Safe_8bit_Add` will be
expanded into the body of the macro, with the argument replaced.

To see what a macro invocation looks like, the `expand` subcommand can
be used. It will expand all macros and print the resulting assembly.

```
% waterbear expand example.s
  add   my_var
  bn    psw, 7, .no_carry_541957e1bc47460b88675e597f900408
  mov   #$FF, acc
.no_carry_541957e1bc47460b88675e597f900408:
```

You can see that `%op` was replaced with `my_var`, and `.no_carry%`
was expanded into `.no_carry_541957e1bc47460b88675e597f900408`, which
is a unique label name.

To further illustrate the unique label names, lets look at an example
with two invocations of `Safe_8bit_Add`.

```
  Safe_8bit_Add my_var
  Safe_8bit_Add my_var2
```

This expands to:

```
% waterbear expand example.s
  add   my_var
  bn    psw, 7, .no_carry_f909e567e61249f6853f0c939d50982f
  mov   #$FF, acc
.no_carry_f909e567e61249f6853f0c939d50982f:
  add   my_var2
  bn    psw, 7, .no_carry_10a78ffefae24a828adf29904d7bae8e
  mov   #$FF, acc
.no_carry_10a78ffefae24a828adf29904d7bae8e:
```

You can see that the `.no_carry` in the first invocation expands to
`.no_carry_f909e567e61249f6853f0c939d50982f`, and the `.no_carry` in
the second invocation expands to
`.no_carry_10a78ffefae24a828adf29904d7bae8e`. This way, you don't need
to worry about macro invocations conflicting with each other.
