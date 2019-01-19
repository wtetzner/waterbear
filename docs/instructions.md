
Instructions
============

Syntax
------

Each instruction has a mnemonic (name), and between 0 and 3
arguments. Arguments are separated by commas.

Each instruction must appear on its own line.

| Args | Form                        |
|------|-----------------------------|
| 0    | `mnemonic`                  |
| 1    | `mnemonic arg`              |
| 2    | `mnemonic arg1, arg2`       |
| 3    | `mnemonic arg1, arg2, arg3` |

### Argument Syntax

There are 3 different types of syntax that can be used for arguments.

1. [*Direct*] The most obvious form is just a numeric expression, with no
   additional syntax. This is used for direct memory references, for
   both absolute and relative addresses. Simple numeric expressions
   are also used for bit specifiers.
2. [*Immediate*] Immediate values start with a `#`, and can be followed by any
   numeric expression.
3. [*Indirect*] Indirect references have special syntax for specifying the mode, of
   the form `@Ri`.

| Syntax Type | Naming                                | Examples                                   |
|-------------|---------------------------------------|--------------------------------------------|
| Direct      | `d9`, `b3`, `a12`, `a16`, `r8`, `r16` | `$45`, `my_label`, `label_foo + 7`         |
| Immediate   | `i8`                                  | `#$45`, `#my_variable`, `#my_variable + 2` |
| Indirect    | `@Ri`, `@Rj`                          | `@R0`, `@R1`, `@R2`, `@R3`                 |

Instruction Reference
=====================

| Arithmetic (8)  | Bitwise (9)     | Memory/Storage (9) | Jump (4)        | Branch (8)      | Call (5)          | NOP (1)       |
|-----------------|-----------------|--------------------|-----------------|-----------------|-------------------|---------------|
| [`add`](#add)   | [`and`](#and)   | [`ld`](#ld)        | [`jmp`](#jmp)   | [`bz`](#bz)     | [`call`](#call)   | [`nop`](#nop) |
| [`addc`](#addc) | [`or`](#or)     | [`st`](#st)        | [`jmpf`](#jmpf) | [`bnz`](#bnz)   | [`callf`](#callf) |               |
| [`sub`](#sub)   | [`xor`](#xor)   | [`mov`](#mov)      | [`br`](#br)     | [`bp`](#bp)     | [`callr`](#callr) |               |
| [`subc`](#subc) | [`rol`](#rol)   | [`ldc`](#ldc)      | [`brf`](#brf)   | [`bpc`](#bpc)   | [`ret`](#ret)     |               |
| [`inc`](#inc)   | [`rolc`](#rolc) | [`push`](#push)    |                 | [`bn`](#bn)     | [`reti`](#reti)   |               |
| [`dec`](#dec)   | [`ror`](#ror)   | [`pop`](#pop)      |                 | [`dbnz`](#dbnz) |                   |               |
| [`mul`](#mul)   | [`rorc`](#rorc) | [`xch`](#xch)      |                 | [`be`](#be)     |                   |               |
| [`div`](#div)   | [`clr1`](#clr1) | [`ldf`](#ldf)      |                 | [`bne`](#bne)   |                   |               |
|                 | [`set1`](#set1) | [`stf`](#stf)      |                 |                 |                   |               |

add
---

The `add` instruction adds its argument to the `acc`
register. Depending on the result of the addition, the `cy`, `ac`, and
`ov` bits are set/unset.

| Form      | Bit Pattern                                    | Cycles |
|-----------|------------------------------------------------|--------|
| `add #i8` | `10000001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `add d9`  | `1000001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `add @Ri` | `100001[r1][r0]`                               | 1      |

addc
----

The `addc` instruction adds both its argument as well as the value of
the carry bit (`cy`) to the `acc` register. Depending on the result of
the addition, the `cy`, `ac`, and `ov` bits are set/unset.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `addc #i8` | `10010001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `addc d9`  | `1001001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `addc @Ri` | `100101[r1][r0]`                               | 1      |

sub
---

The `sub` instruction subtracts its argument from the `acc`
register. Depending on the result of the subtraction, the `cy`, `ac`,
and `ov` bits are set/unset.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `sub #i8`  | `10100001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `sub d9`   | `1010001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `sub @Ri`  | `101001[r1][r0]`                               | 1      |

subc
----

The `subc` instruction subtracts its argument as well as the value of
the carry bit (`cy`) from the `acc` register. Depending on the result
of the subtraction, the `cy`, `ac`, and `ov` bits are set/unset.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `subc #i8` | `10110001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `subc d9`  | `1011001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `subc @Ri` | `101101[r1][r0]`                               | 1      |

inc
---

The `inc` instruction adds one to its argument. It does not change the
value of any of the `psw` flags.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `inc d9`   | `0110001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `inc @Ri`  | `011001[r1][r0]`                               | 1      |

dec
---

The `dec` instruction subtracts one from its argument. It does not
change the value of any of the `psw` flags.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `dec d9`   | `0111001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `dec @Ri`  | `011101[r1][r0]`                               | 1      |

mul
---

The `mul` instruction treats the `acc` and `c` registers as a single
16-bit argument, where `acc` makes up the high 8 bits, and `c` the low
8 bits. This 16-bit argument is multiplied by the `b` register,
resulting in a 24-bit number stored in `acc`, `c`, and `b`. The result
has the high 8 bits in `b`, the middle 8 bits in `acc`, and the low 8
bits in `c`.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `mul`      | `00110000`                                     | 7      |

| Bit  | Effect                                                       |
|------|--------------------------------------------------------------|
| `cy` | Cleared                                                      |
| `ov` | Set if the result is greater than 16-bits, cleared otherwise |
| `ac` | Unchanged                                                    |

div
---

The `div` instruction treats the `acc` and `c` registers as a single
16-bit argument, where `acc` makes up the high 8 bits, and `c` the low
8 bits. This 16-bit argument is divided by the `b` register, resulting
in 16-bit quotient and an 8-bit remainder. The high 8 bits of the
quotient is stored in the `acc` register, and the low 8 bits is stored
in the `c` register. The `b` register contains the remainder.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `div`      | `01000000`                                     | 7      |

| Bit  | Effect                                              |
|------|-----------------------------------------------------|
| `cy` | Cleared                                             |
| `ov` | Set if the remainder is zero, and cleared otherwise |
| `ac` | Unchanged                                           |

and
---

The `and` instruction does a bitwise 'and' operation between its
argument and the `acc` register. It does not change the value of any
of the `psw` flags.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `and #i8`  | `11100001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `and d9`   | `1110001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `and @Ri`  | `111001[r1][r0]`                               | 1      |

or
--

The `or` instruction does a bitwise 'or' operation between its
argument and the `acc` register. It does not change the value of any
of the `psw` flags.

| Form      | Bit Pattern                                    | Cycles |
|-----------|------------------------------------------------|--------|
| `or #i8`  | `11010001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `or d9`   | `1101001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `or @Ri`  | `110101[r1][r0]`                               | 1      |

xor
---

The `xor` instruction does a bitwise 'xor' operation between its
argument and the `acc` register. It does not change the value of any
of the `psw` flags.

| Form       | Bit Pattern                                    | Cycles |
|------------|------------------------------------------------|--------|
| `xor #i8`  | `11110001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `xor d9`   | `1111001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `xor @Ri`  | `111101[r1][r0]`                               | 1      |

rol
---

The `rol` instruction shifts the `acc` register one bit to the left,
wrapping the most significant bit around to the least significant
bit. It does not change the value of any of the `psw` flags.

| Form  | Bit Pattern | Cycles |
|-------|-------------|--------|
| `rol` | `11100000`  | 1      |

rolc
----

The `rolc` instruction shifts the `acc` register one bit to the left,
through the `cy` flag. So each bit in `acc` is moved one to the left,
the most significant bit is copied to the `cy` flag, and the previous
value of the `cy` flag is copied to the least significant bit of the
`acc` register. It does not change the value of the `ac` and `ov`
flags.

| Form   | Bit Pattern | Cycles |
|--------|-------------|--------|
| `rolc` | `11110000`  | 1      |

ror
---

The `ror` instruction shifts the `acc` register to the right, wrapping
the least significant bit around to the most significant bit. It does
not change the value of any of the `psw` flags.

| Form  | Bit Pattern | Cycles |
|-------|-------------|--------|
| `ror` | `11000000`  | 1      |

rorc
----

The `rorc` instruction shifts the `acc` register to the right, through
the `cy` flag. So each bit in `acc` is moved one to the right, the
least significant bit is copied to the `cy` flag, and the previous
value of the `cy` flag is copied to the most significant bit of the
`acc` register. It does not change the value of the `ac` and `ov` flags.

| Form   | Bit Pattern | Cycles |
|--------|-------------|--------|
| `rorc` | `11010000`  | 1      |

ld
--

The `ld` instruction loads the contents of the memory referenced by
its argument into the `acc` register. It does not change the value of
any of the `psw` flags.

| Form     | Bit Pattern                                     | Cycles |
|----------|-------------------------------------------------|--------|
| `ld d9`  | `0000001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]`  | 1      |
| `ld @Ri` | `000001[r1][r0]`                                | 1      |

st
--

The `st` instruction writes value of `acc` to the memory location
referenced by its argument. It does not change the value of any of the
`psw` flags.

| Form     | Bit Pattern                                     | Cycles |
|----------|-------------------------------------------------|--------|
| `st d9`  | `0001001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]`  | 1      |
| `st @Ri` | `000101[r1][r0]`                                | 1      |

mov
---

The `mov` instruction writes a constant value (the first argument) to
the contents of its second argument. It does not change the value of
any of the `psw` flags.

| Form            | Bit Pattern                                                                      | Cycles |
|-----------------|----------------------------------------------------------------------------------|--------|
| `mov #i8, d9`   | `0010001[d8] [d7][d6][d5][d4][d3][d2][d1][d0] [i7][i6][i5][i4][i3][i2][i1][i0]`  | 2      |
| `mov #i8, @Ri`  | `001001[r1][r0] [i7][i6][i5][i4][i3][i2][i1][i0]`                                | 1      |

ldc
---

The `ldc` instruction reads a value from ROM space and writes it to
the `acc` register. The ROM address is computed by adding `acc` to
`tr`. The value of `tr` is a 16-bit number, and is composed of `trh`
and `trl`, where `trh` is the high 8 bits, and `trl` is the low 8
bits. It does not change the value of any of the `psw` flags.

| Form  | Bit Pattern | Cycles |
|-------|-------------|--------|
| `ldc` | `11000001`  | 2      |

push
----

The `push` instruction pushes its argument onto the stack. It first
increments the value of the `sp` register, and then writes its
argument to the position pointed to by `sp`. It does not change the
value of any of the `psw` flags.

| Form      | Bit Pattern                                     | Cycles |
|-----------|-------------------------------------------------|--------|
| `push d9` | `0110000[d8] [d7][d6][d5][d4][d3][d2][d1][d0]`  | 2      |

pop
---

The `pop` instruction pops a value from the top of the stack, and
stores it in the memory pointed to by its argument. It first reads the
value from the cell pointed to by `sp`, and writes it to `acc`, then
it decrements the value of `sp`. It does not change the value of any
of the `psw` flags.

| Form     | Bit Pattern                                     | Cycles |
|----------|-------------------------------------------------|--------|
| `pop d9` | `0111000[a8] [d7][d6][d5][d4][d3][d2][d1][d0]`  | 2      |

xch
---

The `xch` instruction exchanges the contents of its argument with the
contents of the `acc` register. It does not change the value of any of
the `psw` flags.

| Form      | Bit Pattern                                    | Cycles |
|-----------|------------------------------------------------|--------|
| `xch d9`  | `1100001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `xch @Ri` | `110001[r1][r0]`                               | 1      |

