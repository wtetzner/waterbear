
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

| Arithmetic (8)  | Bitwise (9)     | Memory (7)      | Jump (4)        | Branch (8)      | Call (5)          | NOP (1)       |
|-----------------|-----------------|-----------------|-----------------|-----------------|-------------------|---------------|
| [`add`](#add)   | [`and`](#and)   | [`ld`](#ld)     | [`jmp`](#jmp)   | [`bz`](#bz)     | [`call`](#call)   | [`nop`](#nop) |
| [`addc`](#addc) | [`or`](#or)     | [`st`](#st)     | [`jmpf`](#jmpf) | [`bnz`](#bnz)   | [`callf`](#callf) |               |
| [`sub`](#sub)   | [`xor`](#xor)   | [`mov`](#mov)   | [`br`](#br)     | [`bp`](#bp)     | [`callr`](#callr) |               |
| [`subc`](#subc) | [`rol`](#rol)   | [`ldc`](#ldc)   | [`brf`](#brf)   | [`bpc`](#bpc)   | [`ret`](#ret)     |               |
| [`inc`](#inc)   | [`rolc`](#rolc) | [`push`](#push) |                 | [`bn`](#bn)     | [`reti`](#reti)   |               |
| [`dec`](#dec)   | [`ror`](#ror)   | [`pop`](#pop)   |                 | [`dbnz`](#dbnz) |                   |               |
| [`mul`](#mul)   | [`rorc`](#rorc) | [`xch`](#xch)   |                 | [`be`](#be)     |                   |               |
| [`div`](#div)   | [`clr1`](#clr1) |                 |                 | [`bne`](#bne)   |                   |               |
|                 | [`set1`](#set1) |                 |                 |                 |                   |               |

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

| Bit  | Affect                                                       |
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

| Bit  | Affect                                              |
|------|-----------------------------------------------------|
| `cy` | Cleared                                             |
| `ov` | Set if the remainder is zero, and cleared otherwise |
| `ac` | Unchanged                                           |


