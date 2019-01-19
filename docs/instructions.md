
Instructions
============

Syntax
------

Each instruction has a mnemonic (name), and between 0 and 3
arguments. Arguments are separated by commas.

| Args | Form                        |
|------|-----------------------------|
| 0    | `mnemonic`                  |
| 1    | `mnemonic arg`              |
| 2    | `mnemonic arg1, arg2`       |
| 3    | `mnemonic arg1, arg2, arg3` |

### Argument Syntax

There are 3 different types of syntax that can be used for arguments.

1. *Direct* The most obvious form is just a numeric expression, with no
   additional syntax. This is used for direct memory references, for
   both absolute and relative addresses. Simple numeric expressions
   are also used for bit specifiers.
2. *Immediate* Immediate values start with a `#`, and can be followed by any
   numeric expression.
3. *Indirect* Indirect references have special syntax for specifying the mode, of
   the form `@Ri`.

| Syntax Type | Naming                                | Examples                                   |
|-------------|---------------------------------------|--------------------------------------------|
| Direct      | `d9`, `b3`, `a12`, `a16`, `r8`, `r16` | `$45`, `my_label`, `label_foo + 7`         |
| Immediate   | `i8`                                  | `#$45`, `#my_variable`, `#my_variable + 2` |
| Indirect    | `@Ri`, `@Rj`                          | `@R0`, `@R1`, `@R2`, `@R3`                 |

Instruction Reference
=====================

|  Arithmetic     | Bitwise         |
|-----------------|-----------------|
| [`add`](#add)   | [`and`](#and)   |
| [`addc`](#addc) | [`or`](#or)     |
| [`sub`](#sub)   | [`xor`](#xor)   |
| [`subc`](#subc) | [`rol`](#rol)   |
| [`inc`](#inc)   | [`rolc`](#rolc) |
| [`dec`](#dec)   | [`ror`](#ror)   |
| [`mul`](#mul)   | [`rorc`](#rorc) |
| [`div`](#div)   |                 |

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

