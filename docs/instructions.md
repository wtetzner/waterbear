
Instructions
============

Syntax
------

```
mnemonic
```

```
mnemonic arg
```

```
mnemonic arg1, arg2
```

```
mnemonic arg1, arg2, arg3
```

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

The `add` instruction adds its argument to the `acc` register. The
`cy`, `ac`, and `ov` bits are set/unset based on the result of the
addition.

### Forms

| Form      | Bit Pattern                                    | Cycles |
|-----------|------------------------------------------------|--------|
| `add #i8` | `10000001 [i7][i6][i5][i4][i3][i2][i1][i0]`    | 1      |
| `add d9`  | `1000001[d8] [d7][d6][d5][d4][d3][d2][d1][d0]` | 1      |
| `add @Ri` | `100001[a1][a0]`                               | 1      |

addc
----



sub
---

### Forms

```
sub #i8
```

```
sub d9
```

```
sub @Ri
```
