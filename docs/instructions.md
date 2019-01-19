
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

