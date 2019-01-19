
Directives
==========

Directives are used to give directions to the assembler.

<!--ts-->
   * [.byte](#.byte)
   * [.word](#.word)
   * [.org](#.org)
   * [.include](#.include)
       * [asm](#Include+Source+Files)
   * [.cnop](#.cnop)
<!--te-->

.byte
-----

The `.byte` directive inserts bytes directly into the assembler
output. Any expression can be used to specify a byte value, as long as
it evaluates to an 8-bit number.

### Syntax

```
.byte expr
```

```
.byte expr1, expr2,...
```

### Examples

```
.byte $45
```

```
.byte $32, $45, %10100010, 75, label_01 + 7
```

.word
-----

The `.word` directive inserts 16-bit words directly into the assembler
output. Any expression can be used to specify a word value, as long as
it evaluates to a 16-bit number.


### Syntax

```
.word expr
```

```
.word expr1, expr2,...
```

### Examples

```
.word $2190
```

```
.word $8675, $2354, 765, %1001010111010010, 546
```

.org
----

The `.org` directive moves the "output cursor" of the assembler. Any
instructions/directives following an `.org` directive will start
outputting bytes at the location specified by the `.org` directive.

### Syntax

```
.org offset
```

### Example

```
.org $45
```

.include
--------

The `.include` directive tells the assembler to include another file
at the point of the directive. The most common usage is to include
another source file, but there are other forms of `.include` as well.

### Include Source Files

To include another source file, you can use the `asm` form of the
`.include` directive:

```
.include asm "sfr.i"
```

Because this is the most common form of `.include`, you can omit `asm`:

```
.include "sfr.i"
```

### Include Binary Data

A binary file can be directly injected into the assembler output using
the `bytes` form of the `.include` directive. The binary file will
automatically be converted into the appropriate `.byte` directive.

```
.include bytes "file.bin"
```

### Include C Header File

A C preprocessor file can be imported into an assembly source file
using the `cpp` form of the `.include` directive. Only a subset of C
syntax is supported. The motivation behind this feature is to allow
sharing C preprocessor constants between a C program and an assembly
program.

The C preprocessor constants will automatically be converted into
assembly aliases.

```
.include cpp "header.h"
```

.cnop
-----

The `.cnop` directive is used to align the assembler output. The
output is padded in the following manner:

1. Pad to the smallest multiple of `m` that is larger than or equal to the current position.
2. Pad out to an additional `a` bytes.

### Syntax

```
.cnop a, m
```

### Example

This example ensures that the output is aligned to a block size of `$200`.

```
.cnop 0, $200
```
