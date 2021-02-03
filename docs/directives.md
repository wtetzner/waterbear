
Directives
==========

Directives are used to give directions to the assembler. Directive
names start with a `.`, and each directive must appear on its own
line.

<!--ts-->
   * [`.byte` Directive](#byte)
   * [`.word` Directive](#word)
   * [`.text` Directive](#text)
   * [`.string` Directive](#string)
   * [`.org` Directive](#org)
   * [`.include` Directive](#include)
       * [asm](#include-source-files)
       * [bytes](#include-binary-data)
       * [cpp](#include-c-header-file)
       * [icon](#include-icon)
       * [sprite](#include-sprite)
   * [`.cnop` Directive](#cnop)
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

.text
-----

The `.text` directive inserts a string of characters directly into the
assembler output as a stream of bytes. It takes two arguments: a
length, and the string.

If the string is shorter than the specified length, it is padded with spaces.

The `.text` directive is useful for things like the VMU file description fields.

### Syntax

```
.text length string
```

### Examples

```
.text 16 "My Game"
.text 32 "My Game by Me"
```

.string
-----

The `.string` directive inserts a string of characters directly into the
assembler output as a stream of bytes. It takes two arguments: a
length, and the string.

If the string is shorter than the specified length, it is padded with `nul` bytes.

The `.string` directive is useful for things like the VMU file
application identifier field.

### Syntax

```
.string length string
```

### Example

```
.string 16 "My File Creator"
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

### Include Icon

Every VMU file specifies an icon section, as well as an optional
eyecatch image.

The icon can be animated, and both the speed and the number of frames
are specified in the file. Additionally, the icon uses a palette of 16
16-bit colors. It is always 32x32 pixels.

The eyecatch image can be missing, or it can be a single image using a
16-color palette, a 256-color palette, or it can use 16-bit true
color, where each pixel is directly specified using a 16-bit color.

Instead of writing out those bytes directly in the assembly source
file, or using some external tool to generate the right bytes, images
can be directly included using the `.include icon` directive.

```
.include icon "icon.gif" speed=16, eyecatch="eyecatch.png"
```

Both `speed` and `eyecatch` are optional, e.g. you can use any of these variations:

```
.include icon "icon.gif" speed=16
```

```
.include icon "icon.gif" eyecatch="eyecatch.png"
```

```
.include icon "icon.gif"
```

If `speed` is omitted, a default value will be used. If `eyecatch` is
omitted, then an eyecatch image will not be included.

### Include Sprite

The VMU hardware doesn't have the concept of sprites, unlike other
well-known 2D gaming hardware. However, it is still a useful concept
when writing games.

An image file can be included in your ROM by using the
`.include sprite` directive.

```
.include sprite "images/character.png"
```

If you specify `type="masked"`, it will embed a transparency mask
along with the pixel data.

```
.include sprite "images/character.png" type="masked"
```

The sprite format is compatible with the format used by [LibPerspective](http://slum.online/dreamcast/).

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
