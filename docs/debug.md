Debug Info
==========

To emit debug info, use the `--debug` flag in the `assemble` command, e.g.:
```
% waterbear assemble example.s -o example.vms --debug

```
This will emit an `example.vms.debug.json` file alongside `example.vms`.

Format
------

This is the structure of the JSON file.

| Field          | Type                            | Contents                                      |
|----------------|---------------------------------|-----------------------------------------------|
| version        | `String`                        | File format version                           | 
| language       | `String`                        | Original source language (default is `"asm"`) |
| binary         | `String`                        | Path to the VMS binary                        |
| producer       | `String`                        | Name of the software that generated this file |
| hash-algorithm | `String`                        | The algorithm used to hash sources            |
| sources        | [`[Source]`](#source)           | List of [`Source`](#source) files             |
| labels         | [`[Label]`](#labels)            | List of [`Labels`](#label)                    |
| constants      | [`[Constant]`](#constant)       | List of [`Constants`](#constant)              |
| instructions   | [`[Instruction]`](#instruction) | List of [`Instructions`](#instruction)        |

### Source

| Field | Type      | Contents                                              |
|-------|-----------|-------------------------------------------------------|
| path  | `String`  | File path of the source file                          |
| id    | `Integer` | ID of the file, referenced by [`Location`](#location) |
| hash  | `String`  | Hash of the source file, using `hash-algorithm`       |

### Label

| Field  | Type                | Contents                                                       |
|--------|---------------------|----------------------------------------------------------------|
| name   | `String`            | Name of the label                                              |
| span   | [`Span`](#span)     | Text span of the label in the original source file             |
| offset | `Integer`           | Byte offset into the VMS file                                  |
| parent | `String` (optional) | If this is a local label, it refers to the parent global label |

### Constant

| Field | Type            | Contents                                                         |
|-------|-----------------|------------------------------------------------------------------|
| name  | `String`        | Name of the constant                                             |
| span  | [`Span`](#span) | Text span of the constant definition in the original source file |
| value | `Integer`       | The value of the constant                                        |

### Instruction

| Field  | Type            | Contents                                                         |
|--------|-----------------|------------------------------------------------------------------|
| text   | `String`        | A textual representation of the instruction from the source file |
| span   | [`Span`](#span) | Text span of the instruction call from the original source file  |
| offset | `Integer`       | The byte offset of the instruction in the VMS file               |

### Span

| Field | Type                    | Contents                                               |
|-------|-------------------------|--------------------------------------------------------|
| start | [`Location`](#location) | Start location of the span in the original source file |
| end   | [`Location`](#location) | End location of the span in the original source file   |

### Location

| Field       | Type      | Contents                                                                                                |
|-------------|-----------|---------------------------------------------------------------------------------------------------------|
| source      | `Integer` | The ID of the source file, as listed in the `sources` field                                             |
| byte-offset | `Integer` | The byte offset into the original source file. Makes slicing a chunk of text out of a source file easy. |
| line        | `Integer` | Line number (starts at 1)                                                                               |
| column      | `Integer` | Column number, computed in terms of Unicode grapheme clusters (starts at 0)                             |

