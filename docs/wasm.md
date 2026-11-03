WebAssembly builds
==================

`waterbear wasm build` analyzes a WebAssembly module and emits an optimized VMS
image by default:

    waterbear wasm build program.wasm --out program.vms

Use `--output-type` to inspect an intermediate compiler stage instead:

    waterbear wasm build program.wasm --output-type hir
    waterbear wasm build program.wasm --output-type lir
    waterbear wasm build program.wasm --output-type asm --out program.s

The supported output types are `hir`, `lir`, `asm`, and `vms`. When `--out` is
omitted, the selected artifact is written to standard output. VMS output is
binary. VMS assembly always enables Waterbear's jump optimization pass.

The entry point defaults to the exported function `main`. A different exported
function can be supplied as the second positional argument or in a TOML build
configuration:

    waterbear wasm build program.wasm --config waterbear.toml

Configuration
-------------

Every setting is optional. This is a complete example:

```toml
[wasm]
entry = "main"

[package]
vmu_description = "My VMU Program"
dreamcast_description = "My VMU Program by Me"
application_id = "my-vmu-program"

[package.icon]
path = "images/icon.gif"
speed = 10
eyecatch = "images/eyecatch.png"

[interrupts]
external_0 = "on_external_0"
external_1 = "on_external_1"
external_2_or_timer_0_low = "on_timer_0_low"
external_3_or_base_timer = "on_base_timer"
timer_0_high = "on_timer_0_high"
timer_1 = "on_timer_1"
serial_0 = "on_serial_0"
serial_1 = "on_serial_1"
dreamcast_interface = "on_dreamcast"
port_3 = "on_port_3"
```

Icon and eyecatch paths are resolved relative to the configuration file. An
eyecatch requires an icon. When no icon is configured, Waterbear emits a blank
one-frame icon and no eyecatch so the package still has a complete VMS header.

The package text defaults to `Waterbear WASM`, `Waterbear WebAssembly Program`,
and `waterbear-wasm`. The VMU description, Dreamcast description, and
application ID may contain at most 16, 32, and 16 bytes respectively.

Interrupt handlers name exported WebAssembly functions with the signature
`() -> ()`. Waterbear keeps each named function normally callable and installs
a small interrupt wrapper that calls it and returns with `reti`.
