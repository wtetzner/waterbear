use std::fs;

use assert_cmd::cargo::cargo_bin_cmd;
use walrus::{FunctionBuilder, Module};

fn write_minimal_wasm(path: &std::path::Path) {
    let mut module = Module::default();
    let builder = FunctionBuilder::new(&mut module.types, &[], &[]);
    let main = builder.finish(Vec::new(), &mut module.funcs);
    module.exports.add("main", main);
    module.emit_wasm_file(path).unwrap();
}

#[test]
fn emits_each_compiler_stage_and_defaults_to_vms() {
    let directory = assert_fs::TempDir::new().unwrap();
    let input = directory.path().join("minimal.wasm");
    write_minimal_wasm(&input);

    let hir = cargo_bin_cmd!("waterbear")
        .args(["wasm", "build"])
        .arg(&input)
        .args(["--output-type", "hir"])
        .output()
        .unwrap();
    assert!(
        hir.status.success(),
        "{}",
        String::from_utf8_lossy(&hir.stderr)
    );
    let hir = String::from_utf8(hir.stdout).unwrap();
    assert!(hir.starts_with("program {\n"));
    assert!(hir.contains("main fn main()"));
    assert!(!hir.contains("lir program"));

    let lir = cargo_bin_cmd!("waterbear")
        .args(["wasm", "build"])
        .arg(&input)
        .args(["--output-type", "lir"])
        .output()
        .unwrap();
    assert!(
        lir.status.success(),
        "{}",
        String::from_utf8_lossy(&lir.stderr)
    );
    let lir = String::from_utf8(lir.stdout).unwrap();
    assert!(lir.starts_with("lir program {\n"));
    assert!(lir.contains("main fn main"));
    assert!(!lir.contains("ASM:"));

    let asm = cargo_bin_cmd!("waterbear")
        .args(["wasm", "build"])
        .arg(&input)
        .args(["--output-type", "asm"])
        .output()
        .unwrap();
    assert!(
        asm.status.success(),
        "{}",
        String::from_utf8_lossy(&asm.stderr)
    );
    let asm = String::from_utf8(asm.stdout).unwrap();
    assert!(asm.contains(".org 0"));
    assert!(asm.contains(".org $0200"));
    assert!(asm.contains("main:"));
    assert!(!asm.contains("HIR:"));

    let vms = cargo_bin_cmd!("waterbear")
        .args(["wasm", "build"])
        .arg(&input)
        .output()
        .unwrap();
    assert!(
        vms.status.success(),
        "{}",
        String::from_utf8_lossy(&vms.stderr)
    );
    assert!(vms.stdout.len() > 0x0480);
    assert_eq!(&vms.stdout[0x0200..0x0210], b"Waterbear WASM  ");

    let output = directory.path().join("minimal.vms");
    let written = cargo_bin_cmd!("waterbear")
        .args(["wasm", "build"])
        .arg(&input)
        .arg("--out")
        .arg(&output)
        .output()
        .unwrap();
    assert!(
        written.status.success(),
        "{}",
        String::from_utf8_lossy(&written.stderr)
    );
    assert!(written.stdout.is_empty());
    assert_eq!(fs::read(output).unwrap(), vms.stdout);
}
