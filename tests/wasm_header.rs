use assert_cmd::cargo::cargo_bin_cmd;
use predicates::prelude::*;

#[test]
fn writes_c_header_to_standard_output_by_default() {
    cargo_bin_cmd!("waterbear")
        .args(["wasm", "header"])
        .assert()
        .success()
        .stdout(predicate::str::contains("#include <stdint.h>"))
        .stdout(predicate::str::contains(
            "uint8_t waterbear_vmu_sfr_p3_read(void);",
        ))
        .stdout(predicate::str::contains(
            "void waterbear_vmu_sfr_p3_write(uint8_t value);",
        ))
        .stdout(predicate::str::contains(
            "void waterbear_vmu_xram_write(uint8_t bank, uint8_t offset, uint8_t value);",
        ));
}

#[test]
fn selects_cpp_declarations() {
    cargo_bin_cmd!("waterbear")
        .args(["wasm", "header", "--language", "c++"])
        .assert()
        .success()
        .stdout(predicate::str::contains("#include <stdint.h>"))
        .stdout(predicate::str::contains("extern \"C\" {"))
        .stdout(predicate::str::contains(
            "uint8_t waterbear_vmu_xram_read(uint8_t bank, uint8_t offset);",
        ));
}

#[test]
fn writes_rust_declarations_to_a_file() {
    let directory = assert_fs::TempDir::new().unwrap();
    let output = directory.path().join("intrinsics.rs");

    cargo_bin_cmd!("waterbear")
        .args(["wasm", "header", "--language", "rust", "--output"])
        .arg(&output)
        .assert()
        .success()
        .stdout(predicate::str::is_empty());

    let contents = std::fs::read_to_string(output).unwrap();
    assert!(contents.contains("unsafe extern \"C\" {"));
    assert!(contents.contains(
        "pub fn waterbear_vmu_xram_copy_to_main(destination: *mut u8, bank: u8, offset: u8, length: u8);"
    ));
}
