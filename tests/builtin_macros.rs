use std::fs;

use anyhow::{Result, bail};

fn assemble(source: &str, debug: bool) -> Result<(Vec<u8>, Option<serde_json::Value>)> {
    let temp = assert_fs::TempDir::new()?;
    let source_path = temp.path().join("builtins.s");
    let output_path = temp.path().join("builtins.bin");
    fs::write(&source_path, source)?;

    let mut command = assert_cmd::cargo::cargo_bin_cmd!("waterbear");
    command
        .arg("assemble")
        .arg(&source_path)
        .arg("--output")
        .arg(&output_path);
    if debug {
        command.arg("--debug");
    }

    let result = command.output()?;
    if !result.status.success() {
        bail!(
            "assembly failed: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }

    let bytes = fs::read(&output_path)?;
    let debug_info = if debug {
        let debug_path = temp.path().join("builtins.bin.debug.json");
        Some(serde_json::from_slice(&fs::read(debug_path)?)?)
    } else {
        None
    };
    Ok((bytes, debug_info))
}

#[test]
fn assembles_all_builtin_conditional_macro_forms() -> Result<()> {
    let (bytes, _) = assemble(
        r#"
        bzo target
        bnzo target
        bpo $100, 1, target
        bpco $101, 2, target
        bno $102, 3, target
        dbnzo $103, target
        dbnzo @r0, target
        beo #$12, target
        beo $104, target
        beo @r1, #$34, target
        bneo #$56, target
        bneo $105, target
        bneo @r2, #$78, target

        .org $200
target:
        nop
        "#,
        false,
    )?;

    assert_eq!(bytes.len(), 0x201);
    Ok(())
}

#[test]
fn builtin_macros_use_far_jumps_and_hygienic_labels() -> Result<()> {
    let (bytes, _) = assemble(
        r#"
        bzo target
        bzo target
        .org $100
target:
        nop
        "#,
        false,
    )?;

    assert_eq!(
        &bytes[..10],
        &[0x90, 0x03, 0x21, 0x01, 0x00, 0x90, 0x03, 0x21, 0x01, 0x00]
    );
    Ok(())
}

#[test]
fn debug_assembly_tracks_the_embedded_macro_source() -> Result<()> {
    let (_, debug_info) = assemble(
        r#"
        bzo target
target:
        nop
        "#,
        true,
    )?;

    let debug_info = debug_info.expect("debug info");
    let sources = debug_info
        .get("sources")
        .and_then(serde_json::Value::as_array)
        .expect("debug sources");
    assert_eq!(sources.len(), 2);
    Ok(())
}

#[test]
fn expand_command_expands_builtin_macros() -> Result<()> {
    let temp = assert_fs::TempDir::new()?;
    let source_path = temp.path().join("builtins.s");
    fs::write(&source_path, "bzo target\ntarget:\n  nop\n")?;

    let result = assert_cmd::cargo::cargo_bin_cmd!("waterbear")
        .arg("expand")
        .arg(&source_path)
        .output()?;
    if !result.status.success() {
        bail!(
            "expansion failed: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }

    let expanded = String::from_utf8(result.stdout)?;
    assert!(expanded.contains("bnz"));
    assert!(expanded.contains("jmpf"));
    assert!(!expanded.contains("bzo"));
    Ok(())
}
