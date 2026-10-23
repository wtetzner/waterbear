use std::fs;

use anyhow::{Result, bail};
use waterbear::instruction::Instr;

fn assemble(source: &str, optimize_jumps: bool) -> Result<Vec<u8>> {
    let temp = assert_fs::TempDir::new()?;
    let source_path = temp.path().join("input.s");
    let output_path = temp.path().join("output.bin");
    fs::write(&source_path, source)?;

    let mut command = assert_cmd::cargo::cargo_bin_cmd!("waterbear");
    command
        .arg("assemble")
        .arg(&source_path)
        .arg("--output")
        .arg(&output_path);
    if optimize_jumps {
        command.arg("--optimize-jumps");
    }

    let result = command.output()?;
    if !result.status.success() {
        bail!(
            "assembly failed: {}",
            String::from_utf8_lossy(&result.stderr)
        );
    }
    Ok(fs::read(output_path)?)
}

#[test]
fn folds_all_builtin_conditional_macros_to_direct_branches() -> Result<()> {
    let optimized = assemble(
        r#"
start:
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
target:
        nop
        "#,
        true,
    )?;
    let direct = assemble(
        r#"
start:
        bz target
        bnz target
        bp $100, 1, target
        bpc $101, 2, target
        bn $102, 3, target
        dbnz $103, target
        dbnz @r0, target
        be #$12, target
        be $104, target
        be @r1, #$34, target
        bne #$56, target
        bne $105, target
        bne @r2, #$78, target
target:
        nop
        "#,
        false,
    )?;

    assert_eq!(optimized, direct);
    Ok(())
}

#[test]
fn folds_handwritten_patterns_with_every_jump_encoding() -> Result<()> {
    let optimized = assemble(
        r#"
start:
        bnz .skip_br
        br target
.skip_br:
        bnz .skip_jmp
        ; Comments do not obstruct a canonical pattern.
        jmp target
.skip_jmp:
        bnz .skip_jmpf
        jmpf target
.skip_jmpf:
target:
        nop
        "#,
        true,
    )?;
    let direct = assemble(
        r#"
start:
        bz target
        bz target
        bz target
target:
        nop
        "#,
        false,
    )?;

    assert_eq!(optimized, direct);
    Ok(())
}

#[test]
fn folds_trampolines_with_every_jump_encoding() -> Result<()> {
    let optimized = assemble(
        r#"
start:
        bp $100, 0, .true_br
        br .skip_br
.true_br:
        jmp target
.skip_br:
        bp $101, 1, .true_jmp
        jmp .skip_jmp
.true_jmp:
        jmpf target
.skip_jmp:
        bp $102, 2, .true_jmpf
        jmpf .skip_jmpf
.true_jmpf:
        br target
.skip_jmpf:
target:
        nop
        "#,
        true,
    )?;
    let direct = assemble(
        r#"
start:
        bp $100, 0, target
        bp $101, 1, target
        bp $102, 2, target
target:
        nop
        "#,
        false,
    )?;

    assert_eq!(optimized, direct);
    Ok(())
}

#[test]
fn repeats_until_later_folds_enable_earlier_ones() -> Result<()> {
    let optimized = assemble(
        r#"
start:
        bzo target
        bzo .near
.near:
        .text 123 ""
target:
        nop
        "#,
        true,
    )?;
    let direct = assemble(
        r#"
start:
        bz target
        bz .near
.near:
        .text 123 ""
target:
        nop
        "#,
        false,
    )?;

    assert_eq!(optimized, direct);
    Ok(())
}

#[test]
fn chooses_the_smallest_valid_jump_and_call_encodings() -> Result<()> {
    let optimized = assemble(
        r#"
        jmpf target
        callf target
target:
        nop
        "#,
        true,
    )?;
    let direct = assemble(
        r#"
        br target
        call target
target:
        nop
        "#,
        false,
    )?;
    assert_eq!(optimized, direct);

    let optimized = assemble(
        r#"
        jmpf target
        .org $200
target:
        nop
        "#,
        true,
    )?;
    let direct = assemble(
        r#"
        jmp target
        .org $200
target:
        nop
        "#,
        false,
    )?;
    assert_eq!(optimized, direct);
    Ok(())
}

#[test]
fn leaves_far_jumps_and_relative_16_bit_instructions_untouched() -> Result<()> {
    let source = r#"
        jmpf target
        callf target
        brf target
        callr target
        .org $1000
target:
        nop
    "#;

    assert_eq!(assemble(source, true)?, assemble(source, false)?);
    Ok(())
}

#[test]
fn rejects_a_shrink_that_would_break_an_existing_branch() -> Result<()> {
    let source = r#"
        jmpf fixed
        br fixed
        .org $84
fixed:
        nop
    "#;

    assert_eq!(assemble(source, true)?, assemble(source, false)?);
    Ok(())
}

#[test]
fn rebases_a_numeric_jump_to_follow_its_local_destination() -> Result<()> {
    let optimized = assemble(
        r#"
        .org $100
first:
        jmpf $100
        .text 29 ""
second:
        nop
        jmpf $120
        .org $130
        nop
        "#,
        true,
    )?;

    assert_eq!(
        Instr::<i32, u8>::decode(&optimized[0x100..]),
        Some(Instr::Br(-2))
    );
    assert_eq!(
        Instr::<i32, u8>::decode(&optimized[0x120..]),
        Some(Instr::Br(-3))
    );
    Ok(())
}

#[test]
fn rebases_a_numeric_jump_to_the_middle_of_emitted_bytes() -> Result<()> {
    let optimized = assemble(
        r#"
        .org $100
        jmpf $106
        .byte $AA, $BB, $CC, $DD, $EE
        "#,
        true,
    )?;

    assert_eq!(
        Instr::<i32, u8>::decode(&optimized[0x100..]),
        Some(Instr::Br(3))
    );
    assert_eq!(&optimized[0x102..0x107], &[0xAA, 0xBB, 0xCC, 0xDD, 0xEE]);
    Ok(())
}

#[test]
fn preserves_an_instruction_containing_an_interior_numeric_target() -> Result<()> {
    let optimized = assemble(
        r#"
        .org $100
        jmpf $104
        jmpf $2000
        "#,
        true,
    )?;

    assert_eq!(
        Instr::<i32, u8>::decode(&optimized[0x100..]),
        Some(Instr::Br(1))
    );
    assert_eq!(
        Instr::<i32, u8>::decode(&optimized[0x102..]),
        Some(Instr::Jmpf(0x2000))
    );
    Ok(())
}

#[test]
fn internal_labels_do_not_change_local_label_scope() -> Result<()> {
    let optimized = assemble(
        r#"
        .org $100
global:
        jmpf $105
        br .local
        nop
.local:
        nop
        "#,
        true,
    )?;

    assert_eq!(
        Instr::<i32, u8>::decode(&optimized[0x100..]),
        Some(Instr::Br(2))
    );
    assert_eq!(
        Instr::<i32, u8>::decode(&optimized[0x102..]),
        Some(Instr::Br(1))
    );
    Ok(())
}

#[test]
fn leaves_change_sequences_untouched() -> Result<()> {
    let source = r#"
        .org $100
        not1 $10D, 0
        jmpf $1234
        nop
    "#;

    assert_eq!(assemble(source, true)?, assemble(source, false)?);
    Ok(())
}

#[test]
fn preserves_a_trampoline_label_that_has_an_extra_reference() -> Result<()> {
    let optimized = assemble(
        r#"
start:
        bp $100, 0, .true
        br .skip
.true:
        jmpf target
.skip:
        jmp .true
target:
        nop
        "#,
        true,
    )?;

    assert_eq!(
        Instr::<i32, u8>::decode(&optimized),
        Some(Instr::Bp(0x100, 0, 2))
    );
    assert_eq!(optimized.len(), 10);
    Ok(())
}
