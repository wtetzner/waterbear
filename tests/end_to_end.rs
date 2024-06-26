use std::path::{Path, PathBuf};

use anyhow::Context;

fn hash_file(path: &Path) -> anyhow::Result<String> {
    use data_encoding::HEXUPPER;
    use sha2::Digest;
    use std::{fs::File, io::BufReader, io::Read};

    let input =
        File::open(path).with_context(|| format!("File \"{}\" not found", path.display()))?;
    let mut reader = BufReader::new(input);

    let digest = {
        let mut hasher = sha2::Sha512::new();
        let mut buffer = [0; 1024];
        loop {
            let count = reader.read(&mut buffer)?;
            if count == 0 {
                break;
            }
            hasher.update(&buffer[..count]);
        }
        hasher.finalize()
    };
    Ok(HEXUPPER.encode(digest.as_ref()))
}

fn test_assemble_hash(source_file: impl AsRef<Path>, hash: impl AsRef<str>) -> anyhow::Result<()> {
    let hash = hash.as_ref();
    let root = std::env::var_os("CARGO_MANIFEST_DIR")
        .map(|path| PathBuf::from(path))
        .expect("expected CARGO_MANIFEST_DIR to be set");

    let source_file = root.join("test-projects").join(source_file);
    let output_dir = assert_fs::TempDir::new()?;
    let output_file = output_dir.path().join("output.vms");

    let mut cmd = assert_cmd::Command::cargo_bin("waterbear")?;
    cmd.arg("assemble");
    cmd.arg(source_file);
    cmd.arg("--output");
    cmd.arg(&output_file);
    let result = cmd.output()?;
    if !result.status.success() {
        panic!(
            "Assemble failed\nstdout: {{\n{}\n}}\nstderr: {{\n{}\n}}",
            String::from_utf8_lossy(&result.stdout).trim(),
            String::from_utf8_lossy(&result.stderr).trim()
        );
    }

    let file_hash = hash_file(&output_file)?;
    assert_eq!(&file_hash, hash);

    Ok(())
}

#[test]
fn assemble_basic() -> anyhow::Result<()> {
    test_assemble_hash(
        "basic/basic.s",
        "7BEC663C1D789947962A730B1C6BE6DE6A3B3A3ECEDE63D148C08EA864BBED223BDA6B18478048C6AAE41C8BA963AD455AC31A921E7AEF6F21D9FF3C8FD7F5E3",
    )?;

    Ok(())
}

#[test]
fn assemble_basic_quoted() -> anyhow::Result<()> {
    test_assemble_hash(
        "basic/basic-quoted.s",
        "7BEC663C1D789947962A730B1C6BE6DE6A3B3A3ECEDE63D148C08EA864BBED223BDA6B18478048C6AAE41C8BA963AD455AC31A921E7AEF6F21D9FF3C8FD7F5E3",
    )?;

    Ok(())
}
