
use std::ffi;
use std::process;

fn main() {
    let rustc = std::env::var("RUSTC").unwrap_or("<unknown>".to_owned());
    let compiler_version = version(rustc).unwrap_or("<unknown>".to_owned());
    println!("cargo:rustc-env=RUSTC_VERSION={}", compiler_version);

    let cargo = std::env::var("CARGO").unwrap_or("<unknown>".to_owned());
    let cargo_version = version(cargo).unwrap_or("<unknown>".to_owned());
    println!("cargo:rustc-env=CARGO_VERSION={}", cargo_version);

    let gitsha = gitsha().unwrap_or("<unknown>".to_owned());
    println!("cargo:rustc-env=GITSHA={}", gitsha);
}

fn version<P: AsRef<ffi::OsStr>>(command: P) -> Option<String> {
    match process::Command::new(command).arg("-V").output() {
        Ok(o) => {
            let mut v = String::from_utf8(o.stdout).unwrap();
            v.pop();
            Some(v)
        },
        Err(_) => None
    }
}

fn gitsha() -> Option<String> {
    match process::Command::new("git").arg("rev-parse").arg("HEAD").output() {
        Ok(o) => {
            let mut v = String::from_utf8(o.stdout).unwrap();
            v.pop();
            Some(v)
        },
        Err(_) => None
    }
}

