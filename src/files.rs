use std;
use std::fmt;

use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use serde::Serialize;

#[derive(Debug)]
pub enum FileLoadError {
    FileLoadFailure(String, std::io::Error),
    Utf8Error(String, std::string::FromUtf8Error),
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct FileID {
    value: usize,
}

impl FileID {
    pub fn new(value: usize) -> FileID {
        FileID { value: value }
    }
}

impl fmt::Display for FileID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FileID({})", self.value)
    }
}

impl Serialize for FileID {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        serializer.serialize_u64(self.value as u64)
    }
}

pub struct SourceFile {
    id: FileID,
    name: PathBuf,
    contents: String,
}

impl SourceFile {
    pub fn id(&self) -> FileID {
        self.id
    }

    pub fn name(&self) -> &Path {
        &self.name
    }

    pub fn contents(&self) -> &str {
        &self.contents
    }
}

pub struct SourceFiles {
    working_dir: PathBuf,
    files: Vec<SourceFile>,
}

pub fn file_exists(path: impl AsRef<Path>) -> bool {
    path.as_ref().is_file()
}

pub fn load_bytes(path: impl AsRef<Path>) -> Result<Vec<u8>, FileLoadError> {
    let path = path.as_ref();
    let mut file = File::open(path)
        .map_err(|err| FileLoadError::FileLoadFailure(path.display().to_string(), err))?;
    let mut bytes = vec![];
    file.read_to_end(&mut bytes)
        .map_err(|err| FileLoadError::FileLoadFailure(path.display().to_string(), err))?;
    Ok(bytes)
}

pub fn load_file(path: impl AsRef<Path>) -> Result<String, FileLoadError> {
    let path = path.as_ref();
    let bytes = load_bytes(path)?;
    let text = String::from_utf8(bytes)
        .map_err(|err| FileLoadError::Utf8Error(path.display().to_string(), err))?;
    Ok(text)
}

impl SourceFiles {
    pub fn new() -> SourceFiles {
        let working_dir = match std::env::var_os("PWD") {
            Some(pwd) => PathBuf::from(pwd),
            None => std::env::current_dir().unwrap(),
        };
        SourceFiles {
            working_dir: if working_dir.is_absolute() {
                working_dir
            } else {
                working_dir.canonicalize().unwrap()
            },
            files: vec![],
        }
    }

    pub fn sources(&self) -> &[SourceFile] {
        &self.files
    }

    pub fn for_id(&self, id: FileID) -> Option<&SourceFile> {
        if id.value < self.files.len() {
            Some(&self.files[id.value])
        } else {
            None
        }
    }

    pub fn working_directory(&self) -> &Path {
        &self.working_dir
    }

    pub fn get_id(&self, filename: impl AsRef<Path>) -> Option<FileID> {
        let filename = filename.as_ref();
        for (idx, file) in self.files.iter().enumerate() {
            if &file.name == filename {
                return Some(FileID { value: idx });
            }
        }
        None
    }

    pub fn get(&self, filename: &str) -> Option<&str> {
        match self.get_id(filename) {
            Some(id) => Some(&self.files[id.value].contents),
            None => None,
        }
    }

    pub fn path<P: AsRef<Path>>(
        &self,
        root_file: Option<P>,
        filename: impl AsRef<Path>,
    ) -> PathBuf {
        let filename = filename.as_ref();
        if let Some(root_file) = root_file {
            let root_file = root_file.as_ref();
            let correct_path = if filename.is_absolute() {
                filename.to_path_buf()
            } else {
                let root_dir = if root_file.is_dir() {
                    root_file
                } else {
                    root_file.parent().expect("no parent found for file")
                };
                root_dir.join(filename)
            };
            if correct_path.is_file() || !filename.is_file() {
                correct_path
            } else {
                eprintln!(
                    "WARNING: Path \"{}\" not found. Falling back to \"{}\"",
                    correct_path.display(),
                    filename.display()
                );
                eprintln!("         Please change the path to be relative to the source file, instead of relative to your working directory.");
                filename.to_path_buf()
            }
        } else {
            self.working_dir.join(filename)
        }
    }

    pub fn load<P: AsRef<Path>>(
        &mut self,
        root_filename: Option<P>,
        filename: impl AsRef<Path>,
    ) -> Result<&SourceFile, FileLoadError> {
        let filename = filename.as_ref();
        let path = self.path(root_filename, filename);
        match self.get_id(&path) {
            Some(id) => Ok(&self.files[id.value]),
            None => {
                let contents = load_file(&path)?;
                let id_value = self.files.len();
                let file = SourceFile {
                    id: FileID { value: id_value },
                    name: path.to_owned(),
                    contents,
                };
                self.files.push(file);
                Ok(&self.files[id_value])
            }
        }
    }
}
