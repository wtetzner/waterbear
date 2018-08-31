
use std;
use std::fmt;

use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub enum FileLoadError {
    FileLoadFailure(String, std::io::Error),
    Utf8Error(String, std::string::FromUtf8Error)
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Copy,Clone)]
pub struct FileID {
    value: usize
}

impl FileID {
    pub fn new(value: usize) -> FileID { FileID { value: value } }
}

impl fmt::Display for FileID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FileID({})", self.value)
    }    
}

pub struct SourceFile {
    id: FileID,
    name: String,
    contents: String
}

impl SourceFile {
    pub fn id(&self) -> FileID {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn contents(&self) -> &str {
        &self.contents
    }
}

pub struct SourceFiles {
    working_dir: String,
    files: Vec<SourceFile>
}

pub fn load_file(path: &str) -> Result<String, FileLoadError> {
    let mut file = File::open(path).map_err(|err| FileLoadError::FileLoadFailure(path.to_string(), err))?;
    let mut bytes = vec![];
    file.read_to_end(&mut bytes).map_err(|err| FileLoadError::FileLoadFailure(path.to_string(), err))?;

    let text = String::from_utf8(bytes).map_err(|err| FileLoadError::Utf8Error(path.to_string(), err))?;
    Ok(text)
}

impl SourceFiles {
    pub fn new(working_dir: String) -> SourceFiles {
        SourceFiles {
            working_dir: working_dir,
            files: vec![]
        }
    }

    pub fn for_id(&self, id: FileID) -> Option<&SourceFile> {
        if id.value < self.files.len() {
            Some(&self.files[id.value])
        } else {
            None
        }
    }

    pub fn get_id(&self, filename: &str) -> Option<FileID> {
        for (idx, file) in self.files.iter().enumerate() {
            if &file.name == filename {
                return Some(FileID { value: idx })
            }
        }
        None
    }

    pub fn get(&self, filename: &str) -> Option<&str> {
        match self.get_id(filename) {
            Some(id) => Some(&self.files[id.value].contents),
            None => None
        }
    }

    pub fn load(&mut self, filename: &str) -> Result<&SourceFile,FileLoadError> {
        let path = if self.working_dir.is_empty() {
            filename.to_owned()
        } else {
            format!("{}/{}", self.working_dir, filename)
        };
        match self.get_id(&path) {
            Some(id) => Ok(&self.files[id.value]),
            None => {
                let contents = load_file(&path)?;
                let id_value = self.files.len();
                let file = SourceFile {
                    id: FileID { value: id_value },
                    name: filename.to_owned(),
                    contents: contents
                };
                self.files.push(file);
                Ok(&self.files[id_value])
            }
        }
    }
}
