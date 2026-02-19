use std::path::{Path, PathBuf};

use serde::Serialize;

use crate::{
    expression::{Expr, IndirectionMode},
    files::FileID,
    instruction::Instr,
    location::Span,
};

pub trait DebugStorage {
    fn source(&mut self, path: &Path, id: FileID, hash: String);
    fn label(&mut self, span: &Span, name: &str, offset: usize);
    fn nested_label(&mut self, span: &Span, parent: &str, name: &str, offset: usize);
    fn instruction(
        &mut self,
        span: &Span,
        instruction: &Instr<Expr, IndirectionMode>,
        offset: usize,
    );
    fn constant(&mut self, span: &Span, name: &str, value: i32);
}

#[derive(Serialize)]
pub struct Constant {
    name: String,
    span: Span,
    value: i32,
}

#[derive(Serialize)]
pub struct Label {
    name: String,
    span: Span,
    offset: usize,

    #[serde(skip_serializing_if = "Option::is_none")]
    parent: Option<String>,
}

#[derive(Serialize)]
pub struct Source {
    path: PathBuf,
    id: FileID,
    hash: String,
}

#[derive(Serialize)]
pub struct Instruction {
    text: String,
    span: Span,
    offset: usize,
}

#[derive(Default, Serialize)]
pub struct DebugInfo {
    pub version: Option<String>,
    pub language: Option<String>,
    pub binary: Option<PathBuf>,
    pub producer: Option<String>,

    #[serde(rename = "hash-algorithm")]
    pub hash_algorithm: String,
    pub sources: Vec<Source>,
    pub labels: Vec<Label>,
    pub constants: Vec<Constant>,
    pub instructions: Vec<Instruction>,
}

impl DebugStorage for DebugInfo {
    fn source(&mut self, path: &Path, id: FileID, hash: String) {
        self.sources.push(Source {
            path: path.to_path_buf(),
            id,
            hash,
        });
    }

    fn label(&mut self, span: &Span, name: &str, offset: usize) {
        self.labels.push(Label {
            name: name.to_owned(),
            span: span.clone(),
            offset,
            parent: None,
        });
    }

    fn nested_label(&mut self, span: &Span, parent: &str, name: &str, offset: usize) {
        self.labels.push(Label {
            name: name.to_owned(),
            span: span.clone(),
            offset,
            parent: Some(parent.to_owned()),
        });
    }

    fn instruction(
        &mut self,
        span: &Span,
        instruction: &Instr<Expr, IndirectionMode>,
        offset: usize,
    ) {
        self.instructions.push(Instruction {
            text: instruction.to_string(),
            span: span.clone(),
            offset,
        });
    }

    fn constant(&mut self, span: &Span, name: &str, value: i32) {
        self.constants.push(Constant {
            name: name.to_owned(),
            span: span.clone(),
            value,
        });
    }
}

/// Noop impl
impl DebugStorage for () {
    fn source(&mut self, _path: &Path, _id: FileID, _hash: String) {}
    fn label(&mut self, _span: &Span, _name: &str, _offset: usize) {}
    fn nested_label(&mut self, _span: &Span, _parent: &str, _name: &str, _offset: usize) {}
    fn instruction(
        &mut self,
        _span: &Span,
        _instruction: &Instr<Expr, IndirectionMode>,
        _offset: usize,
    ) {
    }
    fn constant(&mut self, _span: &Span, _name: &str, _value: i32) {}
}
