
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct GlobalEnv {}

pub enum LookupError {
    NoDefinition(Path),
    NoNamespace(Path),
    NoSpec(Path)
}

#[derive(Debug, Clone)]
pub struct Namespaces {
    namespaces: HashMap<NamespaceName,Namespace>,
}

impl Namespaces {
    pub fn lookup_definition(&self, path: &Path) -> Result<&Definition,LookupError> {
        if path.is_empty() {
            Err(LookupError::NoNamespace(path.clone()))
        } else {
        }
    }

    pub fn lookup_namespace(&self, name: &NamespaceName) -> Result<&Namespace,LookupError> {
        if ()
    }
}

#[derive(Debug, Clone)]
pub struct Namespace {
    namespaces: HashMap<String,Namespace>,
    modules: HashMap<String,Module>,
    signatures: HashMap<String,Signature>
}

#[derive(Debug, Clone)]
pub enum NamespaceContent {
    Namespace(Box<Namespace>),
    Module(Box<Module>),
    Signature(Box<Signature>),
}

#[derive(Debug, Clone)]
pub struct Module {
    signatures: HashMap<String,Path>,
    modules: HashMap<String,Box<Module>>,
    values: HashMap<String,Box<Expression>>,
    types: HashMap<String,Vec<String>,Box<TypeExpression>>
}

#[derive(Debug, Clone)]
pub enum Definition {
    Module(Box<Module>),
    Signature(Box<Signature>),
    Value(TypeExpression,Expression),
    Type(TypeExpression),
}

#[derive(Debug, Clone)]
pub enum TypeExpression {
    Name(String),
    Variable(String),
    Parameterized(String, Vec<TypeExpression>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NamespaceName {
    name: String
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NamespaceContentName {
    Namespace(String),
    Module(String),
    Signature(String),
    Value(String),
    Type(String)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DefinitionName {
    Module(String),
    Signature(String),
    Value(String),
    Type(String)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SpecName {
    ModuleType(String),
    Type(String),
    Value(String)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PathPart {
    NamespaceContent(NamespaceContentName),
    Definition(DefinitionName),
    Spec(SpecName)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path {
    namespaces: Vec<String>,
    modules: Vec<String>,
    signatures: Vec<String>,
}

impl Path {
    pub fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct Signature {
    specs: HashMap<SpecName,>
}

#[derive(Debug, Clone)]
pub struct Var {
    path: Option<DefinitionName>,
    name: String,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Sint(i64),
    Uint(u64),
    Var(Var)
}
