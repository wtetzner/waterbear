
use unique_id::{UniqueId,UniqueIdGenerator};
use std::string::ToString;
use hamt_rs::HamtMap;

#[derive(Debug,Eq,Hash,Clone)]
pub struct Ident {
    name: String,
    stamp: UniqueId
}

impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool {
        self.stamp == other.stamp
    }
}

impl Ident {
    pub fn new(unique_id_generator: &mut UniqueIdGenerator, name: String) -> Ident {
        Ident {
            name: name,
            stamp: unique_id_generator.next()
        }
    }

    pub fn to_unique_string(&self) -> String {
        format!("{}:{}", self.name, self.stamp.to_string())
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        format!("{}", self.name)
    }
}

#[derive(Debug,Clone)]
pub struct MissingPathError {
    path: Path,
    message: String
}

#[derive(Debug,Eq,PartialEq,Clone)]
pub enum Path {
    Pident(Ident),
    Pdot(Box<Path>, String)
}

impl ToString for Path {
    fn to_string(&self) -> String {
        match self {
            &Path::Pident(ref id) => format!("{}", id.to_string()),
            &Path::Pdot(ref p, ref name) => format!("{}.{}", p.to_string(), name)
        }
    }
}

#[derive(Clone)]
pub struct Subst {
    table: HamtMap<Ident,Path>
}

impl Subst {
    pub fn identity() -> Subst {
        Subst { table: HamtMap::new() }
    }

    pub fn add(&self, ident: Ident, path: Path) -> Subst {
        Subst { table: (self.clone().table).plus(ident, path) }
    }

    pub fn path(&self, path: &Path) -> Result<Path,MissingPathError> {
        match path {
            &Path::Pident(ref id) => match self.table.find(id) {
                Some(p) => Ok(p.clone()),
                None => Err(MissingPathError { path: path.clone(), message: format!("Path '{}' is missing", path.to_string()) })
            },
            &Path::Pdot(ref root, ref field) => Ok(Path::Pdot(Box::new(self.path(root)?.clone()), field.clone()))
        }
    }
}

pub trait CoreSyntax {
    type Term;
    type ValType;
    type DefType;
    type Kind;

    fn new() -> Self;

    fn subst_valtype(&self, subst: &Subst, val_type: &Self::ValType) -> Self::ValType;
    fn subst_deftype(&self, subst: &Subst, def_type: &Self::DefType) -> Self::DefType;
    fn subst_kind(&self, subst: &Subst, kind: &Self::Kind) -> Self::Kind;
}

pub struct TypeDecl<Kind,DefType> {
    kind: Box<Kind>,
    manifest: Box<DefType>
}

pub enum Specification<Kind,DefType,ValType> {
    Value(Ident, Box<ValType>),
    Type(Ident, Box<TypeDecl<Kind,DefType>>),
    Module(Ident, Box<ModType<Kind,DefType,ValType>>)
}

pub enum ModType<Kind,DefType,ValType> {
    Signature(Vec<Specification<Kind,DefType,ValType>>),
    FunctorType(Ident, Box<ModType<Kind,DefType,ValType>>, Box<ModType<Kind,DefType,ValType>>)
}

pub enum ModTerm<Term,Kind,DefType> {
    Longident(Path),
    Structure(Vec<Definition<Term,Kind,DefType>>)
}

pub enum Definition<Term,Kind,DefType> {
    Value(Ident, Box<Term>),
    Type(Ident, Box<Kind>, Box<DefType>),
    Module(Ident,Box<ModTerm<Term,Kind,DefType>>)
}


