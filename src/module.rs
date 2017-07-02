
#![allow(dead_code)]

use unique_id::{UniqueId,UniqueIdGenerator};
use std::string::ToString;
use hamt_rs::HamtMap;

#[derive(Debug,Eq,PartialEq,Hash,Clone)]
pub struct NamespaceName {
    name: String
}

#[derive(Debug,Eq,PartialEq,Hash,Clone)]
pub struct ModuleName {
    name: String
}

#[derive(Debug,Eq,PartialEq,Hash,Clone)]
pub struct SignatureName {
    name: String
}



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

pub struct ModSyntax<Term,ValType,DefType,Kind,Core: CoreSyntax<Term=Term,ValType=ValType,DefType=DefType,Kind=Kind>> {
    core: Core
}

impl<Term,ValType,DefType,Kind,Core: CoreSyntax<Term=Term,ValType=ValType,DefType=DefType,Kind=Kind>> ModSyntax<Term,ValType,DefType,Kind,Core> {
    pub fn new(core: Core) -> ModSyntax<Term,ValType,DefType,Kind,Core> {
        ModSyntax {
            core: core
        }
    }

    pub fn subst_typedecl(&self, decl: &TypeDecl<Kind,DefType>, subst: &Subst) -> TypeDecl<Kind,DefType> {
        TypeDecl {
            kind: Box::new(self.core.subst_kind(subst, &decl.kind)),
            manifest: match &decl.manifest {
                &None => None,
                &Some(ref dty) => Some(Box::new(self.core.subst_deftype(subst, dty.clone())))
            }
        }
    }

    pub fn subst_modtype(&self, mty: &ModType<Kind,DefType,ValType>, subst: &Subst) -> ModType<Kind,DefType,ValType> {
        match mty {
            &ModType::Signature(ref specs) => ModType::Signature(specs.iter().map(|i| self.subst_sig_item(subst, i)).collect()),
            &ModType::FunctorType(ref id, ref mty1, ref mty2) => ModType::FunctorType(
                id.clone(),
                Box::new(self.subst_modtype(mty1, subst)),
                Box::new(self.subst_modtype(mty2, subst)))
        }
    }

    fn subst_sig_item(&self, subst: &Subst, spec: &Specification<Kind,DefType,ValType>) -> Specification<Kind,DefType,ValType> {
        match spec {
            &Specification::Value(ref id, ref vty) => Specification::Value(id.clone(), Box::new(self.core.subst_valtype(subst, vty))),
            &Specification::Type(ref id, ref decl) => Specification::Type(id.clone(), Box::new(self.subst_typedecl(decl, subst))),
            &Specification::Module(ref id, ref mty) => Specification::Module(id.clone(), Box::new(self.subst_modtype(mty, subst)))
        }
    }
}

pub struct TypeDecl<Kind,DefType> {
    kind: Box<Kind>,
    manifest: Option<Box<DefType>>
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

// pub struct Env<Term,ValType,DefType,Kind,Core: CoreSyntax<Term=Term,ValType=ValType,DefType=DefType,Kind=Kind>> {
//     modsyn: Rc<ModSyntax<Term,ValType,DefType,Kind,Core>>,
//     val_table: Rc<HamtMap<Ident,TypeDecl<Kind,DefType>>>,
//     mod_table: Rc<HamtMap<Ident,ModType<Kind,DefType,ValType>>>,
// }

// impl<Term,ValType,DefType,Kind,Core: CoreSyntax<Term=Term,ValType=ValType,DefType=DefType,Kind=Kind>> Env<Term,ValType,DefType,Kind,Core> {
//     pub fn empty(modsyn: ModSyntax<Term,ValType,DefType,Kind,Core>) -> Env<Term,ValType,DefType,Kind,Core> {
//         Env {
//             modsyn: modsyn
//         }
//     }
// }

