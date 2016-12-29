
use unique_id::{UniqueId,UniqueIdGenerator};
use std::string::ToString;

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd)]
pub struct Ident {
    name: String,
    stamp: UniqueId
}

impl Ident {
    pub fn new(unique_id_generator: &mut UniqueIdGenerator, name: String) -> Ident {
        Ident {
            name: name,
            stamp: unique_id_generator.next()
        }
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        format!("{}", self.name)
    }
}

