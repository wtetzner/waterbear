
use location::{Filenames,FileID,Location,Span};

use std::rc::Rc;
use ast;

#[derive(Debug,Clone)]
pub struct ParseError(Span,String);

#[derive(Debug,Clone)]
pub enum Env<V> {
    MultiEntry { ident: ast::Var, value: Rc<V>, parent: Rc<Env<V>> },
    SingleEntry(ast::Var, Rc<V>),
    Empty
}

