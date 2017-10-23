
use location::{Filenames,FileID,Location,Span};

use std::rc::Rc;
use ast;

#[derive(Debug,Clone)]
pub struct ParseError(Span,String);

// #[derive(Debug,Clone)]
// pub struct Env<V> {
//     data: Rc<EnvNode<V>>
// }

// impl<V> Env<V> {
//     pub fn empty<T>() -> Env<T> {
//         Env {
//             data: Rc::new(EnvNode::Empty)
//         }
//     }

//     pub fn with(&self, var: ast::Var, value: V) -> Env<V> {
//         match self.data {
//             &EnvNode::Empty => Env { data: Rc::new(EnvNode::SingleEntry(var, value)) },
//             &EnvNode::SingleEntry(_, _) => Env { data: Rc::new(EnvNode::MultiEntry { ident: var, value: value, parent: self.data }) },
//             &EnvNode::MultiEntry { ident: _, value: _, parent: _ } => Env {
//                 data: Rc::new(EnvNode::MultiEntry { ident: var, value: value, parent: self.data })
//             }
//         }
//     }
// }

// #[derive(Debug,Clone)]
// enum EnvNode<V> {
//     MultiEntry { ident: ast::Var, value: V, parent: Rc<EnvNode<V>> },
//     SingleEntry(ast::Var, V),
//     Empty
// }
