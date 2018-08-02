
use std::collections::HashMap;

pub trait Env<T> {
    fn name(&self) -> &str;
    fn get(&self, name: &str) -> Option<T>;
}


