
use std::collections::HashMap;

pub trait Env<T> {
    fn name(&self) -> &str;
    fn get(&self, name: &str) -> Option<T>;
}

impl<T: Clone> Env<T> for HashMap<String,T> {
    fn name(&self) -> &str {
        "HashMap"
    }

    fn get(&self, name: &str) -> Option<T> {
        self.get(name).map(|x| x.clone())
    }
}

#[derive(Debug)]
pub struct Names {
    pub globals: HashMap<String,i32>,
    pub locals: HashMap<String,HashMap<String,i32>>
}

impl Names {
    pub fn as_env<'n,'h>(&'h self, name: &'n str, current_global: &str) -> NamesEnv<'n,'h,'h> {
        NamesEnv {
            name: name,
            globals: &self.globals,
            locals: &self.locals.get(current_global).unwrap()
        }
    }
}

#[derive(Debug)]
pub struct NamesEnv<'n,'g,'l> {
    name: &'n str,
    globals: &'g HashMap<String,i32>,
    locals: &'l HashMap<String,i32>
}

impl<'n,'g,'l> Env<i32> for NamesEnv<'n,'g,'l> {
    fn name(&self) -> &str {
        self.name
    }

    fn get(&self, name: &str) -> Option<i32> {
        if name.starts_with(".") {
            self.locals.get(name).map(|r| *r)
        } else {
            self.globals.get(name).map(|r| *r)
        }
    }
}
