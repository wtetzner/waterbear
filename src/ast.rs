
#[derive(Debug,Clone)]
pub struct ModulePath {
    namespace: String,
    modules: Vec<String>
}

#[derive(Debug,Clone)]
pub struct Var {
    path: Option<ModulePath>,
    name: String    
}

#[derive(Debug,Clone)]
pub enum Expression {
    Sint(i64),
    Uint(u64),
    Var(Var)
}


