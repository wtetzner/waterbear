use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Metadata {
    labels: Vec<Label>,
    variables: Vec<Variable>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Label {
    name: String,
    address: i32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Variable {
    mnemonic: String,
    name: Option<String>,
    description: Option<String>,
    address: i32,
    bitfields: Vec<Bitfield>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Bitfield {
    bit: u8,
    mnemonic: String,
    name: Option<String>,
    description: Option<String>,
}
