
use std::string::ToString;

#[derive(Debug,Eq,Ord,PartialOrd,PartialEq,Hash,Clone)]
pub struct UniqueId {
    value: usize
}

impl ToString for UniqueId {
    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]
pub struct UniqueIdGenerator {
    current: usize
}

impl UniqueIdGenerator {
    pub fn new(start_value: usize) -> UniqueIdGenerator {
        UniqueIdGenerator {
            current: start_value
        }
    }

    pub fn next(&mut self) -> UniqueId {
        let value = self.current;
        self.current = value + 1;
        UniqueId { value: value }
    }
}

