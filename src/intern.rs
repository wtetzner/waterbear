
#[macro_export]
macro_rules! interner {
    ($intern:ident, $id:ident) => {
        #[derive(Debug,Clone)]
        pub struct $intern {
            strings: Vec<String>
        }

        #[derive(Debug,Eq,PartialEq,Hash,Clone,Copy)]
        pub struct $id {
            id: usize
        }

        impl $intern {
            pub fn with_capacity(capacity: usize) -> $intern {
                $intern {
                    strings: Vec::with_capacity(capacity)
                }
            }

            pub fn intern(&mut self, string: String) -> $id {
                match self.strings.binary_search(&string) {
                    Ok(index) => $id { id: index },
                    Err(_) => {
                        self.strings.push(string);
                        $id { id: self.strings.len() - 1 }
                    }
                }
            }

            pub fn get(&self, id: $id) -> Option<&str> {
                self.strings.get(id.id).map(|v| v.as_ref())
            }
        }
    }
}

interner!(StringIntern,StringID);
