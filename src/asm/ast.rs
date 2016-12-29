
use instruction;
use instruction::{ToVal,B3,D9,A12};
use asm::expression::Expression;
use std::fmt;
use std::fmt::Debug;
use std;

#[derive(Debug)]
pub enum Directive {
    Byte(Vec<Expression>),
    ByteString(Vec<u8>),
    Org(usize),
    Word(Vec<Expression>),
    Include(String)
}

fn escape_string<'a>(bytes: &Vec<u8>) -> String {
    let mut escaped = Vec::new();
    let mut index = 0;
    loop {
        if index >= bytes.len() {
            break;
        }
        let current = bytes[index];
        match current {
            // \a
            0x07 => {
                escaped.push(0x5C);
                escaped.push(0x61)
            },

            // \b
            0x08 => {
                escaped.push(0x5C);
                escaped.push(0x62)
            },

            // \f
            0x0C => {
                escaped.push(0x5C);
                escaped.push(0x66)
            },

            // \n
            0x0A => {
                escaped.push(0x5C);
                escaped.push(0x6E)
            },

            // \r
            0x0D => {
                escaped.push(0x5C);
                escaped.push(0x72)
            },

            // \t
            0x09 => {
                escaped.push(0x5C);
                escaped.push(0x74)
            },

            // \v
            0x0B => {
                escaped.push(0x5C);
                escaped.push(0x76)
            },

            _ => escaped.push(current)
        }
        index = index + 1;
    }
    String::from_utf8(escaped).unwrap()
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Directive::Byte(ref vec) => {
                write!(f, ".byte ")?;
                let mut first = true;
                for b in vec {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", b)?;
                }
                write!(f, "")
            },
            &Directive::ByteString(ref vec) => {
                write!(f, ".byte \"");
                write!(f, "{}\"", escape_string(vec))
            },
            &Directive::Org(ref num) => write!(f, ".org {}", num),
            &Directive::Word(ref vec) => {
                write!(f, ".word ")?;
                let mut first = true;
                for b in vec {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", b)?;
                }
                write!(f, "")
            },
            &Directive::Include(ref path) => write!(f, ".include \"{}\"", path)
        }
    }
}

#[derive(Debug)]
pub enum Statement<S3: ToVal<B3>,S8: ToVal<i8>,S9: ToVal<D9>,S12: ToVal<A12>,S16: ToVal<u16>> {
    Directive(Directive),
    Label(String),
    Instruction(instruction::Instruction<S3,S8,S9,S12,S16>),
    Variable(String, Expression)
}

impl<S3: ToVal<B3> + Debug,S8: ToVal<i8> + Debug,S9: ToVal<D9> + Debug,S12: ToVal<A12> + Debug,S16: ToVal<u16> + Debug> fmt::Display for Statement<S3,S8,S9,S12,S16> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Statement::Directive(ref dir) => {
                write!(f, "{}", dir)
            },
            &Statement::Label(ref text) => {
                write!(f, "{}:", text)
            },
            &Statement::Instruction(ref inst) => write!(f, "  {:?}", inst),
            &Statement::Variable(ref name, ref expr) => {
                write!(f, "{} = {}", name, expr)
            }
        }
    }
}

pub struct Statements {
    pub statements: Vec<Statement<Expression,Expression,Expression,Expression,Expression>>
}

impl fmt::Display for Statements {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for st in self.statements.iter() {
            if first {
                first = false;
            } else {
                write!(f, "\n")?;
            }
            write!(f, "{}", st);
        }
        write!(f, "")
    }
}
