
use instruction::{Instr, IndirectionMode};
use expression::{Expr, EvaluationError};
use std::fmt;
use location::{Span, Positioned};

#[derive(Debug)]
pub enum Directive {
    Byte(Span, Vec<Expr>),
    ByteString(Span, Vec<u8>),
    Org(Span, usize),
    Word(Span, Vec<Expr>),
    Include(Span, String),
    Cnop(Span, Expr,Expr)
}

impl Directive {
    fn eval_cnop_expr(expr: &Expr) -> Result<i32,EvaluationError> {
        use expression::Expr::*;
        match expr {
            Number(_, num) => Ok(*num),
            _ => Err(EvaluationError::MustBeLiteralNumber(expr.span()))
        }
    }

    pub fn eval_cnop(pos: i32, add: &Expr, multiple: &Expr) -> Result<i32,EvaluationError> {
        let add = Directive::eval_cnop_expr(add)?;
        let multiple = Directive::eval_cnop_expr(multiple)?;
        let mut mult = 0;
        loop {
            if pos < mult {
                break;
            }
            mult = mult + multiple;
        }
        Ok(mult + add)
    }

    pub fn size(&self, pos: i32) -> Result<i32,EvaluationError> {
        use self::Directive::*;
        match self {
            Byte(_, bytes) => Ok(bytes.len() as i32),
            ByteString(_, bytes) => Ok(bytes.len() as i32),
            Org(_, location) => Ok((*location as i32) - pos),
            Word(_, words) => Ok((words.len() * 2) as i32),
            Include(_,_) => Ok(0),
            Cnop(_, add, multiple) => Ok(Directive::eval_cnop(pos, add, multiple)? - pos)
        }
    }
}

impl Positioned for Directive {
    fn span(&self) -> Span {
        use self::Directive::*;
        match self {
            Byte(span, _) => span.clone(),
            ByteString(span, _) => span.clone(),
            Org(span, _) => span.clone(),
            Word(span, _) => span.clone(),
            Include(span,_) => span.clone(),
            Cnop(span, _, _) => span.clone()
        }
    }
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Directive::Cnop(_, exp1, exp2) => {
                write!(f, ".cnop ")?;
                write!(f, "{}", exp1)?;
                write!(f, ", ")?;
                write!(f, "{}", exp2)
            },
            Directive::Byte(_, vec) => {
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
            Directive::ByteString(_, vec) => {
                write!(f, ".byte \"")?;
                write!(f, "{}\"", escape_string(vec))
            },
            Directive::Org(_, num) => write!(f, ".org {}", num),
            Directive::Word(_, vec) => {
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
            Directive::Include(_, path) => write!(f, ".include \"{}\"", path)
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Directive(Span, Directive),
    Label(Span, String),
    Instr(Span, Instr<Expr,IndirectionMode>),
    Variable(Span, String, Expr),
    Alias(Span, String, Expr)
}

impl Positioned for Statement {
    fn span(&self) -> Span {
        use self::Statement::*;
        match self {
            Directive(span, _) => span.clone(),
            Label(span, _) => span.clone(),
            Instr(span,_) => span.clone(),
            Variable(span, _, _) => span.clone(),
            Alias(span, _, _) => span.clone()
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Directive(_, dir) => {
                write!(f, "{}", dir)
            },
            Statement::Label(_, text) => {
                write!(f, "{}:", text)
            },
            Statement::Instr(_, inst) => write!(f, "  {:?}", inst),
            Statement::Variable(_, name, expr) => {
                write!(f, "{} = {}", name, expr)
            },
            Statement::Alias(_, name, expr) => {
                write!(f, "{} EQU {}", name, expr)
            }
        }
    }
}



#[derive(Debug)]
pub struct Statements {
    pub statements: Vec<Statement>
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
            write!(f, "{}", st)?;
        }
        write!(f, "")
    }
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
