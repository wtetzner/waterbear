
use std::fmt;
use location::{Positioned, Location, Span};
use env::Env;

#[derive(Debug)]
pub enum EvaluationError {
    NameNotFound(Span,String),
    DivideByZero(Span,String),
    MustBeLiteralNumber(Span)
}

impl EvaluationError {
    pub fn to_string(&self) -> String {
        use EvaluationError::*;
        match self {
            NameNotFound(span, msg) => format!("{}: {}", span, msg),
            DivideByZero(span, msg) => format!("{}: {}", span, msg),
            MustBeLiteralNumber(span) => format!("{}: Must be a literal integer", span)
        }
    }
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
pub enum Expr {
    Name(Span, String),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    UnaryMinus(Location, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
    Number(Span, i32),
    UpperByte(Location, Box<Expr>),
    LowerByte(Location, Box<Expr>)
}

impl Expr {
    pub fn num(num: i32) -> Expr {
        Expr::Number(Span::default(), num)
    }

    pub fn name(name: &str) -> Expr {
        Expr::Name(Span::default(), name.to_owned())
    }
}

impl Positioned for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Name(span, _) => span.clone(),
            Expr::Plus(expr1, expr2) => Span::new(
                expr1.span().start().clone(),
                expr2.span().end().clone()
            ),
            Expr::Minus(expr1, expr2) => Span::new(
                expr1.span().start().clone(),
                expr2.span().end().clone()
            ),
            Expr::UnaryMinus(loc, expr) => Span::new(
                loc.clone(),
                expr.span().end().clone()
            ),
            Expr::Times(expr1, expr2) => Span::new(
                expr1.span().start().clone(),
                expr2.span().end().clone()
            ),
            Expr::Divide(expr1, expr2) => Span::new(
                expr1.span().start().clone(),
                expr2.span().end().clone()
            ),
            Expr::Number(span, _) => span.clone(),
            Expr::UpperByte(loc, expr) => Span::new(
                loc.clone(),
                expr.span().end().clone()
            ),
            Expr::LowerByte(loc, expr) => Span::new(
                loc.clone(),
                expr.span().end().clone()
            )
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Name(_, name) => write!(f, "{}", name),
            Expr::Plus(left, right) => {
                let left_paren = left.complex();
                let right_paren = right.complex();
                if left_paren { write!(f, "(")?; }
                write!(f, "{}", left)?;
                if left_paren { write!(f, ")")?; }

                write!(f, " + ")?;
                
                if right_paren { write!(f, "(")?; }
                write!(f, "{}", right)?;
                if right_paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::Minus(left, right) => {
                let left_paren = left.complex();
                let right_paren = right.complex();
                if left_paren { write!(f, "(")?; }
                write!(f, "{}", left)?;
                if left_paren { write!(f, ")")?; }

                write!(f, " - ")?;
                
                if right_paren { write!(f, "(")?; }
                write!(f, "{}", right)?;
                if right_paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::UnaryMinus(_, expr) => {
                let paren = expr.complex();
                write!(f, "-")?;
                if paren { write!(f, "(")?; }
                write!(f, "{}", expr)?;
                if paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::Times(left, right) => {
                let left_paren = left.complex();
                let right_paren = right.complex();
                if left_paren { write!(f, "(")?; }
                write!(f, "{}", left)?;
                if left_paren { write!(f, ")")?; }

                write!(f, " * ")?;
                
                if right_paren { write!(f, "(")?; }
                write!(f, "{}", right)?;
                if right_paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::Divide(left, right) => {
                let left_paren = left.complex();
                let right_paren = right.complex();
                if left_paren { write!(f, "(")?; }
                write!(f, "{}", left)?;
                if left_paren { write!(f, ")")?; }

                write!(f, " / ")?;
                
                if right_paren { write!(f, "(")?; }
                write!(f, "{}", right)?;
                if right_paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::Number(_, num) => if *num >= 0 && *num <= 9 {
                write!(f, "{}", num)
            } else if *num <= 0xFF {
                write!(f, "${:02X}", num)
            } else {
                write!(f, "${:04X}", num)
            },
            Expr::UpperByte(_, expr) => {
                let paren = expr.complex();
                write!(f, ">")?;
                if paren { write!(f, "(")?; }
                write!(f, "{}", expr)?;
                if paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::LowerByte(_, expr) => {
                let paren = expr.complex();
                write!(f, "<")?;
                if paren { write!(f, "(")?; }
                write!(f, "{}", expr)?;
                if paren { write!(f, ")")?; }
                write!(f, "")
            }
        }
    }
}

impl Expr {
    pub fn complex(&self) -> bool {
        match self {
            Expr::Name(_,_) => false,
            Expr::Plus(_,_) => true,
            Expr::Minus(_,_) => true,
            Expr::UnaryMinus(_,_) => true,
            Expr::Times(_,_) => true,
            Expr::Divide(_,_) => true,
            Expr::Number(_,_) => false,
            Expr::UpperByte(_,_) => true,
            Expr::LowerByte(_,_) => true
        }
    }

    pub fn eval<E: Env<i32>>(&self, name_lookup: &E) -> Result<i32,EvaluationError> {
        match self {
            Expr::Name(_, name) =>
                match name_lookup.get(&name.to_lowercase()) {
                    Some(num) => Ok(num),
                    None => Err(EvaluationError::NameNotFound(self.span(), format!("{} '{}' not found", name_lookup.name(), name)))
                },
            Expr::Plus(left, right) => Ok(left.eval(name_lookup)? + right.eval(name_lookup)?),
            Expr::Minus(left, right) => Ok(left.eval(name_lookup)? - right.eval(name_lookup)?),
            Expr::UnaryMinus(_, expr) => Ok(-1 * expr.eval(name_lookup)?),
            Expr::Times(left, right) => Ok(left.eval(name_lookup)? * right.eval(name_lookup)?),
            Expr::Divide(left, right) => {
                let left_val = left.eval(name_lookup)?;
                let right_val = right.eval(name_lookup)?;
                if right_val == 0 {
                    Err(EvaluationError::DivideByZero(self.span(), format!("Divide by zero: {}/{}", left_val, right_val)))
                } else {
                    Ok(left_val / right_val)
                }
            },
            Expr::Number(_, num) => Ok(*num),
            Expr::UpperByte(_, expr) => {
                let value = expr.eval(name_lookup)?;
                Ok((value >> 8) & 0xFF)
            }
            Expr::LowerByte(_, expr) => {
                let value = expr.eval(name_lookup)?;
                Ok(value & 0xFF)
            }
        }
    }
}

