
use std::fmt;
use location::{Positioned, Location, Span};
use env::Env;
use std::collections::{HashMap};

#[derive(Debug)]
pub enum EvaluationError {
    NameNotFound(Span,String),
    DivideByZero(Span,String),
    MustBeLiteralNumber(Span),
    MacroLabelOutsideOfMacro(Span),
    MacroArgOutsideOfMacro(Span),
    ImmediateValueNotAllowedHere(Span),
    IndirectionModeNotAllowedHere(Span),
    InvalidMacroArg(Span)
}

impl EvaluationError {
    pub fn to_string(&self) -> String {
        use EvaluationError::*;
        match self {
            NameNotFound(span, msg) => format!("{}: {}", span, msg),
            DivideByZero(span, msg) => format!("{}: {}", span, msg),
            MustBeLiteralNumber(span) => format!("{}: Must be a literal integer", span),
            MacroLabelOutsideOfMacro(span) => format!("{}: Macro label ouside of macro", span),
            MacroArgOutsideOfMacro(span) => format!("{}: Macro arg outside of macro", span),
            ImmediateValueNotAllowedHere(span) => format!("{}: Immediate value cannot be used within an expression", span),
            IndirectionModeNotAllowedHere(span) => format!("{}: Indirection Mode cannot be used within an expression", span),
            InvalidMacroArg(span) => format!("{}: Invalid macro arg", span)
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
    LowerByte(Location, Box<Expr>),
    MacroLabel(Span, String),
    MacroArg(Span, String)
}

impl Expr {
    pub fn num(num: i32) -> Expr {
        Expr::Number(Span::default(), num)
    }

    pub fn name(name: &str) -> Expr {
        Expr::Name(Span::default(), name.to_owned())
    }

    pub fn replace_macro_args(
        &self,
        labels: &HashMap<String,String>,
        args: &HashMap<String,Arg>
    ) -> Result<Expr,EvaluationError> {
        use expression::Expr::*;
        match self {
            Name(span, name) => Ok(Expr::Name(span.clone(), name.clone())),
            Plus(left, right) => Ok(Expr::Plus(
                Box::new(left.replace_macro_args(labels, args)?),
                Box::new(right.replace_macro_args(labels, args)?)
            )),
            Minus(left, right) => Ok(Expr::Minus(
                Box::new(left.replace_macro_args(labels, args)?),
                Box::new(right.replace_macro_args(labels, args)?)
            )),
            UnaryMinus(loc, expr) => Ok(Expr::UnaryMinus(
                loc.clone(),
                Box::new(expr.replace_macro_args(labels, args)?)
            )),
            Times(left, right) => Ok(Expr::Times(
                Box::new(left.replace_macro_args(labels, args)?),
                Box::new(right.replace_macro_args(labels, args)?)
            )),
            Divide(left, right) => Ok(Expr::Divide(
                Box::new(left.replace_macro_args(labels, args)?),
                Box::new(right.replace_macro_args(labels, args)?)
            )),
            Number(span, num) => Ok(Expr::Number(span.clone(), *num)),
            UpperByte(loc, expr) => Ok(Expr::UpperByte(
                loc.clone(),
                Box::new(expr.replace_macro_args(labels, args)?)
            )),
            LowerByte(loc, expr) => Ok(Expr::LowerByte(
                loc.clone(),
                Box::new(expr.replace_macro_args(labels, args)?)
            )),
            MacroLabel(span, name) => {
                match labels.get(name) {
                    Some(label) => Ok(Expr::Name(span.clone(), label.clone())),
                    None => Err(EvaluationError::MacroLabelOutsideOfMacro(span.clone()))
                }
            },
            MacroArg(span, name) => {
                use expression::Arg::*;
                match args.get(name) {
                    Some(arg) => match arg {
                        Imm(expr) => Err(EvaluationError::ImmediateValueNotAllowedHere(span.with_parent(expr.span()))),
                        Ex(expr) => expr.replace_macro_args(labels, args),
                        IM(im_span, _) => Err(EvaluationError::IndirectionModeNotAllowedHere(span.with_parent(im_span.clone()))),
                        MacroArg(mspan, name) => Err(EvaluationError::MacroArgOutsideOfMacro(mspan.clone()))
                    },
                    None => Err(EvaluationError::InvalidMacroArg(span.clone()))
                }
            }
        }
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
            ),
            Expr::MacroLabel(span, _) => span.clone(),
            Expr::MacroArg(span, _) => span.clone()
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
            },
            Expr::MacroLabel(_, name) => write!(f, "{}", name),
            Expr::MacroArg(_, name) => write!(f, "{}", name)
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
            Expr::LowerByte(_,_) => true,
            Expr::MacroLabel(_,_) => false,
            Expr::MacroArg(_,_) => false
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
            },
            Expr::MacroLabel(span, _) =>
                Err(EvaluationError::MacroLabelOutsideOfMacro(span.clone())),
            Expr::MacroArg(span, _) =>
                Err(EvaluationError::MacroArgOutsideOfMacro(span.clone()))
        }
    }
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone,Copy)]
pub enum IndirectionMode {
    R0,
    R1,
    R2,
    R3
}

impl IndirectionMode {
    pub fn index(&self) -> u8 {
        match self {
            IndirectionMode::R0 => 0,
            IndirectionMode::R1 => 1,
            IndirectionMode::R2 => 2,
            IndirectionMode::R3 => 3
        }
    }

    pub fn from(num: u8) -> IndirectionMode {
        match num {
            0 => IndirectionMode::R0,
            1 => IndirectionMode::R1,
            2 => IndirectionMode::R2,
            3 => IndirectionMode::R3,
            _ => panic!("Invalid IndirectionMode: {}", num)
        }
    }
}

impl fmt::Display for IndirectionMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use expression::IndirectionMode::*;
        match self {
            R0 => write!(f, "@R0"),
            R1 => write!(f, "@R1"),
            R2 => write!(f, "@R2"),
            R3 => write!(f, "@R3")
        }
    }
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
pub enum Arg {
    Imm(Expr),
    Ex(Expr),
    IM(Span, IndirectionMode),
    MacroArg(Span, String)
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arg::Imm(expr) => write!(f, "#{}", expr),
            Arg::Ex(expr) => write!(f, "{}", expr),
            Arg::IM(_, im) => write!(f, "{}", im),
            Arg::MacroArg(_, name) => write!(f, "{}", name)
        }
    }
}

impl Arg {
    pub fn span(&self) -> Span {
        match self {
            Arg::Imm(expr) => expr.span(),
            Arg::Ex(expr) => expr.span(),
            Arg::IM(span, _) => span.clone(),
            Arg::MacroArg(span, _) => span.clone()
        }
    }
}
