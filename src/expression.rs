
use std::fmt;
use location::{Positioned, Span};
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
    Plus(Span, Box<Expr>, Box<Expr>),
    Minus(Span, Box<Expr>, Box<Expr>),
    UnaryMinus(Span, Box<Expr>),
    Times(Span, Box<Expr>, Box<Expr>),
    Divide(Span, Box<Expr>, Box<Expr>),
    Number(Span, i32),
    UpperByte(Span, Box<Expr>),
    LowerByte(Span, Box<Expr>),
    MacroLabel(Span, String),
    MacroArg(Span, String),
    BitwiseXor(Span, Box<Expr>, Box<Expr>),
    BitwiseAnd(Span, Box<Expr>, Box<Expr>),
    BitwiseOr(Span, Box<Expr>, Box<Expr>)
}

impl Expr {
    pub fn num(num: i32) -> Expr {
        Expr::Number(Span::default(), num)
    }

    pub fn name(name: &str) -> Expr {
        Expr::Name(Span::default(), name.to_owned())
    }

    pub fn with_span(&self, new_span: Span) -> Expr {
        use expression::Expr::*;
        match self {
            BitwiseXor(_, left, right) => BitwiseXor(new_span, left.clone(), right.clone()),
            BitwiseAnd(_, left, right) => BitwiseAnd(new_span, left.clone(), right.clone()),
            BitwiseOr(_, left, right) => BitwiseOr(new_span, left.clone(), right.clone()),
            Name(_, name) => Name(new_span, name.clone()),
            Plus(_, left, right) => Plus(new_span, left.clone(), right.clone()),
            Minus(_, left, right) => Minus(new_span, left.clone(), right.clone()),
            UnaryMinus(_, expr) => UnaryMinus(new_span, expr.clone()),
            Times(_, left, right) => Times(new_span, left.clone(), right.clone()),
            Divide(_, left, right) => Divide(new_span, left.clone(), right.clone()),
            Number(_, num) => Number(new_span, *num),
            UpperByte(_, expr) => UpperByte(new_span, expr.clone()),
            LowerByte(_, expr) => LowerByte(new_span, expr.clone()),
            MacroLabel(_, name) => MacroLabel(new_span, name.clone()),
            MacroArg(_, name) => MacroArg(new_span, name.clone())
        }
    }

    pub fn replace_macro_args(
        &self,
        inv_span: Span,
        labels: &HashMap<String,String>,
        args: &HashMap<String,Arg>
    ) -> Result<Expr,EvaluationError> {
        use expression::Expr::*;
        match self {
            BitwiseXor(span, left, right) => Ok(Expr::BitwiseXor(
                span.with_parent(inv_span.clone()),
                Box::new(left.replace_macro_args(inv_span.clone(), labels, args)?),
                Box::new(right.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            BitwiseAnd(span, left, right) => Ok(Expr::BitwiseAnd(
                span.with_parent(inv_span.clone()),
                Box::new(left.replace_macro_args(inv_span.clone(), labels, args)?),
                Box::new(right.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            BitwiseOr(span, left, right) => Ok(Expr::BitwiseOr(
                span.with_parent(inv_span.clone()),
                Box::new(left.replace_macro_args(inv_span.clone(), labels, args)?),
                Box::new(right.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            Name(span, name) => Ok(Expr::Name(span.with_parent(inv_span.clone()), name.clone())),
            Plus(span, left, right) => Ok(Expr::Plus(
                span.with_parent(inv_span.clone()),
                Box::new(left.replace_macro_args(inv_span.clone(), labels, args)?),
                Box::new(right.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            Minus(span, left, right) => Ok(Expr::Minus(
                span.with_parent(inv_span.clone()),
                Box::new(left.replace_macro_args(inv_span.clone(), labels, args)?),
                Box::new(right.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            UnaryMinus(span, expr) => Ok(Expr::UnaryMinus(
                span.with_parent(inv_span.clone()),
                Box::new(expr.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            Times(span, left, right) => Ok(Expr::Times(
                span.with_parent(inv_span.clone()),
                Box::new(left.replace_macro_args(inv_span.clone(), labels, args)?),
                Box::new(right.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            Divide(span, left, right) => Ok(Expr::Divide(
                span.with_parent(inv_span.clone()),
                Box::new(left.replace_macro_args(inv_span.clone(), labels, args)?),
                Box::new(right.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            Number(span, num) => Ok(Expr::Number(span.with_parent(inv_span.clone()).clone(), *num)),
            UpperByte(span, expr) => Ok(Expr::UpperByte(
                span.with_parent(inv_span.clone()),
                Box::new(expr.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            LowerByte(span, expr) => Ok(Expr::LowerByte(
                span.with_parent(inv_span.clone()),
                Box::new(expr.replace_macro_args(inv_span.clone(), labels, args)?)
            )),
            MacroLabel(span, name) => {
                match labels.get(name) {
                    Some(label) => Ok(Expr::Name(span.with_parent(inv_span.clone()).clone(), label.clone())),
                    None => Err(EvaluationError::MacroLabelOutsideOfMacro(span.clone()))
                }
            },
            MacroArg(span, name) => {
                use expression::Arg::*;
                match args.get(name) {
                    Some(arg) => match arg {
                        Imm(expr) => Err(EvaluationError::ImmediateValueNotAllowedHere(span.with_parent(expr.span()))),
                        Ex(expr) => Ok(expr.replace_macro_args(inv_span.clone(), labels, args)?.with_span(expr.span().with_parent(inv_span.clone()))),
                        IM(im_span, _) => Err(EvaluationError::IndirectionModeNotAllowedHere(span.with_parent(im_span.clone()))),
                        MacroArg(mspan, _) => Err(EvaluationError::MacroArgOutsideOfMacro(mspan.with_parent(inv_span.clone())))
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
            Expr::BitwiseXor(span, _, _) => span.clone(),
            Expr::BitwiseAnd(span, _, _) => span.clone(),
            Expr::BitwiseOr(span, _, _) => span.clone(),
            Expr::Name(span, _) => span.clone(),
            Expr::Plus(span, _, _) => span.clone(),
            Expr::Minus(span, _, _) => span.clone(),
            Expr::UnaryMinus(span, _) => span.clone(),
            Expr::Times(span, _, _) => span.clone(),
            Expr::Divide(span, _, _) => span.clone(),
            Expr::Number(span, _) => span.clone(),
            Expr::UpperByte(span, _) => span.clone(),
            Expr::LowerByte(span, _) => span.clone(),
            Expr::MacroLabel(span, _) => span.clone(),
            Expr::MacroArg(span, _) => span.clone()
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::BitwiseXor(_, left, right) => {
                let left_paren = left.complex();
                let right_paren = right.complex();
                if left_paren { write!(f, "(")?; }
                write!(f, "{}", left)?;
                if left_paren { write!(f, ")")?; }

                write!(f, " ^ ")?;
                
                if right_paren { write!(f, "(")?; }
                write!(f, "{}", right)?;
                if right_paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::BitwiseAnd(_, left, right) => {
                let left_paren = left.complex();
                let right_paren = right.complex();
                if left_paren { write!(f, "(")?; }
                write!(f, "{}", left)?;
                if left_paren { write!(f, ")")?; }

                write!(f, " & ")?;
                
                if right_paren { write!(f, "(")?; }
                write!(f, "{}", right)?;
                if right_paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::BitwiseOr(_, left, right) => {
                let left_paren = left.complex();
                let right_paren = right.complex();
                if left_paren { write!(f, "(")?; }
                write!(f, "{}", left)?;
                if left_paren { write!(f, ")")?; }

                write!(f, " | ")?;
                
                if right_paren { write!(f, "(")?; }
                write!(f, "{}", right)?;
                if right_paren { write!(f, ")")?; }
                write!(f, "")
            },
            Expr::Name(_, name) => write!(f, "{}", name),
            Expr::Plus(_, left, right) => {
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
            Expr::Minus(_, left, right) => {
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
            Expr::Times(_, left, right) => {
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
            Expr::Divide(_, left, right) => {
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
            Expr::BitwiseXor(_,_,_) => true,
            Expr::BitwiseAnd(_,_,_) => true,
            Expr::BitwiseOr(_,_,_) => true,
            Expr::Name(_,_) => false,
            Expr::Plus(_,_,_) => true,
            Expr::Minus(_,_,_) => true,
            Expr::UnaryMinus(_,_) => true,
            Expr::Times(_,_,_) => true,
            Expr::Divide(_,_,_) => true,
            Expr::Number(_,_) => false,
            Expr::UpperByte(_,_) => true,
            Expr::LowerByte(_,_) => true,
            Expr::MacroLabel(_,_) => false,
            Expr::MacroArg(_,_) => false
        }
    }

    pub fn eval<E: Env<i32>>(&self, name_lookup: &E) -> Result<i32,EvaluationError> {
        match self {
            Expr::BitwiseXor(_, left, right) => Ok(left.eval(name_lookup)? ^ right.eval(name_lookup)?),
            Expr::BitwiseAnd(_, left, right) => Ok(left.eval(name_lookup)? & right.eval(name_lookup)?),
            Expr::BitwiseOr(_, left, right) => Ok(left.eval(name_lookup)? | right.eval(name_lookup)?),
            Expr::Name(_, name) =>
                match name_lookup.get(&name.to_lowercase()) {
                    Some(num) => Ok(num),
                    None => Err(EvaluationError::NameNotFound(self.span(), format!("{} '{}' not found", name_lookup.name(), name)))
                },
            Expr::Plus(_, left, right) => Ok(left.eval(name_lookup)? + right.eval(name_lookup)?),
            Expr::Minus(_, left, right) => Ok(left.eval(name_lookup)? - right.eval(name_lookup)?),
            Expr::UnaryMinus(_, expr) => Ok(-1 * expr.eval(name_lookup)?),
            Expr::Times(_, left, right) => Ok(left.eval(name_lookup)? * right.eval(name_lookup)?),
            Expr::Divide(_, left, right) => {
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
