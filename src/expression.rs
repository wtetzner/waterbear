
use std;
use std::collections::HashMap;
use std::fmt;
use parser;
use num;

#[derive(Debug)]
pub enum EvaluationError {
    Failure(String),
    NameNotFound(String),
    DivideByZero(String),
    NumberError(String,std::num::ParseIntError),
    InvalidNumber(String),
    Utf8Error(String),
    FileLoadError(parser::FileLoadError)
}

impl EvaluationError {
    pub fn to_string(&self) -> String {
        match self {
            &EvaluationError::Failure(ref msg) => msg.clone(),
            &EvaluationError::NameNotFound(ref msg) => msg.clone(),
            &EvaluationError::DivideByZero(ref msg) => msg.clone(),
            &EvaluationError::NumberError(ref msg, ref err) => format!("{}: {}", msg, err),
            &EvaluationError::InvalidNumber(ref msg) => msg.clone(),
            &EvaluationError::Utf8Error(ref msg) => msg.clone(),
            &EvaluationError::FileLoadError(ref err) => err.to_string()
        }
    }
}

impl std::convert::From<parser::FileLoadError> for EvaluationError {
    fn from(err: parser::FileLoadError) -> EvaluationError {
        EvaluationError::FileLoadError(err)
    }
}

impl std::convert::From<num::NumberConversionError> for EvaluationError {
    fn from(err: num::NumberConversionError) -> EvaluationError {
        EvaluationError::InvalidNumber(err.message())
    }
}

impl std::convert::From<std::num::ParseIntError> for EvaluationError {
    fn from(err: std::num::ParseIntError) -> EvaluationError {
        EvaluationError::NumberError("Failed to parse number".to_string(), err)
    }
}

impl std::convert::From<std::string::FromUtf8Error> for EvaluationError {
    fn from(_: std::string::FromUtf8Error) -> EvaluationError {
        EvaluationError::Utf8Error("Invalid UTF-8".to_string())
    }
}

#[derive(Debug,Clone)]
pub enum Expression {
    Name(String),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Number(i32),
    UpperByte(Box<Expression>),
    LowerByte(Box<Expression>)
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expression::Name(ref name) => write!(f, "{}", name),
            &Expression::Plus(ref left, ref right) => {
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
            &Expression::Minus(ref left, ref right) => {
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
            &Expression::Times(ref left, ref right) => {
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
            &Expression::Divide(ref left, ref right) => {
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
            &Expression::Number(ref num) => write!(f, "{}", num),
            &Expression::UpperByte(ref expr) => {
                let paren = expr.complex();
                write!(f, ">")?;
                if paren { write!(f, "(")?; }
                write!(f, "{}", expr)?;
                if paren { write!(f, ")")?; }
                write!(f, "")
            },
            &Expression::LowerByte(ref expr) => {
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

impl Expression {
    pub fn complex(&self) -> bool {
        match self {
            &Expression::Name(_) => false,
            &Expression::Plus(_,_) => true,
            &Expression::Minus(_,_) => true,
            &Expression::Times(_,_) => true,
            &Expression::Divide(_,_) => true,
            &Expression::Number(_) => false,
            &Expression::UpperByte(_) => true,
            &Expression::LowerByte(_) => true
        }
    }

    pub fn eval(&self, name_lookup: &HashMap<String,i32>) -> Result<i32,EvaluationError> {
        match self {
            &Expression::Name(ref name) => match name_lookup.get(&name.to_lowercase()) {
                Some(num) => Ok(*num),
                None => Err(EvaluationError::NameNotFound(format!("Name '{}' not found", name)))
            },
            &Expression::Plus(ref left, ref right) => Ok(left.eval(name_lookup)? + right.eval(name_lookup)?),
            &Expression::Minus(ref left, ref right) => Ok(left.eval(name_lookup)? - right.eval(name_lookup)?),
            &Expression::Times(ref left, ref right) => Ok(left.eval(name_lookup)? * right.eval(name_lookup)?),
            &Expression::Divide(ref left, ref right) => {
                let left_val = left.eval(name_lookup)?;
                let right_val = right.eval(name_lookup)?;
                if right_val == 0 {
                    Err(EvaluationError::DivideByZero(format!("Divide by zero: {}/{}", left_val, right_val)))
                } else {
                    Ok(left_val / right_val)
                }
            },
            &Expression::Number(num) => Ok(num as i32),
            &Expression::UpperByte(ref expr) => {
                let value = expr.eval(name_lookup)?;
                Ok((value >> 8) & 0xFF)
            }
            &Expression::LowerByte(ref expr) => {
                let value = expr.eval(name_lookup)?;
                Ok(value & 0xFF)
            }
        }
    }
}
