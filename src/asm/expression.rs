
use std::collections::HashMap;

pub enum EvaluationError {
    NameNotFound(String),
    DivideByZero(String)
}

pub enum Expression {
    Name(String),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Number(u16),
    UpperByte(Box<Expression>),
    LowerByte(Box<Expression>)
}

impl Expression {
    pub fn eval(&self, name_lookup: &HashMap<String,i32>) -> Result<i32,EvaluationError> {
        match self {
            &Expression::Name(ref name) => match name_lookup.get(name) {
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
