
use std;
use std::mem;

#[derive(Debug,Clone)]
pub enum NumberConversionError {
    Failure(String)
}

impl NumberConversionError {
    pub fn message(&self) -> String {
        match self {
            &NumberConversionError::Failure(ref text) => text.to_string()
        }
    }
}

pub fn to_u16(num: i32) -> Result<u16,NumberConversionError> {
    if num >= (std::i16::MIN as i32) && num <= (std::i16::MAX as i32) {
        Ok(unsafe { mem::transmute::<i16, u16>(num as i16) })
    } else if num >= 0 && num <= (std::u16::MAX as i32) {
        Ok(num as u16)
    } else {
        Err(NumberConversionError::Failure(format!("Invalid word value: {}", num)))
    }
}

pub fn to_usize(num: i32) -> Result<usize,NumberConversionError> {
    if num >= (std::isize::MIN as i32) && num <= (std::isize::MAX as i32) {
        Ok(unsafe { mem::transmute::<isize, usize>(num as isize) })
    } else if num >= 0 && num <= (std::usize::MAX as i32) {
        Ok(num as usize)
    } else {
        Err(NumberConversionError::Failure(format!("Invalid usize value: {}", num)))
    }
}

pub fn to_u8(num: i32) -> Result<u8,NumberConversionError> {
    if num >= (std::i8::MIN as i32) && num <= (std::i8::MAX as i32) {
        Ok(unsafe { mem::transmute::<i8, u8>(num as i8) })
    } else if num >= 0 && num <= (std::u8::MAX as i32) {
        Ok(num as u8)
    } else {
        Err(NumberConversionError::Failure(format!("Invalid byte value: {}", num)))
    }
}

pub fn to_i8(num: i32) -> Result<i8,NumberConversionError> {
    if num >= (std::i8::MIN as i32) && num <= (std::i8::MAX as i32) {
        Ok(num as i8)
    } else if num >= 0 && num <= (std::u8::MAX as i32) {
        Ok(unsafe { mem::transmute::<u8, i8>(num as u8) })
    } else {
        Err(NumberConversionError::Failure(format!("Invalid byte value: {}", num)))
    }
}

