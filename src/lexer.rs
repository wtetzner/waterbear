
use location::{Filenames,FileID,Location,Span};
use unicode_segmentation::UnicodeSegmentation;
use std;

use regex::Regex;

#[derive(Debug,Eq,PartialEq,Clone)]
pub struct Token {
    value: TokenValue,
    span: Span
}

#[derive(Debug,Eq,PartialEq,Clone,Copy)]
pub enum NumberBase {
    Hexadecimal,
    Octal,
    Binary,
    Decimal
}

#[derive(Debug,Eq,PartialEq,Clone)]
pub enum TokenValue {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Comma,
    Semicolon,
    Colon,
    RightThinArrow,
    RightFatArrow,
    Equals,
    NotEqual,
    Greater,
    LessThan,
    GreaterEqual,
    LessThanEqual,
    Plus,
    Minus,
    ForwardSlash,
    Asterisk,
    Bang,
    Identifier(String),
    Number { base: NumberBase, literal: String }
}

impl Token {
    pub fn new(start: Location, end: Location, value: TokenValue) -> Token {
        Token {
            value: value,
            span: Span::from_locations(start, end)
        }
    }

    #[inline]
    pub fn matches_ident(&self, name: &str) -> bool {
        match &self.value {
            &TokenValue::Identifier(ref id_name) if id_name == name => true,
            _ => false
        }
    }

    pub fn is_module(&self) -> bool { self.matches_ident("module") }
    pub fn is_namespace(&self) -> bool { self.matches_ident("namespace") }
    pub fn is_signature(&self) -> bool { self.matches_ident("signature") }

    pub fn to_string(&self, filenames: &Filenames) -> String {
        format!("{} {:?}",
                self.span.to_string(filenames),
                self.value)
    }
}

struct Input<'a> {
    file: FileID,
    chars: Vec<&'a str>,
    pos: usize,
    line: usize,
    column: usize
}

impl<'a> Input<'a> {
    pub fn new<'b>(file: FileID, string: &'b String) -> Input<'b> {
        let mut chars: Vec<&'b str> = Vec::with_capacity(string.len() + (string.len() / 3));
        for grapheme in string.graphemes(true) {
            chars.push(grapheme);
        }
        Input {
            file: file,
            chars: chars,
            pos: 0,
            line: 1,
            column: 0
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn line(&self)-> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn next(&mut self) -> Option<&'a str> {
        if self.pos < self.chars.len() {
            let value = self.chars[self.pos];
            self.pos = self.pos + 1;
            if is_newline(value) {
                self.line = self.line + 1;
                self.column = 0;
            } else {
                self.column = self.column + 1;
            }
            Some(value)
        } else {
            None
        }
    }

    pub fn peek_at(&self, offset: usize) -> Option<&'a str> {
        let index = self.pos + offset;
        if index < self.chars.len() {
            Some(self.chars[index])
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&'a str> {
        self.peek_at(0)
    }

    pub fn location(&self) -> Location {
        Location::new(self.file, self.line, self.column)
    }
}

#[derive(Debug)]
pub struct LexerError(Location,String);

impl LexerError {
    pub fn to_string(&self, filenames: &Filenames) -> String {
        let &LexerError(loc, ref msg) = self;
        format!("{} {}", loc.to_string(filenames), msg)
    }

    pub fn location(&self) -> Location {
        let &LexerError(loc, _) = self;
        loc
    }

    pub fn message(&self) -> &str {
        let &LexerError(_, ref msg) = self;
        msg
    }
}

pub fn lex(filenames: &mut Filenames, filename: String, string: String) -> Result<Vec<Token>,LexerError> {
    let mut results: Vec<Token> = vec![];
    let mut input = Input::new(filenames.intern(filename), &string);
    loop {
        skip_whitespace(&mut input);
        let token = read_token(&mut input)?;
        if token.is_some() {
            results.push(token.unwrap());
        } else {
            return Ok(results)
        }
    }
}

macro_rules! token {
    ($expression:expr) => (
        Ok(Some(Token::new(start, input.location(), $expression)))
    )
}

fn read_token<'a>(input: &mut Input<'a>) -> Result<Option<Token>,LexerError> {
    let start = input.location();
    let token = |start, input: &mut Input, value| { Ok(Some(Token::new(start, input.location(), value))) };

    let next = input.next();
    if next.is_some() {
        match next.unwrap() {
            "{" => token(start, input, TokenValue::OpenBrace),
            "}" => token(start, input, TokenValue::CloseBrace),
            "(" => token(start, input, TokenValue::OpenParen),
            ")" => token(start, input, TokenValue::CloseParen),
            "[" => token(start, input, TokenValue::OpenBracket),
            "]" => token(start, input, TokenValue::CloseBracket),
            "," => token(start, input, TokenValue::Comma),
            ";" => token(start, input, TokenValue::Semicolon),
            ":" => token(start, input, TokenValue::Colon),
            "-" => {
                let peeked = input.peek();
                if peeked == Some(">") {
                    input.next();
                    token(start, input, TokenValue::RightThinArrow)
                } else {
                    token(start, input, TokenValue::Minus)
                }
            },
            "=" => {
                let peeked = input.peek();
                if peeked == Some(">") {
                    input.next();
                    token(start, input, TokenValue::RightFatArrow)
                } else {
                    token(start, input, TokenValue::Equals)
                }
            },
            "!" => {
                let peeked = input.peek();
                if peeked == Some("=") {
                    input.next();
                    token(start, input, TokenValue::NotEqual)
                } else {
                    token(start, input, TokenValue::Bang)
                }
            },
            ">" => {
                let peeked = input.peek();
                if peeked == Some("=") {
                    input.next();
                    token(start, input, TokenValue::GreaterEqual)
                } else {
                    token(start, input, TokenValue::Greater)
                }
            },
            "<" => {
                let peeked = input.peek();
                if peeked == Some("=") {
                    input.next();
                    token(start, input, TokenValue::LessThanEqual)
                } else {
                    token(start, input, TokenValue::LessThan)
                }
            },
            "+" => token(start, input, TokenValue::Plus),
            "/" => token(start, input, TokenValue::ForwardSlash),
            "*" => token(start, input, TokenValue::Asterisk),
            "0" => {
                let second = input.peek();
                match second {
                    Some("x") => read_hex(input, start, "0"),
                    Some("b") => read_binary(input, start, "0"),
                    Some("o") => read_octal(input, start, "0"),
                    _ => read_decimal(input, start, "0")
                }
            },
            chr if is_digit(chr) => read_decimal(input, start, chr),
            chr if is_ident_start(chr) => read_identifier(input, start, chr),
            chr => Err(LexerError(start, format!("Unexpected character: {}", chr)))
        }
    } else {
        Ok(None)
    }
}

fn read_binary_digit<'a>(input: &mut Input<'a>) -> Option<&'a str> {
    let next = input.peek();
    if next.is_some() {
        if is_binary_digit(next.unwrap()) {
            input.next();
            Some(next.unwrap())
        } else {
            None
        }
    } else {
        None
    }
}

fn read_binary<'a>(input: &mut Input<'a>, start: Location, first: &str) -> Result<Option<Token>,LexerError> {
    let mut literal = String::new();
    literal.push_str(first);
    literal.push_str(input.next().unwrap());
    loop {
        let peeked = read_binary_digit(input);
        if peeked.is_some() {
            literal.push_str(peeked.unwrap());
        } else {
            let value = TokenValue::Number {
                base: NumberBase::Binary,
                literal: literal
            };
            return Ok(Some(Token::new(start, input.location(), value)))
        }
    }
}

fn read_octal_digit<'a>(input: &mut Input<'a>) -> Option<&'a str> {
    let next = input.peek();
    if next.is_some() {
        if is_octal_digit(next.unwrap()) {
            input.next();
            Some(next.unwrap())
        } else {
            None
        }
    } else {
        None
    }
}

fn read_octal<'a>(input: &mut Input<'a>, start: Location, first: &str) -> Result<Option<Token>,LexerError> {
    let mut literal = String::new();
    literal.push_str(first);
    literal.push_str(input.next().unwrap());
    loop {
        let peeked = read_octal_digit(input);
        if peeked.is_some() {
            literal.push_str(peeked.unwrap());
        } else {
            let value = TokenValue::Number {
                base: NumberBase::Octal,
                literal: literal
            };
            return Ok(Some(Token::new(start, input.location(), value)))
        }
    }
}

fn read_hex_digit<'a>(input: &mut Input<'a>) -> Option<&'a str> {
    let next = input.peek();
    if next.is_some() {
        if is_hex_digit(next.unwrap()) {
            input.next();
            Some(next.unwrap())
        } else {
            None
        }
    } else {
        None
    }
}

fn read_hex<'a>(input: &mut Input<'a>, start: Location, first: &str) -> Result<Option<Token>,LexerError> {
    let mut literal = String::new();
    literal.push_str(first);
    literal.push_str(input.next().unwrap());
    loop {
        let peeked = read_hex_digit(input);
        if peeked.is_some() {
            literal.push_str(peeked.unwrap());
        } else {
            let value = TokenValue::Number {
                base: NumberBase::Hexadecimal,
                literal: literal
            };
            return Ok(Some(Token::new(start, input.location(), value)))
        }
    }
}

fn read_digit<'a>(input: &mut Input<'a>) -> Option<&'a str> {
    let next = input.peek();
    if next.is_some() {
        if is_digit(next.unwrap()) {
            input.next();
            Some(next.unwrap())
        } else {
            None
        }
    } else {
        None
    }
}

fn read_decimal<'a>(input: &mut Input<'a>, start: Location, first: &str) -> Result<Option<Token>,LexerError> {
    let mut literal = String::new();
    literal.push_str(first);
    loop {
        let peeked = read_digit(input);
        if peeked.is_some() {
            literal.push_str(peeked.unwrap());
        } else {
            let value = TokenValue::Number {
                base: NumberBase::Decimal,
                literal: literal
            };
            return Ok(Some(Token::new(start, input.location(), value)))
        }
    }
}

fn is_binary_digit(string: &str) -> bool {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"^[01]$").unwrap();
    }
    REGEX.is_match(string)
}

fn is_octal_digit(string: &str) -> bool {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"^[0-7]$").unwrap();
    }
    REGEX.is_match(string)
}

fn is_hex_digit(string: &str) -> bool {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"^[0-9A-Fa-f]$").unwrap();
    }
    REGEX.is_match(string)
}

fn is_digit(string: &str) -> bool {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"^[0-9]$").unwrap();
    }
    REGEX.is_match(string)
}

fn is_ident_start(string: &str) -> bool {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"^[a-zA-Z]$").unwrap();
    }
    REGEX.is_match(string)
}

fn is_ident_chr(string: &str) -> bool {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"^[a-zA-Z0-9_?!.-]$").unwrap();
    }
    REGEX.is_match(string)
}

fn read_ident_chr<'a>(input: &mut Input<'a>) -> Option<&'a str> {
    let next = input.peek();
    if next.is_some() {
        if is_ident_chr(next.unwrap()) {
            input.next();
            Some(next.unwrap())
        } else {
            None
        }
    } else {
        None
    }
}

fn read_identifier<'a>(input: &mut Input<'a>, start: Location, first: &str) -> Result<Option<Token>,LexerError> {
    let mut identifier = String::new();
    identifier.push_str(first);

    loop {
        let chr = read_ident_chr(input);
        if chr.is_some() {
            identifier.push_str(chr.unwrap());
        } else {
            return Ok(Some(Token::new(start, input.location(), TokenValue::Identifier(identifier))));
        }
    }
}

#[inline]
fn is_newline(s: &str) -> bool {
    s == "\r\n" || s == "\n"
}

#[inline]
fn is_whitespace(s: &str) -> bool {
    is_newline(s) || s == "\r" || s == "\t" || s == " "
}

fn skip_whitespace<'a>(input: &mut Input<'a>) -> bool {
    let start = input.location();
    while skip_whitespace_chunk(input) {}
    start != input.location()
}

fn skip_whitespace_chunk<'a>(input: &mut Input<'a>) -> bool {
    let ws = skip_whitespace_chars(input);
    let comment = skip_comment(input);
    return ws || comment;
}

fn skip_whitespace_chars<'a>(input: &mut Input<'a>) -> bool {
    let start = input.location();
    loop {
        let peeked = input.peek();
        if peeked.is_some() {
            if is_whitespace(peeked.unwrap()) {
                input.next();
            } else {
                return start != input.location();
            }
        } else {
            return start != input.location();
        }
    }
}

fn skip_comment<'a>(input: &mut Input<'a>) -> bool {
    if input.peek() == Some("/") && input.peek_at(1) == Some("/") {
        input.next();
        input.next();
        loop {
            let peeked = input.peek();
            if !peeked.is_some() {
                return true;
            } else if is_newline(peeked.unwrap()) {
                input.next();
                return true;
            } else {
                input.next();
            }
        }
    } else {
        return false;
    }
}
