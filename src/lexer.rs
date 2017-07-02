
use location::{Filenames,FileID,Location,Span};
use unicode_segmentation::UnicodeSegmentation;
use std;

#[derive(Debug,Eq,PartialEq,Clone)]
pub struct Token {
    value: TokenValue,
    span: Span
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
    Identifier(String)
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
            if value == "\r\n" || value == "\n" {
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
pub enum LexerError {
    UnexpectedCharacter(String)
    // FailedToReadFile(std::io::Error),
    // UnknownSectionName(String),
    // UnexpectedEndOfInput(String),
    // UnknownOpcode(String)
}

impl LexerError {
    pub fn to_string(&self) -> String {
        match self {
            &LexerError::UnexpectedCharacter(ref msg) => msg.clone()
        }
    }
}

pub fn lex(filenames: &mut Filenames, filename: String, string: String) -> Result<Vec<Token>,LexerError> {
    let mut results: Vec<Token> = vec![];
    let mut input = Input::new(filenames.intern(filename), &string);
    loop {
        skip_whitespace(&mut input);
        let mut token = read_token(&mut input)?;
        if token.is_some() {
            results.push(token.unwrap());
        } else {
            return Ok(results)
        }
    }
    Ok(results)
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
            chr => Err(LexerError::UnexpectedCharacter(format!("Unexpected character: {}", chr)))
        }
    } else {
        Ok(None)
    }
}

#[inline]
fn is_whitespace(s: &str) -> bool {
    s == "\r" || s == "\n" || s == "\r\n" || s == "\t" || s == " "
}

fn skip_whitespace<'a>(input: &mut Input<'a>) {
    loop {
        let peeked = input.peek();
        if peeked.is_some() {
            if is_whitespace(peeked.unwrap()) {
                input.next();
            } else {
                return;
            }
        } else {
            return;
        }
    }
}
