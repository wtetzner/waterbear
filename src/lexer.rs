
use input;
use location::Span;
use std::io;
use files::SourceFiles;
use input::Input;

#[derive(Clone, Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    Hash,
    Comma,
    Colon,
    Name(String),
    Number(u32),
    Equals,
    Plus,
    Times,
    Minus,
    Divide,
    UpperByte,
    LowerByte,
    String(String),
    R0,
    R1,
    R2,
    R3,
    EOF
}

impl TokenType {
    pub fn with_span(self, span: Span) -> Token {
        Token::new(self, span)
    }
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    span: Span
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Token {
        Token { token_type: token_type, span: span }
    }

    pub fn token_type(&self) -> &TokenType { &self.token_type }
    pub fn span(&self) -> &Span { &self.span }
}

fn read_literal<'a>(input: &Input<'a>, literal: &str, create: fn(&str) -> TokenType) -> Option<(Input<'a>,Token)> {
    if input.starts_with(literal) {
        let new_input = input.update(literal.len());
        let span = Span::new(input.location(), new_input.location());
        Some((new_input, Token::new(create(literal), span)))
    } else {
        None
    }
}

fn read_one<'a>(input: &Input<'a>, create: fn(&str) -> TokenType) -> Option<(Input<'a>,Token)> {
    match input.peek() {
        Some(txt) => {
            let new_input = input.update(txt.len());
            let span = Span::new(input.location(), new_input.location());
            Some((new_input, Token::new(create(txt), span)))
        },
        None => None
    }
}

fn read_token<'a>(input: &Input<'a>) -> Option<(Input<'a>,Token)> {
    match input.peek() {
        Some("(") => read_one(input, |str| TokenType::LeftParen),
        Some(")") => read_one(input, |str| TokenType::RightParen),
        Some("#") => read_one(input, |str| TokenType::Hash),
        Some(",") => read_one(input, |str| TokenType::Comma),
        Some(":") => read_one(input, |str| TokenType::Colon),
        Some("=") => read_one(input, |str| TokenType::Equals),
        Some("+") => read_one(input, |str| TokenType::Plus),
        Some("*") => read_one(input, |str| TokenType::Times),
        Some("-") => read_one(input, |str| TokenType::Minus),
        Some("/") => read_one(input, |str| TokenType::Divide),
        Some(">") => read_one(input, |str| TokenType::UpperByte),
        Some("<") => read_one(input, |str| TokenType::LowerByte),
        _ => None
    }
    // None
}

pub fn lex(files: &mut SourceFiles, filename: &str) -> Vec<Token> {
    vec![]
}
