
use input::Input;
use location::{Location,Span};
use ast::{Statement,Statements};
use lexer::{Token,TokenType,LexerError};
use lexer;
use expression::Expr;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedChar(Location),
    ExpectedTokenNotFound(String, Token),
    UnexpectedEof
}

impl From<LexerError> for ParseError {
    fn from(error: LexerError) -> Self {
        use lexer::LexerError::*;
        match error {
            UnexpectedChar(loc) => ParseError::UnexpectedChar(loc.clone())
        }
    }
}

// pub fn parse_input(input: &Input) -> Result<Statements,ParseError> {
//     let tokens = lexer::lex_input(input)?;
// }

// pub fn parse_statement(tokens: &[Token], pos: usize) -> Result<Statement,ParseError> {
    
// }

pub fn parse_expr<'a>(tokens: TokenStream<'a>) -> Result<(TokenStream<'a>,Expr),ParseError> {
    match tokens.peek() {
        Some(tok) => {
            use lexer::TokenType::*;
            match tok.token_type() {
                Number(num) => Ok((tokens.at(1), Expr::Number(tok.span().clone(), *num))),
                _ => Err(ParseError::ExpectedTokenNotFound("Number".to_owned(), tok.clone()))
            }
        },
        None => Err(ParseError::UnexpectedEof)
    }
}

pub fn parse_i8<'a>(tokens: TokenStream<'a>) -> Result<(TokenStream<'a>,Expr),ParseError> {
    tokens.consume(TokenType::Hash)?;
    parse_expr(tokens)
}

pub struct TokenStream<'a> {
    pos: usize,
    tokens: &'a [Token]
}

impl<'a> TokenStream<'a> {
    pub fn from<'b>(tokens: &'b [Token]) -> TokenStream<'b> {
        TokenStream {
            pos: 0,
            tokens: tokens
        }
    }

    pub fn at_label(&self) -> bool {
        if self.pos + 1 < self.tokens.len() {
            (self.check(Token::is_name)
             && self.check(Token::is_colon))
        } else {
            false
        }
    }

    pub fn read_name(&self) -> Result<(TokenStream<'a>,Span,String),ParseError> {
        use lexer::TokenType::*;
        let tok = self.peek();
        match tok.map(|t| t.token_type()) {
            Some(Name(name)) => {
                let token = tok.unwrap();
                Ok((self.at(1), token.span().clone(), name.clone()))
            },
            Some(_) => Err(ParseError::ExpectedTokenNotFound("Name".to_owned(), tok.unwrap().clone())),
            None => Err(ParseError::UnexpectedEof)
        }
    }

    pub fn consume(&self, token_type: TokenType) -> Result<TokenStream<'a>,ParseError> {
        self.assert(token_type)?;
        Ok(self.at(1))
    }

    pub fn at(&self, amount: usize) -> TokenStream<'a> {
        TokenStream {
            pos: self.pos + amount,
            tokens: self.tokens
        }
    }

    pub fn peek_at(&self, pos: usize) -> Option<&Token> {
        let loc = self.pos + pos;
        if loc < self.tokens.len() {
            Some(&self.tokens[loc])
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.peek_at(0)
    }

    pub fn check_at(&self, pos: usize, check: fn(&Token) -> bool) -> bool {
        self.peek_at(pos).map(check).unwrap_or(false)
    }

    pub fn check(&self, check: fn(&Token) -> bool) -> bool {
        self.check_at(0, check)
    }

    pub fn assert(&self, token_type: TokenType) -> Result<(),ParseError> {
        match self.peek() {
            Some(tok) if *tok.token_type() == token_type => Ok(()),
            Some(tok) => Err(ParseError::ExpectedTokenNotFound(token_type.name().to_owned(), tok.clone())),
            None => Err(ParseError::UnexpectedEof)
        }
    }
}
