
use std;
use input::Input;
use location::{Location,Span};
use ast::{Statement,Statements};
use lexer::{Token,TokenType,LexerError};
use lexer;
use expression::Expr;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedChar(Location),
    ExpectedTokenNotFound(&'static str, Token),
    InvalidExpression(Location),
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


fn parse_i8<'a>(tokens: &mut TokenStream<'a>) -> Result<Expr,ParseError> {
    tokens.consume(TokenType::Hash)?;
    parse_expr(tokens)
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone,Copy)]
enum Precedence {
    Sum = 3,
    Product = 4,
    Prefix = 6
}

fn parse_expr<'a>(tokens: &mut TokenStream<'a>) -> Result<Expr,ParseError> {
    let prefix = {
        let mut m: HashMap<ExprTokenType,Box<dyn PrefixParselet>> = HashMap::new();
        m.insert(ExprTokenType::Paren, Box::new(ParenParselet(Precedence::Prefix)));
        m.insert(ExprTokenType::Name, Box::new(NameParselet(Precedence::Prefix)));
        m.insert(ExprTokenType::Number, Box::new(NumberParselet(Precedence::Prefix)));
        m.insert(ExprTokenType::Minus, Box::new(PrefixOperatorParselet(Precedence::Prefix)));
        m.insert(ExprTokenType::UpperByte, Box::new(PrefixOperatorParselet(Precedence::Prefix)));
        m.insert(ExprTokenType::LowerByte, Box::new(PrefixOperatorParselet(Precedence::Prefix)));
        m
    };

    let infix = {
        let mut m: HashMap<ExprTokenType,Box<dyn InfixParselet>> = HashMap::new();
        m.insert(ExprTokenType::Plus, Box::new(BinaryOperatorParselet(Precedence::Sum)));
        m.insert(ExprTokenType::Times, Box::new(BinaryOperatorParselet(Precedence::Product)));
        m.insert(ExprTokenType::Minus, Box::new(BinaryOperatorParselet(Precedence::Sum)));
        m.insert(ExprTokenType::Divide, Box::new(BinaryOperatorParselet(Precedence::Product)));
        m
    };

    let parser = ExprParser {
        prefix: prefix,
        infix: infix
    };
    parser.parse(tokens, 0)
}

struct ExprParser {
    prefix: HashMap<ExprTokenType,Box<dyn PrefixParselet>>,
    infix: HashMap<ExprTokenType,Box<dyn InfixParselet>>
}

fn expr_tok_type(loc: Location, token: &Token) -> Result<ExprTokenType,ParseError> {
    let tok_type = token.token_type().expr_type();
    match tok_type {
        Some(typ) => {
            Ok(typ)
        },
        None => Err(ParseError::InvalidExpression(loc))
    }
}

impl ExprParser {
    pub fn parse(&self, tokens: &mut TokenStream, precedence: i32) -> Result<Expr,ParseError> {
        let tok = tokens.next()?;
        let loc = tok.span().start().clone();
        let typ = expr_tok_type(loc.clone(), &tok)?;

        let prefix = self.prefix(loc.clone(), typ)?;
        let mut left = prefix.parse(self, tokens, tok)?;
        while precedence < self.precedence(loc.clone(), tokens) {
            let next_tok = tokens.next()?;
            let next_typ = expr_tok_type(loc.clone(), &next_tok)?;
            let infix = self.infix(loc.clone(), next_typ)?;
            left = infix.parse(self, tokens, left, next_tok)?;
        }
        Ok(left)
    }

    pub fn precedence(&self, loc: Location, tokens: &TokenStream) -> i32 {
        match tokens.peek() {
            Some(tok) => {
                let tok_type = tok.token_type().expr_type();
                match tok_type {
                    Some(typ) => match self.infix(loc, typ) {
                        Ok(parselet) => parselet.precedence(),
                        Err(_) => 0
                    },
                    None => 0
                }
            },
            None => 0
        }
    }

    pub fn infix(&self, location: Location, token_type: ExprTokenType) -> Result<&Box<dyn InfixParselet>,ParseError> {
        match self.infix.get(&token_type) {
            Some(parselet) => Ok(parselet),
            None => Err(ParseError::InvalidExpression(location))
        }
    }

    pub fn prefix(&self, location: Location, token_type: ExprTokenType) -> Result<&Box<dyn PrefixParselet>,ParseError> {
        match self.prefix.get(&token_type) {
            Some(parselet) => Ok(parselet),
            None => Err(ParseError::InvalidExpression(location))
        }
    }
}

struct TokenStream<'a> {
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
             && self.check_at(1, Token::is_colon))
        } else {
            false
        }
    }

    pub fn read_number(&mut self) -> Result<(Span,i32),ParseError> {
        use lexer::TokenType::*;
        let tok = self.current()?;
        match tok.token_type() {
            Number(num) => {
                self.advance();
                Ok((tok.span().clone(), *num))
            },
            _ => Err(ParseError::ExpectedTokenNotFound("Number", tok.clone()))
        }
    }

    pub fn read_name(&mut self) -> Result<(Span,String),ParseError> {
        use lexer::TokenType::*;
        let tok = self.current()?;
        match tok.token_type() {
            Name(name) => {
                self.advance();
                Ok((tok.span().clone(), name.clone()))
            },
            _ => Err(ParseError::ExpectedTokenNotFound("Name", tok.clone()))
        }
    }

    pub fn advance_by(&mut self, amount: usize) {
        self.pos += amount;
    }

    pub fn advance(&mut self) {
        self.advance_by(1);
    }

    pub fn current(&self) -> Result<Token,ParseError> {
        match self.peek() {
            Some(tok) => Ok(tok.clone()),
            None => Err(ParseError::UnexpectedEof)
        }
    }

    pub fn next(&mut self) -> Result<Token,ParseError> {
        let tok = self.current()?;
        self.advance();
        Ok(tok)

    }

    pub fn consume(&mut self, token_type: TokenType) -> Result<(),ParseError> {
        self.assert(token_type)?;
        self.pos += 1;
        Ok(())
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
            Some(tok) => Err(ParseError::ExpectedTokenNotFound(token_type.name(), tok.clone())),
            None => Err(ParseError::UnexpectedEof)
        }
    }
}

// Parselets for Pratt Parser (used for parsing expressions)

// -- Prefix Parselets --
trait PrefixParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, token: Token) -> Result<Expr,ParseError>;
    fn precedence(&self) -> i32;
}

struct ParenParselet(Precedence);

impl PrefixParselet for ParenParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, token: Token) -> Result<Expr,ParseError> {
        let result = parser.parse(tokens, 0)?;
        tokens.consume(TokenType::RightParen)?;
        Ok(result)
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

struct NameParselet(Precedence);

impl PrefixParselet for NameParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, token: Token)
                 -> Result<Expr,ParseError> {
        match token.token_type() {
            TokenType::Name(name) =>
                Ok(Expr::Name(token.span().clone(), name.to_owned())),
            _ => Err(ParseError::ExpectedTokenNotFound("Name", token.clone()))
        }
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

struct NumberParselet(Precedence);

impl PrefixParselet for NumberParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, token: Token)
                 -> Result<Expr,ParseError> {
        match token.token_type() {
            TokenType::Number(num) =>
                Ok(Expr::Number(token.span().clone(), *num)),
            _ => Err(ParseError::ExpectedTokenNotFound("Number", token.clone()))
        }
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

struct PrefixOperatorParselet(Precedence);

impl PrefixParselet for PrefixOperatorParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, token: Token) -> Result<Expr,ParseError> {
        let result = parser.parse(tokens, self.precedence())?;
        use lexer::TokenType::*;
        match token.token_type() {
            Minus => Ok(Expr::UnaryMinus(token.span().start().clone(), Box::new(result))),
            UpperByte => Ok(Expr::UpperByte(token.span().start().clone(), Box::new(result))),
            LowerByte => Ok(Expr::LowerByte(token.span().start().clone(), Box::new(result))),
            _ => Err(ParseError::ExpectedTokenNotFound("PrefixOperator", token.clone()))
        }
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

// -- Infix Parselets --

trait InfixParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, left: Expr, token: Token) -> Result<Expr,ParseError>;
    fn precedence(&self) -> i32;
}

struct BinaryOperatorParselet(Precedence);

impl InfixParselet for BinaryOperatorParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, left: Expr, token: Token) -> Result<Expr,ParseError> {
        let right = parser.parse(tokens, self.precedence())?;
        use lexer::TokenType::*;
        match token.token_type() {
            Plus => Ok(Expr::Plus(Box::new(left), Box::new(right))),
            Times => Ok(Expr::Times(Box::new(left), Box::new(right))),
            Minus => Ok(Expr::Minus(Box::new(left), Box::new(right))),
            Divide => Ok(Expr::Divide(Box::new(left), Box::new(right))),
            _ => Err(ParseError::ExpectedTokenNotFound("Binary Operator", token.clone()))
        }
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

// Expression type stuff

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
enum ExprTokenType {
    Paren,
    Name,
    Number,
    Plus,
    Times,
    Minus,
    Divide,
    UpperByte,
    LowerByte
}

impl lexer::TokenType {
    fn expr_type(&self) -> Option<ExprTokenType> {
        use lexer::TokenType::*;
        match self {
            LeftParen => Some(ExprTokenType::Paren),
            Name(_) => Some(ExprTokenType::Name),
            Number(_) => Some(ExprTokenType::Number),
            Plus => Some(ExprTokenType::Plus),
            Times => Some(ExprTokenType::Times),
            Minus => Some(ExprTokenType::Minus),
            Divide => Some(ExprTokenType::Divide),
            UpperByte => Some(ExprTokenType::UpperByte),
            LowerByte => Some(ExprTokenType::LowerByte),
            _ => None
        }
    }
}

#[cfg(test)]
mod test {
    use parser;
    use parser::{ParseError,TokenStream};
    use expression::Expr;
    use lexer;
    use input::Input;
    use files::FileID;
    use env::Env;
    use std::collections::HashMap;

    #[test]
    fn test_expression_parser() {
        check_expression_parser(
            "fred + 2 * 7 - 21 * (6 + 7)",
            "(fred + (2 * 7)) - (21 * (6 + 7))"
        );
        check_expression_parser(
            "bob + sam + -12 * 7",
            "(bob + sam) + ((-12) * 7)"
        );
        check_expression_parser(
            "bob + sam + -12 * 7 + ->fred - <sam",
            "(((bob + sam) + ((-12) * 7)) + (-(>fred))) - (<sam)"
        );
    }

    #[test]
    fn test_expression_parser_eval() {
        let env = HashMap::new();
        let val = parse_expr("3 + 7 * -4")
            .expect("failed to parse expression")
            .eval(&env)
            .expect("failed to eval expression");
        assert_eq!(val, -25);
    }

    fn check_expression_parser(text: &str, expected: &str) {
        let expr = parse_expr(text).expect("failed to parse expression");
        let printed = format!("{}", expr);
        assert_eq!(expected, printed);
        let expr2 = parse_expr(&printed).expect("failed to parse printed expression");
        let printed2 = format!("{}", expr2);
        assert_eq!(printed, printed2);
    }

    fn parse_expr(text: &str) -> Result<Expr,ParseError> {
        let file = FileID::new(7);
        let input = Input::new(file, text);
        let tokens = lexer::lex_input(&input)?;
        println!("tokens: {:?}", tokens);
        let mut token_stream = TokenStream::from(&tokens);
        parser::parse_expr(&mut token_stream)
    }
}
