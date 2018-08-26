
use std;
use input::Input;
use location::{Location,Span,Positioned};
use ast::{Statement,Statements,Directive};
use lexer::{Token,TokenType,LexerError};
use lexer;
use expression::Expr;
use std::collections::HashMap;
use instruction::{IndirectionMode,Instr};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedChar(Location),
    UnexpectedToken(Token),
    ExpectedTokenNotFound(&'static str, Token),
    InvalidExpression(Location),
    MissingBytes(Span),
    MissingWords(Span),
    WrongInstructionArgs(String,Vec<String>),
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

type PResult<T> = Result<T,ParseError>;

struct LineIterator<'a> {
    pos: usize,
    tokens: &'a [Token]
}

impl<'a> Iterator for LineIterator<'a> {
    type Item = &'a [Token];

    fn next(&mut self) -> Option<&'a [Token]> {
        if self.pos < self.tokens.len() {
            let start = self.pos;
            let start_line = self.tokens[start].span().end().line();
            let mut end = start + 1;
            while end < self.tokens.len() && start_line == self.tokens[end].span().start().line() {
                end += 1;
            }
            Some(&self.tokens[start..end])
        } else {
            None
        }
    }
}

fn lines(tokens: &[Token]) -> impl Iterator<Item = &[Token]> {
    LineIterator {
        pos: 0,
        tokens: tokens
    }
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
enum Arg {
    Imm(Expr),
    Ex(Expr),
    IM(Span, IndirectionMode)
}

impl Arg {
    fn arg_type(&self) -> ArgType {
        match self {
            Arg::Imm(_) => ArgType::Imm,
            Arg::Ex(_) => ArgType::Ex,
            Arg::IM(_,_) => ArgType::IM
        }
    }

    fn span(&self) -> Span {
        match self {
            Arg::Imm(expr) => expr.span(),
            Arg::Ex(expr) => expr.span(),
            Arg::IM(span, _) => span.clone()
        }
    }
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone,Copy)]
enum ArgType {
    Imm,
    Ex,
    IM
}

impl ArgType {
    pub fn to_str(&self) -> &str {
        match self {
            ArgType::Imm => "#i8",
            ArgType::Ex => "mem",
            ArgType::IM => "@Ri"
        }
    }
}

macro_rules! instr_pat {
    ( $name:expr, $func:expr, $arg1:expr ) => {
        ($name, v) if v.len() == 1 && v[0].arg_type() == $arg1 => $func(v[0])
    };
    ( $name:expr, $func:expr, $arg1:expr, $arg2:expr ) => {
        ($name, v) if v.len() == 2 && v[0].arg_type() == $arg1 && v[1].arg_type() == $arg2 => $func(v[0], v[1])
    };
    ( $name:expr, $func:expr, $arg1:expr, $arg2:expr, $arg3:expr ) => {
        ($name, v) if v.len() == 3 && v[0].arg_type() == $arg1 && v[1].arg_type() == $arg2 && v[2].arg_type() == $arg3 => $func(v[0], v[1], v[2])
    };
}

macro_rules! instr_print {
    ( $name:expr, $func:expr, $arg1:expr ) => {
        format!("{} {}", $name, $arg1.to_string())
    };
    ( $name:expr, $func:expr, $arg1:expr, $arg2:expr ) => {
        format!("{} {}, {}", $name, $arg1.to_string(), $arg2.to_string())
    };
    ( $name:expr, $func:expr, $arg1:expr, $arg2:expr, $arg3:expr ) => {
        format("{} {}, {}, {}", $name, $arg1.to_string(), $arg2.to_string(), $arg3.to_string())
    };
}

macro_rules! match_instr {
    ( ($n:expr, $vec:expr) ( $name:expr, ( ($func:expr, $arg:expr,*) ),* ),* ) => {
        match ($n, $vec) {
            ( instr_pat!($name, $func, $arg,*),* )
            ( ($name,_) => Err(ParseError::WrongInstructionArgs($name, vec![instr_print!($name, $func, $arg,*),*]>)) )
        }
    }
}

// trait InstrParser {
//     fn name(&self) -> &str;
//     fn parse(
//         &self,
//         tokens: &mut TokenStream,
//         overloads: Vec<>) -> PResult<Statement>;
// }

struct Parser {
    expr_parser: ExprParser//,
    // instr_parsers: HashMap<String,Box<dyn InstrParser>>
}

impl Parser {
    fn create() -> Parser {
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

        let expr_parser = ExprParser {
            prefix: prefix,
            infix: infix
        };

        // let instr_parsers = {
        //     let mut m: HashMap<String,Box<dyn InstrParser>> = HashMap::new();
        //     m.inst1arg3("add", Instr::Add_i8, Instr::Add_d9, Instr::Add_Ri);
        //     m.inst1arg3("addc", Instr::Addc_i8, Instr::Addc_d9, Instr::Addc_Ri);
        //     m.inst1arg3("sub", Instr::Sub_i8, Instr::Sub_d9, Instr::Sub_Ri);
        //     m.inst1arg3("subc", Instr::Subc_i8, Instr::Subc_d9, Instr::Subc_Ri);
        //     m.inst1arg2("inc", Instr::Inc_d9, Instr::Inc_Ri);
        //     m.inst1arg2("dec", Instr::Dec_d9, Instr::Dec_Ri);
        //     m.inst0arg("mul", Instr::Mul);
        //     m.inst0arg("div", Instr::Div);
        //     m.inst1arg3("and", Instr::And_i8, Instr::And_d9, Instr::And_Ri);
        //     m.inst1arg3("or", Instr::Or_i8, Instr::Or_d9, Instr::Or_Ri);
        //     m.inst1arg3("xor", Instr::Xor_i8, Instr::Xor_d9, Instr::Xor_Ri);
        //     m.inst0arg("rol", Instr::Rol);
        //     m.inst0arg("rolc", Instr::Rolc);
        //     m.inst0arg("ror", Instr::Ror);
        //     m.inst0arg("rorc", Instr::Rorc);
        //     m.inst1arg2("ld", Instr::Ld_d9, Instr::Ld_Ri);
        //     m.inst1arg2("st", Instr::St_d9, Instr::St_Ri);

        //     m.inst0arg("ldc", Instr::Ldc);

        //     m.inst0arg("nop", Instr::Nop);
        //     m
        // };

        Parser {
            expr_parser//,
            //instr_parsers
        }
    }

    fn parse_statement(&self, tokens: &mut TokenStream) -> Result<Statement,ParseError> {
        if tokens.check(Token::is_name) && tokens.check_at(1, Token::is_colon) {
            let tok = tokens.next()?;
            if let TokenType::Name(name) = tok.token_type() {
                let colon = tokens.next()?;
                let span = Span::from(tok.span(), colon.span());
                return Ok(Statement::Label(span, name.to_owned()));
            }
        }

        // TODO: Parse instruction

        // Try to parse a directive
        if let Some(stmt) = self.parse_directive(tokens)? {
            return Ok(stmt);
        }
        println!("after dir");
        let tok = tokens.next()?;
        println!("tok.is_name(): {}, tokens.is_empty(): {}", tok.is_name(), tokens.is_empty());

        // Parse label on its own line
        if tok.is_name() && tokens.is_empty() {
            if let TokenType::Name(name) = tok.token_type() {
                return Ok(Statement::Label(tok.span().clone(), name.to_owned()));
            }
        }

        if tok.is_name() && tokens.check_at(1, |tok| tok.has_name("equ")) {
            let _equ = tokens.next()?;
            let expr = self.parse_expr(tokens)?;
            let span = Span::from(tok.span(), &expr.span());
            return Ok(Statement::Alias(span, tok.get_name().unwrap().to_owned(), expr))
        }

        if tok.is_name() && tokens.check_at(1, Token::is_equ) {
            let _equ = tokens.next()?;
            let expr = self.parse_expr(tokens)?;
            let span = Span::from(tok.span(), &expr.span());
            return Ok(Statement::Variable(span, tok.get_name().unwrap().to_owned(), expr))
        }

        Err(ParseError::UnexpectedToken(tok))
    }

    fn parse_instr(&self, tokens: &mut TokenStream) -> Result<Option<Statement>,ParseError> {
        if let Some(Token::Name(n)) = tokens.peek() {
            // let name: String = n.clone();
            let (span, name, args) = parse_gen_instr(tokens)?;
            match_instr!((name, &args)
                ("add",
                 (Instr::Add_i8, ArgType::Imm),
                 (Instr::Add_d9, ArgType::Mem))
            )
            // match name {
            //     "add" => {
            //         let (span, name, args) = parse_gen_instr(tokens)?;
                    
            //     }
            //     _ => Ok(None)
            // }
        } else {
            Ok(None)
        }
     }

    fn parse_gen_instr(&self, tokens: &mut TokenStream) -> Result<(Span,String,Vec<Arg>),ParseError> {
        let (nspan, name) = tokens.read_name()?;
        let args = self.parse_args(tokens)?;
        if args.is_empty() {
            Ok((nspan, name, args))
        } else {
            let span = Span::from(&nspan, &args.last().unwrap().span());
            Ok((span, name, args))
        }
    }

    fn parse_args(&self, tokens: &mut TokenStream) -> Result<Vec<Arg>,ParseError> {
        self.parse_list(tokens, |toks| self.parse_arg(toks))
    }

    fn parse_exprs(&self, tokens: &mut TokenStream) -> Result<Vec<Expr>,ParseError> {
        self.parse_list(tokens, |toks| self.parse_expr(toks))
    }

    fn parse_list<T,F>(
        &self,
        tokens: &mut TokenStream,
        parser: F) -> Result<Vec<T>,ParseError>
    where F: Fn(&mut TokenStream) -> Result<T,ParseError> {
        if tokens.is_empty() {
            Ok(vec![])
        } else {
            let mut results = vec![];
            let value = parser(tokens)?;
            results.push(value);
            while !tokens.is_empty() && !tokens.check(Token::is_eof) {
                let _comma = tokens.consume(TokenType::Comma)?;
                let value = parser(tokens)?;
                results.push(value);
            }
            Ok(results)
        }
    }

    fn parse_directive(&self, tokens: &mut TokenStream) -> Result<Option<Statement>,ParseError> {
        let peeked = tokens.peek().map(|t| t.clone());
        match peeked {
            Some(tok) => {
                if tok.has_name(".byte") {
                    let ident = tokens.next()?;
                    if tokens.check(Token::is_string) {
                        let (sspan, string) = tokens.read_string()?;
                        let span = Span::from(ident.span(), &sspan);
                        Ok(Some(Statement::Directive(Directive::ByteString(span, string.bytes().collect()))))
                    } else {
                        let exprs = self.parse_exprs(tokens)?;
                        if exprs.is_empty() {
                            Err(ParseError::MissingBytes(ident.span().clone()))
                        } else {
                            let span = Span::from(ident.span(), &exprs.last().unwrap().span());
                            Ok(Some(Statement::Directive(Directive::Byte(span, exprs))))
                        }
                    }
                } else if tok.has_name(".org") {
                    let ident = tokens.next()?;
                    let (nspan, num) = tokens.read_number()?;
                    let span = Span::from(ident.span(), &nspan);
                    Ok(Some(Statement::Directive(Directive::Org(span, num as usize))))
                } else if tok.has_name(".word") {
                    let ident = tokens.next()?;
                    let exprs = self.parse_exprs(tokens)?;
                    if exprs.is_empty() {
                        Err(ParseError::MissingWords(ident.span().clone()))
                    } else {
                        let span = Span::from(ident.span(), &exprs.last().unwrap().span());
                        Ok(Some(Statement::Directive(Directive::Word(span, exprs))))
                    }
                } else if tok.has_name(".include") {
                    let ident = tokens.next()?;
                    let (str_span, string) = tokens.read_string()?;
                    let span = Span::from(ident.span(), &str_span);
                    Ok(Some(Statement::Directive(Directive::Include(span, string))))
                } else if tok.has_name(".cnop") {
                    let ident = tokens.next()?;
                    let first = self.parse_expr(tokens)?;
                    tokens.consume(TokenType::Comma)?;
                    let second = self.parse_expr(tokens)?;
                    let span = Span::from(ident.span(), &second.span());
                    Ok(Some(Statement::Directive(Directive::Cnop(span, first, second))))
                } else {
                    Ok(None)
                }
            },
            None => Ok(None)
        }
    }

    fn parse_arg(&self, tokens: &mut TokenStream) -> Result<Arg,ParseError> {
        if tokens.check(Token::is_indirection_mode) {
            let (span, im) = self.parse_im(tokens)?;
            Ok(Arg::IM(span, im))
        } else if tokens.check(Token::is_hash) {
            Ok(Arg::Imm(self.parse_i8(tokens)?))
        } else {
            Ok(Arg::Ex(self.parse_expr(tokens)?))
        }
    }

    fn parse_expr(&self, tokens: &mut TokenStream) -> Result<Expr,ParseError> {
        self.expr_parser.parse(tokens, 0)
    }

    fn parse_i8(&self, tokens: &mut TokenStream) -> Result<Expr,ParseError> {
        tokens.consume(TokenType::Hash)?;
        self.parse_expr(tokens)
    }

    fn parse_im(&self, tokens: &mut TokenStream) -> Result<(Span,IndirectionMode),ParseError> {
        let tok = tokens.next()?;
        let im = match tok.token_type() {
            TokenType::R0 => Ok(IndirectionMode::R0),
            TokenType::R1 => Ok(IndirectionMode::R1),
            TokenType::R2 => Ok(IndirectionMode::R2),
            TokenType::R3 => Ok(IndirectionMode::R3),
            _ => Err(ParseError::ExpectedTokenNotFound("Indirection Mode (@R[0-3])", tok.clone()))
        };
        let im = im?;
        Ok((tok.span().clone(), im))
    }
}

#[derive(Debug,Clone)]
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

    pub fn len(&self) -> usize {
        self.tokens.len() - self.pos
    }

    pub fn is_empty(&self) -> bool {
        self.pos >= self.tokens.len() || self.check(Token::is_eof)
    }

    pub fn at_label(&self) -> bool {
        if self.pos + 1 < self.tokens.len() {
            (self.check(Token::is_name)
             && self.check_at(1, Token::is_colon))
        } else {
            false
        }
    }

    pub fn read_string(&mut self) -> Result<(Span,String),ParseError> {
        use lexer::TokenType::*;
        let tok = self.current()?;
        match tok.token_type() {
            String(text) => {
                self.advance();
                Ok((tok.span().clone(), text.to_owned()))
            },
            _ => Err(ParseError::ExpectedTokenNotFound("String", tok.clone()))
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

    pub fn next_if(&mut self, expected: &'static str, pred: fn(&Token) -> bool) -> Result<Token,ParseError> {
        let tok = self.next()?;
        if pred(&tok) {
            Ok(tok)
        } else {
            Err(ParseError::ExpectedTokenNotFound(expected, tok))
        }
    }

    pub fn consume(&mut self, token_type: TokenType) -> Result<Token,ParseError> {
        self.assert(token_type)?;
        self.next()
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

// ----- Expression Parsing -----

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone,Copy)]
enum Precedence {
    Sum = 3,
    Product = 4,
    Prefix = 6
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

// Parselets for Pratt Parser (used for parsing expressions)

// -- Prefix Parselets --
trait PrefixParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, token: Token) -> Result<Expr,ParseError>;
    fn precedence(&self) -> i32;
}

struct ParenParselet(Precedence);

impl PrefixParselet for ParenParselet {
    fn parse<'a>(&self, parser: &ExprParser, tokens: &mut TokenStream<'a>, _token: Token) -> Result<Expr,ParseError> {
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
    use ast::Statement;

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
        let parser = parser::Parser::create();
        parser.parse_expr(&mut token_stream)
    }

    #[test]
    fn test_parser_org() {
        let line = ".org $44";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".org 68", printed);
    }

    #[test]
    fn test_parser_byte() {
        let line = ".byte $44, $65, 0x32, 0b10110";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".byte 68, 101, 50, 22", printed);
    }

    #[test]
    fn test_parser_word() {
        let line = ".word $4478, $6543, 0x3221, 0b1011100001100100";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".word 17528, 25923, 12833, 47204", printed);
    }

    #[test]
    fn test_parser_cnop() {
        let line = ".cnop $40, $22";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".cnop 64, 34", printed);
    }

    #[test]
    fn test_parser_include() {
        let line = ".include \"sfr.i\"";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".include \"sfr.i\"", printed);
    }

    #[test]
    fn test_parser_bytestring() {
        let line = ".byte \"foo bar baz\"";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".byte \"foo bar baz\"", printed);
    }

    #[test]
    fn test_parser_label() {
        let line = ".loop";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".loop:", printed);
    }

    #[test]
    fn test_parser_label2() {
        let line = ".include:";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".include:", printed);
    }

    fn parse_statement(text: &str) -> Result<Statement,ParseError> {
        let file = FileID::new(7);
        let input = Input::new(file, text);
        let tokens = lexer::lex_input(&input)?;
        println!("tokens: {:?}", tokens);
        let mut token_stream = TokenStream::from(&tokens);
        let parser = parser::Parser::create();
        parser.parse_statement(&mut token_stream)
    }
}
