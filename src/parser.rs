
use location::{Location,Span,Positioned};
use ast::{
    Statement,
    Statements,
    Directive,
    ByteValue,
    IncludeType,
    ArgType,
    MacroDefinition,
    MacroStatement
};
use lexer::{Token,TokenType,LexerError};
use lexer;
use expression::{Expr,Arg,IndirectionMode};
use std::collections::{HashMap,HashSet};
use instruction::Instr;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedChar(Location),
    UnexpectedToken(Token),
    InvalidInstruction(Token),
    ExpectedTokenNotFound(&'static str, Token),
    InvalidExpression(Location),
    MissingBytes(Span),
    MissingWords(Span),
    UnknownDirective(Token),
    UnknownInstruction(Span),
    WrongInstructionArgs(Span,String,Vec<Vec<ArgType>>),
    MacroNameConflictsWithInstruction(Span, String),
    MacroAlreadyExists(Span, Span, String),
    DuplicateMacroArg(Span),
    InvalidMacroArg(Span),
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
            let start_file = self.tokens[start].span().end().file();
            let mut end = start + 1;
            while end < self.tokens.len() && start_line == self.tokens[end].span().start().line() && start_file == self.tokens[end].span().start().file() {
                end += 1;
            }
            self.pos = end;
            Some(&self.tokens[start..end])
        } else {
            None
        }
    }
}

type Macros = HashMap<String,MacroDefinition>;

pub fn lines(tokens: &[Token]) -> impl Iterator<Item = &[Token]> {
    LineIterator {
        pos: 0,
        tokens: tokens
    }
}

pub struct Parser {
    expr_parser: ExprParser
}

fn instr(span: Span, instr: Instr<Expr,IndirectionMode>) -> Result<Option<Statement>,ParseError> {
    Ok(Some(Statement::Instr(span, instr)))
}

fn wrong_args(span: Span, name: &str, types: Vec<Vec<ArgType>>) -> Result<Option<Statement>,ParseError> {
    Err(ParseError::WrongInstructionArgs(span, name.to_owned(), types))
}

impl Parser {
    pub fn create() -> Parser {
        let prefix = {
            let mut m: HashMap<ExprTokenType,Box<dyn PrefixParselet>> = HashMap::new();
            m.insert(ExprTokenType::Paren, Box::new(ParenParselet(Precedence::Prefix)));
            m.insert(ExprTokenType::Name, Box::new(NameParselet(Precedence::Prefix)));
            m.insert(ExprTokenType::MacroLabel, Box::new(MacroLabelParselet(Precedence::Prefix)));
            m.insert(ExprTokenType::MacroArg, Box::new(MacroArgParselet(Precedence::Prefix)));
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
            m.insert(ExprTokenType::BitwiseXor, Box::new(BinaryOperatorParselet(Precedence::Bitwise)));
            m.insert(ExprTokenType::BitwiseAnd, Box::new(BinaryOperatorParselet(Precedence::Bitwise)));
            m.insert(ExprTokenType::BitwiseOr, Box::new(BinaryOperatorParselet(Precedence::Bitwise)));
            m
        };

        let expr_parser = ExprParser {
            prefix: prefix,
            infix: infix
        };

        Parser {
            expr_parser
        }
    }

    fn parse_macro_statement(&self, tokens: &mut TokenStream) -> Result<Vec<MacroStatement>,ParseError> {
        let mut stmts = vec![];
        if tokens.check(Token::is_name) && tokens.check_at(1, Token::is_colon) {
            let tok = tokens.next()?;
            if let TokenType::Name(name) = tok.token_type() {
                let colon = tokens.next()?;
                let span = Span::from(tok.span(), colon.span());
                stmts.push(MacroStatement::Label(span, name.to_owned()));
            }
        } else if tokens.check(Token::is_macro_label) && tokens.check_at(1, Token::is_colon) {
            let tok = tokens.next()?;
            if let TokenType::MacroLabel(name) = tok.token_type() {
                let colon = tokens.next()?;
                let span = Span::from(tok.span(), colon.span());
                stmts.push(MacroStatement::MacroLabel(span, name.to_owned()));
            }
        } else if tokens.check(Token::is_local_label_name) && tokens.len() == 1 {
            let tok = tokens.next()?;
            if let TokenType::Name(name) = tok.token_type() {
                let colon = tokens.next()?;
                let span = Span::from(tok.span(), colon.span());
                stmts.push(MacroStatement::Label(span, name.to_owned()));
            }
        } else if tokens.check(Token::is_local_macro_label) && tokens.len() == 1 {
            let tok = tokens.next()?;
            if let TokenType::MacroLabel(name) = tok.token_type() {
                stmts.push(MacroStatement::MacroLabel(tok.span().clone(), name.to_owned()));
            }
        }

        if tokens.check(Token::is_name) {
            let tok = tokens.peek().unwrap().clone();
            if let TokenType::Name(name) = tok.token_type() {
                if !name.starts_with(".")
                {
                    let (span, name, args) = self.parse_gen_instr(tokens)?;
                    stmts.push(MacroStatement::Instr(span, name, args));
                } else {
                    return Err(ParseError::UnknownInstruction(tok.span().clone()));
                }
            }
        }

        if !tokens.is_empty() {
            Err(ParseError::UnexpectedToken(tokens.next()?))
        } else {
            Ok(stmts)
        }
    }

    fn parse_macro_header(&self, macros: &Macros, tokens: &mut TokenStream) -> Result<(Span,String,Vec<(Span,String)>),ParseError> {
        tokens.read_macro_name()?;
        let (nspan, name) = tokens.read_name()?;
        if Instr::<Expr,IndirectionMode>::exists(&name) {
            return Err(ParseError::MacroNameConflictsWithInstruction(nspan.clone(), name.clone()));
        }
        if let Some(macro_def) = macros.get(&name) {
            return Err(ParseError::MacroAlreadyExists(nspan.clone(), macro_def.span().clone(), name.clone()));
        }

        let args = self.parse_args(tokens)?;
        let mut seen: HashSet<String> = HashSet::new();
        let mut arg_names = vec![];
        for arg in args.iter() {
            use parser::Arg::*;
            match arg {
                MacroArg(span, name) => {
                    if seen.contains(name) {
                        return Err(ParseError::DuplicateMacroArg(span.clone()));
                    } else {
                        seen.insert(name.clone());
                        arg_names.push((arg.span().clone(), name.clone()));
                    }
                },
                other => return Err(ParseError::InvalidMacroArg(other.span()))
            }
        }

        if !tokens.is_empty() {
            return Err(ParseError::UnexpectedToken(tokens.next()?));
        }

        Ok((nspan.clone(), name.clone(), arg_names))
    }

    fn parse_macros<'a>(&self, tokens: &'a [Token]) -> Result<(Vec<&'a [Token]>,Macros),ParseError> {
        let mut macros: Macros = HashMap::new();
        let lines: Vec<&[Token]> = {
            let mut results = vec![];
            for line in lines(tokens) {
                results.push(line);
            }
            results
        };
        let mut macro_lines: Vec<bool> = vec![false; lines.len()];

        let mut idx = 0;
        while idx < lines.len() {
            let line = lines[idx];
            let mut tokens = TokenStream::from(line);

            if tokens.check(|tok| tok.has_macro_name("%macro")) {
                let (nspan, name, args) = self.parse_macro_header(&macros, &mut tokens)?;
                macro_lines[idx] = true;
                idx = idx + 1;
                let mut statements = vec![];
                while idx < lines.len() {
                    let line = lines[idx];
                    let mut tokens = TokenStream::from(line);
                    if tokens.check(|tok| tok.has_macro_name("%end")) {
                        tokens.next()?;
                        macro_lines[idx] = true;
                        if !tokens.is_empty() {
                            return Err(ParseError::UnexpectedToken(tokens.next()?));
                        } else {
                            idx = idx + 1;
                            break;
                        }
                    } else {
                        let mut stmts = self.parse_macro_statement(&mut tokens)?;
                        statements.append(&mut stmts);
                        macro_lines[idx] = true;
                    }
                    idx = idx + 1;
                }
                macros.insert(name.clone(), MacroDefinition::new(
                    nspan.clone(),
                    name.clone(),
                    args,
                    statements
                ));
            } else {
                idx = idx + 1;
            }
        }

        let mut new_lines = vec![];
        for idx in 0..lines.len() {
            if !macro_lines[idx] {
                new_lines.push(lines[idx].clone());
            }
        }

        Ok((new_lines, macros))
    }

    pub fn parse(&self, tokens: &[Token]) -> Result<Statements,ParseError> {
        let (lines, macros) = self.parse_macros(tokens)?;
        let mut statements = vec![];
        for line in lines.iter() {
            let mut token_stream = TokenStream::from(line);
            self.parse_line(&mut token_stream, &mut statements)?;
        }
        Ok(Statements::new(macros, statements))
    }

    fn parse_line(&self, tokens: &mut TokenStream, results: &mut Vec<Statement>) -> Result<(),ParseError> {
        if !tokens.is_empty() {
            let stmt = self.parse_statement(tokens)?;
            let is_label = stmt.is_label();
            results.push(stmt);
            if is_label && !tokens.is_empty() {
                match self.parse_instr(tokens)? {
                    Some(instr) => {
                        results.push(instr);
                    },
                    None => {
                        return Err(ParseError::InvalidInstruction(tokens.next()?));
                    }
                }
            }
            if !tokens.is_empty() {
                return Err(ParseError::UnexpectedToken(tokens.next()?));
            }
        }
        Ok(())
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

        if tokens.check(|tok| tok.is_name())
            && !(tokens.check_at(1, |tok| tok.has_name("equ"))
                 || tokens.check_at(1, |tok| tok.is_equ())
                 || tokens.check(|tok| tok.name_starts_with(".")))
        {
            // Parse instruction
            if let Some(instr) = self.parse_instr(tokens)? {
                return Ok(instr);
            }
        }

        // Try to parse a directive
        if let Some(stmt) = self.parse_directive(tokens)? {
            return Ok(stmt);
        }

        let tok = tokens.next()?;

        // Parse label on its own line
        if tok.is_name() && tokens.is_empty() && tok.name_matching(|n| n.starts_with(".")) {
            if let TokenType::Name(name) = tok.token_type() {
                return Ok(Statement::Label(tok.span().clone(), name.to_owned()));
            }
        }

        if tok.is_name() && tokens.check(|tok| tok.has_name("equ")) {
            let _equ = tokens.next()?;
            let expr = self.parse_expr(tokens)?;
            let span = Span::from(tok.span(), &expr.span());
            return Ok(Statement::Alias(span, tok.get_name().unwrap().to_owned(), expr))
        }

        if tok.is_name() && tokens.check(Token::is_equ) {
            let _equ = tokens.next()?;
            let expr = self.parse_expr(tokens)?;
            let span = Span::from(tok.span(), &expr.span());
            return Ok(Statement::Variable(span, tok.get_name().unwrap().to_owned(), expr))
        }

        if tok.is_name() && tok.name_matching(|n| n.starts_with(".")) {
            return Err(ParseError::UnknownDirective(tok));
        }
        if tok.is_name() {
            return Err(ParseError::UnknownInstruction(tok.span().clone()));
        }
        Err(ParseError::UnexpectedToken(tok))
    }

    fn parse_instr(&self, tokens: &mut TokenStream) -> Result<Option<Statement>,ParseError> {
        let tok = tokens.peek().map(|t| t.token_type().clone());
        if let Some(TokenType::Name(n)) = tok {
            if !Instr::<Expr,IndirectionMode>::exists(&n) {
                if !n.starts_with(".") {
                    let (span, name, args) = self.parse_gen_instr(tokens)?;
                    return Ok(Some(Statement::MacroCall(span, name, args)));
                } else {
                    return Ok(None);
                }
            }
            let (span, name, args) = self.parse_gen_instr(tokens)?;

            make_instr(span, name, &args)
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

    fn parse_byte_vals(&self, tokens: &mut TokenStream) -> Result<Vec<ByteValue>,ParseError> {
        self.parse_list(tokens, |toks| self.parse_byte_value(toks))
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

    fn parse_byte_value(&self, tokens: &mut TokenStream) -> Result<ByteValue,ParseError> {
        if tokens.check(Token::is_string) {
            let (sspan, string) = tokens.read_string()?;
            Ok(ByteValue::String(sspan, string.bytes().collect()))
        } else {
            self.parse_expr(tokens).map(|expr| ByteValue::Expr(expr))
        }
    }

    pub fn parse_directive(&self, tokens: &mut TokenStream) -> Result<Option<Statement>,ParseError> {
        let peeked = tokens.peek().map(|t| t.clone());
        match peeked {
            Some(tok) => {
                if tok.has_name(".byte") {
                    let ident = tokens.next()?;
                    let vals = self.parse_byte_vals(tokens)?;
                    if vals.is_empty() {
                        Err(ParseError::MissingBytes(ident.span().clone()))
                    } else {
                        let span = Span::from(ident.span(), &vals.last().unwrap().span());
                        Ok(Some(Statement::Directive(Directive::Byte(span, vals))))
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
                    if tokens.check(|tok| tok.is_string()) {
                        let (str_span, string) = tokens.read_string()?;
                        let span = Span::from(ident.span(), &str_span);
                        Ok(Some(Statement::Directive(Directive::Include(span, IncludeType::Asm, string))))
                    } else {
                        let (_, inc_type) = tokens.read_include_type()?;
                        let (str_span, string) = tokens.read_string()?;
                        let span = Span::from(ident.span(), &str_span);
                        Ok(Some(Statement::Directive(Directive::Include(span, inc_type, string))))
                    }
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
            let expr = self.parse_expr(tokens)?;
            match expr {
                Expr::MacroArg(span, name) =>
                    Ok(Arg::MacroArg(span.clone(), name.clone())),
                _ => Ok(Arg::Ex(expr))
            }
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

    pub fn remaining(&self) -> &[Token] {
        if !self.is_empty() {
            &self.tokens[self.pos..]
        } else {
            &self.tokens[self.tokens.len() - 1..]
        }
    }

    pub fn len(&self) -> usize {
        self.tokens.len() - self.pos
    }

    pub fn is_empty(&self) -> bool {
        self.pos >= self.tokens.len() || self.check(Token::is_eof)
    }

    pub fn read_include_type(&mut self) -> Result<(Span,IncludeType),ParseError> {
        if self.check(|tok| tok.has_name("asm")) {
            let tok = self.next()?;
            Ok((tok.span().clone(), IncludeType::Asm))
        } else if self.check(|tok| tok.has_name("bytes")) {
            let tok = self.next()?;
            Ok((tok.span().clone(), IncludeType::Bytes))
        } else {
            let tok = self.current()?;
            Err(ParseError::ExpectedTokenNotFound("Include Type", tok.clone()))
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

    pub fn read_macro_name(&mut self) -> Result<(Span,String),ParseError> {
        use lexer::TokenType::*;
        let tok = self.current()?;
        match tok.token_type() {
            MacroIdent(name) => {
                self.advance();
                Ok((tok.span().clone(), name.clone()))
            },
            _ => Err(ParseError::ExpectedTokenNotFound("Macro Ident", tok.clone()))
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

pub fn make_instr(
    span: Span,
    name: String,
    args: &[Arg]
) -> Result<Option<Statement>,ParseError> {
    use expression::Arg::*;
    use instruction::Instr::*;
    match (name.as_str(), args) {
        ("add", [Imm(imm)]) => instr(span, Add_i8(imm.clone())),
        ("add", [Ex(mem)]) => instr(span, Add_d9(mem.clone())),
        ("add", [IM(_, im)]) => instr(span, Add_Ri(im.clone())),
        ("add", _) => wrong_args(span, "add", vec![
            vec![ArgType::Imm],
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("addc", [Imm(imm)]) => instr(span, Addc_i8(imm.clone())),
        ("addc", [Ex(mem)]) => instr(span, Addc_d9(mem.clone())),
        ("addc", [IM(_, im)]) => instr(span, Addc_Ri(im.clone())),
        ("addc", _) => wrong_args(span, "addc", vec![
            vec![ArgType::Imm],
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("sub", [Imm(imm)]) => instr(span, Sub_i8(imm.clone())),
        ("sub", [Ex(mem)]) => instr(span, Sub_d9(mem.clone())),
        ("sub", [IM(_, im)]) => instr(span, Sub_Ri(im.clone())),
        ("sub", _) => wrong_args(span, "sub", vec![
            vec![ArgType::Imm],
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("subc", [Imm(imm)]) => instr(span, Subc_i8(imm.clone())),
        ("subc", [Ex(mem)]) => instr(span, Subc_d9(mem.clone())),
        ("subc", [IM(_, im)]) => instr(span, Subc_Ri(im.clone())),
        ("subc", _) => wrong_args(span, "subc", vec![
            vec![ArgType::Imm],
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("inc", [Ex(mem)]) => instr(span, Inc_d9(mem.clone())),
        ("inc", [IM(_, im)]) => instr(span, Inc_Ri(im.clone())),
        ("inc", _) => wrong_args(span, "inc", vec![
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("dec", [Ex(mem)]) => instr(span, Dec_d9(mem.clone())),
        ("dec", [IM(_, im)]) => instr(span, Dec_Ri(im.clone())),
        ("dec", _) => wrong_args(span, "dec", vec![
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("mul", []) => instr(span, Mul),
        ("mul", _) => wrong_args(span, "mul", vec![]),
        ("div", []) => instr(span, Div),
        ("div", _) => wrong_args(span, "div", vec![]),
        ("and", [Imm(imm)]) => instr(span, And_i8(imm.clone())),
        ("and", [Ex(mem)]) => instr(span, And_d9(mem.clone())),
        ("and", [IM(_, im)]) => instr(span, And_Ri(im.clone())),
        ("and", _) => wrong_args(span, "and", vec![
            vec![ArgType::Imm],
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("or", [Imm(imm)]) => instr(span, Or_i8(imm.clone())),
        ("or", [Ex(mem)]) => instr(span, Or_d9(mem.clone())),
        ("or", [IM(_, im)]) => instr(span, Or_Ri(im.clone())),
        ("or", _) => wrong_args(span, "or", vec![
            vec![ArgType::Imm],
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("xor", [Imm(imm)]) => instr(span, Xor_i8(imm.clone())),
        ("xor", [Ex(mem)]) => instr(span, Xor_d9(mem.clone())),
        ("xor", [IM(_, im)]) => instr(span, Xor_Ri(im.clone())),
        ("xor", _) => wrong_args(span, "xor", vec![
            vec![ArgType::Imm],
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("rol", []) => instr(span, Rol),
        ("rol", _) => wrong_args(span, "rol", vec![]),
        ("rolc", []) => instr(span, Rolc),
        ("rolc", _) => wrong_args(span, "rolc", vec![]),
        ("ror", []) => instr(span, Ror),
        ("ror", _) => wrong_args(span, "ror", vec![]),
        ("rorc", []) => instr(span, Rorc),
        ("rorc", _) => wrong_args(span, "rorc", vec![]),
        ("ld", [Ex(mem)]) => instr(span, Ld_d9(mem.clone())),
        ("ld", [IM(_, im)]) => instr(span, Ld_Ri(im.clone())),
        ("ld", _) => wrong_args(span, "ld", vec![
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("st", [Ex(mem)]) => instr(span, St_d9(mem.clone())),
        ("st", [IM(_, im)]) => instr(span, St_Ri(im.clone())),
        ("st", _) => wrong_args(span, "st", vec![
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("mov", [Imm(imm), Ex(mem)]) => instr(span, Mov_d9(imm.clone(), mem.clone())),
        ("mov", [Imm(imm), IM(_, im)]) => instr(span, Mov_Rj(imm.clone(), im.clone())),
        ("mov", _) => wrong_args(span, "mov", vec![
            vec![ArgType::Imm, ArgType::D9],
            vec![ArgType::Imm, ArgType::IM]
        ]),
        ("ldc", []) => instr(span, Ldc),
        ("ldc", _) => wrong_args(span, "ldc", vec![]),
        ("push", [Ex(mem)]) => instr(span, Push(mem.clone())),
        ("push", _) => wrong_args(span, "push", vec![
            vec![ArgType::D9]
        ]),
        ("pop", [Ex(mem)]) => instr(span, Pop(mem.clone())),
        ("pop", _) => wrong_args(span, "pop", vec![
            vec![ArgType::D9]
        ]),
        ("xch", [Ex(mem)]) => instr(span, Xch_d9(mem.clone())),
        ("xch", [IM(_, im)]) => instr(span, Xch_Ri(im.clone())),
        ("xch", _) => wrong_args(span, "xch", vec![
            vec![ArgType::D9],
            vec![ArgType::IM]
        ]),
        ("jmp", [Ex(mem)]) => instr(span, Jmp(mem.clone())),
        ("jmp", _) => wrong_args(span, "jmp", vec![
            vec![ArgType::A12]
        ]),
        ("jmpf", [Ex(mem)]) => instr(span, Jmpf(mem.clone())),
        ("jmpf", _) => wrong_args(span, "jmpf", vec![
            vec![ArgType::A16]
        ]),
        ("br", [Ex(mem)]) => instr(span, Br(mem.clone())),
        ("br", _) => wrong_args(span, "br", vec![
            vec![ArgType::R8]
        ]),
        ("brf", [Ex(mem)]) => instr(span, Brf(mem.clone())),
        ("brf", _) => wrong_args(span, "brf", vec![
            vec![ArgType::R16]
        ]),
        ("bz", [Ex(mem)]) => instr(span, Bz(mem.clone())),
        ("bz", _) => wrong_args(span, "bz", vec![
            vec![ArgType::R8]
        ]),
        ("bnz", [Ex(mem)]) => instr(span, Bnz(mem.clone())),
        ("bnz", _) => wrong_args(span, "bnz", vec![
            vec![ArgType::R8]
        ]),
        ("bp", [Ex(m1), Ex(m2), Ex(m3)]) => instr(span, Bp(m1.clone(), m2.clone(), m3.clone())),
        ("bp", _) => wrong_args(span, "bp", vec![
            vec![ArgType::D9, ArgType::B3, ArgType::R8]
        ]),
        ("bpc", [Ex(m1), Ex(m2), Ex(m3)]) => instr(span, Bpc(m1.clone(), m2.clone(), m3.clone())),
        ("bpc", _) => wrong_args(span, "bpc", vec![
            vec![ArgType::D9, ArgType::B3, ArgType::R8]
        ]),
        ("bn", [Ex(m1), Ex(m2), Ex(m3)]) => instr(span, Bn(m1.clone(), m2.clone(), m3.clone())),
        ("bn", _) => wrong_args(span, "bn", vec![
            vec![ArgType::D9, ArgType::B3, ArgType::R8]
        ]),
        ("dbnz", [Ex(m1), Ex(m2)]) => instr(span, Dbnz_d9(m1.clone(), m2.clone())),
        ("dbnz", [IM(_, im), Ex(mem)]) => instr(span, Dbnz_Ri(im.clone(), mem.clone())),
        ("dbnz", _) => wrong_args(span, "dbnz", vec![
            vec![ArgType::D9, ArgType::R8],
            vec![ArgType::IM, ArgType::R8]
        ]),
        ("be", [Imm(imm), Ex(mem)]) => instr(span, Be_i8(imm.clone(), mem.clone())),
        ("be", [Ex(m1), Ex(m2)]) => instr(span, Be_d9(m1.clone(), m2.clone())),
        ("be", [IM(_, im), Imm(imm), Ex(mem)]) => instr(span, Be_Rj(im.clone(), imm.clone(), mem.clone())),
        ("be", _) => wrong_args(span, "be", vec![
            vec![ArgType::Imm, ArgType::R8],
            vec![ArgType::D9, ArgType::R8],
            vec![ArgType::IM, ArgType::Imm, ArgType::R8]
        ]),
        ("bne", [Imm(imm), Ex(mem)]) => instr(span, Bne_i8(imm.clone(), mem.clone())),
        ("bne", [Ex(m1), Ex(m2)]) => instr(span, Bne_d9(m1.clone(), m2.clone())),
        ("bne", [IM(_, im), Imm(imm), Ex(mem)]) => instr(span, Bne_Rj(im.clone(), imm.clone(), mem.clone())),
        ("bne", _) => wrong_args(span, "bne", vec![
            vec![ArgType::Imm, ArgType::R8],
            vec![ArgType::D9, ArgType::R8],
            vec![ArgType::IM, ArgType::Imm, ArgType::R8]
        ]),
        ("call", [Ex(mem)]) => instr(span, Call(mem.clone())),
        ("call", _) => wrong_args(span, "call", vec![
            vec![ArgType::A12]
        ]),
        ("callf", [Ex(mem)]) => instr(span, Callf(mem.clone())),
        ("callf", _) => wrong_args(span, "callf", vec![
            vec![ArgType::A16]
        ]),
        ("callr", [Ex(mem)]) => instr(span, Callr(mem.clone())),
        ("callr", _) => wrong_args(span, "callr", vec![
            vec![ArgType::R16]
        ]),
        ("ret", []) => instr(span, Ret),
        ("ret", _) => wrong_args(span, "ret", vec![]),
        ("reti", []) => instr(span, Reti),
        ("reti", _) => wrong_args(span, "reti", vec![]),
        ("clr1", [Ex(d9), Ex(b3)]) => instr(span, Clr1(d9.clone(), b3.clone())),
        ("clr1", _) => wrong_args(span, "clr1", vec![
            vec![ArgType::D9, ArgType::B3]
        ]),
        ("set1", [Ex(d9), Ex(b3)]) => instr(span, Set1(d9.clone(), b3.clone())),
        ("set1", _) => wrong_args(span, "set1", vec![
            vec![ArgType::D9, ArgType::B3]
        ]),
        ("not1", [Ex(d9), Ex(b3)]) => instr(span, Not1(d9.clone(), b3.clone())),
        ("not1", _) => wrong_args(span, "not1", vec![
            vec![ArgType::D9, ArgType::B3]
        ]),
        ("nop", []) => instr(span, Nop),
        ("nop", _) => wrong_args(span, "nop", vec![]),
        ("ldf", []) => instr(span, Ldf),
        ("ldf", _) => wrong_args(span, "ldf", vec![]),
        ("stf", []) => instr(span, Stf),
        ("stf", _) => wrong_args(span, "stf", vec![]),
        _ => Err(ParseError::UnknownInstruction(span))
    }
}

// ----- Expression Parsing -----

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone,Copy)]
enum Precedence {
    Bitwise = 1,
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
    fn parse<'a>(&self, _parser: &ExprParser, _tokens: &mut TokenStream<'a>, token: Token)
                 -> Result<Expr,ParseError> {
        match token.token_type() {
            TokenType::Name(name) =>
                Ok(Expr::Name(token.span().clone(), name.to_owned())),
            _ => Err(ParseError::ExpectedTokenNotFound("Name", token.clone()))
        }
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

struct MacroArgParselet(Precedence);

impl PrefixParselet for MacroArgParselet {
    fn parse<'a>(&self, _parser: &ExprParser, _tokens: &mut TokenStream<'a>, token: Token)
                 -> Result<Expr,ParseError> {
        match token.token_type() {
            TokenType::MacroIdent(name) =>
                Ok(Expr::MacroArg(token.span().clone(), name.to_owned())),
            _ => Err(ParseError::ExpectedTokenNotFound("Macro Arg", token.clone()))
        }
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

struct MacroLabelParselet(Precedence);

impl PrefixParselet for MacroLabelParselet {
    fn parse<'a>(&self, _parser: &ExprParser, _tokens: &mut TokenStream<'a>, token: Token)
                 -> Result<Expr,ParseError> {
        match token.token_type() {
            TokenType::MacroLabel(name) =>
                Ok(Expr::MacroLabel(token.span().clone(), name.to_owned())),
            _ => Err(ParseError::ExpectedTokenNotFound("Macro Label", token.clone()))
        }
    }
    fn precedence(&self) -> i32 { self.0 as i32 }
}

struct NumberParselet(Precedence);

impl PrefixParselet for NumberParselet {
    fn parse<'a>(&self, _parser: &ExprParser, _tokens: &mut TokenStream<'a>, token: Token)
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
            Minus => Ok(Expr::UnaryMinus(token.span().clone(), Box::new(result))),
            UpperByte => Ok(Expr::UpperByte(token.span().clone(), Box::new(result))),
            LowerByte => Ok(Expr::LowerByte(token.span().clone(), Box::new(result))),
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
            Plus => Ok(Expr::Plus(Span::from(&left.span(), &right.span()), Box::new(left), Box::new(right))),
            Times => Ok(Expr::Times(Span::from(&left.span(), &right.span()), Box::new(left), Box::new(right))),
            Minus => Ok(Expr::Minus(Span::from(&left.span(), &right.span()), Box::new(left), Box::new(right))),
            Divide => Ok(Expr::Divide(Span::from(&left.span(), &right.span()), Box::new(left), Box::new(right))),
            Caret => Ok(Expr::BitwiseXor(Span::from(&left.span(), &right.span()), Box::new(left), Box::new(right))),
            Ampersand => Ok(Expr::BitwiseAnd(Span::from(&left.span(), &right.span()), Box::new(left), Box::new(right))),
            Pipe => Ok(Expr::BitwiseOr(Span::from(&left.span(), &right.span()), Box::new(left), Box::new(right))),
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
    LowerByte,
    MacroLabel,
    MacroArg,
    BitwiseXor,
    BitwiseAnd,
    BitwiseOr
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
            MacroIdent(_) => Some(ExprTokenType::MacroArg),
            MacroLabel(_) => Some(ExprTokenType::MacroLabel),
            Caret => Some(ExprTokenType::BitwiseXor),
            Ampersand => Some(ExprTokenType::BitwiseAnd),
            Pipe => Some(ExprTokenType::BitwiseOr),
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
    use std::collections::HashMap;
    use ast::Statement;

    #[test]
    fn test_expression_parser() {
        check_expression_parser(
            "fred + 2 * 7 - 21 * (6 + 7)",
            "(fred + (2 * 7)) - ($15 * (6 + 7))"
        );
        check_expression_parser(
            "bob + sam + -12 * 7",
            "(bob + sam) + ((-$0C) * 7)"
        );
        check_expression_parser(
            "bob + sam + -12 * 7 + ->fred - <sam",
            "(((bob + sam) + ((-$0C) * 7)) + (-(>fred))) - (<sam)"
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
        let mut token_stream = TokenStream::from(&tokens);
        let parser = parser::Parser::create();
        parser.parse_expr(&mut token_stream)
    }

    #[test]
    fn test_parser_org() {
        let line = ".org $44";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".org $0044", printed);
    }

    #[test]
    fn test_parser_byte() {
        let line = ".byte $44, $65, 0x32, 0b10110";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".byte $44, $65, $32, $16", printed);
    }

    #[test]
    fn test_parser_word() {
        let line = ".word $4478, $6543, 0x3221, 0b1011100001100100";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".word $4478, $6543, $3221, $B864", printed);
    }

    #[test]
    fn test_parser_cnop() {
        let line = ".cnop $40, $22";
        let stmt = parse_statement(line).expect("failed to parse statement");
        let printed = format!("{}", stmt);
        assert_eq!(".cnop $40, $22", printed);
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
        let mut token_stream = TokenStream::from(&tokens);
        let parser = parser::Parser::create();
        parser.parse_statement(&mut token_stream)
    }
}
