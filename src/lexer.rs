use crate::input::Input;
use crate::location::{Location, Span};
use lazy_static::lazy_static;
use regex::Regex;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub enum Radix {
    Decimal,
    Hex,
    Binary,
    Octal,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone)]
pub enum TokenType {
    Ampersand,
    Caret,
    Pipe,
    LeftParen,
    RightParen,
    Hash,
    Comma,
    Colon,
    DoubleColon,
    Name(String),
    Number(i32, Radix),
    Equals,
    Plus,
    Times,
    Minus,
    Divide,
    UpperByte,
    LowerByte,
    String(String),
    MacroIdent(String),
    MacroLabel(String),
    R0,
    R1,
    R2,
    R3,
    EOF,
}

impl TokenType {
    pub fn name(&self) -> &'static str {
        match self {
            TokenType::Ampersand => "'&'",
            TokenType::Caret => "'^'",
            TokenType::Pipe => "'|'",
            TokenType::LeftParen => "'('",
            TokenType::RightParen => "')'",
            TokenType::Hash => "'#'",
            TokenType::Comma => "','",
            TokenType::Colon => "':'",
            TokenType::DoubleColon => "'::'",
            TokenType::Name(_) => "Name",
            TokenType::Number(_, _) => "Number",
            TokenType::Equals => "'='",
            TokenType::Plus => "'+'",
            TokenType::Times => "'*'",
            TokenType::Minus => "'-'",
            TokenType::Divide => "'/'",
            TokenType::UpperByte => "'>'",
            TokenType::LowerByte => "'<'",
            TokenType::String(_) => "String",
            TokenType::MacroIdent(_) => "Macro Identifier",
            TokenType::MacroLabel(_) => "Macro Label",
            TokenType::R0 => "'@R0'",
            TokenType::R1 => "'@R1'",
            TokenType::R2 => "'@R2'",
            TokenType::R3 => "'@R3'",
            TokenType::EOF => "EOF",
        }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone)]
pub struct Token {
    token_type: TokenType,
    span: Span,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.token_type {
            TokenType::Ampersand => write!(f, "&"),
            TokenType::Caret => write!(f, "^"),
            TokenType::Pipe => write!(f, "|"),
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::Hash => write!(f, "#"),
            TokenType::Comma => write!(f, ","),
            TokenType::Colon => write!(f, ":"),
            TokenType::DoubleColon => write!(f, "::"),
            TokenType::Name(name) => write!(f, "{}", name),
            TokenType::Number(num, _) => write!(f, "{}", num),
            TokenType::Equals => write!(f, "="),
            TokenType::Plus => write!(f, "+"),
            TokenType::Times => write!(f, "*"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Divide => write!(f, "/"),
            TokenType::UpperByte => write!(f, ">"),
            TokenType::LowerByte => write!(f, "<"),
            TokenType::String(text) => write!(f, "{}", text),
            TokenType::MacroIdent(name) => write!(f, "{}", name),
            TokenType::MacroLabel(name) => write!(f, "{}", name),
            TokenType::R0 => write!(f, "@R0"),
            TokenType::R1 => write!(f, "@R1"),
            TokenType::R2 => write!(f, "@R2"),
            TokenType::R3 => write!(f, "@R3"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Token {
        Token {
            token_type: token_type,
            span: span,
        }
    }

    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn is_colon(&self) -> bool {
        match self.token_type() {
            TokenType::Colon => true,
            _ => false,
        }
    }

    pub fn is_double_colon(&self) -> bool {
        match self.token_type() {
            TokenType::DoubleColon => true,
            _ => false,
        }
    }

    pub fn is_equ(&self) -> bool {
        match self.token_type() {
            TokenType::Equals => true,
            _ => false,
        }
    }

    pub fn has_name(&self, name: &str) -> bool {
        match self.token_type() {
            TokenType::Name(n) => name == n,
            _ => false,
        }
    }

    pub fn name_starts_with(&self, starts: &str) -> bool {
        match self.token_type() {
            TokenType::Name(n) => n.starts_with(starts),
            _ => false,
        }
    }

    pub fn macro_label_starts_with(&self, starts: &str) -> bool {
        match self.token_type() {
            TokenType::MacroLabel(n) => n.starts_with(starts),
            _ => false,
        }
    }

    pub fn has_macro_name(&self, name: &str) -> bool {
        match self.token_type() {
            TokenType::MacroIdent(n) => name == n,
            _ => false,
        }
    }

    pub fn get_name(&self) -> Option<&str> {
        match self.token_type() {
            TokenType::Name(ref n) => Some(n),
            _ => None,
        }
    }

    pub fn is_name(&self) -> bool {
        match self.token_type {
            TokenType::Name(_) => true,
            _ => false,
        }
    }

    pub fn is_num(&self) -> bool {
        match self.token_type {
            TokenType::Number(_, _) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self.token_type {
            TokenType::String(_) => true,
            _ => false,
        }
    }

    pub fn name_matching(&self, pred: fn(&str) -> bool) -> bool {
        match self.token_type {
            TokenType::Name(ref n) => pred(n),
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        use crate::lexer::TokenType::*;
        match self.token_type {
            EOF => true,
            _ => false,
        }
    }

    pub fn is_hash(&self) -> bool {
        use crate::lexer::TokenType::*;
        match self.token_type {
            Hash => true,
            _ => false,
        }
    }

    pub fn is_macro_ident(&self) -> bool {
        use crate::lexer::TokenType::*;
        match self.token_type {
            MacroIdent(_) => true,
            _ => false,
        }
    }

    pub fn is_local_label_name(&self) -> bool {
        use crate::lexer::TokenType::*;
        match &self.token_type {
            Name(n) => n.starts_with("."),
            _ => false,
        }
    }

    pub fn is_local_macro_label(&self) -> bool {
        use crate::lexer::TokenType::*;
        match &self.token_type {
            MacroLabel(n) => n.starts_with("."),
            _ => false,
        }
    }

    pub fn is_macro_label(&self) -> bool {
        use crate::lexer::TokenType::*;
        match self.token_type {
            MacroLabel(_) => true,
            _ => false,
        }
    }

    pub fn is_indirection_mode(&self) -> bool {
        use crate::lexer::TokenType::*;
        match self.token_type {
            R0 | R1 | R2 | R3 => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum LexerError {
    UnexpectedChar(Location),
}

fn clean_str(string: &str) -> String {
    let mut result: Vec<u8> = vec![];
    let mut escape = false;
    let len = string.len();
    for (pos, byte) in string.bytes().enumerate() {
        if pos == 0 || pos == len - 1 {
            continue;
        }
        if escape {
            escape = false;
            match byte {
                b'r' => result.push(b'\r'),
                b'n' => result.push(b'\n'),
                b't' => result.push(b'\t'),
                b'b' => result.push(8),
                b'0' => result.push(b'\0'),
                c => result.push(c),
            }
        } else if byte == b'\\' {
            escape = true;
        } else {
            result.push(byte);
        }
    }
    if escape {
        result.push(b'\\');
    }

    // This should never fail (I hope...).  But even if it does, it's
    // a bug in the code, and there's no reasonable course of action
    // for calling code to recover.
    String::from_utf8(result).unwrap()
}

struct Matcher {
    regex: Regex,
    convert: fn(&str) -> TokenType,
}

impl Matcher {
    pub fn new(re: &str, convert: fn(&str) -> TokenType) -> Matcher {
        Matcher {
            regex: Regex::new(("^".to_owned() + re).as_str()).unwrap(),
            convert: convert,
        }
    }

    pub fn do_match<'a>(&self, input: &Input<'a>) -> Option<(Input<'a>, Token)> {
        match self.regex.captures_iter(input.as_str()).next() {
            Some(cap) => {
                let capture: &str = &cap[0];
                let convert = &self.convert;
                let token_type = convert(capture);
                let new_input = input.update(capture.len());
                let span = Span::new(input.location(), new_input.location());
                Some((new_input, Token::new(token_type, span)))
            }
            None => None,
        }
    }
}

fn to_indr_mode(text: &str) -> TokenType {
    match text.to_uppercase().as_str() {
        "@R0" => TokenType::R0,
        "@R1" => TokenType::R1,
        "@R2" => TokenType::R2,
        "@R3" => TokenType::R3,
        // This shouldn't ever happen
        _ => panic!("Invalid Indirection Mode: {}", text),
    }
}

fn end_of_line(pos: usize, text: &str) -> bool {
    (pos < text.len() && text.as_bytes()[pos] == b'\n')
        || (pos < text.len() - 1
            && text.as_bytes()[pos] == b'\r'
            && text.as_bytes()[pos + 1] == b'\n')
        || pos >= text.len()
}

pub fn skip_to_eol<'a>(input: &Input<'a>) -> Input<'a> {
    let mut skipped = 0;
    let text = input.as_str();
    while !end_of_line(skipped, text) {
        skipped = skipped + 1
    }
    input.update(skipped)
}

fn skip_star_comment<'a>(input: &Input<'a>) -> Input<'a> {
    let text = input.as_str();
    if text.len() > 0 && input.start_of_line() && text.as_bytes()[0] == b'*' {
        return skip_to_eol(input);
    }
    input.clone()
}

fn skip_comment<'a>(input: &Input<'a>) -> Input<'a> {
    let text = input.as_str();
    if text.len() > 0 && text.as_bytes()[0] == b';' {
        return skip_to_eol(input);
    }
    input.clone()
}

fn skip_whitespace_and_comments<'a>(input: &Input<'a>) -> Input<'a> {
    let mut first = true;
    let mut last_pos = 0;
    let mut current = input.clone();
    while current.pos() > last_pos || first {
        if first {
            first = false;
        }
        last_pos = current.pos();
        let mut new_input = current.skip_whitespace();
        new_input = skip_star_comment(&new_input);
        new_input = skip_comment(&new_input);
        current = new_input;
    }
    current
}

fn to_hex_num(text: &str, prefix: usize) -> TokenType {
    let num = i32::from_str_radix(&(&text[prefix..]).replace('\'', ""), 16).unwrap();
    TokenType::Number(num, Radix::Hex)
}

fn to_oct_num(text: &str) -> TokenType {
    let num = i32::from_str_radix(&(&text[2..]).replace('\'', ""), 8).unwrap();
    TokenType::Number(num, Radix::Octal)
}

fn to_bin_num(text: &str, prefix: usize) -> TokenType {
    let num = i32::from_str_radix(&(&text[prefix..]).replace('\'', ""), 2).unwrap();
    TokenType::Number(num, Radix::Binary)
}

fn to_dec_num(text: &str) -> TokenType {
    let num = i32::from_str_radix(&text.replace('\'', ""), 10).unwrap();
    TokenType::Number(num, Radix::Decimal)
}

fn read_token<'a>(
    input: &Input<'a>,
    skip_ws: fn(&Input<'a>) -> Input<'a>,
) -> Option<(Input<'a>, Token)> {
    lazy_static! {
        static ref MATCHERS: Vec<Matcher> = {
            fn number(prefix: Option<&str>, digit: &str) -> String {
                if let Some(prefix) = prefix {
                    format!("{prefix}{digit}(?:'{digit}|{digit})*")
                } else {
                    format!("{digit}(?:'{digit}|{digit})*")
                }
            }
            let ident = "[a-zA-Z_\\.][a-zA-Z\\$0-9_\\.]*";
            let macro_ident = format!("%{}", ident);
            let macro_label = format!("{}%", ident);
            let hex_num = &number(Some("\\$"), "[a-fA-F0-9]");
            let hex_num2 = &number(Some("0[xX]"), "[a-fA-F0-9]");
            let bin_num = &number(Some("%"), "[01]");
            let bin_num2 = &number(Some("0[bB]"), "[01]");
            let oct_num = &number(Some("0[oO]"), "[0-7]");
            let dec_num = &number(None, "[0-9]");

            vec![
                Matcher::new(&macro_ident, |text| {
                    TokenType::MacroIdent(text.to_lowercase())
                }),
                Matcher::new(&macro_label, |text| {
                    TokenType::MacroLabel(text.to_lowercase())
                }),
                Matcher::new(r"\(", |_| TokenType::LeftParen),
                Matcher::new(r"\)", |_| TokenType::RightParen),
                Matcher::new("#", |_| TokenType::Hash),
                Matcher::new(",", |_| TokenType::Comma),
                Matcher::new("::", |_| TokenType::DoubleColon),
                Matcher::new(":", |_| TokenType::Colon),
                Matcher::new("=", |_| TokenType::Equals),
                Matcher::new(r"\+", |_| TokenType::Plus),
                Matcher::new(r"\*", |_| TokenType::Times),
                Matcher::new("-", |_| TokenType::Minus),
                Matcher::new("/", |_| TokenType::Divide),
                Matcher::new("\\&", |_| TokenType::Ampersand),
                Matcher::new("\\|", |_| TokenType::Pipe),
                Matcher::new("\\^", |_| TokenType::Caret),
                Matcher::new(">", |_| TokenType::UpperByte),
                Matcher::new("<", |_| TokenType::LowerByte),
                Matcher::new(r#""(\\.|[^"\\])*""#, |text| {
                    TokenType::String(clean_str(text))
                }),
                Matcher::new("@[rR][0123]", |text| to_indr_mode(text)),
                Matcher::new(ident, |text| TokenType::Name(text.to_lowercase())),
                Matcher::new(hex_num, |text| to_hex_num(text, 1)),
                Matcher::new(oct_num, |text| to_oct_num(text)),
                Matcher::new(hex_num2, |text| to_hex_num(text, 2)),
                Matcher::new(bin_num, |text| to_bin_num(text, 1)),
                Matcher::new(bin_num2, |text| to_bin_num(text, 2)),
                Matcher::new(dec_num, |text| to_dec_num(text)),
            ]
        };
    }
    let input = skip_ws(input);
    if input.eof() {
        let span = Span::new(input.location(), input.location());
        Some((input.clone(), Token::new(TokenType::EOF, span)))
    } else {
        for matcher in MATCHERS.iter() {
            match matcher.do_match(&input) {
                None => (),
                matched => return matched,
            }
        }
        None
    }
}

pub fn lex_for_ws<'a>(
    input: &Input<'a>,
    skip_ws: fn(&Input<'a>) -> Input<'a>,
) -> Result<Vec<Token>, LexerError> {
    let mut results: Vec<Token> = vec![];
    let mut cinput = input.clone();
    let mut current = read_token(&input, skip_ws);
    while current.is_some() && current.as_ref().unwrap().1.token_type != TokenType::EOF {
        let (input, token) = current.unwrap();
        results.push(token);
        current = read_token(&input, skip_ws);
        cinput = input;
    }
    if current.is_none() {
        Err(LexerError::UnexpectedChar(cinput.location()))
    } else {
        results.push(Token::new(
            TokenType::EOF,
            Span::new(cinput.location().clone(), cinput.location().clone()),
        ));
        Ok(results)
    }
}

pub fn lex_input(input: &Input) -> Result<Vec<Token>, LexerError> {
    lex_for_ws(input, skip_whitespace_and_comments)
}

#[cfg(test)]
mod tests {
    use crate::files::FileID;
    use crate::input::Input;
    use crate::lexer;
    use crate::lexer::{skip_whitespace_and_comments, LexerError, Token, TokenType};
    use crate::location::{Location, Span};

    #[test]
    fn test_clean_str() {
        let input = "\"foo \\n bar \\r \\b \\t \\ \\\\ \\\"";
        let output = "foo \n bar \r \x08 \t  \\ \\";
        assert!(lexer::clean_str(input) == output);
    }

    #[test]
    fn test_read_token() {
        let file = FileID::new(7);
        let text = "@R3";
        let input = Input::new(file, text);
        let span = Span::new(Location::new(file, 0, 1, 0), Location::new(file, 3, 1, 3));
        match lexer::read_token(&input, skip_whitespace_and_comments) {
            Some((_new_input, token)) => assert!(token == Token::new(TokenType::R3, span)),
            None => panic!("Failed to read token"),
        }
    }

    #[test]
    fn test_lex_input() {
        let file = FileID::new(7);
        let text = "@R3@R0,+:";
        let input = Input::new(file, text);
        match lexer::lex_input(&input) {
            Ok(tokens) => {
                let toks: Vec<TokenType> = tokens
                    .iter()
                    .map(|token| token.token_type.clone())
                    .collect();
                assert_eq!(
                    toks,
                    vec![
                        TokenType::R3,
                        TokenType::R0,
                        TokenType::Comma,
                        TokenType::Plus,
                        TokenType::Colon,
                        TokenType::EOF
                    ]
                );
            }
            Err(err) => panic!("Failed to read token: {:?}", err),
        }
    }

    #[test]
    fn test_lex_input_bad_token() {
        let file = FileID::new(7);
        let text = "@R3@R0,+:!-";
        let input = Input::new(file, text);
        match lexer::lex_input(&input) {
            Ok(_tokens) => panic!("Expected bad token"),
            Err(err) => match err {
                LexerError::UnexpectedChar(ref loc) if *loc == Location::new(file, 9, 1, 9) => (),
                e => panic!("Unexpected error: {:?}", e),
            },
        }
    }

    #[test]
    fn test_end_of_line() {
        let text = "@R3@R0,+:!-\nfoo";
        assert!(!lexer::end_of_line(10, text));
        assert!(lexer::end_of_line(11, text));
        assert!(!lexer::end_of_line(12, text));
        assert!(lexer::end_of_line(15, text));
        assert!(lexer::end_of_line(25, text));
    }

    #[test]
    fn test_skip_comment() {
        let file = FileID::new(7);
        let text = ";fred is cool\nbob";
        let input = Input::new(file, text);
        let new_input = lexer::skip_comment(&input);
        assert_eq!("\nbob", new_input.as_str());
    }
}
