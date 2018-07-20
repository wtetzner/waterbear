
use input;
use location::Span;
use std::io;
use files::SourceFiles;
use input::Input;
use regex::Regex;
use location::Location;

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
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

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
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

#[derive(Debug)]
pub enum LexerError {
    UnexpectedChar(Location)
}

fn clean_str(string: &str) -> String {
    let mut result: Vec<u8> = vec![];
    let mut escape = false;
    for byte in string.bytes() {
        if escape {
            escape = false;
            match byte {
                b'r' => result.push(b'\r'),
                b'n' => result.push(b'\n'),
                b't' => result.push(b'\t'),
                b'b' => result.push(8),
                c => result.push(c)
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

struct Matcher {
    regex: Regex,
    convert: fn(&str) -> TokenType
}

impl Matcher {
    pub fn new(re: &str, convert: fn(&str) -> TokenType) -> Matcher {
        Matcher {
            regex: Regex::new(("^".to_owned() + re).as_str()).unwrap(),
            convert: convert
        }
    }

    pub fn do_match<'a>(&self, input: &Input<'a>) -> Option<(Input<'a>,Token)> {
        match self.regex.captures_iter(input.as_str()).next() {
            Some(cap) => {
                let capture: &str = &cap[0];
                let convert = &self.convert;
                let token_type = convert(capture);
                let new_input = input.update(capture.len());
                let span = Span::new(input.location(), new_input.location());
                Some((new_input, Token::new(token_type, span)))
            },
            None => None
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
        _ => panic!("Invalid Indirection Mode: {}", text)
    }
}

fn read_token<'a>(input: &Input<'a>) -> Option<(Input<'a>,Token)> {
    lazy_static! {
        static ref MATCHERS: Vec<Matcher> = vec![
            Matcher::new(r"\(", |_| TokenType::LeftParen),
            Matcher::new(r"\)", |_| TokenType::RightParen),
            Matcher::new("#",   |_| TokenType::Hash),
            Matcher::new(",",   |_| TokenType::Comma),
            Matcher::new(":",   |_| TokenType::Colon),
            Matcher::new("=",   |_| TokenType::Equals),
            Matcher::new(r"\+", |_| TokenType::Plus),
            Matcher::new(r"\*", |_| TokenType::Times),
            Matcher::new("-",   |_| TokenType::Minus),
            Matcher::new("/",   |_| TokenType::Divide),
            Matcher::new(">",   |_| TokenType::UpperByte),
            Matcher::new("<",   |_| TokenType::LowerByte),
            Matcher::new(r#""(\\.|[^"\\])*""#, |text| TokenType::String(clean_str(text))),
            Matcher::new(r"@[rR][0123]", |text| to_indr_mode(text))
                
        ];
    }
    if input.eof() {
        let span = Span::new(input.location(), input.location());
        Some((input.clone(), Token::new(TokenType::EOF, span)))
    } else {
        for matcher in MATCHERS.iter() {
            match matcher.do_match(input) {
                None => (),
                matched => return matched
            }
        }
        None
    }
}

pub fn lex_input(input: &Input) -> Result<Vec<Token>,LexerError> {
    let mut results: Vec<Token> = vec![];
    let mut cinput = input.clone();
    let mut current = read_token(&input);
    while current.is_some() && current.as_ref().unwrap().1.token_type != TokenType::EOF {
        let (input, token) = current.unwrap();
        results.push(token);
        current = read_token(&input);
        cinput = input;
    }
    if current.is_none() {
        Err(LexerError::UnexpectedChar(cinput.location()))
    } else {
        Ok(results)
    }
}

pub fn lex(files: &mut SourceFiles, filename: &str) -> Vec<Token> {
    vec![]
}



#[cfg(test)]
mod tests {
    use lexer;
    use lexer::{Token, TokenType, LexerError};
    use input::Input;
    use files::FileID;
    use location::{Location, Span};

    #[test]
    fn test_clean_str() {
        let input = "foo \\n bar \\r \\b \\t \\ \\\\ \\";
        let output = "foo \n bar \r \x08 \t  \\ \\";
        assert!(lexer::clean_str(input) == output);
    }

    #[test]
    fn test_read_token() {
        let file = FileID::new(7);
        let text = "@R3";
        let input = Input::new(file, text);
        let span = Span::new(
            Location::new(file, 0, 1, 0),
            Location::new(file, 3, 1, 3)
        );
        match lexer::read_token(&input) {
            Some((new_input, token)) => assert!(token == Token::new(TokenType::R3, span)),
            None => panic!("Failed to read token")
        }
    }

    #[test]
    fn test_lex_input() {
        let file = FileID::new(7);
        let text = "@R3@R0,+:";
        let input = Input::new(file, text);
        match lexer::lex_input(&input) {
            Ok(tokens) => {
                let toks: Vec<TokenType> = tokens.iter()
                    .map(|token| token.token_type.clone())
                    .collect();
                assert_eq!(toks, vec![TokenType::R3, TokenType::R0, TokenType::Comma, TokenType::Plus, TokenType::Colon]);
            },
            Err(err) => panic!("Failed to read token: {:?}", err)
        }
    }

    #[test]
    fn test_lex_input_bad_token() {
        let file = FileID::new(7);
        let text = "@R3@R0,+:!-";
        let input = Input::new(file, text);
        match lexer::lex_input(&input) {
            Ok(tokens) => panic!("Expected bad token"),
            Err(err) => match err {
                LexerError::UnexpectedChar(ref loc) if *loc == Location::new(file, 9, 1, 9) => (),
                e => panic!("Unexpected error: {:?}", e)
            }
        }
    }
}

