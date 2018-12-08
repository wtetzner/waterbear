
use crate::lexer;
use crate::lexer::{Token,TokenType};

use crate::parser;
use crate::parser::ParseError;

use crate::input::Input;
use crate::location::{Span};

fn skip_multiline_comment<'a>(input: &Input<'a>) -> Input<'a> {
    let text = input.as_str();
    let mut skipped = 0;
    if text.starts_with("/*") {
        skipped = skipped + 2;
        while skipped < text.len()
            && !text[skipped..].starts_with("*/") {
            skipped = skipped + 1;
        }
        if text[skipped..].starts_with("*/") {
            return input.update(skipped + 2);
        } else {
            return input.update(skipped);
        }
    }
    input.clone()
}

fn skip_line_comment<'a>(input: &Input<'a>) -> Input<'a> {
    let text = input.as_str();
    if text.starts_with("//") {
        return lexer::skip_to_eol(input);
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
        new_input = skip_line_comment(&new_input);
        new_input = skip_multiline_comment(&new_input);
        current = new_input;
    }
    current
}

pub fn lex_input(input: &Input) -> Result<Vec<Token>,ParseError> {
    let raw_tokens = lexer::lex_for_ws(input, skip_whitespace_and_comments)?;
    let parser = parser::Parser::create();
    let mut output = vec![];
    for line in parser::lines(&raw_tokens) {
        let mut tokens = parser::TokenStream::from(line);
        if tokens.check(Token::is_hash)
            && tokens.check_at(1, |t| t.has_name("define")) {
            let hash = tokens.next()?;
            let define = tokens.next()?;
            tokens.read_name()?;
            parser.parse_expr(&mut tokens)?;

            output.push(line[2].clone());
            output.push(Token::new(
                TokenType::Name("equ".to_owned()),
                Span::from(hash.span(), define.span())));
            for token in line[3..].iter() {
                output.push(token.clone());
            }
        } else {
            for token in line.iter() {
                output.push(token.clone());
            }
        }
    }
    Ok(output)
}

