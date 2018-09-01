
use std;
use lexer;
use parser;
use std::path::Path;
use ast::{Statements,Statement,Directive};
use expression::{EvaluationError};
use std::collections::HashMap;
use location::{Span, Positioned, Location};
use env::{Env,Names};
use instruction::{EncodingError};
use lexer::{Token,LexerError};
use parser::{ParseError,ArgType,Parser};
use files::{FileLoadError,SourceFiles};
use input::Input;

pub fn assemble_file(mut files: &mut SourceFiles, filename: &str) -> Result<Vec<u8>,AssemblyError> {
    let path = Path::new(filename);
    let parser = parser::Parser::create();

    let tokens = {
        let file = files.load(path.file_name().expect("expected filename").to_str().unwrap())?;
        let input = Input::new(file.id(), file.contents());
        lexer::lex_input(&input)?
    };
    let statements = parser.parse(&tokens)?;
    let statements = replace_includes(&parser, &mut files, &statements)?;
    assemble(&statements)
}

fn replace_includes(parser: &Parser, files: &mut SourceFiles, statements: &Statements) -> Result<Statements,AssemblyError> {
    let mut results = vec![];
    for statement in statements.statements.iter() {
        if let Statement::Directive(Directive::Include(_, path)) = statement {
            let tokens = {
                let file = files.load(&path)?;
                let input = Input::new(file.id(), file.contents());
                lexer::lex_input(&input)?
            };
            let statements = replace_includes(parser, files, &parser.parse(&tokens)?)?;
            for stmt in statements.statements.iter() {
                results.push(stmt.clone());
            }
        } else {
            results.push(statement.clone());
        }
    }
    Ok(Statements { statements: results })
}

pub fn assemble(statements: &Statements) -> Result<Vec<u8>,AssemblyError> {
    let (max_pos, names) = compute_names(statements)?;
    let mut output = vec![0; max_pos - 1];
    generate_bytes(statements, &names, &mut output)?;
    Ok(output)
}

fn generate_bytes(statements: &Statements, names: &Names, output: &mut Vec<u8>) -> Result<(),AssemblyError> {
    let mut pos: usize = 0;
    let mut current_global = "".to_owned();
    for statement in statements.statements.iter() {
        use ast::Statement::*;
        match statement {
            Directive(dir) => {
                use ast::Directive::*;
                match dir {
                    Byte(_, bytes) => {
                        let env = names.as_env("Name", &current_global);
                        for expr in bytes.iter() {
                            let b: u8 = (expr.eval(&env)? & 0xFF) as u8;
                            output[pos] = b;
                            pos = pos + 1;
                        }
                    },
                    ByteString(_, bytes) => {
                        for b in bytes.iter() {
                            output[pos] = *b;
                            pos = pos + 1;
                        }
                    },
                    Org(span, location) => {
                        if *location > 0xFFFF {
                            return Err(AssemblyError::InvalidCodeLocation(span.clone(), *location as i32))
                        }
                        pos = *location;
                    },
                    Word(_, words) => {
                        let env = names.as_env("Name", &current_global);
                        for expr in words.iter() {
                            let w: u16 = (expr.eval(&env)? & 0xFFFF) as u16;
                            output[pos] = (w & 0xFF) as u8;
                            pos = pos + 1;
                            output[pos] = ((w >> 8) & 0xFF) as u8;
                            pos = pos + 1;
                        }
                    },
                    Include(_, _) => panic!("There should be no .include directives left at this point".to_string()),
                    Cnop(_, _add, _multiple) => {
                        pos += dir.size(pos as i32)? as usize;
                    }
                }
            },
            Label(_, name) => {
                if !name.starts_with(".") {
                    current_global = name.clone();
                }
            },
            Instr(_, instr) => {
                let next_pos = pos + instr.size();
                let bytes = instr.eval(next_pos, &current_global, &names)?.encode();
                for b in bytes.iter() {
                    output[pos] = *b;
                    pos = pos + 1;
                }
            },
            Variable(_, _, _) | Alias(_, _, _) => {}
        }
    }
    Ok(())
}

#[derive(Debug)]
pub enum AssemblyError {
    NameNotFound(Span,String),
    DivideByZero(Span,String),
    MustBeLiteralNumber(Span),
    NameAlreadyExists(Span,Span,String),
    InvalidCodeLocation(Span,i32),
    NumOutOfRange {
        span: Span,
        bits: usize,
        value: i32
    },
    SignedNumOutOfRange {
        span: Span,
        bits: usize,
        value: i32
    },
    InvalidAddress {
        span: Span,
        value: i32
    },
    AddrBitsDontMatch {
        span: Span,
        pos: usize,
        value: i32,
        pos_top: u8,
        value_top: u8
    },
    UnexpectedChar(Location),
    UnexpectedToken(Token),
    InvalidInstruction(Token),
    ExpectedTokenNotFound(&'static str, Token),
    InvalidExpression(Location),
    MissingBytes(Span),
    MissingWords(Span),
    UnknownDirective(Token),
    UnknownInstruction(Token),
    WrongInstructionArgs(Span,String,Vec<Vec<ArgType>>),
    UnexpectedEof,
    FileLoadFailure(String, std::io::Error),
    FileUtf8Error(String, std::string::FromUtf8Error)
}

impl From<LexerError> for AssemblyError {
    fn from(error: LexerError) -> Self {
        use lexer::LexerError::*;
        match error {
            UnexpectedChar(loc) => AssemblyError::UnexpectedChar(loc)
        }
    }
}

impl From<FileLoadError> for AssemblyError {
    fn from(error: FileLoadError) -> Self {
        use files::FileLoadError::*;
        match error {
            FileLoadFailure(file, err) => AssemblyError::FileLoadFailure(file, err),
            Utf8Error(file, err) => AssemblyError::FileUtf8Error(file, err)
        }
    }
}

impl From<ParseError> for AssemblyError {
    fn from(error: ParseError) -> Self {
        use parser::ParseError::*;
        match error {
            UnexpectedChar(loc) => AssemblyError::UnexpectedChar(loc),
            UnexpectedToken(tok) => AssemblyError::UnexpectedToken(tok),
            InvalidInstruction(tok) => AssemblyError::InvalidInstruction(tok),
            ExpectedTokenNotFound(name, tok) => AssemblyError::ExpectedTokenNotFound(name, tok),
            InvalidExpression(loc) => AssemblyError::InvalidExpression(loc),
            MissingBytes(span) => AssemblyError::MissingBytes(span),
            MissingWords(span) => AssemblyError::MissingWords(span),
            UnknownDirective(tok) => AssemblyError::UnknownDirective(tok),
            UnknownInstruction(tok) => AssemblyError::UnknownInstruction(tok),
            WrongInstructionArgs(span, name, types) => AssemblyError::WrongInstructionArgs(span, name, types),
            UnexpectedEof => AssemblyError::UnexpectedEof
        }
    }
}

impl From<EncodingError> for AssemblyError {
    fn from(error: EncodingError) -> Self {
        use instruction::EncodingError::*;
        match error {
            NumOutOfRange { span, bits, value } => AssemblyError::NumOutOfRange { span, bits, value },
            SignedNumOutOfRange { span, bits, value } => AssemblyError::SignedNumOutOfRange { span, bits, value },
            InvalidAddress { span, value } => AssemblyError::InvalidAddress { span, value },
            AddrBitsDontMatch { span, pos, value, pos_top, value_top } =>
                AssemblyError::AddrBitsDontMatch { span, pos, value, pos_top, value_top },
            EvalError(eval) => AssemblyError::from(eval)
        }
    }
}

impl From<EvaluationError> for AssemblyError {
    fn from(error: EvaluationError) -> Self {
        use expression::EvaluationError::*;
        match error {
            NameNotFound(span, name) => AssemblyError::NameNotFound(span.clone(), name.clone()),
            DivideByZero(span, message) => AssemblyError::DivideByZero(span.clone(), message.clone()),
            MustBeLiteralNumber(span) => AssemblyError::MustBeLiteralNumber(span.clone())
        }
    }
}

fn add_name(
    name: String,
    value: NameValue,
    names: &mut HashMap<String,NameValue>
) -> Result<(), AssemblyError> {
    if names.contains_key(&name) {
        Err(AssemblyError::NameAlreadyExists(value.span.clone(), names[&name].span.clone(), name.clone()))
    } else {
        names.insert(name, value);
        Ok(())
    }
}

#[derive(Debug)]
struct NameValue {
    span: Span,
    value: i32
}

#[derive(Debug)]
struct NamesBuilder {
    globals: HashMap<String,NameValue>,
    locals: HashMap<String,HashMap<String,NameValue>>
}

fn compute_labels(statements: &Statements) -> Result<NamesBuilder,AssemblyError> {
    let mut globals = HashMap::new();
    let mut locals = HashMap::new();
    let mut local = HashMap::new();
    let mut current_global = "".to_owned();
    let mut pos: i32 = 0;
    for statement in statements.statements.iter() {
        use ast::Statement::*;
        match statement {
            Directive(dir) => {
                pos += dir.size(pos)?
            },
            Label(_, name) => {
                if name.starts_with(".") {
                    let val = NameValue { span: statement.span(), value: pos };
                    add_name(name.to_lowercase(), val, &mut local)?;
                } else {
                    let val = NameValue { span: statement.span(), value: pos };
                    add_name(name.to_lowercase(), val, &mut globals)?;
                    locals.insert(current_global.clone(), local);
                    local = HashMap::new();
                    current_global = name.to_lowercase();
                }
            },
            Instr(_, instr) => {
                pos += instr.size() as i32;
            },
            Variable(_, _name, _expr) | Alias(_, _name, _expr) => {}
        }
    }
    if !locals.contains_key(&current_global) {
        locals.insert(current_global.clone(), local);
    }
    Ok(NamesBuilder { globals, locals })
}

fn compute_names(statements: &Statements) -> Result<(usize, Names),AssemblyError> {
    let labels = compute_labels(statements)?;
    let mut globals = labels.globals;
    let locals = labels.locals;
    let mut pos: i32 = 0;
    let mut max_pos = 0;
    for statement in statements.statements.iter() {
        use ast::Statement::*;
        match statement {
            Directive(dir) => {
                pos += dir.size(pos)?
            },
            Label(_, _name) => (),
            Instr(_, instr) => {
                pos += instr.size() as i32;
            },
            Variable(_, name, expr) | Alias(_, name, expr) => {
                let val = {
                    let env = MapEnv::new("Name", &globals);
                    NameValue { span: statement.span(), value: expr.eval(&env)? }
                };
                add_name(name.to_lowercase(), val, &mut globals)?;
            }
        }
        if pos > max_pos {
            max_pos = pos;
        }
    }
    let mut new_globals = HashMap::new();
    for key in globals.keys() {
        new_globals.insert(key.to_owned(), globals[key].value);
    }
    let mut new_locals = HashMap::new();
    for key in locals.keys() {
        let mut new_local = HashMap::new();
        for lkey in locals[key].keys() {
            new_local.insert(lkey.to_owned(), locals[key][lkey].value);
        }
        new_locals.insert(key.to_owned(), new_local);
    }
    Ok(((max_pos + 1) as usize, Names { globals: new_globals, locals: new_locals }))
}

// Env stuff

pub struct MapEnv<'a,'b> {
    name: &'a str,
    map: &'b HashMap<String,NameValue>
}

impl<'a,'b> MapEnv<'a,'b> {
    fn new<'c,'d>(name: &'c str, map: &'d HashMap<String,NameValue>) -> MapEnv<'c,'d> {
        MapEnv { name: name, map: map }
    }
}

impl<'a,'b> Env<i32> for MapEnv<'a,'b> {
    fn name(&self) -> &str {
        self.name
    }

    fn get(&self, name: &str) -> Option<i32> {
        self.map.get(name).map(|v| v.value)
    }
}

