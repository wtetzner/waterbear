
use std;
use lexer;
use parser;
use std::path::Path;
use ast::{
    Statements,
    Statement,
    Directive,
    ByteValue,
    IncludeType,
    ArgType,
    MacroStatement
};
use expression::{EvaluationError,Expr,Arg};
use std::collections::HashMap;
use location::{Span, Positioned, Location};
use env::{Env,Names};
use instruction::{EncodingError};
use lexer::{Token,LexerError};
use parser::{ParseError,Parser};
use files::{FileLoadError,SourceFiles};
use files;
use input::Input;
use uuid::Uuid;

pub fn assemble_file(files: &mut SourceFiles, filename: &str) -> Result<Vec<u8>,AssemblyError> {
    let statements = read_statements(files, filename)?;
    let statements = expand_macros(&statements)?;
    assemble(&statements)
}

pub fn expand_file(files: &mut SourceFiles, filename: &str) -> Result<(),AssemblyError> {
    let statements = read_statements(files, filename)?;
    let statements = expand_macros(&statements)?;
    println!("{}", statements);
    Ok(())
}

fn read_statements(mut files: &mut SourceFiles, filename: &str) -> Result<Statements,AssemblyError> {
    let path = Path::new(filename);
    let parser = parser::Parser::create();

    let tokens = {
        let tokens = {
            let file = files.load(path.file_name().expect("expected filename").to_str().unwrap())?;
            let input = Input::new(file.id(), file.contents());
            lexer::lex_input(&input)?
        };
        replace_includes(&parser, &mut files, &tokens)?
    };
    let stmts = parser.parse(&tokens)?;
    let stmts = replace_byte_includes(&mut files, &stmts)?;
    Ok(stmts)
}

fn replace_includes(parser: &Parser, files: &mut SourceFiles, tokens: &[Token]) -> Result<Vec<Token>,AssemblyError> {
    let mut results = vec![];
    for line in parser::lines(tokens) {
        let mut stream = parser::TokenStream::from(line);
        match parser.parse_directive(&mut stream) {
            Ok(Some(stmt)) => match stmt {
                Statement::Directive(Directive::Include(_span, typ, path)) => {
                    match typ {
                        IncludeType::Asm => {
                            let new_tokens = {
                                let file = files.load(&path)?;
                                let input = Input::new(file.id(), file.contents());
                                lexer::lex_input(&input)?
                            };
                            let new_tokens = replace_includes(parser, files, &new_tokens)?;
                            for token in new_tokens.iter() {
                                results.push(token.clone());
                            }
                            if !stream.is_empty() {
                                return Err(AssemblyError::UnexpectedToken(stream.next()?));
                            }
                        },
                        IncludeType::Bytes => {
                            for token in line.iter() {
                                results.push(token.clone());
                            }
                        }
                    }
                },
                _ => {
                    for token in line.iter() {
                        results.push(token.clone());
                    }
                }
            },
            _ => {
                for token in line.iter() {
                    results.push(token.clone());
                }
            }
        }
    }
    Ok(results)
}

fn replace_byte_includes(files: &mut SourceFiles, statements: &Statements) -> Result<Statements,AssemblyError> {
    let mut results = vec![];
    for statement in statements.iter() {
        if let Statement::Directive(Directive::Include(span, typ, path)) = statement {
            match typ {
                IncludeType::Asm => {
                    panic!("There should be no ASM .includes at this stage.");
                },
                IncludeType::Bytes => {
                    let bytes = files::load_bytes(&files.path(&path))?;
                    let mut byte_vals = vec![];
                    for byte in bytes.iter() {
                        byte_vals.push(ByteValue::Expr(Expr::num(*byte as i32)));
                    }
                    results.push(Statement::Directive(Directive::Byte(span.clone(), byte_vals)));
                }
            }
        } else {
            results.push(statement.clone());
        }
    }
    Ok(statements.with_statements(results))
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
    for statement in statements.iter() {
        use ast::Statement::*;
        match statement {
            Directive(dir) => {
                use ast::Directive::*;
                match dir {
                    Byte(_, bytes) => {
                        let env = names.as_env("Name", &current_global);
                        for item in bytes.iter() {
                            match item {
                                ByteValue::Expr(expr) => {
                                    let value = expr.eval(&env)?;
                                    if value < (std::i8::MIN as i32)
                                        || value > (std::u8::MAX as i32) {
                                        return Err(
                                            AssemblyError::ByteOutOfRange {
                                                span: expr.span(),
                                                value: value
                                            }
                                        );
                                    }
                                    let b: u8 = (value & 0xFF) as u8;
                                    output[pos] = b;
                                    pos = pos + 1;
                                },
                                ByteValue::String(_, vec) => {
                                    for byte in vec.iter() {
                                        output[pos] = *byte;
                                        pos = pos + 1;
                                    }
                                }
                            }
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
                            let value = expr.eval(&env)?;
                            if value < (std::i16::MIN as i32)
                                || value > (std::u16::MAX as i32) {
                                return Err(
                                    AssemblyError::WordOutOfRange {
                                        span: expr.span(),
                                        value: value
                                    }
                                );
                            }
                            let w: u16 = (value & 0xFFFF) as u16;
                            output[pos] = (w & 0xFF) as u8;
                            pos = pos + 1;
                            output[pos] = ((w >> 8) & 0xFF) as u8;
                            pos = pos + 1;
                        }
                    },
                    Include(_, _, _) => panic!("There should be no .include directives left at this point".to_string()),
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
            Variable(_, _, _) | Alias(_, _, _) => {},
            Comment(_) => {},
            MacroCall(span, name, args) => {
                panic!("Shouldn't be any macro calls left: {}:{}:{:?}", span, name, args);
            }
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
    ByteOutOfRange {
        span: Span,
        value: i32
    },
    WordOutOfRange {
        span: Span,
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
    UnknownInstruction(Span),
    WrongInstructionArgs(Span,String,Vec<Vec<ArgType>>),
    UnexpectedEof,
    FileLoadFailure(String, std::io::Error),
    FileUtf8Error(String, std::string::FromUtf8Error),
    MacroNameConflictsWithInstruction(Span, String),
    MacroAlreadyExists(Span, Span, String),
    DuplicateMacroArg(Span),
    InvalidMacroArg(Span),
    WrongNumberOfMacroArgs(Span, Span, usize, usize),
    DuplicateLabel(Span),
    MacroLabelOutsideOfMacro(Span),
    MacroArgOutsideOfMacro(Span),
    ImmediateValueNotAllowedHere(Span),
    IndirectionModeNotAllowedHere(Span),
    NoSuchMacro(Span, String)
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
            UnknownInstruction(span) => AssemblyError::UnknownInstruction(span),
            WrongInstructionArgs(span, name, types) => AssemblyError::WrongInstructionArgs(span, name, types),
            MacroNameConflictsWithInstruction(span, string) => AssemblyError::MacroNameConflictsWithInstruction(span, string),
            MacroAlreadyExists(span1, span2, string) => AssemblyError::MacroAlreadyExists(span1, span2, string),
            DuplicateMacroArg(span) => AssemblyError::DuplicateMacroArg(span),
            InvalidMacroArg(span) => AssemblyError::InvalidMacroArg(span),
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
            MustBeLiteralNumber(span) => AssemblyError::MustBeLiteralNumber(span.clone()),
            MacroLabelOutsideOfMacro(span) => AssemblyError::MacroLabelOutsideOfMacro(span),
            MacroArgOutsideOfMacro(span) => AssemblyError::MacroArgOutsideOfMacro(span),
            ImmediateValueNotAllowedHere(span) => AssemblyError::ImmediateValueNotAllowedHere(span),
            IndirectionModeNotAllowedHere(span) => AssemblyError::IndirectionModeNotAllowedHere(span),
            InvalidMacroArg(span) => AssemblyError::InvalidMacroArg(span)
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
    for statement in statements.iter() {
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
            Variable(_, _name, _expr) | Alias(_, _name, _expr) => {},
            Comment(_) => {},
            MacroCall(span, name, args) => {
                panic!("Shouldn't be any macro calls left: {}:{}:{:?}", span, name, args);
            }
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
    for statement in statements.iter() {
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
            },
            Comment(_) => {},
            MacroCall(span, name, args) => {
                panic!("Shouldn't be any macro calls left: {}:{}:{:?}", span, name, args);
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

fn expand_macros(statements: &Statements) -> Result<Statements,AssemblyError> {
    let mut new = vec![];
    for stmt in statements.iter() {
        match stmt {
            Statement::MacroCall(span, name, args) => {
                let mut stmts = expand(statements, span.clone(), name.to_owned(), args)?;
                new.append(&mut stmts);
            },
            _ => {
                new.push(stmt.clone());
            }
        }
    }
    Ok(statements.with_statements(new))
}

fn replace_args(inv_span: Span, labels: &HashMap<String,String>, argmap: &HashMap<String,Arg>, args: &[Arg]) -> Result<Vec<Arg>,AssemblyError> {
    let mut new_args = vec![];
    for arg in args {
        match arg {
            Arg::MacroArg(aspan, aname) => {
                match argmap.get(aname) {
                    Some(arg) => {
                        new_args.push(arg.clone());
                    },
                    None => {
                        return Err(AssemblyError::InvalidMacroArg(aspan.clone()))
                    }
                }
            },
            Arg::Imm(expr) => {
                new_args.push(Arg::Imm(expr.replace_macro_args(inv_span.clone(), labels, argmap)?));
            },
            Arg::Ex(expr) => {
                new_args.push(Arg::Ex(expr.replace_macro_args(inv_span.clone(), labels, argmap)?))
            },
            _ => {
                new_args.push(arg.clone());
            }
        }
    }
    Ok(new_args)
}

fn gen_labels(statements: &[MacroStatement]) -> Result<HashMap<String,String>,AssemblyError> {
    let mut map = HashMap::new();
    for stmt in statements.iter() {
        match stmt {
            MacroStatement::MacroLabel(span, name) => {
                if !map.contains_key(name) {
                    let label_name = format!("{}_{}", name.replace("%", ""), Uuid::new_v4().to_string().replace("-", ""));
                    map.insert(name.to_owned(), label_name);
                } else {
                    return Err(AssemblyError::DuplicateLabel(span.clone()));
                }
            },
            _ => {},
        }
    }
    Ok(map)
}

fn expand(stmts: &Statements, span: Span, name: String, args: &[Arg]) -> Result<Vec<Statement>,AssemblyError> {
    let macrodef = match stmts.macro_def(&name) {
        Some(def) => def,
        None => {
            return Err(AssemblyError::NoSuchMacro(span.clone(), name.clone()));
        }
    };
    if args.len() != macrodef.args().len() {
        return Err(AssemblyError::WrongNumberOfMacroArgs(
            span.clone(),
            macrodef.span().clone(),
            macrodef.args().len(),
            args.len()
        ))
    }
    let argmap = {
        let mut argmap = HashMap::new();
        let arg_defs = macrodef.args();
        for idx in 0..args.len() {
            let (_, name) = arg_defs[idx].clone();
            let arg = args[idx].clone();
            argmap.insert(name, arg);
        }
        argmap
    };
    let labels = gen_labels(macrodef.body())?;
    let mut statements = vec![];
    for stmt in macrodef.body().iter() {
        use ast::MacroStatement::*;
        match stmt {
            Instr(ispan, iname, iargs) => {
                match stmts.macro_def(iname) {
                    Some(_def) => {
                        let new_args = replace_args(span.clone(), &labels, &argmap, iargs)?;
                        let mut new_stmts = expand(stmts, ispan.with_parent(span.clone()), iname.to_owned(), new_args.as_slice())?;
                        statements.append(&mut new_stmts);
                    },
                    None => {
                        let new_args = replace_args(span.clone(), &labels, &argmap, iargs)?;
                        let instr = parser::make_instr(ispan.with_parent(span.clone()), iname.to_owned(), new_args.as_slice())?;
                        match instr {
                            Some(ins) => {
                                statements.push(ins);
                            },
                            None => {
                                return Err(AssemblyError::UnknownInstruction(ispan.with_parent(span.clone())));
                            }
                        }
                    }
                }
            },
            Label(span, string) => {
                statements.push(Statement::Label(span.clone(), string.clone()));
            },
            MacroLabel(span, name) => {
                let label_name = labels[name].clone();
                statements.push(Statement::Label(span.clone(), label_name));
            }
        }
    }
    Ok(statements)
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
