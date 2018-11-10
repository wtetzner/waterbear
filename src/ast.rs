
use instruction::Instr;
use expression::{Expr, EvaluationError, Arg, IndirectionMode};
use std::fmt;
use location::{Span, Positioned};
use std::collections::HashMap;

#[derive(Debug,Clone)]
pub enum MacroStatement {
    Instr(Span, String, Vec<Arg>),
    Label(Span, String),
    MacroLabel(Span, String)
}

#[derive(Debug,Clone)]
pub struct MacroDefinition {
    span: Span,
    name: String,
    args: Vec<(Span,String)>,
    body: Vec<MacroStatement>
}

impl MacroDefinition {
    pub fn new(
        span: Span,
        name: String,
        args: Vec<(Span,String)>,
        body: Vec<MacroStatement>
    ) -> MacroDefinition {
        MacroDefinition { span, name, args, body }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> &[(Span,String)] {
        self.args.as_slice()
    }

    pub fn body(&self) -> &[MacroStatement] {
        self.body.as_slice()
    }
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone,Copy)]
pub enum ArgType {
    Imm,
    D9,
    IM,
    B3,
    A12,
    A16,
    R8,
    R16,
    Macro
}

impl ArgType {
    pub fn is_immediate(&self) -> bool {
        if let ArgType::Imm = self {
            true
        } else {
            false
        }
    }

    pub fn is_indirection_mode(&self) -> bool {
        if let ArgType::IM = self {
            true
        } else {
            false
        }
    }

    pub fn is_mem(&self) -> bool {
        match self {
            ArgType::Imm => false,
            ArgType::IM => false,
            ArgType::D9 |
            ArgType::B3 |
            ArgType::A12 |
            ArgType::A16 |
            ArgType::R8 |
            ArgType::R16 => true,
            ArgType::Macro => false
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            ArgType::Imm => "#i8",
            ArgType::D9 => "d9",
            ArgType::IM => "@Ri",
            ArgType::B3 => "b3",
            ArgType::A12 => "a12",
            ArgType::A16 => "a16",
            ArgType::R8 => "r8",
            ArgType::R16 => "r16",
            ArgType::Macro => "%arg"
        }
    }
}

#[derive(Debug,Clone)]
pub enum ByteValue {
    Expr(Expr),
    String(Span, Vec<u8>)
}

impl ByteValue {
    pub fn span(&self) -> Span {
        match self {
            ByteValue::Expr(expr) => expr.span(),
            ByteValue::String(span, _) => span.clone()
        }
    }
}

#[derive(Debug,Clone)]
pub enum IncludeType {
    Asm,
    Bytes
}

impl fmt::Display for IncludeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IncludeType::Asm => write!(f, "asm"),
            IncludeType::Bytes => write!(f, "bytes")
        }
    }
}

#[derive(Debug,Clone)]
pub enum Directive {
    Byte(Span, Vec<ByteValue>),
    Org(Span, usize),
    Word(Span, Vec<Expr>),
    Include(Span, IncludeType, String),
    Cnop(Span, Expr,Expr)
}

impl Directive {
    fn eval_cnop_expr(expr: &Expr) -> Result<i32,EvaluationError> {
        use expression::Expr::*;
        match expr {
            Number(_, num) => Ok(*num),
            _ => Err(EvaluationError::MustBeLiteralNumber(expr.span()))
        }
    }

    pub fn eval_cnop(pos: i32, add: &Expr, multiple: &Expr) -> Result<i32,EvaluationError> {
        let add = Directive::eval_cnop_expr(add)?;
        let multiple = Directive::eval_cnop_expr(multiple)?;
        if multiple == 0 {
            Ok(add)
        } else {
            let mut mult = 0;
            loop {
                if pos <= mult {
                    break;
                }
                mult = mult + multiple;
            }
            Ok(mult + add)
        }
    }

    pub fn size(&self, pos: i32) -> Result<i32,EvaluationError> {
        use self::Directive::*;
        match self {
            Byte(_, bytes) => {
                let mut count: usize = 0;
                for item in bytes.iter() {
                    match item {
                        ByteValue::Expr(_) => count = count + 1,
                        ByteValue::String(_, vals) => count = count + vals.len()
                    }
                }
                Ok(count as i32)
            },
            Org(_, location) => Ok((*location as i32) - pos),
            Word(_, words) => Ok((words.len() * 2) as i32),
            Include(_,_,_) => Ok(0),
            Cnop(_, add, multiple) => Ok(Directive::eval_cnop(pos, add, multiple)? - pos)
        }
    }

    pub fn span(&self) -> Span {
        use self::Directive::*;
        match self {
            Byte(span, _) => span.clone(),
            Org(span, _) => span.clone(),
            Word(span, _) => span.clone(),
            Include(span, _, _) => span.clone(),
            Cnop(span, _, _) => span.clone()
        }
    }
}

impl Positioned for Directive {
    fn span(&self) -> Span {
        use self::Directive::*;
        match self {
            Byte(span, _) => span.clone(),
            Org(span, _) => span.clone(),
            Word(span, _) => span.clone(),
            Include(span, _,_) => span.clone(),
            Cnop(span, _, _) => span.clone()
        }
    }
}

impl fmt::Display for ByteValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ByteValue::Expr(expr) => match expr {
                Expr::Number(_, num) => write!(f, "${:02X}", num),
                _ => write!(f, "{}", self)
            },
            ByteValue::String(_, vec) => {
                write!(f, "\"{}\"", escape_string(vec))
            }
        }
    }
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Directive::Cnop(_, exp1, exp2) => {
                write!(f, ".cnop ")?;
                write!(f, "{}", exp1)?;
                write!(f, ", ")?;
                write!(f, "{}", exp2)
            },
            Directive::Byte(_, vec) => {
                write!(f, ".byte ")?;
                let mut first = true;
                for b in vec {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", b)?;
                }
                write!(f, "")
            },
            Directive::Org(_, num) => if *num == 0 {
                write!(f, ".org {}", num)
            } else {
                write!(f, ".org ${:04X}", num)
            },
            Directive::Word(_, vec) => {
                write!(f, ".word ")?;
                let mut first = true;
                for b in vec {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", b)?;
                }
                write!(f, "")
            },
            Directive::Include(_, typ, path) => {
                match typ {
                    IncludeType::Asm => write!(f, ".include \"{}\"", path),
                    _ => write!(f, ".include {} \"{}\"", typ, path)
                }
            }
        }
    }
}

#[derive(Debug,Clone)]
pub enum Statement {
    Directive(Directive),
    Label(Span, String),
    Instr(Span, Instr<Expr,IndirectionMode>),
    Variable(Span, String, Expr),
    Alias(Span, String, Expr),
    Comment(String),
    MacroCall(Span, String, Vec<Arg>)
}

impl Statement {
    pub fn is_label(&self) -> bool {
        match self {
            Statement::Label(_,_) => true,
            _ => false
        }
    }

    pub fn comment(text: &str) -> Statement {
        Statement::Comment(text.to_owned())
    }

    pub fn label(text: &str) -> Statement {
        Statement::Label(Span::default(), text.to_owned())
    }

    pub fn instr(instr: Instr<Expr,IndirectionMode>) -> Statement {
        Statement::Instr(Span::default(), instr)
    }
}

impl Positioned for Statement {
    fn span(&self) -> Span {
        use self::Statement::*;
        match self {
            Directive(dir) => dir.span(),
            Label(span, _) => span.clone(),
            Instr(span,_) => span.clone(),
            Variable(span, _, _) => span.clone(),
            Alias(span, _, _) => span.clone(),
            Comment(_) => Span::default(),
            MacroCall(span, name, args) => span.clone()
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Directive(dir) => {
                write!(f, "{}", dir)
            },
            Statement::Label(_, text) => {
                write!(f, "{}:", text)
            },
            Statement::Instr(_, inst) => write!(f, "  {}", inst),
            Statement::Variable(_, name, expr) => {
                write!(f, "{} = {}", name, expr)
            },
            Statement::Alias(_, name, expr) => {
                write!(f, "{} EQU {}", name, expr)
            },
            Statement::Comment(ref comment) => {
                if comment.trim().is_empty() {
                    write!(f, "{}", comment)
                } else if comment.starts_with("\n") {
                    writeln!(f, "")?;
                    write!(f, "; {}", &comment[1..comment.len()])
                } else {
                    write!(f, "; {}", comment)
                }
            },
            Statement::MacroCall(span, name, args) => {
                write!(f, "{}", name)?;
                for arg in args.iter() {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct Statements {
    macros: HashMap<String,MacroDefinition>,
    statements: Vec<Statement>,
}

impl Statements {
    pub fn new(
        macros: HashMap<String,MacroDefinition>,
        statements: Vec<Statement>
    ) -> Statements {
        Statements { macros, statements }
    }

    pub fn as_slice(&self) -> &[Statement] {
        self.statements.as_slice()
    }

    pub fn macro_def(&self, name: &str) -> Option<&MacroDefinition> {
        self.macros.get(name)
    }

    pub fn iter(&self) -> impl Iterator<Item=&Statement> {
        self.statements.iter()
    }

    pub fn with_statements(&self, stmts: Vec<Statement>) -> Statements {
        Statements::new(self.macros.clone(), stmts)
    }
}

impl fmt::Display for Statements {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for st in self.statements.iter() {
            if first {
                first = false;
            } else {
                write!(f, "\n")?;
            }
            write!(f, "{}", st)?;
        }
        write!(f, "")
    }
}

fn escape_string<'a>(bytes: &Vec<u8>) -> String {
    let mut escaped = Vec::new();
    let mut index = 0;
    loop {
        if index >= bytes.len() {
            break;
        }
        let current = bytes[index];
        match current {
            // \a
            0x07 => {
                escaped.push(0x5C);
                escaped.push(0x61)
            },

            // \b
            0x08 => {
                escaped.push(0x5C);
                escaped.push(0x62)
            },

            // \f
            0x0C => {
                escaped.push(0x5C);
                escaped.push(0x66)
            },

            // \n
            0x0A => {
                escaped.push(0x5C);
                escaped.push(0x6E)
            },

            // \r
            0x0D => {
                escaped.push(0x5C);
                escaped.push(0x72)
            },

            // \t
            0x09 => {
                escaped.push(0x5C);
                escaped.push(0x74)
            },

            // \v
            0x0B => {
                escaped.push(0x5C);
                escaped.push(0x76)
            },

            _ => escaped.push(current)
        }
        index = index + 1;
    }
    String::from_utf8(escaped).unwrap()
}
