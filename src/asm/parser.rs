
use pest::{StringInput, Token, Parser, Input};
use asm::ast::*;
use asm::instruction::*;
use asm::expression::*;

#[derive(Debug,Clone)]
enum Arg {
    I8(Expression),
    Ri(IndirectionMode),
    Mem(Expression)
}

#[derive(Debug,Clone)]
enum Args {
    Empty,
    One(Arg),
    Two(Arg,Arg),
    Three(Arg,Arg,Arg),
    Many(Vec<Arg>)
}

impl Args {
    fn prepend(self, arg: Arg) -> Args {
        match self {
            Args::Empty => Args::One(arg),
            Args::One(arg1) => Args::Two(arg, arg1),
            Args::Two(arg1, arg2) => Args::Three(arg, arg1, arg2),
            Args::Three(arg1, arg2, arg3) => Args::Many(vec![arg, arg1, arg2, arg3]),
            Args::Many(vec) => {
                let mut result = Vec::with_capacity(vec.len() + 1);
                result.push(arg);
                for a in vec {
                    result.push(a);
                }
                Args::Many(result)
            }
        }
    }
}

impl_rdp! {
    grammar! {
        statements = { statement* }
        statement = _{ label | instruction }

        num_expression = _{
            { ["("] ~ num_expression ~ [")"] | upper_byte | lower_byte | number | name }
            addition       = { plus  | minus }
            multiplication = { times | slash }
        }
        number = _{ hex_num | oct_num | dec_num }
        dec_num = @{ ["0"] | (['1'..'9'] ~ ['0'..'9']*) }
        oct_num = @{ ["0"] ~ ['0'..'9']+ }
        hex_num = _{ hex_num1 | hex_num2 }
        hex_num1 = @{ ["$"] ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
        hex_num2 = @{ ["0"] ~ (["X"] | ["x"]) ~ (['a'..'f'] | ['A'..'F'] | ['0'..'9'])+ }
        plus   =  { ["+"] }
        minus  =  { ["-"] }
        times  =  { ["*"] }
        slash  =  { ["/"] }

        upper_byte = { [">"] ~ num_expression }
        lower_byte = { ["<"] ~ num_expression }

        directive = @{ ["."] ~ name }
        label = { name ~ [":"] }

        name = @{ (['a'..'z'] | ['A'..'Z'] | ["_"] | ["."]) ~ (['a'..'z'] | ['A'..'Z'] | ["$"] | ["_"] | ["."] | ['0'..'9'])* }

        instruction = { name ~ args? ~ &newline }
        args = { (arg ~ ([","] ~ arg)*)}

        arg = _{ imm8 | ri | mem }

        imm8 = { ["#"] ~ num_expression }
        ri = { ["@R0"] | ["@R1"] | ["@R2"] | ["@R3"] | ["@r0"] | ["@r1"] | ["@r2"] | ["@r3"]}
        mem = { num_expression }

        whitespace = _{ [" "] | ["\t"] | ["\r"] | ["\n"] | comment_semi }
        comment_semi = _{ [";"] ~ (!newline)* ~ newline }
        newline = _{ ["\r\n"] | ["\n"] }
    }

    process! {
        to_args(&self) -> Result<Args,EvaluationError> {
            (_: args, head: to_arg(), mut tail: to_args()) => {
                let head: Arg = head?;
                let tail: Args = tail?;
                Ok(tail.prepend(head))
            },
            () => { Ok(Args::Empty) }
        }

        to_arg(&self) -> Result<Arg,EvaluationError> {
            (_: imm8, expr: to_expr()) => Ok(Arg::I8(expr?)),
            (&ri: ri) => match ri.to_lowercase().as_ref() {
                "@r0" => Ok(Arg::Ri(IndirectionMode::R0)),
                "@r1" => Ok(Arg::Ri(IndirectionMode::R1)),
                "@r2" => Ok(Arg::Ri(IndirectionMode::R2)),
                "@r3" => Ok(Arg::Ri(IndirectionMode::R3)),
                _ => unreachable!()
            },
            (_: mem, expr: to_expr()) => Ok(Arg::Mem(expr?))
        }

        // enum Arg {
        //     I8(Expression),
        //     Ri(IndirectionMode),
        //     Mem(Expression)
        // }
        to_instr(&self) -> Result<Instruction<Expression,Expression,Expression,Expression,Expression>,EvaluationError> {
            (_:instruction, &name: name, args: to_args()) => {
                let args = args?;
                println!("name: {}, args: {:?}", name.to_lowercase(), &args);
                match (name.to_lowercase().as_ref(), args) {
                    ("add", Args::One(Arg::I8(i8))) => Ok(Instruction::Add_i8(i8)),
                    ("add", Args::One(Arg::Mem(mem))) => Ok(Instruction::Add_d9(mem)),
                    ("add", Args::One(Arg::Ri(ri))) => Ok(Instruction::Add_Ri(ri)),
                    ("add", args) => Err(EvaluationError::Failure("Invalid add instruction".to_string())),

                    ("addc", Args::One(Arg::I8(i8))) => Ok(Instruction::Addc_i8(i8)),
                    ("addc", Args::One(Arg::Mem(mem))) => Ok(Instruction::Addc_d9(mem)),
                    ("addc", Args::One(Arg::Ri(ri))) => Ok(Instruction::Addc_Ri(ri)),
                    ("addc", args) => Err(EvaluationError::Failure("Invalid addc instruction".to_string())),

                    (n,_) => Err(EvaluationError::Failure(format!("Invalid instruction: {}", n)))
                }
            }
        }

        // Statement<Expression,Expression,Expression,Expression,Expression>
        to_statement(&self) -> Result<Statement<Expression,Expression,Expression,Expression,Expression>,EvaluationError> {
            (_: label, &name: name) => Ok(Statement::Label(name.to_string())),
            (_: instruction, instruction: to_instr()) => Ok(Statement::Instruction(instruction?)),
            (_: directive) => Ok(Statement::Label("directive".to_string())),
            (_: hex_num1) => Ok(Statement::Label("hex_num1".to_string())),
            (_: name) => Ok(Statement::Label("name".to_string())),
            (_: statements) => Ok(Statement::Label("statements".to_string())),
            () => Ok(Statement::Label("huh?".to_string()))
        }

        to_statements(&self) -> Result<Vec<Statement<Expression,Expression,Expression,Expression,Expression>>,EvaluationError> {
            (_: statements, head: to_statement(), mut tail: to_statements()) => {
                let head = head?;
                let mut tail = tail?;
                tail.insert(0, head);
                Ok(tail)
            },
            () => {
                Ok(Vec::new())
            }
        }

        to_expr(&self) -> Result<Expression,EvaluationError> {
            (&number: dec_num) => Ok(Expression::Number(number.parse::<i32>()?)),
            (&number: oct_num) =>  Ok(Expression::Number(i32::from_str_radix(number, 8)?)),
            (&number: hex_num1) => Ok(Expression::Number(i32::from_str_radix(&number[1..], 16)?)),
            (&number: hex_num2) =>  Ok(Expression::Number(i32::from_str_radix(&number[2..], 16)?)),
            (_: upper_byte, value: to_expr()) => Ok(Expression::UpperByte(Box::new(value?))),
            (_: lower_byte, value: to_expr()) => Ok(Expression::LowerByte(Box::new(value?))),
            (&name: name) => Ok(Expression::Name(name.to_string())),
            (_: addition, left: to_expr(), sign, right: to_expr()) => {
                match sign.rule {
                    Rule::plus  => Ok(Expression::Plus(Box::new(left?), Box::new(right?))),
                    Rule::minus => Ok(Expression::Minus(Box::new(left?), Box::new(right?))),
                    _ => unreachable!()
                }
            },
            (_: multiplication, left: to_expr(), sign, right: to_expr()) => {
                match sign.rule {
                    Rule::times => Ok(Expression::Times(Box::new(left?), Box::new(right?))),
                    Rule::slash => Ok(Expression::Divide(Box::new(left?), Box::new(right?))),
                    _ => unreachable!()
                }
            }
        }
    }
}

pub fn run_parser() {
    let text = include_str!("../../example.s").trim().to_string() + "\n";
    let mut parser = Rdp::new(StringInput::new(&text));

    assert!(parser.statements());
    // assert!(parser.end());

    let result = parser.to_statements();
    println!("result: {:?}", result);

    // assert_eq!(result, 44);
}

