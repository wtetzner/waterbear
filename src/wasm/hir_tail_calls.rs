//! Recognition and canonicalization of VMU HIR tail calls.
//!
//! WebAssembly producers do not need to emit the tail-call proposal's
//! `return_call` instruction for the VMU backend to benefit. This pass also
//! recognizes the ordinary call-and-return shape emitted by existing
//! compilers. Recognition is deliberately semantic and conservative: the
//! returned value must be the call result itself, without observable post-call
//! work.

use super::hir::{
    Block, Expression, ExpressionKind, FunctionType, Place, Program, Statement, Type,
};

/// Replaces calls whose results flow unchanged into a function return with the
/// explicit HIR tail-call form.
pub fn canonicalize(program: &mut Program) {
    for function in program.all_functions_mut() {
        canonicalize_function(function);
    }
}

fn canonicalize_function(function: &mut super::hir::Function) {
    canonicalize_block(&mut function.body);
}

fn canonicalize_block(block: &mut Block) {
    for statement in &mut block.statements {
        match statement {
            Statement::Block { body, .. } | Statement::Loop { body, .. } => {
                canonicalize_block(body);
            }
            Statement::If {
                then_body,
                else_body,
                ..
            } => {
                canonicalize_block(then_body);
                if let Some(else_body) = else_body {
                    canonicalize_block(else_body);
                }
            }
            Statement::Switch { cases, default, .. } => {
                for case in cases {
                    canonicalize_block(&mut case.body);
                }
                canonicalize_block(default);
            }
            _ => {}
        }
    }

    if let Some(candidate) = tail_candidate(&block.statements) {
        replace_tail_call(&mut block.statements, candidate);
        return;
    }

    let Some(Statement::Return(returned)) = block.statements.last() else {
        return;
    };
    let Some(result) = TailResult::from_return(returned) else {
        return;
    };
    let prefix_length = block.statements.len() - 1;
    canonicalize_tail_predecessor(&mut block.statements[..prefix_length], &result);
}

// Structured WASM results lower to assignments into a join variable followed
// by a return outside the enclosing block. Follow that returned variable into
// each branch so those calls are not hidden by the join.
fn canonicalize_tail_block(block: &mut Block, result: &TailResult) {
    if let Some(candidate) = tail_path_candidate(&block.statements, result) {
        replace_tail_call(&mut block.statements, candidate);
        return;
    }
    canonicalize_tail_predecessor(&mut block.statements, result);
}

fn canonicalize_tail_predecessor(statements: &mut [Statement], result: &TailResult) {
    let Some(statement) = statements.last_mut() else {
        return;
    };
    match statement {
        Statement::Block { body, .. } => canonicalize_tail_block(body, result),
        Statement::If {
            then_body,
            else_body,
            ..
        } => {
            canonicalize_tail_block(then_body, result);
            if let Some(else_body) = else_body {
                canonicalize_tail_block(else_body, result);
            }
        }
        Statement::Switch { cases, default, .. } => {
            for case in cases {
                canonicalize_tail_block(&mut case.body, result);
            }
            canonicalize_tail_block(default, result);
        }
        _ => {}
    }
}

fn replace_tail_call(statements: &mut Vec<Statement>, candidate: TailCandidate) {
    statements.truncate(candidate.call_index);
    remove_temporary(statements, candidate.temporary.as_deref());
    statements.push(match candidate.target {
        TailTarget::Direct(function) => Statement::TailCall {
            function,
            arguments: candidate.arguments,
        },
        TailTarget::Indirect { target, signature } => Statement::TailCallIndirect {
            target,
            signature,
            arguments: candidate.arguments,
        },
    });
}

fn tail_path_candidate(statements: &[Statement], result: &TailResult) -> Option<TailCandidate> {
    match result {
        TailResult::Unit => {
            let index = statements.len().checked_sub(1)?;
            let call = call_info(&statements[index])?;
            if call.destination.is_some() {
                return None;
            }
            Some(TailCandidate {
                call_index: index,
                temporary: None,
                target: call.target,
                arguments: call.arguments,
            })
        }
        TailResult::Place(result) => {
            if let Some(index) = statements.len().checked_sub(1)
                && let Some(call) = call_info(&statements[index])
                && let Some(destination @ Place::Variable(temporary)) = &call.destination
                && destination == result
            {
                return Some(TailCandidate {
                    call_index: index,
                    temporary: Some(temporary.clone()),
                    target: call.target,
                    arguments: call.arguments,
                });
            }

            let [.., call, assignment] = statements else {
                return None;
            };
            let call = call_info(call)?;
            let Some(destination @ Place::Variable(temporary)) = &call.destination else {
                return None;
            };
            let Statement::Assign {
                destination: assigned,
                value,
            } = assignment
            else {
                return None;
            };
            if assigned != result || !forwards_call_result(value, destination) {
                return None;
            }
            Some(TailCandidate {
                call_index: statements.len() - 2,
                temporary: Some(temporary.clone()),
                target: call.target,
                arguments: call.arguments,
            })
        }
    }
}

fn remove_temporary(statements: &mut Vec<Statement>, temporary: Option<&str>) {
    if temporary.is_some_and(|temporary| {
        matches!(
            statements.last(),
            Some(Statement::Declare {
                name,
                initializer: None,
                ..
            }) if name == temporary
        )
    }) {
        statements.pop();
    }
}

struct TailCandidate {
    call_index: usize,
    target: TailTarget,
    arguments: Vec<Expression>,
    temporary: Option<String>,
}

enum TailTarget {
    Direct(String),
    Indirect {
        target: Expression,
        signature: FunctionType,
    },
}

struct CallInfo {
    destination: Option<Place>,
    target: TailTarget,
    arguments: Vec<Expression>,
}

fn call_info(statement: &Statement) -> Option<CallInfo> {
    match statement {
        Statement::Call {
            destination,
            function,
            arguments,
        } => Some(CallInfo {
            destination: destination.clone(),
            target: TailTarget::Direct(function.clone()),
            arguments: arguments.clone(),
        }),
        Statement::CallIndirect {
            destination,
            target,
            signature,
            arguments,
        } => Some(CallInfo {
            destination: destination.clone(),
            target: TailTarget::Indirect {
                target: target.clone(),
                signature: signature.clone(),
            },
            arguments: arguments.clone(),
        }),
        _ => None,
    }
}

fn tail_candidate(statements: &[Statement]) -> Option<TailCandidate> {
    let [.., call, return_] = statements else {
        return None;
    };
    let call = call_info(call)?;
    let Statement::Return(returned) = return_ else {
        return None;
    };

    let temporary = match (&call.destination, returned) {
        (None, None) => None,
        (Some(destination @ Place::Variable(name)), Some(returned))
            if forwards_call_result(returned, destination) =>
        {
            Some(name.clone())
        }
        _ => return None,
    };
    Some(TailCandidate {
        call_index: statements.len() - 2,
        target: call.target,
        arguments: call.arguments,
        temporary,
    })
}

fn forwards_call_result(expression: &Expression, destination: &Place) -> bool {
    forwarded_place(expression).is_some_and(|place| place == destination)
}

fn forwarded_place(expression: &Expression) -> Option<&Place> {
    match &expression.kind {
        ExpressionKind::Read(place) => Some(place),
        ExpressionKind::Aggregate(elements) => {
            let Type::Tuple(fields) = &expression.ty else {
                return None;
            };
            let first = elements
                .first()
                .and_then(|element| projected_place(element, 0))?;
            (fields.len() == elements.len()
                && elements.iter().enumerate().all(|(index, element)| {
                    element.ty == fields[index]
                        && projected_place(element, index).is_some_and(|place| place == first)
                }))
            .then_some(first)
        }
        _ => None,
    }
}

fn projected_place(expression: &Expression, expected_index: usize) -> Option<&Place> {
    let ExpressionKind::ExtractElement { aggregate, index } = &expression.kind else {
        return None;
    };
    if *index != expected_index {
        return None;
    }
    forwarded_place(aggregate)
}

enum TailResult {
    Unit,
    Place(Place),
}

impl TailResult {
    fn from_return(returned: &Option<Expression>) -> Option<Self> {
        match returned {
            None => Some(Self::Unit),
            Some(expression) => match forwarded_place(expression) {
                Some(place @ Place::Variable(_)) => Some(Self::Place(place.clone())),
                _ => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::canonicalize_block;
    use crate::wasm::hir::{
        BitWidth, Block, ConversionOp, Expression, ExpressionKind, FunctionType, Place, Statement,
        Type,
    };

    fn bits(width: u8) -> Type {
        Type::Bits(BitWidth::new(width).unwrap())
    }

    fn read(name: &str, ty: Type) -> Expression {
        Expression {
            ty,
            kind: ExpressionKind::Read(Place::Variable(name.into())),
        }
    }

    #[test]
    fn canonicalizes_a_returned_call_and_removes_its_temporary() {
        let ty = bits(8);
        let mut block = Block {
            statements: vec![
                Statement::Declare {
                    name: "result".into(),
                    ty: ty.clone(),
                    mutable: true,
                    initializer: None,
                },
                Statement::Call {
                    destination: Some(Place::Variable("result".into())),
                    function: "callee".into(),
                    arguments: Vec::new(),
                },
                Statement::Return(Some(read("result", ty))),
            ],
        };

        canonicalize_block(&mut block);

        assert_eq!(
            block.statements,
            vec![Statement::TailCall {
                function: "callee".into(),
                arguments: Vec::new(),
            }]
        );
    }

    #[test]
    fn canonicalizes_a_returned_indirect_call() {
        let ty = bits(8);
        let target = read("function", Type::Pointer);
        let signature = FunctionType {
            parameters: vec![ty.clone()],
            result: ty.clone(),
        };
        let mut block = Block {
            statements: vec![
                Statement::Declare {
                    name: "result".into(),
                    ty: ty.clone(),
                    mutable: true,
                    initializer: None,
                },
                Statement::CallIndirect {
                    destination: Some(Place::Variable("result".into())),
                    target: target.clone(),
                    signature: signature.clone(),
                    arguments: vec![read("argument", ty.clone())],
                },
                Statement::Return(Some(read("result", ty.clone()))),
            ],
        };

        canonicalize_block(&mut block);

        assert_eq!(
            block.statements,
            vec![Statement::TailCallIndirect {
                target,
                signature,
                arguments: vec![read("argument", ty)],
            }]
        );
    }

    #[test]
    fn leaves_calls_with_post_call_conversions_alone() {
        let source = bits(8);
        let target = bits(16);
        let mut block = Block {
            statements: vec![
                Statement::Call {
                    destination: Some(Place::Variable("result".into())),
                    function: "callee".into(),
                    arguments: Vec::new(),
                },
                Statement::Return(Some(Expression {
                    ty: target,
                    kind: ExpressionKind::Convert {
                        op: ConversionOp::ZeroExtend,
                        operand: Box::new(read("result", source)),
                    },
                })),
            ],
        };

        canonicalize_block(&mut block);

        assert!(matches!(block.statements[0], Statement::Call { .. }));
        assert!(matches!(block.statements[1], Statement::Return(_)));
    }

    #[test]
    fn follows_a_returned_join_variable_into_structured_branches() {
        let ty = bits(8);
        let mut block = Block {
            statements: vec![
                Statement::Declare {
                    name: "joined".into(),
                    ty: ty.clone(),
                    mutable: true,
                    initializer: None,
                },
                Statement::If {
                    condition: read("condition", Type::Bool),
                    then_body: Block {
                        statements: vec![
                            Statement::Declare {
                                name: "result".into(),
                                ty: ty.clone(),
                                mutable: true,
                                initializer: None,
                            },
                            Statement::Call {
                                destination: Some(Place::Variable("result".into())),
                                function: "callee".into(),
                                arguments: Vec::new(),
                            },
                            Statement::Assign {
                                destination: Place::Variable("joined".into()),
                                value: read("result", ty.clone()),
                            },
                        ],
                    },
                    else_body: Some(Block {
                        statements: vec![Statement::Assign {
                            destination: Place::Variable("joined".into()),
                            value: read("fallback", ty.clone()),
                        }],
                    }),
                },
                Statement::Return(Some(read("joined", ty))),
            ],
        };

        canonicalize_block(&mut block);

        let Statement::If {
            then_body,
            else_body,
            ..
        } = &block.statements[1]
        else {
            panic!("expected the conditional to remain")
        };
        assert!(matches!(
            then_body.statements.as_slice(),
            [Statement::TailCall { function, .. }] if function == "callee"
        ));
        assert!(matches!(
            else_body.as_ref().unwrap().statements.as_slice(),
            [Statement::Assign { .. }]
        ));
        assert!(matches!(block.statements[2], Statement::Return(_)));
    }
}
