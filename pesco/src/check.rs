use crate::parse::Token;
use crate::parse::TokenKind;
use std::process;

macro_rules! exit(
    () => {
        {
            process::exit(1);
        }
    };
    ($($arg:tt)*) => {
        {
            eprintln!($($arg)*);
            process::exit(1);
        }
    };
);

#[derive(Debug, PartialEq, Clone, Copy)]
enum DataType {
    Int,
    Flt,
    Bool,
    Str,
}

pub fn check(tokens: &[Token]) {
    let mut stack: Vec<DataType> = Vec::with_capacity(1024);
    let mut conds: Vec<(TokenKind, usize, usize)> = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        let t = &tokens[i];

        match t.kind {
            TokenKind::IntLit => stack.push(DataType::Int),
            TokenKind::FltLit => stack.push(DataType::Flt),
            TokenKind::StrLit => stack.push(DataType::Str),
            TokenKind::KwTrue => stack.push(DataType::Bool),
            TokenKind::KwFalse => stack.push(DataType::Bool),

            TokenKind::OpDup => stack.push(*stack.last().unwrap_or_else(|| {
                exit!(
                    "line {} - not enough elements on stack to perform 'dup' operation",
                    t.line
                )
            })),
            TokenKind::Op2Dup => {
                stack.push(*stack.get(stack.len() - 2).unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '2dup' operation",
                        t.line
                    )
                }));
                stack.push(*stack.get(stack.len() - 2).unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '2dup' operation",
                        t.line
                    )
                }));
            }
            TokenKind::OpOver => stack.push(*stack.get(stack.len() - 2).unwrap_or_else(|| {
                exit!(
                    "{} - not enough elements on stack to perform 'over' operation",
                    t.line
                )
            })),
            TokenKind::OpRot => {
                let c = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'rot' operation",
                        t.line
                    )
                });
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'rot' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'rot' operation",
                        t.line
                    )
                });
                stack.extend([c, a, b]);
            }
            TokenKind::OpIRot => {
                let c = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'irot' operation",
                        t.line
                    )
                });
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'irot' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'irot' operation",
                        t.line
                    )
                });
                stack.extend([b, c, a]);
            }
            TokenKind::OpSwap => {
                let len = stack.len();
                if len < 2 {
                    exit!(
                        "line {} -  not enough elements on stack to perform 'swap' operation",
                        t.line
                    );
                }
                stack.swap(len - 2, len - 1);
            }
            TokenKind::OpPop => {
                stack
                    .pop()
                    .unwrap_or_else(|| exit!("line {} - cannot 'pop' empty stack", t.line));
            }

            TokenKind::OpAdd => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'add' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'add' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform 'add' operation",
                        t.line
                    );
                } else if a == DataType::Flt || b == DataType::Flt {
                    stack.push(DataType::Flt);
                } else {
                    stack.push(DataType::Int);
                }
            }
            TokenKind::OpSub => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'sub' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'sub' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform 'sub' operation",
                        t.line
                    );
                } else if a == DataType::Flt || b == DataType::Flt {
                    stack.push(DataType::Flt);
                } else {
                    stack.push(DataType::Int);
                }
            }
            TokenKind::OpMul => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'mul' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'mul' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform 'mul' operation",
                        t.line
                    );
                } else if a == DataType::Flt || b == DataType::Flt {
                    stack.push(DataType::Flt);
                } else {
                    stack.push(DataType::Int);
                }
            }
            TokenKind::OpDiv => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'div' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform 'div' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform 'div' operation",
                        t.line
                    );
                } else if a == DataType::Flt || b == DataType::Flt {
                    stack.push(DataType::Flt);
                } else {
                    stack.push(DataType::Int);
                }
            }

            TokenKind::OpLT => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '<' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '<' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform '<' operation",
                        t.line
                    );
                } else {
                    stack.push(DataType::Bool);
                }
            }
            TokenKind::OpGT => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '>' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '>' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform '>' operation",
                        t.line
                    );
                } else {
                    stack.push(DataType::Bool);
                }
            }
            TokenKind::OpLE => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '<=' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '<=' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform '<=' operation",
                        t.line
                    );
                } else {
                    stack.push(DataType::Bool);
                }
            }
            TokenKind::OpGE => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '>=' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '>=' operation",
                        t.line
                    )
                });
                if (a != DataType::Int && a != DataType::Flt)
                    || (b != DataType::Int && b != DataType::Flt)
                {
                    exit!(
                        "line {} - incompatible types to perform '>=' operation",
                        t.line
                    );
                } else {
                    stack.push(DataType::Bool);
                }
            }
            TokenKind::OpEQ => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '==' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '==' operation",
                        t.line
                    )
                });
                if a != b {
                    if (a != DataType::Int && a != DataType::Flt)
                        || (b != DataType::Int && b != DataType::Flt)
                    {
                        exit!(
                            "line {} - incompatible types to perform '!=' operation",
                            t.line
                        );
                    }
                    stack.push(DataType::Bool);
                } else {
                    stack.push(DataType::Bool);
                }
            }
            TokenKind::OpNE => {
                let b = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '!=' operation",
                        t.line
                    )
                });
                let a = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - not enough elements on stack to perform '!=' operation",
                        t.line
                    )
                });
                if a != b {
                    if (a != DataType::Int && a != DataType::Flt)
                        || (b != DataType::Int && b != DataType::Flt)
                    {
                        exit!(
                            "line {} - incompatible types to perform '!=' operation",
                            t.line
                        );
                    }
                    stack.push(DataType::Bool);
                } else {
                    stack.push(DataType::Bool);
                }
            }

            TokenKind::OpOut => {
                stack.pop().unwrap_or_else(|| {
                    exit!("line {} - no elements on stack to '->' print", t.line)
                });
            }
            TokenKind::KwPuts => {
                stack
                    .pop()
                    .unwrap_or_else(|| exit!("line {} - no elements on stack to 'puts'", t.line));
            }
            TokenKind::OpErr => {
                stack.pop().unwrap_or_else(|| {
                    exit!("line {} - no elements on stack to '->' print", t.line)
                });
            }
            TokenKind::OpIn => unimplemented!(),

            TokenKind::KwIf => conds.push((TokenKind::KwIf, i, stack.len())),
            TokenKind::KwWhile => conds.push((TokenKind::KwWhile, i, stack.len())),
            TokenKind::KwDo => {
                if stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - keyword 'do' must be preceded by boolean expression",
                        t.line
                    )
                }) != DataType::Bool
                {
                    exit!(
                        "line {} - keyword 'do' must be preceded by boolean expression",
                        t.line
                    );
                }
            }
            TokenKind::KwEnd => {
                let (kind, _, len) = conds.pop().unwrap();
                if kind == TokenKind::KwWhile && len != stack.len() {
                    exit!("line {} - while loop may not modify stack size", t.line);
                }
            }

            TokenKind::OpArr => unimplemented!(),

            TokenKind::KwStr => {
                stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - no enough elements on stack to cast to 'str'",
                        t.line
                    )
                });
                stack.push(DataType::Str);
            }
            TokenKind::KwInt => {
                stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - no enough elements on stack to cast to 'int'",
                        t.line
                    )
                });
                stack.push(DataType::Int);
            }
            TokenKind::KwFlt => {
                let v = stack.pop().unwrap_or_else(|| {
                    exit!(
                        "line {} - no enough elements on stack to cast to 'float'",
                        t.line
                    )
                });
                if v == DataType::Bool {
                    exit!("line {} - 'bool' cannot be cast to 'float'", t.line);
                } else {
                    stack.push(DataType::Flt);
                }
            }

            TokenKind::Ident => unimplemented!(),

            _ => (),
        }

        i += 1;
    }
}
