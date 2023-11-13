use crate::data::Data;
use crate::parse::Token;
use crate::parse::TokenKind;

pub fn eval(tokens: &[Token]) {
    let mut conds: Vec<(TokenKind, usize)> = Vec::new();
    let mut stack: Vec<Data> = Vec::with_capacity(1024);
    let mut seek_end = false;
    let mut i = 0;

    while i < tokens.len() {
        let t = &tokens[i];

        if seek_end {
            if t.kind == TokenKind::KwEnd {
                seek_end = false;
            }
            i += 1;
            continue;
        }

        match t.kind {
            TokenKind::IntLit | TokenKind::FltLit | TokenKind::StrLit => stack.push(t.val.clone()),
            TokenKind::KwTrue => stack.push(Data::Bool(true)),
            TokenKind::KwFalse => stack.push(Data::Bool(false)),

            TokenKind::OpDup => stack.push(stack.last().unwrap().clone()),
            TokenKind::Op2Dup => {
                stack.push(stack[stack.len() - 2].clone());
                stack.push(stack[stack.len() - 2].clone());
            }
            TokenKind::OpOver => stack.push(stack[stack.len() - 2].clone()),
            TokenKind::OpRot => {
                let c = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.extend([c, a, b]);
            }
            TokenKind::OpIRot => {
                let c = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.extend([b, c, a]);
            }
            TokenKind::OpSwap => {
                let len = stack.len();
                stack.swap(len - 2, len - 1);
            }
            TokenKind::OpPop => {
                stack.pop().unwrap();
            }

            TokenKind::OpAdd => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.add(&b).unwrap());
            }
            TokenKind::OpSub => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.sub(&b).unwrap());
            }
            TokenKind::OpMul => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.mul(&b).unwrap());
            }
            TokenKind::OpDiv => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.div(&b).unwrap());
            }
            TokenKind::_AddImm => {
                let a = stack.pop().unwrap();
                stack.push(a.add(&t.val).unwrap());
            }
            TokenKind::_SubImm => {
                let a = stack.pop().unwrap();
                stack.push(a.sub(&t.val).unwrap());
            }
            TokenKind::_MulImm => {
                let a = stack.pop().unwrap();
                stack.push(a.mul(&t.val).unwrap());
            }
            TokenKind::_DivImm => {
                let a = stack.pop().unwrap();
                stack.push(a.div(&t.val).unwrap());
            }
            TokenKind::_OpInc => {
                let a = stack.pop().unwrap();
                stack.push(a.add(&Data::Int(1)).unwrap());
            }
            TokenKind::_OpDec => {
                let a = stack.pop().unwrap();
                stack.push(a.sub(&Data::Int(1)).unwrap());
            }

            TokenKind::OpLT => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.lt(&b).unwrap());
            }
            TokenKind::OpGT => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.gt(&b).unwrap());
            }
            TokenKind::OpLE => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.le(&b).unwrap());
            }
            TokenKind::OpGE => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.ge(&b).unwrap());
            }
            TokenKind::OpEQ => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.eq(&b).unwrap());
            }
            TokenKind::OpNE => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a.ne(&b).unwrap());
            }

            TokenKind::OpOut => print!("{}", stack.pop().unwrap()),
            TokenKind::KwPuts => print!("{}", stack.pop().unwrap()),
            TokenKind::OpErr => eprint!("{}", stack.pop().unwrap()),
            TokenKind::OpIn => unimplemented!(),

            TokenKind::KwIf => conds.push((TokenKind::KwIf, i)),
            TokenKind::KwWhile => conds.push((TokenKind::KwWhile, i)),
            TokenKind::KwDo => {
                if let Some(Data::Bool(false)) = stack.pop() {
                    seek_end = true;
                }
            }
            TokenKind::KwEnd => {
                let (kind, begin) = conds.pop().unwrap();
                if kind == TokenKind::KwWhile {
                    i = begin;
                    conds.push((kind, begin));
                }
            }
            TokenKind::_CmpDo => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match t.val.clone().into_int() {
                    0 => if a.ge(&b).unwrap().into_bool() {
                        seek_end = true;
                    }
                    1 => if a.le(&b).unwrap().into_bool() {
                        seek_end = true;
                    }
                    2 => if a.gt(&b).unwrap().into_bool() {
                        seek_end = true;
                    }
                    3 => if a.lt(&b).unwrap().into_bool() {
                        seek_end = true;
                    }
                    4 => if a.ne(&b).unwrap().into_bool() {
                        seek_end = true;
                    }
                    5 => if a.eq(&b).unwrap().into_bool() {
                        seek_end = true;
                    }
                    _ => ()
                }

                // 0 lt -> nl
                // 1 gt -> ng
                // 2 le -> g
                // 3 ge -> l
                // 4 eq -> ne
                // 5 ne -> e
            }

            TokenKind::OpArr => unimplemented!(),

            TokenKind::KwStr => match stack.pop().unwrap() {
                Data::Bool(b) => stack.push(Data::Str(b.to_string())),
                Data::Int(i) => stack.push(Data::Str(i.to_string())),
                Data::Flt(f) => stack.push(Data::Str(f.to_string())),
                Data::Str(s) => stack.push(Data::Str(s)),
            },
            TokenKind::KwInt => match stack.pop().unwrap() {
                Data::Bool(b) => stack.push(Data::Int(b as i64)),
                Data::Int(i) => stack.push(Data::Int(i)),
                Data::Flt(f) => stack.push(Data::Int(f as i64)),
                Data::Str(s) => stack.push(Data::Int(s.parse::<i64>().unwrap_or_else(|_| {
                    panic!(
                        "line {} - provided string cannot be interpreted as 'int'",
                        t.line
                    )
                }))),
            },
            TokenKind::KwFlt => match stack.pop().unwrap() {
                Data::Bool(_) => (),
                Data::Int(i) => stack.push(Data::Flt(i as f64)),
                Data::Flt(f) => stack.push(Data::Flt(f)),
                Data::Str(s) => stack.push(Data::Flt(s.parse::<f64>().unwrap_or_else(|_| {
                    panic!(
                        "line {} - provided string cannot be interpreted as 'float'",
                        t.line
                    )
                }))),
            },

            TokenKind::Ident => unimplemented!(),

            _ => (),
        }

        i += 1;
    }
}
