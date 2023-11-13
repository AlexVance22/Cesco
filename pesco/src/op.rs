use crate::data::Data;
use crate::gen::{Mnemonic, Operand};
use crate::parse::{Token, TokenKind};

// cmp_jmp
// psh_use
// dup_use
// add_one
// add_imm

enum TokState {
    None,
    PushInt { val: i64, loc: usize },
    PushStr,
    Cmp { val: i32, loc: usize }
}

enum AsmState {
    None,
    PushVal { op: Operand, loc: usize },
}


pub fn op_prog(mut tokens: Vec<Token>) -> Vec<Token> {
    let mut state = TokState::None;
    let mut i = 0;

    while i < tokens.len() {
        match tokens[i].kind {
            TokenKind::IntLit => state = TokState::PushInt{ val: tokens[i].val.clone().into_int(), loc: i },

            TokenKind::StrLit => state = TokState::PushStr,

            TokenKind::OpOut => {
                if let TokState::PushStr = state {
                    tokens[i].val = Data::Str("puts".to_string());
                    tokens[i].kind = TokenKind::KwPuts;
                }
            }

            TokenKind::OpAdd => {
                if let TokState::PushInt{ val, loc } = state {
                    if val == 1 {
                        tokens.remove(loc);
                        let line = tokens.remove(loc).line;
                        tokens.insert(loc, Token{ val: Data::Int(0), line, kind: TokenKind::_OpInc });
                        i -= 1;
                    } else {
                        tokens.remove(loc);
                        let line = tokens.remove(loc).line;
                        tokens.insert(loc, Token{ val: Data::Int(val), line, kind: TokenKind::_AddImm });
                        i -= 1;
                    }
                }
                state = TokState::None;
            }
            TokenKind::OpSub => {
                if let TokState::PushInt{ val, loc } = state {
                    if val == 1 {
                        tokens.remove(loc);
                        let line = tokens.remove(loc).line;
                        tokens.insert(loc, Token{ val: Data::Int(0), line, kind: TokenKind::_OpDec });
                        i -= 1;

                    } else {
                        tokens.remove(loc);
                        let line = tokens.remove(loc).line;
                        tokens.insert(loc, Token{ val: Data::Int(val), line, kind: TokenKind::_SubImm });
                        i -= 1;
                    }
                }
                state = TokState::None;
            }
            TokenKind::OpMul => {
                if let TokState::PushInt{ val, loc } = state {
                    tokens.remove(loc);
                    let line = tokens.remove(loc).line;
                    tokens.insert(loc, Token{ val: Data::Int(val), line, kind: TokenKind::_MulImm });
                    i -= 1;
                }
                state = TokState::None;
            }
            TokenKind::OpDiv => {
                if let TokState::PushInt{ val, loc } = state {
                    tokens.remove(loc);
                    let line = tokens.remove(loc).line;
                    tokens.insert(loc, Token{ val: Data::Int(val), line, kind: TokenKind::_DivImm });
                    i -= 1;
                }
                state = TokState::None;
            }

            TokenKind::OpLT => state = TokState::Cmp{ val: 0, loc: i },
            TokenKind::OpGT => state = TokState::Cmp{ val: 1, loc: i },
            TokenKind::OpLE => state = TokState::Cmp{ val: 2, loc: i },
            TokenKind::OpGE => state = TokState::Cmp{ val: 3, loc: i },
            TokenKind::OpEQ => state = TokState::Cmp{ val: 4, loc: i },
            TokenKind::OpNE => state = TokState::Cmp{ val: 5, loc: i },

            TokenKind::KwDo => {
                if let TokState::Cmp{ val, loc } = state {
                    tokens.remove(loc);
                    let line = tokens.remove(loc).line;
                    tokens.insert(loc, Token{ val: Data::Int(val as i64), line, kind: TokenKind::_CmpDo });
                    i -= 1;
                }
                state = TokState::None;
            }

            _ => state = TokState::None,
        }

        i += 1;
    }

    tokens
}

pub fn op_asm(mut tokens: Vec<Mnemonic>) -> Vec<Mnemonic> {
    let mut state = AsmState::None;
    let mut i = 0;

    while i < tokens.len() {
        let t = tokens[i].clone();

        match t {
            Mnemonic::Push(op) => state = AsmState::PushVal{ op, loc: i },

            Mnemonic::Pop(reg) => {
                if let AsmState::PushVal{ op, loc } = state {
                    match op {
                        Operand::Imm(j) => {
                            tokens.remove(loc);
                            tokens.remove(loc);
                            tokens.insert(loc, Mnemonic::Mov(reg, Operand::Imm(j)));
                            i -= 1;
                        }
                        Operand::Reg(r) => {
                            tokens.remove(loc);
                            tokens.remove(loc);
                            i -= 2;
                            if reg != r {
                                tokens.insert(loc, Mnemonic::Mov(reg, Operand::Reg(r)));
                                i += 1;
                            }
                        }
                        _ => ()
                    }
                }
                state = AsmState::None;
            }

            _ => state = AsmState::None,
        }

        i += 1;
    }

    tokens
}
