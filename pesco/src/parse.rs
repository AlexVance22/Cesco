use crate::data::Data;
use std::{collections::HashMap, str::Chars};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    IntLit,
    FltLit,
    StrLit,
    OpDup,
    Op2Dup,
    OpOver,
    OpRot,
    OpIRot,
    OpSwap,
    OpPop,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpLT,
    OpGT,
    OpLE,
    OpGE,
    OpEQ,
    OpNE,
    OpOut,
    OpErr,
    OpIn,
    OpArr,
    KwStr,
    KwInt,
    KwFlt,
    KwIf,
    KwWhile,
    KwDo,
    KwEnd,
    KwTrue,
    KwFalse,
    KwVar,
    KwFunc,
    Ident,
    KwPuts,

    _OpInc,
    _OpDec,
    _AddImm,
    _SubImm,
    _MulImm,
    _DivImm,
    _CmpDo
}

#[derive(Debug, Clone)]
pub struct Token {
    pub val: Data,
    pub line: u32,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(val: &str, line: u32, ops: &HashMap<String, TokenKind>) -> Self {
        if val.starts_with('"') && val.ends_with('"') {
            return Self {
                val: Data::Str(val[1..val.len() - 1].to_string()),
                line,
                kind: TokenKind::StrLit,
            };
        }

        if let Ok(n) = val.parse::<i64>() {
            return Self {
                val: Data::Int(n),
                line,
                kind: TokenKind::IntLit,
            };
        }

        if let Ok(f) = val.parse::<f64>() {
            return Self {
                val: Data::Flt(f),
                line,
                kind: TokenKind::FltLit,
            };
        }

        let kind = if ops.contains_key(val) {
            *ops.get(val).unwrap()
        } else {
            TokenKind::Ident
        };

        Self {
            val: Data::Str(val.to_string()),
            line,
            kind,
        }
    }
}

pub fn _tokenise(src: &str, ops: &HashMap<String, TokenKind>) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut token = String::new();
    let mut strlit = false;
    let mut escape = false;
    let mut line = 0u32;

    for c in src.chars() {
        if c == '\n' {
            line += 1;
        }

        if strlit {
            if escape {
                match c {
                    'n' => token.push('\n'),
                    't' => token.push('\t'),
                    'r' => token.push('\r'),
                    '\\' => token.push('\\'),
                    '"' => token.push('"'),
                    _ => (),
                }
                escape = false;
            } else if c == '\\' {
                escape = true;
            } else if c == '"' {
                strlit = false;
                token.push(c);
                tokens.push(Token::new(&token, line, ops));
                token = String::new();
            } else {
                token.push(c);
            }
        } else {
            if c == '"' {
                strlit = true;
            }

            if c == '\n' || c == '\t' || c == '\r' || c == ' ' {
                if !token.is_empty() {
                    tokens.push(Token::new(&token, line, ops));
                    token = String::new();
                }
            } else {
                token.push(c);
            }
        }
    }

    tokens.push(Token::new(&token, line, ops));

    tokens
}


struct Tokens<'a, 'b> {
    src: &'a str,
    chars: Chars<'a>,
    index: usize,
    line: u32,
    done: bool,
    ops: &'b HashMap<String, TokenKind>,
}

impl<'a, 'b> Tokens<'a, 'b> {
    fn new(src: &'a str, ops: &'b HashMap<String, TokenKind>) -> Self {
        Tokens{ src, chars: src.chars(), index: 0, line: 0, done: false, ops }
    }
}

impl<'a, 'b> Iterator for Tokens<'a, 'b> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None
        }

        let mut toklen = 0usize;
        let mut strlit = false;
        let mut escape = false;
    
        while let Some(c) = self.chars.next() {
            if c == '\n' {
                self.line += 1;
            }
    
            if strlit {
                match c {
                    '"' if !escape => {
                        let token = &self.src[self.index..(self.index + toklen)];
                        self.index += toklen;
                        return Some(Token::new(token, self.line, self.ops))
                    }
                    '\\' if !escape => escape = true,
                    _ => toklen += 1
                }
            } else {
                match c {
                    '\n'|'\t'|'\r'|' ' => if toklen > 0 {
                        let token = &self.src[self.index..(self.index + toklen)];
                        self.index += toklen;
                        return Some(Token::new(token, self.line, self.ops))
                    }
                    '"' => strlit = true,
                    _ => toklen += 1
                }
            }
        }
    
        self.done = true;

        let token = &self.src[self.index..(self.index + toklen)];
        self.index += toklen;
        Some(Token::new(token, self.line, self.ops))
    }
}


pub fn tokenise(src: &str, ops: &HashMap<String, TokenKind>) -> Vec<Token> {
    let tokens = Tokens::new(src, ops);

    tokens.collect()
}