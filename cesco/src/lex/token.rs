use std::str::FromStr;
use crate::span::Span;


#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    LeftBrack, RightBrack, Less, Greater, Comma,
    Dot, Colon, Semicolon, Bang, Add, Sub, Mul, Div,
    Mod, Equal, BitAnd, BitOr, BitXor, BitNot, Nullable,
  
    // double-character tokens
    GreaterEq, LessEq,
    BangEq, EqualEq,
    AddEq, SubEq,
    MulEq, DivEq,
    ModEq,
    LShiftEq, RShiftEq,
    Define, Bind, Arrow,
    And, Or,
    AndEq, OrEq, XorEq,
    LShift, RShift,
  
    //types
    I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, Bool, Str, Null,
  
    // keywords
    Struct, Func, If, Else, For, While, Break, Def, Extern,
    Return, This, Mut, Pub, Obj, Comp, Import, Ask, As, Yield, In,

    // literals.
    Integer(i64), Float(f64), Character(char), Boolean(bool), StrLit(String),

    Identifier(String),

    Eof,
}


impl FromStr for TokenKind {
    type Err = super::Error;

    fn from_str(lexeme: &str) -> Result<Self, Self::Err> {
        use TokenKind::*;

        Ok(
        if lexeme.starts_with('"') && lexeme.ends_with('"') {
            let mut chars = lexeme.chars();
            chars.next();
            chars.next_back();
            StrLit(chars.as_str().to_string())
        } else if lexeme.starts_with('\'') && lexeme.ends_with('\'') {
            Character(lexeme.chars().nth(1).unwrap())
        } else if let Some(lex) = lexeme.strip_prefix("0x") {
            Integer(i64::from_str_radix(lex, 16)?)
        } else if let Some(lex) = lexeme.strip_prefix("0o") {
            Integer(i64::from_str_radix(lex, 8)?)
        } else if let Some(lex) = lexeme.strip_prefix("0b") {
            Integer(i64::from_str_radix(lex, 2)?)
        } else if let Ok(val) = lexeme.parse::<i64>() {
            Integer(val)
        } else if let Ok(val) = lexeme.parse::<f64>() {
            Float(val)
        } else if let Ok(val) = lexeme.parse::<bool>() {
            Boolean(val)
        } else {
            match lexeme {
                "(" => LeftParen,   ")" => RightParen,      "{" => LeftBrace,   "}" => RightBrace,
                "[" => LeftBrack,   "]" => RightBrack,      "<" => Less,        ">" => Greater,
                "," => Comma,       "." => Dot,             ":" => Colon,       ";" => Semicolon,
                "!" => Bang,
                "+" => Add,         "-" => Sub,             "*" => Mul,         "/" => Div,
                "%" => Mod,         "<<" => LShift,         ">>" => RShift,     "&" => BitAnd,
                "|" => BitOr,       "^" => BitXor,          "~" => BitNot,      "=" => Equal,
                "?" => Nullable,
                "!=" => BangEq,     "==" => EqualEq,        "<=" => LessEq,     ">=" => GreaterEq,
                "+=" => AddEq,      "-=" => SubEq,          "*=" => MulEq,      "/=" => DivEq,
                "%=" => ModEq,      "&=" => AndEq,          "|=" => OrEq,       "^=" => XorEq,
                "<<=" => LShiftEq,  ">>=" => RShiftEq,
                "&&" => And,        "||" => Or,             "::" => Bind,       "->" => Arrow,
                ":=" => Define,
                "struct" => Struct, "func" => Func,         "if" => If,         "else" => Else,
                "for" => For,       "while" => While,       "break" => Break,   "default" => Def,
                "extern" => Extern, "return" => Return,     "this" => This,     "mut" => Mut,
                "pub" => Pub,       "obj" => Obj,           "comp" => Comp,     "import" => Import,
                "ask" => Ask,       "as" => As,             "yield" => Yield,   "in" => In,
                "i8" => I8,         "i16" => I16,           "i32" => I32,       "i64" => I64,
                "u8" => U8,         "u16" => U16,           "u32" => U32,       "u64" => U64,
                "f32" => F32,       "f64" => F64,           "char" => U8,       "bool" => Bool,
                "str" => Str,       "null" => Null,
                _ => Identifier(lexeme.to_string())
            }
        }
        )
    }
}



#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}


impl Token {
    pub fn from(val: impl AsRef<str>, span: Span) -> Token {
        Token{ kind: val.as_ref().parse().unwrap(), span }
    }

    pub fn as_err(kind: TokenKind, span: Span) -> Token {
        Token{ kind, span }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
