use std::num::ParseIntError;
use crate::span::Span;
use thiserror::Error;
use super::Token;


#[derive(Error, Debug)]
pub enum Error {
    #[error("{} - invalid characters in identifier '{:?}'", .0.span, .0.kind)]
    InvalidIdent(Token),
    #[error("{0} - integer literal contains invalid characters")]
    BadIntLit(Span),
    #[error("{0} - hexadecimal literal contains invalid characters")]
    BadHexLit(#[from] ParseIntError),
    #[error("{0} - float literal contains invalid characters")]
    BadFltLit(Span),
    #[error("{0} - character literal must contain a single character")]
    BadCharLit(Span),
    #[error("{0} - invalid escape sequence '\\{1}'")]
    BadEscape(Span, char),
    #[error("{0} - '{1}' is not a valid operator")]
    InvalidOperator(Span, String),
    #[error("{0} unexpected character '{1}'")]
    InvalidCharacter(Span, char)
}