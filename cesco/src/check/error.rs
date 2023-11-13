use thiserror::Error;
use crate::hir::ast as hir;
use crate::parse::ast;
use crate::span::Span;


#[derive(Error, Debug)]
#[allow(unused)]
pub enum Error {
    #[error("{0} - identifier '{1}' was not declared before usage")]
    VarNotDeclared(Span, String),
    #[error("{0} - function '{1}' was not declared before usage")]
    FuncNotFound(Span, String),
    #[error("{0} - struct '{1}' was not declared before usage")]
    StructNotFound(Span, String),
    #[error("{0} - field '{1}' not present in struct '{2}'")]
    FieldNotFound(Span, String, String),
    #[error("{0} - expected struct object for field access operator '.'")]
    ExprNotStruct(Span),
    #[error("{0} - function should return '{1}', found '{2}'")]
    BadReturn(Span, hir::Type, hir::Type),
    #[error("{0} - ask fallback should yield '{1}', found '{2}'")]
    BadYield(Span, hir::Type, hir::Type),
    #[error("{0} - cannot assign expression of type '{2}' to variable of type '{1}'")]
    BadAssigment(Span, hir::Type, hir::Type),
    #[error("{0} - cannot assign to immutable variable of type '{1}'")]
    BadMutation(Span, hir::Type),
    #[error("{0} - expression in if condition must be a boolean")]
    BadIfCond(Span),
    #[error("{0} - expression in while condition must be a boolean")]
    BadWhileCond(Span),
    #[error("{0} - dereferenced expression was not reference type")]
    BadDeref(Span),
    #[error("{0} - must 'ask' pointer types for values as they could be null")]
    BadPtrGet(Span),
    #[error("{0} - types '{1}' is not usable in {2} operation")]
    InvalidUnary(Span, ast::Type, String),
    #[error("{0} - types '{1}' and '{2}' are incompatible for {3} operation")]
    InvalidExpression(Span, ast::Type, ast::Type, String)
}