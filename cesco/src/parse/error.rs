use thiserror::Error;
use super::ast::Expression;
use crate::lex::TokenKind;
use crate::span::Span;


#[derive(Error, Debug, Clone)]
#[repr(u32)]
pub enum Error {
    #[error("ERROR: {0} - expected item (import, func, struct), found '{1:?}'")]
    BadGlobalItem(Span, TokenKind),

    #[error("ERROR: {0} - expected file name in import statement, found '{1:?}'")]
    BadImport(Span, TokenKind),
    #[error("ERROR: {0} - expected function name, found '{1:?}'")]
    BadFuncName(Span, TokenKind),
    #[error("ERROR: {0} - expected struct name, found '{1:?}'")]
    BadStructName(Span, TokenKind),

    #[error("ERROR: {0} - 'this' may only be the first parameter to a method")]
    InvalidThis(Span),
    #[error("ERROR: {0} - expected comma or '&' after 'this', found {1:?}")]
    EndOfThis(Span, TokenKind),
    #[error("ERROR: {0} - expected parameter, found {1:?}")]
    ExpectedParam(Span, TokenKind),
    #[error("ERROR: {0} - expected parameter type after 'mut' declaration, found {1:?}")]
    ExpectedParamType(Span, TokenKind),
    #[error("ERROR: {0} - expected return type after '->', found {1:?}")]
    ExpectedRetType(Span, TokenKind),

    #[error("ERROR: {0} - array type declaration must be terminated with ']'")]
    BadArrDecl(Span),
    #[error("ERROR: {0} - expected integer constant in array type declaration, found {1:?}")]
    BadArrLen(Span, TokenKind),
    #[error("ERROR: {0} - vector type declaration must be terminated with ')'")]
    BadVecDecl(Span),
    #[error("ERROR: {0} - expected integer constant in vector type declaration, found {1:?}")]
    BadVecLen(Span, TokenKind),

    #[error("ERROR: {0} - expected semicolon after statement, found '{1:?}'")]
    ExpectedSemicolon(Span, TokenKind),

    #[error("ERROR: {0} - expected ';' at end of statement, found '{1:?}'")]
    EndOfStatementInterrupt(Span, TokenKind),
    #[error("ERROR: {0} - expected ';' at end of statement, reached end of file")]
    EndOfStatementFile(Span),
    #[error("ERROR: {0} - expected '}}' at end of code block, reached end of file")]
    EndOfBlockFile(Span),
    #[error("ERROR: {0} - unexpectedly reached end of file")]
    EndOfFile(Span),
    #[error("ERROR: {0} - variable declaration was given no name before assignment")]
    DeclMissingName(Span),
    #[error("ERROR: {0} - expected assignment operator '=' after declaration, found '{1:?}'")]
    ExpectedEqual(Span, TokenKind),

    #[error("ERROR: {0} - expected binary operator in expression, found '{1:?}'")]
    ExpectedOperator(Span, TokenKind),
    #[error("ERROR: {0} - expected expression, found '{1:?}'")]
    ExpectedExpr(Span, TokenKind),
    #[error("ERROR: {0} - expected struct field or method name, found '{1:?}'")]
    ExpectedFieldMethod(Span, TokenKind),
    //#[error("line {0} - parentheses not closed before expression terminator '{1:?}'")]
    //ParensNotClosed(u32, TokenKind),
    #[error("ERROR: {0} - found 'ask' block without corresponding 'else' fallback")]
    AskWithoutElse(Span, TokenKind),
    #[error("ERROR: {0} - expected expression, reached end of file")]
    EndOfExprFile(Span),
    #[error("ERROR: {0} - array access operator '[]' requires index expression")]
    EmptyIndex(Span),
    #[error("ERROR: {0} - expression '{1:?}' is not callable")]
    NotCallable(Span, Expression),
    #[error("ERROR: {0} - expected an expression here")]
    EmptyExpr(Span),

    #[error("ERROR: {0} - invalid token '{1:?}' in expression")]
    UnknownExprError(Span, TokenKind),
}