#[macro_use]
pub mod ast;
mod expr;
mod error;

use crate::lex::{ Token, TokenKind };
use crate::span::{ Span, Item };
use ast::*;
use std::collections::HashMap;
pub use error::Error;

pub trait CloneIter<T>: Clone + Iterator<Item = T> {}
impl<T, U: Clone + Iterator<Item = T>> CloneIter<T> for U {}


fn next_token(ts: &mut impl CloneIter<Token>, errors: &mut Vec<Error>) -> Result<Token, Vec<Error>> {
    let next = ts.next().unwrap();

    if next.kind == TokenKind::Eof {
        errors.push(Error::EndOfFile(next.span));
        Err(errors.to_vec())
    } else {
        Ok(next)
    }
}



fn mangle(name: String, assoc: ast::Association, names: &mut HashMap<String, usize>) -> String {
    let base = match assoc {
        ast::Association::None => format!("__N_{}", name),
        ast::Association::Static(s) => format!("__S_{}_{}", s, name),
        ast::Association::Method(s) => format!("__M_{}_{}", s, name)
    };

    if let Some(n) = names.get_mut(&base) {
        *n += 1;
        format!("{}_{}", base, *n)
    } else {
        names.insert(base.clone(), 0);
        format!("{}_0", base)
    }
}


fn get_type(ts: &mut impl CloneIter<Token>, mut next: Token) -> Result<(Item<Type>, Token), Error> {
    use TokenKind::*;

    if !matches!(next.kind, I8|I16|I32|I64|U8|U16|U32|U64|F32|F64|Bool|Str|Null|Identifier(_)) {
        next = ts.next().ok_or(Error::EndOfStatementFile(next.span))?;
    }
    let mut t = Item{ val: match &next.kind {
        TokenKind::I8 => Type::I8,
        TokenKind::I16 => Type::I16,
        TokenKind::I32 => Type::I32,
        TokenKind::I64 => Type::I64,
        TokenKind::U8 => Type::U8,
        TokenKind::U16 => Type::U16,
        TokenKind::U32 => Type::U32,
        TokenKind::U64 => Type::U64,
        TokenKind::F32 => Type::F32,
        TokenKind::F64 => Type::F64,
        TokenKind::Bool => Type::Bool,
        TokenKind::Str => Type::Str,
        TokenKind::Null => Type::Null,
        TokenKind::Identifier(id) => Type::User(id.clone()),
        _ => unreachable!()
    }, span: next.span.clone() };

    next = ts.next().ok_or(Error::EndOfStatementFile(next.span))?;
    match next {
        Token{ kind: LeftParen, span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            if let Integer(i) = next.kind {
                t.val = Type::VecOf(Box::new(t.val), i as usize);
                next = ts.next().ok_or(Error::EndOfStatementFile(next.span))?;
                if next.kind != RightParen {
                    return Err(Error::BadVecDecl(span))
                }
                t.span.end = next.span.end;
            } else {
                return Err(Error::BadVecLen(span, next.kind))
            }
        }
        Token{ kind: LeftBrack, span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            if let Integer(i) = next.kind {
                t.val = Type::ArrOf(Box::new(t.val), i as usize);
                next = ts.next().ok_or(Error::EndOfStatementFile(next.span))?;
                if next.kind != RightBrack {
                    return Err(Error::BadArrDecl(span))
                }
                t.span.end = next.span.end;
            } else {
                return Err(Error::BadArrLen(span, next.kind))
            }
        }
        Token{ kind: Mul,      span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            t.span.end = span.end;
            return Ok((Item{ val: Type::PtrTo(Box::new(t.val)), span: t.span }, next))
        }
        Token{ kind: BitAnd,   span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            t.span.end = span.end;
            return Ok((Item{ val: Type::RefTo(Box::new(t.val)), span: t.span }, next))
        }
        Token{ kind: Nullable, span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            t.span.end = span.end;
            return Ok((Item{ val: Type::OptOf(Box::new(t.val)), span: t.span }, next))
        }
        _ => return Ok((t, next)),
    }
    next = ts.next().ok_or(Error::EndOfStatementFile(next.span))?;
    match next {
        Token{ kind: Mul,      span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            t.span.end = span.end;
            Ok((Item{ val: Type::PtrTo(Box::new(t.val)), span: t.span }, next))
        }
        Token{ kind: BitAnd,   span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            t.span.end = span.end;
            Ok((Item{ val: Type::RefTo(Box::new(t.val)), span: t.span }, next))
        }
        Token{ kind: Nullable, span } => {
            next = ts.next().ok_or(Error::EndOfStatementFile(span.clone()))?;
            t.span.end = span.end;
            Ok((Item{ val: Type::OptOf(Box::new(t.val)), span: t.span }, next))
        }
        _ => Ok((t, next)),
    }
}


fn get_block(ts: &mut impl CloneIter<Token>) -> (Vec<Statement>, Vec<Error>) {
    use TokenKind::*;

    let mut result = Vec::new();
    let mut errors = Vec::new();

    while let Some(mut next) = ts.next() {
        match next {
            Token{ kind: I8|I16|I32|I64|U8|U16|U32|U64|F32|F64|Bool|Str|Null, .. } => {
                match get_type(ts, next.clone()) {
                    Ok((datatype, mut next)) => {
                        let mut decl = Variable{ datatype, ..Default::default() };
                        match next {
                            Token{ kind: Identifier(name), span } => {
                                decl.name = Item{ val: name, span };
                                next = ts.next().unwrap();
                                if next.kind != Equal {
                                    errors.push(Error::DeclMissingName(next.span));
                                    while next.kind != Equal { next = ts.next().unwrap(); }
                                } else {
                                    let val = match expr::parse_right(ts, expr::Terminal::Semicolon as u32) {
                                        Ok(Some(val)) => val,
                                        Ok(None) => { errors.push(Error::EmptyExpr(next.span)); Item::mock(Expression::Null) }
                                        Err(err) => { errors.push(err); Item::mock(Expression::Null) }
                                    };
                                    result.push(Statement::VarDecl{ decl, val });
                                }
                            }
                            Token{ span, .. } => {
                                errors.push(Error::DeclMissingName(span));
                                while next.kind != Equal { next = ts.next().unwrap(); }
                            }
                        }
                    }
                    Err(e) => {
                        errors.push(e);
                        while next.kind != Semicolon { next = ts.next().unwrap(); }
                    }
                }
            }
            Token{ kind: Identifier(_), .. } => {
                let mut ts_cloned = ts.clone();

                if let Ok(expr) = expr::parse(&mut ts_cloned, Some(next.clone()), expr::Terminal::Semicolon as u32) {
                    *ts = ts_cloned;
                    result.push(Statement::Expression(expr));
                } else {
                    match get_type(ts, next.clone()) {
                        Ok((datatype, mut next)) => {
                            let mut decl = Variable{ datatype, ..Default::default() };
                            match next {
                                Token{ kind: Identifier(name), span } => {
                                    decl.name = Item{ val: name, span };
                                    next = ts.next().unwrap();
                                    if next.kind != Equal {
                                        errors.push(Error::DeclMissingName(next.span));
                                        while next.kind != Equal { next = ts.next().unwrap(); }
                                    } else {
                                        let val = match expr::parse_right(ts, expr::Terminal::Semicolon as u32) {
                                            Ok(Some(val)) => val,
                                            Ok(None) => { errors.push(Error::EmptyExpr(next.span)); Item::mock(Expression::Null) }
                                            Err(err) => { errors.push(err); Item::mock(Expression::Null) }
                                        };
                                        result.push(Statement::VarDecl{ decl, val });
                                    }
                                }
                                Token{ span, .. } => {
                                    errors.push(Error::DeclMissingName(span));
                                    while next.kind != LeftBrace { next = ts.next().unwrap(); }
                                }
                            }
                        }
                        Err(e) => {
                            errors.push(e);
                            while next.kind != LeftBrace { next = ts.next().unwrap(); }
                        }
                    }
                }  
            }
            Token{ kind: Mut,   span } => {
                match get_type(ts, Token{ kind: Mut, span: span.clone() }) {
                    Ok((datatype, mut next)) => {
                        let mut decl = Variable{ datatype, mutable: Item{ val: true, span: span.clone() }, ..Default::default() };
                        if let Token{ kind: Identifier(name), span } = next {
                            decl.name = Item{ val: name, span };
                            next = ts.next().unwrap();
                            if next.kind != Equal {
                                errors.push(Error::DeclMissingName(next.span));
                                while next.kind != Equal { next = ts.next().unwrap(); }
                            } else {
                                let val = match expr::parse_right(ts, expr::Terminal::Semicolon as u32) {
                                    Ok(Some(val)) => val,
                                    Ok(None) => { errors.push(Error::EmptyExpr(next.span)); Item::mock(Expression::Null) }
                                    Err(err) => { errors.push(err); Item::mock(Expression::Null) }
                                };
                                result.push(Statement::VarDecl{ decl, val });
                            }
                        } else {
                            errors.push(Error::DeclMissingName(span));
                            while next.kind != LeftBrace { next = ts.next().unwrap(); }
                        }
                    }
                    Err(e) => {
                        errors.push(e);
                        while next.kind != LeftBrace { next = ts.next().unwrap(); }
                    }
                }
            }
            Token{ kind: While,         .. } => {
                match expr::parse_right(ts, expr::Terminal::LeftBrace as u32) {
                    Ok(Some(cond)) => {
                        let cond = cond;
                        let (body, errs) = get_block(ts);
                        errors.extend(errs);
                        if cond.val != Expression::BoolLit(false) {
                            result.push(Statement::While{ cond, body });
                        }
                    }
                    Ok(None) => {
                        let (body, errs) = get_block(ts);
                        errors.extend(errs);
                        errors.push(Error::EmptyExpr(next.span));
                        result.push(Statement::While{ cond: Item::mock(Expression::IntLit(0)), body });
                    }
                    Err(err) => {
                        let (body, errs) = get_block(ts);
                        errors.extend(errs);
                        errors.push(err);
                        result.push(Statement::While{ cond: Item::mock(Expression::IntLit(0)), body });
                    }
                }
            }
            Token{ kind: If,            .. } => {
                match expr::parse_right(ts, expr::Terminal::LeftBrace as u32) {
                    Ok(Some(cond)) => {
                        let cond = cond;
                        let (body, errs) = get_block(ts);
                        errors.extend(errs);
                        if cond.val != Expression::BoolLit(false) {
                            result.push(Statement::If{ cond, body });
                        }
                    }
                    Ok(None) => {
                        let (body, errs) = get_block(ts);
                        errors.extend(errs);
                        errors.push(Error::EmptyExpr(next.span));
                        result.push(Statement::While{ cond: Item::mock(Expression::IntLit(0)), body });
                    }
                    Err(err) => {
                        let (body, errs) = get_block(ts);
                        errors.push(err);
                        errors.extend(errs);
                        result.push(Statement::If{ cond: Item::mock(Expression::IntLit(0)), body });
                    }
                }
            }
            Token{ kind: For,           .. } => {
                if let Token{ kind: Identifier(varname), span } = next {
                    next = ts.next().unwrap();
                    let varname = Item{ val: varname, span };
                    if let Token{ kind: In, .. } = next {
                        match expr::parse_right(ts, expr::Terminal::LeftBrace as u32) {
                            Ok(Some(iterable)) => {
                                let (body, errs) = get_block(ts);
                                errors.extend(errs);
                                result.push(Statement::For{ varname, iterable, body });
                            }
                            Ok(None) => {
                                let (body, errs) = get_block(ts);
                                errors.extend(errs);
                                errors.push(Error::EmptyExpr(next.span));
                                result.push(Statement::For{ varname, iterable: Item::mock(Expression::Null), body });
                            }
                            Err(e) => {
                                let (body, errs) = get_block(ts);
                                errors.extend(errs);
                                errors.push(e);
                                result.push(Statement::For{ varname, iterable: Item::mock(Expression::Null), body });
                            }
                        }
                    }
                }
            }
            Token{ kind: Break,         .. } => {
                result.push(Statement::Break);
                match ts.next() {
                    Some(Token{ kind: Semicolon, .. }) => (),
                    Some(Token{ kind, span }) => {
                        errors.push(Error::EndOfStatementInterrupt(span, kind));
                        while next.kind != Semicolon { next = ts.next().unwrap(); }
                    }
                    None => {
                        errors.push(Error::EndOfStatementFile(next.span));
                        return (result, errors)
                    }
                }
            }
            Token{ kind: Yield,         .. } => {
                match expr::parse_right(ts, expr::Terminal::Semicolon as u32) {
                    Ok(Some(val)) => {
                        result.push(Statement::Yield(val));
                    }
                    Ok(None) => {
                        errors.push(Error::EmptyExpr(next.span));
                        result.push(Statement::Yield(Item::mock(Expression::IntLit(0))));
                    }
                    Err(e) => {
                        errors.push(e);
                        result.push(Statement::Yield(Item::mock(Expression::IntLit(0))));
                    }
                }
            }
            Token{ kind: Return,        .. } => {
                match expr::parse_right(ts, expr::Terminal::Semicolon as u32) {
                    Ok(Some(val)) => {
                        result.push(Statement::Return(Some(val)));
                    }
                    Ok(None) => {
                        result.push(Statement::Return(None));
                    }
                    Err(e) => {
                        errors.push(e);
                        result.push(Statement::Yield(Item::mock(Expression::IntLit(0))));
                    }
                }
            }
            Token{ kind: RightBrace,    .. } => {
                return (result, errors)
            }
            Token{ kind: Semicolon,     .. } => (),
            _ => {
                match expr::parse(ts, Some(next), expr::Terminal::Semicolon as u32) {
                    Ok(ex) => {
                        result.push(Statement::Expression(ex));
                    }
                    Err(err) => {
                        errors.push(err);
                    }
                }
            }
        }
    }

    errors.push(Error::EndOfBlockFile(Span::default()));
    (result, errors)
}


pub fn parse(tokens: Vec<Token>) -> Result<Ast, Vec<Error>> {
    use TokenKind::*;

    let mut ts = tokens.into_iter();
    let mut names = HashMap::new();

    let mut result = Ast::new();
    let mut errors = Vec::new();

    while let Some(mut next) = ts.next() {
        match next {
            Token{ kind: Import,    .. } => {
                next = next_token(&mut ts, &mut errors)?;
                if let Token{ kind: Identifier(id), .. } = next {
                    result.imports.push(id);
                } else {
                    errors.push(Error::BadImport(next.span, next.kind));
                    next = next_token(&mut ts, &mut errors)?;
                    while next.kind != Semicolon { next = next_token(&mut ts, &mut errors)?; }
                    continue
                };
                next = next_token(&mut ts, &mut errors)?;
                if next.kind != Semicolon { errors.push(Error::ExpectedSemicolon(next.span, next.kind.clone())); }
                while next.kind != Semicolon { next = next_token(&mut ts, &mut errors)?; }
            }
            Token{ kind: Func,      .. } => {
                // FIRST ID =========================================================================================================================
                next = next_token(&mut ts, &mut errors)?;
                let first = if let Token{ kind: Identifier(id), span } = next {
                    Item::new(id.clone(), span)
                } else {
                    errors.push(Error::BadFuncName(next.span, next.kind));
                    next = next_token(&mut ts, &mut errors)?;
                    while next.kind != LeftParen { next = next_token(&mut ts, &mut errors)? }
                    Item::new(String::new(), next.span)
                };
                // SECOND ID ========================================================================================================================
                next = next_token(&mut ts, &mut errors)?;
                let (name, mut assoc) = match next {
                    Token{ kind: LeftParen, span } => {
                        (first, Item::new(Association::None, span))
                    }
                    Token{ kind: Dot, .. } => {
                        next = next_token(&mut ts, &mut errors)?;
                        if let Token{ kind: Identifier(id), span } = next {
                            next = next_token(&mut ts, &mut errors)?;
                            if next.kind != LeftParen {
                                errors.push(Error::BadFuncName(next.span, next.kind));
                                next = next_token(&mut ts, &mut errors)?;
                                while next.kind != LeftParen { next = next_token(&mut ts, &mut errors)? }
                                (Item::new(id.clone(), span), Item::new(Association::Static(first.val), first.span))
                            } else {
                                (Item::new(id.clone(), span), Item::new(Association::Static(first.val), first.span))
                            }
                        } else {
                            errors.push(Error::BadFuncName(next.span.clone(), next.kind.clone()));
                            while next.kind != LeftParen { next = next_token(&mut ts, &mut errors)? }
                            (Item::new(String::new(), next.span), Item::new(Association::Static(first.val), first.span))
                        }
                    }
                    _ => {
                        errors.push(Error::BadFuncName(next.span, next.kind));
                        next = next_token(&mut ts, &mut errors)?;
                        while next.kind != LeftParen { next = next_token(&mut ts, &mut errors)? }
                        (Item::new(String::new(), first.span.clone()), Item::new(Association::None, first.span))
                    }
                };
                // PARAMETER LIST ===================================================================================================================
                next = next_token(&mut ts, &mut errors)?;
                let mut params = Vec::new();
                while next.kind != RightParen {
                    match next {
                        Token{ kind: I8|I16|I32|I64|U8|U16|U32|U64|F32|F64|Bool|Str|Null|Identifier(_), .. } => {
                            let (datatype, n) = get_type(&mut ts, next).unwrap();
                            next = n;
                            if let Token{ kind: Identifier(name), span } = next {
                                params.push(Variable{ mutable: Item::new(false, datatype.span.clone()), datatype, name: Item::new(name, span) });
                            }
                            next = next_token(&mut ts, &mut errors)?;
                            if next.kind == Comma {
                                next = next_token(&mut ts, &mut errors)?;
                            }
                        }
                        Token{ kind: This, span } => {
                            if !params.is_empty() {
                                errors.push(Error::InvalidThis(span.clone()));
                            }
                            let datatype = if let Association::Static(id) = assoc.val {
                                assoc.val = Association::Method(id.clone());
                                Item{ val: Type::User(id), span }
                            } else {
                                errors.push(Error::InvalidThis(span.clone()));
                                Item{ val: Type::User(String::new()), span }
                            };
                            next = next_token(&mut ts, &mut errors)?;
                            match next {
                                Token{ kind: BitAnd, span } => {
                                    next = next_token(&mut ts, &mut errors)?;
                                    let datatype = Item{ val: Type::RefTo(Box::new(datatype.val)), span: Span{ end: span.end, ..datatype.span } };
                                    params.push(Variable{
                                        mutable: Item::new(false, datatype.span.clone()), 
                                        name: Item::new("this".to_string(), datatype.span.clone()),
                                        datatype
                                    });
                                    if next.kind == Comma {
                                        next = next_token(&mut ts, &mut errors)?;
                                    }
                                }
                                Token{ kind: Comma, .. } => {
                                    params.push(Variable{
                                        mutable: Item::new(false, datatype.span.clone()), 
                                        name: Item::new("this".to_string(), datatype.span.clone()),
                                        datatype
                                    });
                                    next = next_token(&mut ts, &mut errors)?;
                                }
                                _ => {
                                    errors.push(Error::EndOfThis(next.span, next.kind));
                                    next = next_token(&mut ts, &mut errors)?;
                                    while next.kind != Comma { next = next_token(&mut ts, &mut errors)?; }
                                }
                            }
                        }
                        Token{ kind: Mut,  span } => {
                            let mutable = Item{ val: true, span };
                            next = next_token(&mut ts, &mut errors)?;
                            match next {
                                Token{ kind: I8|I16|I32|I64|U8|U16|U32|U64|F32|F64|Bool|Str|Null|Identifier(_), .. } => {
                                    let (datatype, n) = get_type(&mut ts, next).unwrap();
                                    next = n;
                                    if let Token{ kind: Identifier(name), span } = next {
                                        params.push(Variable{ datatype, name: Item{ val: name, span }, mutable });
                                    }
                                    next = next_token(&mut ts, &mut errors)?;
                                    if next.kind == Comma {
                                        next = next_token(&mut ts, &mut errors)?;
                                    }
                                }
                                Token{ kind: This, span } => {
                                    if !params.is_empty() {
                                        errors.push(Error::InvalidThis(span.clone()));
                                    }
                                    let datatype = if let Association::Static(id) = assoc.val {
                                        assoc.val = Association::Method(id.clone());
                                        Item{ val: Type::User(id), span: span.clone() }
                                    } else {
                                        errors.push(Error::InvalidThis(span.clone()));
                                        Item{ val: Type::User(String::new()), span }
                                    };
                                    next = next_token(&mut ts, &mut errors)?;
                                    match next {
                                        Token{ kind: BitAnd, span } => {
                                            next = next_token(&mut ts, &mut errors)?;
                                            let datatype = Item{ val: Type::RefTo(Box::new(datatype.val)), span: Span{ end: span.end, ..datatype.span } };
                                            params.push(Variable{ name: Item::new("this".to_string(), datatype.span.clone()), datatype, mutable });
                                            if next.kind == Comma {
                                                next = next_token(&mut ts, &mut errors)?;
                                            }
                                        }
                                        Token{ kind: Comma, .. } => {
                                            params.push(Variable{ name: Item::new("this".to_string(), datatype.span.clone()), datatype, mutable });
                                            next = next_token(&mut ts, &mut errors)?;
                                        }
                                        _ => {
                                            errors.push(Error::EndOfThis(next.span, next.kind));
                                            next = next_token(&mut ts, &mut errors)?;
                                            while next.kind != Comma { next = next_token(&mut ts, &mut errors)?; }
                                        }
                                    }
                                }
                                _ => {
                                    errors.push(Error::ExpectedParamType(next.span, next.kind));
                                    next = next_token(&mut ts, &mut errors)?;
                                    while next.kind != Comma { next = next_token(&mut ts, &mut errors)?; }
                                }
                            }
                        }
                        _ => {
                            errors.push(Error::ExpectedParam(next.span, next.kind));
                            next = next_token(&mut ts, &mut errors)?;
                            while next.kind != Comma && next.kind != RightParen { next = next_token(&mut ts, &mut errors)?; }
                        }
                    }
                }
                // RETURN TYPE OR BODY ==============================================================================================================
                next = next_token(&mut ts, &mut errors)?;
                match next {
                    Token{ kind: LeftBrace, .. } => {
                        let (body, errs) = get_block(&mut ts);
                        let mangle = mangle(name.val.clone(), assoc.val.clone(), &mut names);
                        result.funcs.insert((name.val.clone(), assoc.val.clone()), ast::Func{ mangle, name, assoc, params, body, ..Default::default() });
                        errors.extend(errs);
                    }
                    Token{ kind: Arrow, .. } => {
                        next = next_token(&mut ts, &mut errors)?;
                        match next {
                            Token{ kind: I8|I16|I32|I64|U8|U16|U32|U64|F32|F64|Bool|Str|Null|Identifier(_), .. } => {
                                let (rettype, next) = get_type(&mut ts, next).unwrap();
                                if let Token{ kind: LeftBrace, .. } = next {
                                    let (body, errs) = get_block(&mut ts);
                                    let mangle = mangle(name.val.clone(), assoc.val.clone(), &mut names);
                                    result.funcs.insert((name.val.clone(), assoc.val.clone()), ast::Func{ mangle, name, assoc, params, rettype, body });
                                    errors.extend(errs);
                                }
                            }
                            _ => {
                                errors.push(Error::ExpectedRetType(next.span.clone(), next.kind.clone()));
                                while next.kind != LeftBrace { next = next_token(&mut ts, &mut errors)?; }
                                let (body, errs) = get_block(&mut ts);
                                let mangle = mangle(name.val.clone(), assoc.val.clone(), &mut names);
                                result.funcs.insert((name.val.clone(), assoc.val.clone()), ast::Func{ mangle, name, assoc, params, body, ..Default::default() });
                                errors.extend(errs);
                            }
                        }
                    }
                    _ => {
                        errors.push(Error::ExpectedRetType(next.span, next.kind));
                        next = next_token(&mut ts, &mut errors)?;
                        while next.kind != Comma { next = next_token(&mut ts, &mut errors)?; }
                    }
                }
            }
            Token{ kind: Struct,    .. } => {
                next = next_token(&mut ts, &mut errors)?;
                let name = if let Token{ kind: Identifier(id), span } = next {
                    Item{ val: id.clone(), span }
                } else {
                    unreachable!()
                };
                next = next_token(&mut ts, &mut errors)?;
                if next.kind != LeftBrace {
                    unreachable!()
                }
                next = next_token(&mut ts, &mut errors)?;
                let mut fields = Vec::new();
                while next.kind != RightBrace {
                    match next {
                        Token{ kind: I8|I16|I32|I64|U8|U16|U32|U64|F32|F64|Bool|Str|Null|Identifier(_), .. } => {
                            let (datatype, n) = get_type(&mut ts, next).unwrap();
                            next = n;
                            if let Token{ kind: Identifier(name), span } = next {
                                fields.push(Field{ datatype, name: Item::new(name, span.clone()), access: Item::new(AccessMod::Private, span) });
                            }
                            next = next_token(&mut ts, &mut errors)?;
                            if next.kind == Comma {
                                next = next_token(&mut ts, &mut errors)?;
                            }
                        }
                        Token{ kind: Pub, .. } => {
                            next = next_token(&mut ts, &mut errors)?;
                            match next {
                                Token{ kind: I8|I16|I32|I64|U8|U16|U32|U64|F32|F64|Bool|Str|Null|Identifier(_), .. } => {
                                    let typeinfo = get_type(&mut ts, next).unwrap();
                                    let datatype = typeinfo.0;
                                    next = typeinfo.1;
                                    if let Token{ kind: Identifier(name), span } = next {
                                        fields.push(Field{ datatype, name: Item::new(name, span.clone()), access: Item::new(AccessMod::Public, span) });
                                    }
                                    next = next_token(&mut ts, &mut errors)?;
                                    if next.kind == Comma {
                                        next = next_token(&mut ts, &mut errors)?;
                                    }
                                }
                                _ => unreachable!()
                            }
                        }
                        _ => unreachable!("{:?}", next)
                    }
                }
                result.structs.insert(name.val.clone(), ast::Struct{ name, fields });
            }
            Token{ kind: Eof,       .. } => {
                if !errors.is_empty() {
                    return Err(errors)
                } else {
                    return Ok(result)
                }
            }
            _ => {
                errors.push(Error::BadGlobalItem(next.span, next.kind));
                next = next_token(&mut ts, &mut errors)?;
                while next.kind != RightBrace && next.kind != Semicolon { next = next_token(&mut ts, &mut errors)? }
                next_token(&mut ts, &mut errors)?;
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(result)
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_void_func() {
        let tokens = crate::lex::lex("
            func some_function() {

            }", "dummy")
            .unwrap();

        let ast = parse(tokens);

        println!("{:#?}", ast);
    }
    
    #[test]
    fn parse_param_func() {
        let tokens = crate::lex::lex("
            func some_function(f32 a, i16[5] b, mut bool c) {

            }", "dummy")
            .unwrap();

        let ast = parse(tokens);

        println!("{:#?}", ast);
    }

    #[test]
    fn parse_static_method() {
        let tokens = crate::lex::lex("
            func SomeType.some_function(f32 a, i16[5] b, mut bool c) {

            }", "dummy")
            .unwrap();

        let ast = parse(tokens);

        println!("{:#?}", ast);
    }

    #[test]
    fn parse_method() {
        let tokens = crate::lex::lex("
            func SomeType.some_function(this, i16[5] b, mut bool c) {

            }", "dummy")
            .unwrap();

        let ast = parse(tokens);

        println!("{:#?}", ast);
    }

    #[test]
    fn parse_return_type() {
        let tokens = crate::lex::lex("
            struct Thing {
                i16 x,
                i16 y,
            }

            func some_function(f32 a, i16[5] b, mut bool c) -> Thing(2) {

            }", "dummy")
            .unwrap();

        let ast = parse(tokens);

        println!("{:#?}", ast);
    }

    #[test]
    fn parse_if_while() {
        let tokens = crate::lex::lex("
            func some_func() {

                bool a = false;

                while true {
                    if a {
                        i16 c = 5;
                    }
                }

                while false {
                    if true {
                        i32 x = 10;
                    }
                }

            }", "dummy")
            .unwrap();

        let ast = parse(tokens);

        println!("{:#?}", ast);
    }
    

    #[test]
    fn parse_struct1() {
        let tokens = crate::lex::lex("
            struct Player {
                pub f32(2) pos,
                pub u32 health,
                i32 money,
            }", "dummy")
            .unwrap();

        let test_case = parse(tokens)
                                    .expect("parsing failed:").structs
                                    .remove("Player")
                                    .expect("struct 'Player' was encountered while parsing");

        let expect = def_struct!(Player,
                                            (pub F32(2) pos),
                                            (pub U32 health),
                                            (I32 money)
                                        );

        assert_eq!(test_case, expect);
    }

    #[test]
    fn parse_struct2() {
        let tokens = crate::lex::lex("
            struct Health {
                pub u32 value,
            }

            struct Player {
                pub f32(2) pos,
                pub Health health,
                i32 money,
            }", "dummy")
            .unwrap();

        let test_case = parse(tokens)
                                    .expect("parsing failed:").structs
                                    .remove("Player")
                                    .expect("struct 'Player' was encountered while parsing");

        let expect = crate::def_struct!(Player,
                                            (pub F32(2) pos),
                                            (pub, "Health" health),
                                            (I32 money)
                                        );

        assert_eq!(test_case, expect);
    }
}
