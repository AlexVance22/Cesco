use crate::lex::{ Token, TokenKind::{ self, * } };
use super::{ CloneIter, Error, ast::* };
use crate::span::{ Span, Item };


macro_rules! scan_token(
    ($next:ident, $ts:ident) => {
        *$next = $ts.next().ok_or(Error::EndOfExprFile(Span::default()))?;
    }
);


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum Terminal {
    RightParen  = 1,
    RightBrack  = 1 << 1,
    RightBrace  = 1 << 2,
    Semicolon   = 1 << 3,
    LeftBrace   = 1 << 4,
    Comma       = 1 << 5,
    Else        = 1 << 6,
}


fn is_set(a: u32, b: Terminal) -> bool {
    (a & (b as u32)) != 0
}

fn is_terminal(t: &TokenKind, term: u32) -> bool {
    match t {
        RightParen if is_set(term, Terminal::RightParen) => true,
        RightBrack if is_set(term, Terminal::RightBrack) => true,
        RightBrace if is_set(term, Terminal::RightBrace) => true,
        Semicolon if is_set(term, Terminal::Semicolon) => true,
        LeftBrace if is_set(term, Terminal::LeftBrace) => true,
        Comma if is_set(term, Terminal::Comma) => true,
        Else if is_set(term, Terminal::Else) => true,
        _ => false
    }
}


fn parse_as(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let a = parse_un(ts, next, term)?;

    match next {
        Token{ kind: Equal, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            Ok(Item{ span: Span::merge(a.span.clone(), b.span.clone()), val: Expression::Assign{ left: Box::new(a), right: Box::new(b) } })
        }
        Token{ kind: AddEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::Add(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: SubEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::Sub(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: MulEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::Mul(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: DivEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::Div(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: ModEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::Mod(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: OrEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::BitOr(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: XorEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::BitXor(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: AndEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::BitAnd(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: LShiftEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::LShift(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind: RShiftEq, span } => {
            let b = parse_right(ts, term)?.ok_or(Error::EmptyExpr(span.clone()))?;
            let span = Span::merge(a.span.clone(), b.span.clone());
            let right = Item::new(Expression::RShift(Box::new(a.clone()), Box::new(b)), span.clone());
            Ok(Item{ span, val: Expression::Assign{ left: Box::new(a), right: Box::new(right) } })
        }
        Token{ kind, span } => {
            if is_terminal(kind, term) {
                Ok(a)
            } else {
                Err(Error::ExpectedOperator(span.clone(), kind.clone()))
            }
        }
    }
}

fn parse_te(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    match next {
        Token{ kind: Ask, span } => {
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let expr = match parse_or(ts, next, Terminal::Else as u32) {
                Ok(expr) => Box::new(expr),
                Err(Error::ExpectedOperator(line, kind)) => return Err(Error::AskWithoutElse(line, kind)),
                Err(err) => return Err(err),
            };
            scan_token!(next, ts);
            if let Token{ kind: LeftBrace, .. } = next {
                let body = AskFallback::Block(super::get_block(ts).0);
                let endspan = std::mem::take(&mut next.span);
                scan_token!(next, ts);
                Ok(Item::new(Expression::Ask{ expr, body }, Span::merge(span, endspan)))
            } else {
                let body = AskFallback::Expression(Box::new(parse_or(ts, next, term)?));
                Ok(Item::new(Expression::Ask{ expr, body }, Span::merge(span, next.span.clone())))
            }
        },
        _ => parse_or(ts, next, term)
    }
}

fn parse_or(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_an(ts, next, term)?;

    loop {
        match next {
            Token{ kind: Or, .. } => {
                scan_token!(next, ts);
                let b = parse_an(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Or(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_an(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_bo(ts, next, term)?;

    loop {
        match next {
            Token{ kind: And, .. } => {
                scan_token!(next, ts);
                let b = parse_bo(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::And(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: Or|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_bo(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_bx(ts, next, term)?;

    loop {
        match next {
            Token{ kind: BitOr, .. } => {
                scan_token!(next, ts);
                let b = parse_bx(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::BitOr(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: And|Or|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_bx(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_ba(ts, next, term)?;

    loop {
        match next {
            Token{ kind: BitXor, .. } => {
                scan_token!(next, ts);
                let b = parse_ba(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::BitXor(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: BitOr|And|Or|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_ba(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_eq(ts, next, term)?;

    loop {
        match next {
            Token{ kind: BitAnd, .. } => {
                scan_token!(next, ts);
                let b = parse_eq(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::BitAnd(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: BitXor|BitOr|And|Or|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    } 
}

fn parse_eq(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_cm(ts, next, term)?;

    loop {
        match next {
            Token{ kind: EqualEq, .. } => {
                scan_token!(next, ts);
                let b = parse_cm(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Eq(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: BangEq, .. } => {
                scan_token!(next, ts);
                let b = parse_cm(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Ne(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: BitAnd|BitXor|BitOr|And|Or|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_cm(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_sh(ts, next, term)?;

    loop {
        match next {
            Token{ kind: Less, .. } => {
                scan_token!(next, ts);
                let b = parse_sh(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Lt(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: Greater, .. } => {
                scan_token!(next, ts);
                let b = parse_sh(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Gt(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: LessEq, .. } => {
                scan_token!(next, ts);
                let b = parse_sh(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Le(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: GreaterEq, .. } => {
                scan_token!(next, ts);
                let b = parse_sh(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Ge(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: EqualEq|BangEq|BitAnd|BitXor|BitOr|And|Or|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_sh(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_ad(ts, next, term)?;

    loop {
        match next {
            Token{ kind: LShift, .. } => {
                scan_token!(next, ts);
                let b = parse_ad(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::LShift(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: RShift, .. } => {
                scan_token!(next, ts);
                let b = parse_ad(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::RShift(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: BitAnd|BitXor|BitOr|And|Or|Less|Greater|LessEq|GreaterEq|EqualEq|BangEq|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_ad(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_mu(ts, next, term)?;

    loop {
        match next {
            Token{ kind: Add, .. } => {
                scan_token!(next, ts);
                let b = parse_mu(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Add(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: Sub, .. } => {
                scan_token!(next, ts);
                let b = parse_mu(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Sub(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: LShift|RShift|BitAnd|BitXor|BitOr|And|Or|Less|Greater|LessEq|GreaterEq|EqualEq|BangEq|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_mu(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32) -> Result<Item<Expression>, Error> {
    let mut a = parse_un(ts, next, term)?;

    loop {
        match next {
            Token{ kind: Mul, .. } => {
                scan_token!(next, ts);
                let b = parse_un(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Mul(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: Div, .. } => {
                scan_token!(next, ts);
                let b = parse_un(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Div(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: Mod, .. } => {
                scan_token!(next, ts);
                let b = parse_un(ts, next, term)?;
                let span = Span::merge(a.span.clone(), b.span.clone());
                a = Item::new(Expression::Mod(Box::new(a), Box::new(b)), span);
            }
            Token{ kind: LShift|RShift|Add|Sub|BitAnd|BitXor|BitOr|And|Or|Less|Greater|LessEq|GreaterEq|EqualEq|BangEq|Ask, .. } => {
                return Ok(a)
            }
            Token{ kind, span } => {
                if is_terminal(kind, term) {
                    return Ok(a)
                } else {
                    return Err(Error::ExpectedOperator(span.clone(), kind.clone()))
                }
            }
        }
    }
}

fn parse_un(ts: &mut impl CloneIter<Token>, next: &mut Token, _term: u32) -> Result<Item<Expression>, Error> {
    match next {
        Token{ kind: This, span } => {
            let span = span.clone();
            let id = "this".to_string();
            scan_token!(next, ts);
            let (mut exp, mut found) = parse_po(ts, next, _term, Item::new(Expression::Identifier(id), span))?;
            while found {
                (exp, found) = parse_po(ts, next, _term, exp)?;
            }
            Ok(exp)
        }
        Token{ kind: Identifier(id), span } => {
            let id = std::mem::take(id);
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let (mut exp, mut found) = parse_po(ts, next, _term, Item::new(Expression::Identifier(id), span))?;
            while found {
                (exp, found) = parse_po(ts, next, _term, exp)?;
            }
            Ok(exp)
        }
        Token{ kind: Integer(i), span } => {
            let i = *i;
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let (mut exp, mut found) = parse_po(ts, next, _term, Item::new(Expression::IntLit(i), span))?;
            while found {
                (exp, found) = parse_po(ts, next, _term, exp)?;
            }
            Ok(exp)
        }
        Token{ kind: Float(f), span } => {
            let f = *f;
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let (mut exp, mut found) = parse_po(ts, next, _term, Item::new(Expression::FltLit(f), span))?;
            while found {
                (exp, found) = parse_po(ts, next, _term, exp)?;
            }
            Ok(exp)
        }
        Token{ kind: Boolean(b), span } => {
            let b = *b;
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let (mut exp, mut found) = parse_po(ts, next, _term, Item::new(Expression::BoolLit(b), span))?;
            while found {
                (exp, found) = parse_po(ts, next, _term, exp)?;
            }
            Ok(exp)
        }
        Token{ kind: Character(c), span } => {
            let c = *c;
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let (mut exp, mut found) = parse_po(ts, next, _term, Item::new(Expression::CharLit(c), span))?;
            while found {
                (exp, found) = parse_po(ts, next, _term, exp)?;
            }
            Ok(exp)
        }
        Token{ kind: StrLit(s), span } => {
            let s = std::mem::take(s);
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let (mut exp, mut found) = parse_po(ts, next, _term, Item::new(Expression::StrLit(s), span))?;
            while found {
                (exp, found) = parse_po(ts, next, _term, exp)?;
            }
            Ok(exp)
        }
        Token{ kind: Sub, span } => {
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let val = Box::new(parse_un(ts, next, _term)?);
            let vspan = val.span.clone();
            Ok(Item::new(Expression::Neg(val), Span::merge(span, vspan)))
        }
        Token{ kind: Bang, span } => {
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let val = Box::new(parse_un(ts, next, _term)?);
            let vspan = span.clone();
            Ok(Item::new(Expression::Not(val), Span::merge(span, vspan)))
        }
        Token{ kind: BitNot, span } => {
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let val = Box::new(parse_un(ts, next, _term)?);
            let vspan = span.clone();
            Ok(Item::new(Expression::BitNot(val), Span::merge(span, vspan)))
        }
        Token{ kind: Mul, span } => {
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let val = Box::new(parse_un(ts, next, _term)?);
            let vspan = span.clone();
            Ok(Item::new(Expression::Deref(val), Span::merge(span, vspan)))
        }
        Token{ kind: BitAnd, span } => {
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let val = Box::new(parse_un(ts, next, _term)?);
            let vspan = span.clone();
            Ok(Item::new(Expression::AddrOf(val), Span::merge(span, vspan)))
        }
        Token{ kind: LeftParen, .. } => {
            scan_token!(next, ts);
            let exp = parse_te(ts, next, Terminal::RightParen as u32)?;
            if let Token{ kind: RightParen, .. } = next {
                scan_token!(next, ts);
                let (mut exp, mut found) = parse_po(ts, next, _term, exp)?;
                while found {
                    (exp, found) = parse_po(ts, next, _term, exp)?;
                }
                Ok(exp)
            } else {
                unreachable!()
            }
        }
        Token{ kind: LeftBrack, span } => {
            let span = std::mem::take(span);
            scan_token!(next, ts);
            let mut args = Vec::new();
            while next.kind != RightBrack {
                let exp = parse_te(ts, next, Terminal::RightBrack as u32 | Terminal::Comma as u32)?;
                args.push(exp);
                if next.kind == Comma {
                    scan_token!(next, ts);
                }
            }
            scan_token!(next, ts);
            Ok(Item::new(Expression::ArrLit(args), Span::merge(span, next.span.clone())))
        }
        Token{ kind, span } => Err(Error::ExpectedExpr(span.clone(), kind.clone()))
    }
}

fn parse_po(ts: &mut impl CloneIter<Token>, next: &mut Token, term: u32, left: Item<Expression>) -> Result<(Item<Expression>, bool), Error> {
    match *next {
        Token{ kind: Colon, .. } => {
            scan_token!(next, ts);
            let fname = if let Token{ kind: Identifier(name), .. } = next {
                let name = std::mem::take(name);
                scan_token!(next, ts);
                name
            } else {
                return Err(Error::ExpectedFieldMethod(next.span.clone(), next.kind.clone()))
            };
            scan_token!(next, ts);
            if next.kind != LeftParen {
                return Err(Error::BadFuncName(next.span.clone(), next.kind.clone()))
            }
            scan_token!(next, ts);
            let mut args = Vec::new();
            while next.kind != RightParen {
                let exp = parse_te(ts, next, Terminal::RightParen as u32 | Terminal::Comma as u32)?;
                args.push(exp);
                if next.kind == Comma {
                    scan_token!(next, ts);
                }
            }
            match left.val {
                Expression::Identifier(name) => {
                    scan_token!(next, ts);
                    Ok((Item::new(Expression::Call{ name: fname, args, assoc: Association::Static(name) }, Span::merge(left.span, next.span.clone())), true))
                }
                _ => Err(Error::NotCallable(left.span.clone(), left.val))
            }
        }
        Token{ kind: Dot, .. } => {
            scan_token!(next, ts);
            if let Token{ kind: Identifier(name), span } = next {
                let span = std::mem::take(span);
                let name = std::mem::take(name);
                let memspan = Span::merge(left.span.clone(), span.clone());
                scan_token!(next, ts);
                Ok((Item::new(Expression::Member{ obj: Box::new(left), member: Box::new(Item::new(Expression::Identifier(name), span)), deref: false }, memspan), true))
            } else {
                Err(Error::ExpectedFieldMethod(next.span.clone(), next.kind.clone()))
            }
        }
        Token{ kind: Arrow, .. } => {
            scan_token!(next, ts);
            if let Token{ kind: Identifier(name), span } = next {
                let span = std::mem::take(span);
                let name = std::mem::take(name);
                let memspan = Span::merge(left.span.clone(), span.clone());
                scan_token!(next, ts);
                Ok((Item::new(Expression::Member{ obj: Box::new(left), member: Box::new(Item::new(Expression::Identifier(name), span)), deref: true }, memspan), true))
            } else {
                Err(Error::ExpectedFieldMethod(next.span.clone(), next.kind.clone()))
            }
        }
        Token{ kind: LeftBrack, .. } => {
            scan_token!(next, ts);
            if let Token{ kind: RightBrack, span } = next {
                return Err(Error::EmptyIndex(span.clone()))
            }
            let exp = parse_te(ts, next, Terminal::RightBrack as u32)?;
            if let Token{ kind: RightBrack, span: endspan } = next {
                let endspan = std::mem::take(endspan);
                scan_token!(next, ts);
                let lspan = left.span.clone();
                Ok((Item::new(Expression::ArrGet{ obj: Box::new(left), index: Box::new(exp) }, Span::merge(lspan, endspan)), true))
            } else {
                Err(Error::UnknownExprError(next.span.clone(), next.kind.clone()))
            }
        }
        Token{ kind: LeftParen, .. } => {
            scan_token!(next, ts);
            let mut args = Vec::new();
            while next.kind != RightParen {
                let exp = parse_te(ts, next, Terminal::RightParen as u32 | Terminal::Comma as u32)?;
                args.push(exp);
                if next.kind == Comma {
                    scan_token!(next, ts);
                }
            }
            match left.val {
                Expression::Member { obj, member, deref } => {
                    if let Expression::Identifier(name) = member.val {
                        let endspan = std::mem::take(&mut next.span);
                        scan_token!(next, ts);
                        if deref {
                            args.insert(0, *obj);
                        } else {
                            args.insert(0, Item::new(Expression::AddrOf(obj), member.span));
                        }
                        Ok((Item::new(Expression::Call{ name, args, assoc: Association::Method("".to_string()) }, Span::merge(left.span, endspan)), true))
                    } else {
                        Err(Error::NotCallable(member.span.clone(), member.val))
                    }
                }
                /*
                Expression::PtrMember { obj, member } => {
                    if let Expression::Identifier(name) = *member {
                        scan_token!(next, ts);
                        args.insert(0, *obj);
                        Ok((Expression::Call{ name, args, assoc: Association::Method("".to_string()) }, true))
                    } else {
                        Err(Error::NotCallable(next.span.clone(), *member))
                    }
                }
                */
                Expression::Identifier(name) => {
                    let endspan = next.span.clone();
                    scan_token!(next, ts);
                    Ok((Item::new(Expression::Call{ name, args, assoc: Association::None }, Span::merge(left.span, endspan)), true))
                }
                _ => Err(Error::NotCallable(left.span.clone(), left.val))
            }
        }
        Token{ kind: LeftBrace, .. } => {
            if is_set(term, Terminal::LeftBrace) {
                return Ok((left, false))
            }
            scan_token!(next, ts);
            let mut args = Vec::new();
            while next.kind != RightBrace {
                let left = if let Token{ kind: Identifier(id), span } = next {
                    Item::new(std::mem::take(id), std::mem::take(span))
                } else {
                    return Err(Error::UnknownExprError(next.span.clone(), next.kind.clone()))
                };
                scan_token!(next, ts);
                match next.kind {
                    Comma => {
                        args.push((left.val.clone(), Item::new(Expression::Identifier(left.val), left.span)));
                        scan_token!(next, ts);
                    }
                    Colon => {
                        scan_token!(next, ts);
                        let exp = parse_te(ts, next, Terminal::RightBrace as u32 | Terminal::Comma as u32)?;
                        args.push((left.val, exp));
                        if next.kind == Comma {
                            scan_token!(next, ts);
                        }
                    }
                    _ => return Err(Error::UnknownExprError(next.span.clone(), next.kind.clone()))
                }
            }
            if let Expression::Identifier(name) = left.val {
                let endspan = std::mem::take(&mut next.span);
                scan_token!(next, ts);
                Ok((Item::new(Expression::StructLit(name, args), Span::merge(left.span, endspan)), true))
            }
            else {
                Err(Error::NotCallable(left.span, left.val))
            } 
        }
        _ => Ok((left, false))
    }
}


pub fn parse(ts: &mut impl CloneIter<Token>, next: Option<Token>, term: u32) -> Result<Item<Expression>, Error> {
    let mut next = if let Some(n) = next {
        n
    } else {
        ts.next().ok_or(Error::EndOfExprFile(Span::default()))?
    };

    match next {
        Token{ kind: Def,       span } => Ok(Item::new(Expression::Default, span)),
        Token{ kind: Null,      span } => Ok(Item::new(Expression::Null, span)),
        Token{ kind: Semicolon, span } => Err(Error::EmptyExpr(span)),
        Token{ kind: LeftBrace, span } => Err(Error::EmptyExpr(span)),
        _ => parse_as(ts, &mut next, term)
    }
}

pub fn parse_right(ts: &mut impl CloneIter<Token>, term: u32) -> Result<Option<Item<Expression>>, Error> {
    let mut next = ts.next().ok_or(Error::EndOfExprFile(Span::default()))?;

    match next {
        Token{ kind: Def,  span } => Ok(Some(Item::new(Expression::Default, span))),
        Token{ kind: Null, span } => Ok(Some(Item::new(Expression::Null, span))),
        Token{ kind: Semicolon, .. } if is_set(term, Terminal::Semicolon) => Ok(None),
        Token{ kind: LeftBrace, .. } if is_set(term, Terminal::LeftBrace) => Ok(None),
        _ => Ok(Some(parse_te(ts, &mut next, term)?))
    }
}