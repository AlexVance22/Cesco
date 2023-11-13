#![allow(unused)]


use crate::lex::{ Token, TokenKind };
use super::ast::Association;
use super::CloneIter;


macro_rules! scan_token(
    ($next:ident, $ts:ident) => {
        $next = $ts.next().ok_or(())?
    }
);


#[derive(Debug)]
pub enum Decl {
    Func(String, Association),
    Struct(String),
}


pub fn get_names<'a>(mut ts: impl CloneIter<&'a Token>) -> Result<Vec<Decl>, ()> {
    use TokenKind::*;

    let mut result = Vec::new();


    while let Some(mut next) = ts.next() {
        match next {
            Token{ kind: Func, .. } => {
                scan_token!(next, ts);
                let first = if let Token{ kind: Identifier(id), .. } = next {
                    id.clone()
                } else {
                    return Err(())
                };
                scan_token!(next, ts);
                match next {
                    Token{ kind: LeftParen, .. } => {
                        result.push(Decl::Func(first, Association::None));
                        continue
                    }
                    Token{ kind: Dot, .. } => (),
                    _ => return Err(())
                }
                scan_token!(next, ts);
                let second = if let Token{ kind: Identifier(id), .. } = next {
                    id.clone()
                } else {
                    return Err(())
                };
                scan_token!(next, ts);
                if next.kind != LeftParen {
                    return Err(())
                }
                scan_token!(next, ts);
                match next {
                    Token{ kind: This, .. } => {
                        result.push(Decl::Func(first, Association::Method(second)));
                        continue
                    }
                    Token{ kind: Mut, .. } => (),
                    _ => ()
                }
                scan_token!(next, ts);
                match next {
                    Token{ kind: This, .. } => {
                        result.push(Decl::Func(first, Association::Method(second)));
                        continue
                    }
                    _ => result.push(Decl::Func(first, Association::Static(second)))
                }
            }

            Token{ kind: Struct, .. } => {
                scan_token!(next, ts);
                if let Token{ kind: Identifier(id), .. } = next {
                    result.push(Decl::Struct(id.clone()))
                }
                continue
            }
            _ => continue,
        }
    }

    Ok(result)
}
