mod token;
mod error;

pub use token::{ Token, TokenKind };
use crate::span::Span;
pub use error::Error;


enum Mode {
    //valid
    Void,

    Ident,

    NumZero,
    NumInt,
    NumIntEnd,
    Num,
    NumDot,
    NumEnd,

    EqDuo,
    ShiftEq,

    Str,
    Char,
    CharEnd,

    LineComment,
    BlockComment,
    BlockCommentEnd,

    //error
    SeekSingleQuote(Error),
    SeekIdEnd(Error),
}


fn trim_escapes(val: &str, mut charbeg: u32, line: u32, file: &str) -> Result<String, Error> {
    let mut result = String::new();
    let mut esc = false;
    let mut charend = charbeg;

    for c in val.chars() {
        charend += 1;

        if esc {
            match c {
                'n' => result.push('\n'),
                't' => result.push('\t'),
                'r' => result.push('\r'),
                '0' => result.push('\0'),
                '\\' => result.push('\\'),
                '\'' => result.push('\''),
                '\"' => result.push('\"'),
                _ => return Err(Error::BadEscape(Span::new(charbeg, charend, line, file), c))
            }
            esc = false;
        } else if c == '\\' {
            esc = true;
            charbeg = charend; 
        } else {
            result.push(c);
        }
    }

    Ok(result)
}


pub fn lex(src: &str, file: &str) -> Result<Vec<Token>, Vec<Error>> {
    let mut errors: Vec<Error> = Vec::new();
    let mut tokens = Vec::new();
    let mut mode = Mode::Void;

    let mut begin: usize = 0;
    let mut line: u32 = 1;
    let mut charbeg: u32 = 1;
    let mut charend: u32 = 1;

    for (i, c) in src.chars().enumerate() {
        if c == '\n' {
            line += 1;
            charend = 1;
        } else {
            charend += 1;
        }

        if c.len_utf8() > 1 { return Err(errors) }

        mode = match mode {
            Mode::Void => match c {
                ' '|'\t'|'\r'|'\n' => Mode::Void,
                'a'..='z'|'A'..='Z'|'_' => {
                    begin = i;
                    charbeg = charend;
                    Mode::Ident
                }
                '0' => {
                    begin = i;
                    charbeg = charend;
                    Mode::NumZero
                }
                '1'..='9' => {
                    begin = i;
                    charbeg = charend;
                    Mode::Num
                }
                '"' => {
                    begin = i;
                    charbeg = charend;
                    Mode::Str
                }
                '\'' => {
                    begin = i;
                    charbeg = charend;
                    Mode::Char
                }
                '('|')'|'['|']'|'{'|'}'|','|'.'|';'|'?' => {
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|'='|':' =>  {
                    begin = i;
                    Mode::EqDuo
                }
                _ => {
                    errors.push(Error::InvalidCharacter(Span::new(charbeg, charend, line, file), c));
                    Mode::Void
                }
            }
            
            Mode::Ident => match c {
                'a'..='z'|'A'..='Z'|'0'..='9'|'_' => Mode::Ident,
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '('|')'|'['|']'|'{'|'}'|','|'.'|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|'='|':' =>  {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::EqDuo
                }
                '"' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Str
                }
                '\'' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Str
                }
                _ => Mode::SeekIdEnd(Error::InvalidIdent(Token::as_err(TokenKind::Identifier(src[begin..=i].to_string()), Span::new(charbeg, charend, line, file))))
            }
            
            Mode::NumZero => match c {
                'x'|'o'|'b' => Mode::NumInt,
                '0'..='9' => Mode::Num,
                '_' => Mode::Num,
                '.' => Mode::NumDot,
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '('|')'|'['|']'|'{'|'}'|','|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|'='|':' =>  {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::EqDuo
                }
                _ => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    errors.push(Error::BadIntLit(Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
            }
            Mode::NumInt => match c {
                '0'..='9' => Mode::NumIntEnd,
                _ => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    errors.push(Error::BadIntLit(Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
            }
            Mode::NumIntEnd => match c {
                '0'..='9' => Mode::NumIntEnd,
                '_' => Mode::NumIntEnd,
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '('|')'|'['|']'|'{'|'}'|','|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|'='|':' =>  {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::EqDuo
                }
                _ => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    errors.push(Error::BadIntLit(Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
            }
            Mode::Num => match c {
                '0'..='9' => Mode::Num,
                '_' => Mode::Num,
                '.' => Mode::NumDot,
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '('|')'|'['|']'|'{'|'}'|','|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|'='|':' =>  {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::EqDuo
                }
                _ => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    errors.push(Error::BadIntLit(Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
            } 
            Mode::NumDot => match c {
                '0'..='9' => Mode::NumEnd,
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '('|')'|'['|']'|'{'|'}'|','|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|'='|':' =>  {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::EqDuo
                }
                _ => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    errors.push(Error::BadFltLit(Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
            }
            Mode::NumEnd => match c {
                '0'..='9' => Mode::NumEnd,
                '_' => Mode::Num,
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '('|')'|'['|']'|'{'|'}'|','|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|'='|':' =>  {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::EqDuo
                }
                _ => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    errors.push(Error::BadFltLit(Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
            }
            
            Mode::EqDuo => match c {
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '0' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::NumZero
                }
                '1'..='9' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Num
                }
                'a'..='z'|'A'..='Z'|'_' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Ident
                }
                '"' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Str
                }
                '\'' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Char
                }
                '('|')'|'['|']'|'{'|'}'|','|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '=' => {
                    tokens.push(Token::from(&src[begin..=i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'+'|'-'|':' =>  {
                    if src.chars().nth(begin).unwrap() == c {
                        tokens.push(Token::from(&src[begin..=i], Span::new(charbeg, charend, line, file)));
                        Mode::Void
                    } else {
                        errors.push(Error::InvalidOperator(Span::new(charbeg, charend, line, file), src[begin..=i].to_string()));
                        Mode::Void
                    }
                }
                '<' => {
                    if src.chars().nth(begin).unwrap() == c {
                        Mode::ShiftEq
                    } else {
                        errors.push(Error::InvalidOperator(Span::new(charbeg, charend, line, file), src[begin..=i].to_string()));
                        Mode::Void
                    }
                }
                '>' => {
                    let last = src.chars().nth(begin).unwrap();
                    if last == '-' {
                        tokens.push(Token::from(&src[begin..=i], Span::new(charbeg, charend, line, file)));
                        Mode::Void
                    } else if last == c {
                        Mode::ShiftEq
                    } else {
                        errors.push(Error::InvalidOperator(Span::new(charbeg, charend, line, file), src[begin..=i].to_string()));
                        Mode::Void
                    }
                }
                '/' => {
                    if src.chars().nth(begin).unwrap() == '/' {
                        Mode::LineComment
                    } else {
                        errors.push(Error::InvalidOperator(Span::new(charbeg, charend, line, file), src[begin..=i].to_string()));
                        Mode::Void
                    }
                }
                '*' => {
                    if src.chars().nth(begin).unwrap() == '/' {
                        Mode::BlockComment
                    } else {
                        errors.push(Error::InvalidOperator(Span::new(charbeg, charend, line, file), src[begin..=i].to_string()));
                        Mode::Void
                    }
                }
                _ => {
                    errors.push(Error::InvalidOperator(Span::new(charbeg, charend, line, file), src[begin..=i].to_string()));
                    Mode::Void
                }
            }
            Mode::ShiftEq => match c {
                ' '|'\t'|'\r'|'\n' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '0' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::NumZero
                }
                '1'..='9' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Num
                }
                'a'..='z'|'A'..='Z'|'_' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Ident
                }
                '"' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Str
                }
                '\'' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    charbeg = charend;
                    Mode::Char
                }
                '('|')'|'['|']'|'{'|'}'|','|';'|'?' => {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    tokens.push(Token::from(c.to_string(), Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '=' => {
                    tokens.push(Token::from(&src[begin..=i], Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                '&'|'|'|'^'|'+'|'-'|'*'|'/'|'%'|'<'|'>'|'!'|':' =>  {
                    tokens.push(Token::from(&src[begin..i], Span::new(charbeg, charend, line, file)));
                    begin = i;
                    Mode::EqDuo
                }
                _ => {
                    errors.push(Error::InvalidOperator(Span::new(charbeg, charend, line, file), src[begin..=i].to_string()));
                    Mode::Void
                }
            }

            Mode::Str => match c {
                '"' => {
                    let escaped = trim_escapes(&src[begin..=i], charbeg, line, file).map_err(|e| vec![e])?;
                    tokens.push(Token::from(escaped, Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                _ => Mode::Str
            }
            Mode::Char => match c {
                '\'' => {
                    errors.push(Error::BadCharLit(Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                _ => Mode::CharEnd
            }
            Mode::CharEnd => match c {
                '\'' => {
                    let escaped = trim_escapes(&src[begin..=i], charbeg, line, file).map_err(|e| vec![e])?;
                    tokens.push(Token::from(escaped, Span::new(charbeg, charend, line, file)));
                    Mode::Void
                }
                _ => Mode::SeekSingleQuote(Error::BadCharLit(Span::new(charbeg, charend, line, file)))
            }
        
            Mode::LineComment => match c {
                '\n' => Mode::Void,
                _ => Mode::LineComment
            }
            Mode::BlockComment => match c {
                '*' => Mode::BlockCommentEnd,
                _ => Mode::BlockComment
            }
            Mode::BlockCommentEnd => match c {
                '/' => Mode::Void,
                _ => Mode::BlockComment
            }

            // error termination

            Mode::SeekSingleQuote(error) => match c {
                '\'' => {
                    errors.push(error);
                    Mode::Void
                }
                _ => Mode::SeekSingleQuote(error)
            }
            Mode::SeekIdEnd(mut error) => match c {
                'a'..='z'|'A'..='Z'|'0'..='9'|'_' => {
                    if let Error::InvalidIdent(token) = &mut error {
                        if let TokenKind::Identifier(ident) = &mut token.kind {
                            ident.push(c);
                        }
                    }
                    Mode::SeekIdEnd(error)
                }
                _ => {
                    errors.push(error);
                    Mode::Void
                }
            }
        }
    }

    if errors.is_empty() {
        tokens.push(Token{ kind: TokenKind::Eof, span: Span::new(charend, charend, line, file) });
        Ok(tokens)
    } else {
        Err(errors)
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_simple() {
        let tokens = lex("
            func from_vec2() -> f32 {
                f32 angle = atan(y, x);
                f32 rad = length();
                str mut hello = \"hello world\";

                while true {
                    rad = 5.0;

                    while grog {
                        break;
                    }
                }

                return angle;
            }", "dummy.cco")
            .unwrap();

        for tok in tokens {
            println!("{:?}", tok.kind);
        }
    }

    #[test]
    fn lex_all() {
        let tokens = lex("
            import io;

            func test_func2() -> i32 {
                bool a = true;
                mut i32 abc = 120;
                str c23 = \"Hello World\\n\";
                f64[2] arr = [5.5, 10.5];

                io.console.out(c23);

                while true {
                    f32(3) pos = (5.75, 10., 2.);

                    while grog {
                        break;
                    }
                }
            }
            ", "dummy.cco")
            .unwrap();

        for tok in tokens {
            println!("{:?}", tok.kind);
        }
    }

    #[test]
    fn lex_escapes() {
        let tokens = lex("
            mut str hello = \"hello \\\\n world\";
            mut str hello = \"hello \\n world\";
            mut str hello = \"hello \n world\";
            ", "dummy.cco")
            .unwrap();

        for tok in tokens {
            if let TokenKind::StrLit(val) = tok.kind {
                println!("{}", val);
            }
        }
    }

    #[test]
    fn lex_error() {
        let tokens = lex("
            import io;

            func test_func2() -> i32 {
                bool a = true;
                mut i32 abc = 120;
                str c23 = \"Hello World\\n\";
                f64[2] arr = [5.5, 10.5];

                io.console.out(c23);

                while true {
                    f32(3) pos = (5.75, 10., 2.);

                    while grog {
                        break;
                    }
                }
            }
            ", "dummy.cco")
            .unwrap();

        for tok in tokens {
            println!("{:?}", tok.kind);
        }
    }
}
