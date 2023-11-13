use std::collections::HashMap;
use crate::span::Span;
use crate::check;
use super::*;


#[derive(Debug, Clone, PartialEq)]
pub enum AskFallback {
    Expression(Box<Item<Expression>>),
    Block(Vec<Statement>),
}


#[derive(Debug, Default, Clone, PartialEq)]
pub enum Expression {
    #[default]
    Default,
    Null,

    // constants
    IntLit(i64),
    FltLit(f64),
    BoolLit(bool),
    CharLit(char),
    StrLit(String),
    Identifier(String),
    ArrLit(Vec<Item<Expression>>),
    StructLit(String, Vec<(String, Item<Expression>)>),

    // math
    Neg(Box<Item<Expression>>),
    Add(Box<Item<Expression>>, Box<Item<Expression>>),
    Sub(Box<Item<Expression>>, Box<Item<Expression>>),
    Mul(Box<Item<Expression>>, Box<Item<Expression>>),
    Div(Box<Item<Expression>>, Box<Item<Expression>>),
    Mod(Box<Item<Expression>>, Box<Item<Expression>>),

    // logic
    Not(Box<Item<Expression>>),
    And(Box<Item<Expression>>, Box<Item<Expression>>),
    Or (Box<Item<Expression>>, Box<Item<Expression>>),

    // comparison
    Lt(Box<Item<Expression>>, Box<Item<Expression>>),
    Gt(Box<Item<Expression>>, Box<Item<Expression>>),
    Le(Box<Item<Expression>>, Box<Item<Expression>>),
    Ge(Box<Item<Expression>>, Box<Item<Expression>>),
    Eq(Box<Item<Expression>>, Box<Item<Expression>>),
    Ne(Box<Item<Expression>>, Box<Item<Expression>>),

    // bitwise
    BitNot(Box<Item<Expression>>),
    BitAnd(Box<Item<Expression>>, Box<Item<Expression>>),
    BitOr (Box<Item<Expression>>, Box<Item<Expression>>),
    BitXor(Box<Item<Expression>>, Box<Item<Expression>>),
    LShift(Box<Item<Expression>>, Box<Item<Expression>>),
    RShift(Box<Item<Expression>>, Box<Item<Expression>>),

    // pointers
    Deref (Box<Item<Expression>>),
    AddrOf(Box<Item<Expression>>),

    // fanciness
    Assign{ left: Box<Item<Expression>>, right: Box<Item<Expression>>},
    Call  { name: String, args: Vec<Item<Expression>>, assoc: Association },
    Ask   { expr: Box<Item<Expression>>, body: AskFallback },
    Member{ obj: Box<Item<Expression>>, member: Box<Item<Expression>>, deref: bool },
    ArrGet{ obj: Box<Item<Expression>>, index: Box<Item<Expression>> },
}

impl Expression {
    pub fn is_lit(&self) -> bool {
        matches!(self, Self::Default|Self::Null|Self::IntLit(_)|Self::FltLit(_)|Self::BoolLit(_)|Self::CharLit(_)|Self::StrLit(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Identifier(_))
    }

    pub fn is_leaf(&self) -> bool {
        self.is_lit() || self.is_ident()
    }

    pub fn is_const(&self) -> bool {
        match self {
            Self::Default|Self::Null|Self::IntLit(_)|Self::FltLit(_)|Self::BoolLit(_)|Self::CharLit(_)|Self::StrLit(_) => true,
            Self::Identifier(_) => false,
            Self::ArrLit(exprs) => {
                exprs.iter().all(|e| e.val.is_const())
            }
            Self::StructLit(_, exprs) => {
                exprs.iter().all(|e| e.1.val.is_const())
            }

            Self::Neg(v) => v.val.is_const(),
            Self::Add(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Sub(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Mul(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Div(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Mod(l, r) => l.val.is_const() && r.val.is_const(),
        
            Self::Not(v) => v.val.is_const(),
            Self::And(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Or (l, r) => l.val.is_const() && r.val.is_const(),
        
            Self::Lt(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Gt(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Le(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Ge(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Ne(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Eq(l, r) => l.val.is_const() && r.val.is_const(),
        
            Self::BitNot(v) => v.val.is_const(),
            Self::BitAnd(l, r) => l.val.is_const() && r.val.is_const(),
            Self::BitOr (l, r) => l.val.is_const() && r.val.is_const(),
            Self::BitXor(l, r) => l.val.is_const() && r.val.is_const(),
            Self::LShift(l, r) => l.val.is_const() && r.val.is_const(),
            Self::RShift(l, r) => l.val.is_const() && r.val.is_const(),

            Self::Deref (v) => v.val.is_const(),
            Self::AddrOf(v) => v.val.is_const(),

            Self::Assign{ .. } => false,
            Self::Call  { .. } => false,
            Self::Ask   { expr, body } => {
                match body {
                    AskFallback::Block(_) => false,
                    AskFallback::Expression(right) => expr.val.is_const() && right.val.is_const()
                }
            }
            Self::Member{ .. } => false,
            Self::ArrGet{ obj, index } => obj.val.is_const() && index.val.is_const(),
        }
    }

    pub fn is_lvalue(&self) -> bool {
        match self {
            Self::Default|Self::Null|Self::IntLit(_)|Self::FltLit(_)|Self::BoolLit(_)|Self::CharLit(_)|Self::StrLit(_) => false,
            Self::Identifier(_) => true,
            Self::ArrLit   (..) => false,
            Self::StructLit(..) => false,

            Self::Neg(..) |
            Self::Add(..) |
            Self::Sub(..) |
            Self::Mul(..) |
            Self::Div(..) |
            Self::Mod(..) |
            Self::Not(..) |
            Self::And(..) |
            Self::Or (..) |
            Self::Lt (..) |
            Self::Gt (..) |
            Self::Le (..) |
            Self::Ge (..) |
            Self::Ne (..) |
            Self::Eq (..) |
            Self::BitNot(..) |
            Self::BitAnd(..) |
            Self::BitOr (..) |
            Self::BitXor(..) |
            Self::LShift(..) |
            Self::RShift(..) => false,

            Self::Deref (..) => true,
            Self::AddrOf(..) => false,

            Self::Assign{ .. } => false,
            Self::Call  { .. } => false,
            Self::Ask   { expr, body } => {
                match body {
                    AskFallback::Block(_) => false,
                    AskFallback::Expression(right) => expr.val.is_lvalue() && right.val.is_lvalue()
                }
            }
            Self::Member{ obj, .. } => obj.val.is_lvalue(),
            Self::ArrGet{ obj, .. } => obj.val.is_lvalue(),
        }
    }

    pub fn is_ptr(&self, locals: &HashMap<String, (Type, bool)>, ast: &Ast) -> bool {
        match self {
            Self::Default|Self::Null|Self::IntLit(_)|Self::FltLit(_)|Self::BoolLit(_)|Self::CharLit(_)|Self::StrLit(_) => false,
            Self::Identifier(id) => {
                matches!(locals.get(id), Some((Type::PtrTo(_)|Type::RefTo(_), _)))
            }
            Self::ArrLit   (..) => false,
            Self::StructLit(..) => false,

            Self::Neg(..) => false,
            Self::Add(left, ..) => left.val.is_ptr(locals, ast),
            Self::Sub(left, ..) => left.val.is_ptr(locals, ast),
            Self::Mul(..) |
            Self::Div(..) |
            Self::Mod(..) |
            Self::Not(..) |
            Self::And(..) |
            Self::Or (..) |
            Self::Lt (..) |
            Self::Gt (..) |
            Self::Le (..) |
            Self::Ge (..) |
            Self::Ne (..) |
            Self::Eq (..) |
            Self::BitNot(..) |
            Self::BitAnd(..) |
            Self::BitOr (..) |
            Self::BitXor(..) |
            Self::LShift(..) |
            Self::RShift(..) => false,

            Self::Deref (val) => val.val.is_ptr(locals, ast),
            Self::AddrOf(..) => true,

            Self::Assign{ .. } => false,
            Self::Call  { name, assoc, .. } => {
                if let Some(func) = ast.funcs.get(&(name.clone(), assoc.clone())) {
                    func.rettype.val.is_ptr()
                } else {
                    false
                }
            }  
            Self::Ask   { expr, body } => {
                match body {
                    AskFallback::Block(_) => false,
                    AskFallback::Expression(right) => expr.val.is_ptr(locals, ast) && right.val.is_ptr(locals, ast)
                }
            }
            Self::Member{ obj, .. } => obj.val.is_lvalue(),
            Self::ArrGet{ obj, .. } => obj.val.is_lvalue(),
        }
    }


    fn type_of_math(left: &Item<Expression>, right: &Item<Expression>, locals: &HashMap<String, (Type, bool)>, ast: &Ast) -> Result<(Type, bool), check::Error> {
        let left_t = left.val.type_of(locals, &left.span, ast)?;
        let right_t = right.val.type_of(locals, &right.span, ast)?;
        if left_t == right_t && left_t.0.is_numeric() && right_t.0.is_numeric() {
            Ok(left_t)
        } else {
            let span = Span{ beg: left.span.end, end: right.span.beg, line: left.span.line, file: left.span.file.clone() };
            Err(check::Error::InvalidExpression(span, left_t.0, right_t.0, "math".to_string()))
        }
    }

    fn type_of_logic(left: &Item<Expression>, right: &Item<Expression>, locals: &HashMap<String, (Type, bool)>, ast: &Ast) -> Result<(Type, bool), check::Error> {
        let left_t = left.val.type_of(locals, &left.span, ast)?;
        let right_t = right.val.type_of(locals, &right.span, ast)?;
        if left_t.0.is_boolean() && right_t.0.is_boolean() {
            Ok((Type::Bool, true))
        } else {
            let span = Span{ beg: left.span.end, end: right.span.beg, line: left.span.line, file: left.span.file.clone() };
            Err(check::Error::InvalidExpression(span, left_t.0, right_t.0, "comparison".to_string()))
        }
    }

    fn type_of_cmp(left: &Item<Expression>, right: &Item<Expression>, locals: &HashMap<String, (Type, bool)>, ast: &Ast) -> Result<(Type, bool), check::Error> {
        let left_t = left.val.type_of(locals, &left.span, ast)?;
        let right_t = right.val.type_of(locals, &right.span, ast)?;
        if left_t == right_t && left_t.0.is_numeric() && right_t.0.is_numeric() {
            Ok((Type::Bool, true))
        } else {
            let span = Span{ beg: left.span.end, end: right.span.beg, line: left.span.line, file: left.span.file.clone() };
            Err(check::Error::InvalidExpression(span, left_t.0, right_t.0, "comparison".to_string()))
        }
    }

    fn type_of_eq(left: &Item<Expression>, right: &Item<Expression>, locals: &HashMap<String, (Type, bool)>, ast: &Ast) -> Result<(Type, bool), check::Error> {
        let left_t = left.val.type_of(locals, &left.span, ast)?;
        let right_t = right.val.type_of(locals, &right.span, ast)?;
        if left_t == right_t {
            Ok((Type::Bool, true))
        } else {
            let span = Span{ beg: left.span.end, end: right.span.beg, ..left.span.clone() };
            Err(check::Error::InvalidExpression(span, left_t.0, right_t.0, "equality".to_string()))
        }
    }


    pub fn type_of(&self, locals: &HashMap<String, (Type, bool)>, span: &Span, ast: &Ast) -> Result<(Type, bool), check::Error> {
        match self {
            Self::Null => Ok((Type::Null, false)),
            Self::IntLit(_) => Ok((Type::I64, false)),
            Self::FltLit(_) => Ok((Type::F64, false)),
            Self::BoolLit(_) => Ok((Type::Bool, false)),
            Self::CharLit(_) => Ok((Type::Char, false)),
            Self::StrLit(_) => Ok((Type::Str, false)),
            Self::Identifier(id) => {
                if let Some(t) = locals.get(id) {
                    Ok(t.clone())
                } else {
                    Err(check::Error::VarNotDeclared(span.clone(), id.clone()))
                }
            }
            Self::ArrLit(elems) => {
                if let Some(e) = elems.first() {
                    Ok((Type::ArrOf(Box::new(e.val.type_of(locals, &e.span, ast)?.0), elems.len()), true))
                } else {
                    Ok((Type::ArrOf(Box::new(Type::Null), elems.len()), true))
                }
            }
            Self::StructLit(t, _) => Ok((Type::User(t.clone()), true)),

            Expression::Neg(left) => {
                let t = left.val.type_of(locals, &left.span, ast)?; 
                if t.0.is_numeric() {
                    Ok(t)
                } else {
                    Err(check::Error::InvalidUnary(Span::default(), t.0, "negate".to_string()))
                }
            }
            Expression::Add(left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::Sub(left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::Mul(left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::Div(left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::Mod(left, right) => Self::type_of_math(left, right, locals, ast),
        
            Expression::Not(left) => {
                let t = left.val.type_of(locals, &left.span, ast)?; 
                if t.0.is_boolean() {
                    Ok((Type::Bool, true))
                } else {
                    Err(check::Error::InvalidUnary(Span::default(), t.0, "not".to_string()))
                }
            }
            Expression::And(left, right) => Self::type_of_logic(left, right, locals, ast),
            Expression::Or (left, right) => Self::type_of_logic(left, right, locals, ast),
        
            Expression::Lt(left, right) => Self::type_of_cmp(left, right, locals, ast),
            Expression::Gt(left, right) => Self::type_of_cmp(left, right, locals, ast),
            Expression::Le(left, right) => Self::type_of_cmp(left, right, locals, ast),
            Expression::Ge(left, right) => Self::type_of_cmp(left, right, locals, ast),
            Expression::Eq(left, right) => Self::type_of_eq (left, right, locals, ast),
            Expression::Ne(left, right) => Self::type_of_eq (left, right, locals, ast),

            Expression::BitNot(left) => {
                let t = left.val.type_of(locals, &left.span, ast)?; 
                if t.0.is_numeric() {
                    Ok(t)
                } else {
                    Err(check::Error::InvalidUnary(Span::default(), t.0, "bitwise not".to_string()))
                }
            }
            Expression::BitAnd(left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::BitOr (left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::BitXor(left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::LShift(left, right) => Self::type_of_math(left, right, locals, ast),
            Expression::RShift(left, right) => Self::type_of_math(left, right, locals, ast),
        
            Expression::Deref (left) => {
                let t = left.val.type_of(locals, &left.span, ast)?; 
                match t.0 {
                    Type::RefTo(u) => Ok((*u, t.1)),
                    Type::PtrTo(_) => Err(check::Error::BadPtrGet(Span::default())),
                    _ => Err(check::Error::BadDeref(Span::default()))
                }
            }
            Expression::AddrOf(left) => {
                let t = left.val.type_of(locals, &left.span, ast)?; 
                Ok((Type::RefTo(Box::new(t.0)), t.1))
            }
        
            Expression::Assign{ .. } => {
                Ok((Type::Null, true))
            }
            Expression::Call  { name, assoc, .. } => {
                if let Some(func) = ast.funcs.get(&(name.clone(), assoc.clone())) {
                    Ok((func.rettype.val.clone(), true))
                } else {
                    Err(check::Error::FuncNotFound(Span::default(), name.clone()))
                }
            }
            Expression::Ask   { expr, body: _ } => {
                expr.val.type_of(locals, &expr.span, ast)
            }
            Expression::Member{ obj, member, deref } => {
                let mut t = obj.val.type_of(locals, &obj.span, ast)?;
                if *deref {
                    t.0 = if let Type::RefTo(t) = t.0 {
                        *t
                    } else {
                        return Err(check::Error::BadDeref(obj.span.clone()))
                    };
                }
                if let Type::User(s) = t.0 {
                    if let Some(s) = ast.structs.get(&s) {
                        if let Expression::Identifier(m) = &member.val {
                            if let Some(f) = s.get(m) {
                                return Ok((f.datatype.val.clone(), t.1))
                            }
                            return Err(check::Error::FieldNotFound(member.span.clone(), m.clone(), s.name.val.clone()))
                        }
                        unreachable!()
                    }
                    unreachable!()
                    //return Err(check::Error::StructNotFound(Span::default(), s))
                }
                Err(check::Error::ExprNotStruct(obj.span.clone()))
            }
            Expression::ArrGet{ obj, index: _ } => {
                obj.val.type_of(locals, &obj.span, ast)
            }
        
            Expression::Default => unreachable!()
        }
    }
}
