#![allow(unused)]

use super::*;


#[derive(Debug, Clone, PartialEq, Default)]
pub enum Expression {
    #[default]
    NullPtr,
    Null(Type),
    Identifier(Type, String),

    // constants
    I8Lit(i8),
    I16Lit(i16),
    I32Lit(i32),
    I64Lit(i64),
    U8Lit(u8),
    U16Lit(u16),
    U32Lit(u32),
    U64Lit(u64),
    F32Lit(f32),
    F64Lit(f64),
    BoolLit(bool),
    CharLit(char),
    StrLit(String),
    ArrLit(Type, Vec<Item<Expression>>),
    StructLit(Type, String, Vec<(String, Item<Expression>)>),

    // math
    Neg(Type, Box<Item<Expression>>),
    Add(Type, Box<Item<Expression>>, Box<Item<Expression>>),
    Sub(Type, Box<Item<Expression>>, Box<Item<Expression>>),
    Mul(Type, Box<Item<Expression>>, Box<Item<Expression>>),
    Div(Type, Box<Item<Expression>>, Box<Item<Expression>>),
    Mod(Type, Box<Item<Expression>>, Box<Item<Expression>>),

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
    BitNot(Type, Box<Item<Expression>>),
    BitAnd(Type, Box<Item<Expression>>, Box<Item<Expression>>),
    BitOr (Type, Box<Item<Expression>>, Box<Item<Expression>>),
    BitXor(Type, Box<Item<Expression>>, Box<Item<Expression>>),
    LShift(Type, Box<Item<Expression>>, Box<Item<Expression>>),
    RShift(Type, Box<Item<Expression>>, Box<Item<Expression>>),

    // pointers
    AddrOf(Type, Box<Item<Expression>>),
    Deref (Type, Box<Item<Expression>>),
    DerefChecked(Type, Box<Item<Expression>>),

    // fanciness
    Assign{ ty: Type, left: Box<Item<Expression>>, right: Box<Item<Expression>> },
    Call  { ty: Type, name: String, args: Vec<Item<Expression>> },
    Member{ ty: Type, obj: Box<Item<Expression>>, member: Box<Item<Expression>> },
    ArrGet{ ty: Type, obj: Box<Item<Expression>>, index: Box<Item<Expression>> },
}

impl Expression {
    pub fn is_lit(&self) -> bool {
        matches!(self, Self::NullPtr|Self::Null(_)|Self::I8Lit(_)|Self::I16Lit(_)|Self::I32Lit(_)|Self::I64Lit(_)|Self::U8Lit(_)|Self::U16Lit(_)|Self::U32Lit(_)|Self::U64Lit(_)|Self::F32Lit(_)|Self::F64Lit(_)|Self::BoolLit(_)|Self::CharLit(_)|Self::StrLit(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Identifier(..))
    }

    pub fn is_leaf(&self) -> bool {
        self.is_lit() || self.is_ident()
    }

    pub fn is_const(&self) -> bool {
        match self {
            Self::NullPtr|Self::Null(_)|Self::I8Lit(_)|Self::I16Lit(_)|Self::I32Lit(_)|Self::I64Lit(_)|Self::U8Lit(_)|Self::U16Lit(_)|Self::U32Lit(_)|Self::U64Lit(_)|Self::F32Lit(_)|Self::F64Lit(_)|Self::BoolLit(_)|Self::CharLit(_)|Self::StrLit(_) => true,
            Self::Identifier(..) => false,
            Self::ArrLit(_, exprs) => {
                exprs.iter().all(|e| e.val.is_const())
            }
            Self::StructLit(.., exprs) => {
                exprs.iter().all(|e| e.1.val.is_const())
            }

            Self::Neg(_, v) => v.val.is_const(),
            Self::Add(_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::Sub(_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::Mul(_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::Div(_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::Mod(_, l, r) => l.val.is_const() && r.val.is_const(),
        
            Self::Not(v) => v.val.is_const(),
            Self::And(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Or (l, r) => l.val.is_const() && r.val.is_const(),
        
            Self::Lt(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Gt(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Le(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Ge(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Ne(l, r) => l.val.is_const() && r.val.is_const(),
            Self::Eq(l, r) => l.val.is_const() && r.val.is_const(),
        
            Self::BitNot(_, v) => v.val.is_const(),
            Self::BitAnd(_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::BitOr (_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::BitXor(_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::LShift(_, l, r) => l.val.is_const() && r.val.is_const(),
            Self::RShift(_, l, r) => l.val.is_const() && r.val.is_const(),

            Self::AddrOf(_, v) => v.val.is_const(),
            Self::Deref (_, v) => v.val.is_const(),
            Self::DerefChecked(_, v) => v.val.is_const(),

            Self::Assign{ .. } => false,
            Self::Call  { .. } => false,
            Self::Member{ .. } => false,
            Self::ArrGet{ obj, index, .. } => obj.val.is_const() && index.val.is_const(),
        }
    }

    pub fn is_lvalue(&self) -> bool {
        match self {
            Self::NullPtr|Self::Null(_)|Self::I8Lit(_)|Self::I16Lit(_)|Self::I32Lit(_)|Self::I64Lit(_)|Self::U8Lit(_)|Self::U16Lit(_)|Self::U32Lit(_)|Self::U64Lit(_)|Self::F32Lit(_)|Self::F64Lit(_)|Self::BoolLit(_)|Self::CharLit(_)|Self::StrLit(_) => false,
            Self::Identifier(..) => true,
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

            Self::AddrOf(..) => false,
            Self::Deref (..) => true,
            Self::DerefChecked(..) => true,

            Self::Assign{ .. } => false,
            Self::Call  { .. } => false,
            Self::Member{ obj, .. } => obj.val.is_lvalue(),
            Self::ArrGet{ obj, .. } => obj.val.is_lvalue(),
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self.type_of(), Type::RefTo(_)|Type::PtrTo(_))
    }

    pub fn type_of(&self) -> Type {
        match self {
            Self::NullPtr => Type::PtrTo(Box::new(Type::Null)),
            Self::Null(t) => t.clone(),
            Self::I8Lit(_) => Type::I8,
            Self::I16Lit(_) => Type::I16,
            Self::I32Lit(_) => Type::I32,
            Self::I64Lit(_) => Type::I64,
            Self::U8Lit(_) => Type::U8,
            Self::U16Lit(_) => Type::U16,
            Self::U32Lit(_) => Type::U32,
            Self::U64Lit(_) => Type::U64,
            Self::F32Lit(_) => Type::F32,
            Self::F64Lit(_) => Type::F64,
            Self::BoolLit(_) => Type::Bool,
            Self::CharLit(_) => Type::Char,
            Self::StrLit(_) => Type::Str,
            Self::Identifier(t, _) => t.clone(),
            Self::ArrLit(t, _) => t.clone(),
            Self::StructLit(t, ..) => t.clone(),

            Self::Neg(t, ..) => t.clone(),
            Self::Add(t, ..) => t.clone(),
            Self::Sub(t, ..) => t.clone(),
            Self::Mul(t, ..) => t.clone(),
            Self::Div(t, ..) => t.clone(),
            Self::Mod(t, ..) => t.clone(),
        
            Self::Not(..) => Type::Bool,
            Self::And(..) => Type::Bool,
            Self::Or (..) => Type::Bool,
        
            Self::Lt(..) => Type::Bool,
            Self::Gt(..) => Type::Bool,
            Self::Le(..) => Type::Bool,
            Self::Ge(..) => Type::Bool,
            Self::Ne(..) => Type::Bool,
            Self::Eq(..) => Type::Bool,
        
            Self::BitNot(t, ..) => t.clone(),
            Self::BitAnd(t, ..) => t.clone(),
            Self::BitOr (t, ..) => t.clone(),
            Self::BitXor(t, ..) => t.clone(),
            Self::LShift(t, ..) => t.clone(),
            Self::RShift(t, ..) => t.clone(),

            Self::AddrOf(t, ..) => t.clone(),
            Self::Deref (t, ..) => t.clone(),
            Self::DerefChecked(t, ..) => t.clone(),

            Self::Assign{ ty, .. } => ty.clone(),
            Self::Call  { ty, .. } => ty.clone(),
            Self::Member{ ty, .. } => ty.clone(),
            Self::ArrGet{ ty, .. } => ty.clone(),
        }
    }
}
