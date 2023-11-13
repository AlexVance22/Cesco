use std::fmt::Display;


#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    #[default]
    Null,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
    Str,
    User(String, crate::mir::SizeInfo),
    PtrTo(Box<Type>),
    RefTo(Box<Type>),
    OptOf(Box<Type>),
    ArrOf(Box<Type>, usize),
    VecOf(Box<Type>, usize),
}

#[allow(unused)]
impl Type {   
    pub fn array(inner: Type, len: usize) -> Self {
        Type::ArrOf(Box::new(inner), len)
    }

    pub fn vector(inner: Type, len: usize) -> Self {
        Type::VecOf(Box::new(inner), len)
    }
}

impl Type {
    #[allow(unused)]
    pub fn is_unit(&self) -> bool {
        !matches!(self, Self::ArrOf(..)|Self::VecOf(..))
    }

    #[allow(unused)]
    pub fn is_value(&self) -> bool {
        !matches!(self, Self::PtrTo(_)|Self::RefTo(_))
    }


    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn is_integral(&self) -> bool {
        !matches!(self, Self::F64|Self::F32|Self::Str|Self::Null|Self::User(..))
    }

    pub fn is_numeric(&self) -> bool {
        !matches!(self, Self::Str|Self::Null|Self::User(..))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::PtrTo(_)|Type::RefTo(_))
    }


    pub fn inner(&self) -> Option<&Type> {
        match self {
            Self::ArrOf(t, _) => Some(t.as_ref()),
            Self::VecOf(t, _) => Some(t.as_ref()),
            Self::PtrTo(t) => Some(t.as_ref()),
            Self::RefTo(t) => Some(t.as_ref()),
            Self::OptOf(t) => Some(t.as_ref()),
            _ => None,
        }
    }

    pub fn into_inner(self) -> Option<Type> {
        match self {
            Self::ArrOf(t, _) => Some(*t),
            Self::VecOf(t, _) => Some(*t),
            Self::PtrTo(t) => Some(*t),
            Self::RefTo(t) => Some(*t),
            Self::OptOf(t) => Some(*t),
            _ => None,
        }
    }


    pub fn size_of(&self) -> usize {
        match self {
            Type::I8 => 1,
            Type::I16 => 2,
            Type::I32 => 4,
            Type::I64 => 8,
            Type::U8 => 1,
            Type::U16 => 2,
            Type::U32 => 4,
            Type::U64 => 8,
            Type::F32 => 4,
            Type::F64 => 8,
            Type::Bool => 1,
            Type::Char => 1,
            Type::Str => 8,
            Type::Null => 0,
            Type::User(_, info) => info.size,
            Type::PtrTo(_) => 8,
            Type::RefTo(_) => 8,
            Type::OptOf(t) => t.size_of() * 2,
            Type::ArrOf(t, i) => t.size_of() * i,
            Type::VecOf(t, i) => t.size_of() * i,
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::I8 => 1,
            Type::I16 => 2,
            Type::I32 => 4,
            Type::I64 => 8,
            Type::U8 => 1,
            Type::U16 => 2,
            Type::U32 => 4,
            Type::U64 => 8,
            Type::F32 => 4,
            Type::F64 => 8,
            Type::Bool => 1,
            Type::Char => 1,
            Type::Str => 8,
            Type::Null => 1,
            Type::User(_, info) => info.align, 
            Type::PtrTo(_) => 8,
            Type::RefTo(_) => 8,
            Type::OptOf(t) => t.align_of(),
            Type::ArrOf(t, _) => t.align_of(),
            Type::VecOf(t, _) => t.align_of(),
        }
    }

    pub fn size_info(&self) -> crate::mir::SizeInfo {
        crate::mir::SizeInfo{ size: self.size_of(), align: self.align_of() }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::Str => write!(f, "str"),
            Self::User(s, ..) => write!(f, "{}", s),
            Self::PtrTo(t) => write!(f, "{}*", *t),
            Self::RefTo(t) => write!(f, "{}&", *t),
            Self::OptOf(t) => write!(f, "{}?", *t),
            Self::ArrOf(t, i) => write!(f, "{}[{}]", *t, i),
            Self::VecOf(t, i) => write!(f, "{}({})", *t, i),
        }
    }
}
