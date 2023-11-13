use std::collections::HashMap;
use std::fmt::Display;
use super::Struct;


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
    User(String),
    PtrTo(Box<Type>),
    RefTo(Box<Type>),
    OptOf(Box<Type>),
    ArrOf(Box<Type>, usize),
    VecOf(Box<Type>, usize),
}

impl Type {
    pub fn is_unit(&self) -> bool {
        !matches!(self, Self::ArrOf(..)|Self::VecOf(..))
    }

    pub fn is_value(&self) -> bool {
        !matches!(self, Self::PtrTo(_)|Self::RefTo(_))
    }


    pub fn is_boolean(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn is_integral(&self) -> bool {
        !matches!(self, Self::F64|Self::F32|Self::Str|Self::Null|Self::User(_))
    }

    pub fn is_numeric(&self) -> bool {
        !matches!(self, Self::Str|Self::Null|Self::User(_))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Self::PtrTo(_)|Self::RefTo(_))
    }


    pub fn size_of(&self, types: &HashMap<String, Struct>) -> usize {
        match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::F32 => 4,
            Self::F64 => 8,
            Self::Bool => 1,
            Self::Char => 1,
            Self::Str => 8,
            Self::Null => 0,
            Self::User(t) => {
                if let Some(s) = types.get(t) {
                    s.size_of(types)
                } else {
                    0
                }
            }
            Self::PtrTo(_) => 8,
            Self::RefTo(_) => 8,
            Self::OptOf(t) => t.size_of(types) * 2,
            Self::ArrOf(t, i) => t.size_of(types) * i,
            Self::VecOf(t, i) => t.size_of(types) * i,
        }
    }

    pub fn align_of(&self, types: &HashMap<String, Struct>) -> usize {
        match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::F32 => 4,
            Self::F64 => 8,
            Self::Bool => 1,
            Self::Char => 1,
            Self::Str => 8,
            Self::Null => 1,
            Self::User(t) => {
                if let Some(s) = types.get(t) {
                    s.align_of(types)
                } else {
                    0
                }
            }
            Self::PtrTo(_) => 8,
            Self::RefTo(_) => 8,
            Self::OptOf(t) => t.align_of(types),
            Self::ArrOf(t, _) => t.align_of(types),
            Self::VecOf(t, _) => t.align_of(types),
        }
    }

    pub fn size_info(&self, types: &HashMap<String, Struct>) -> crate::mir::SizeInfo {
        crate::mir::SizeInfo{ size: self.size_of(types), align: self.align_of(types) }
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
            Self::User(s) => write!(f, "{}", s),
            Self::PtrTo(t) => write!(f, "{}*", *t),
            Self::RefTo(t) => write!(f, "{}&", *t),
            Self::OptOf(t) => write!(f, "{}?", *t),
            Self::ArrOf(t, i) => write!(f, "{}[{}]", *t, i),
            Self::VecOf(t, i) => write!(f, "{}({})", *t, i),
        }
    }
}
