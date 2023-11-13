use std::{
    fmt::Display,
    ops::{
        Neg,
        Add,
        Sub,
        Mul,
        Div,
        Rem,
        Not,
        BitOr,
        BitXor,
        BitAnd,
        Shl,
        Shr,
    }
};
use crate::hir::ast::Type;


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum IntType {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl IntType {
    pub fn signed(val: i64, size: usize) -> Self {
        match size {
            1 => Self::I8(val as i8),
            2 => Self::I16(val as i16),
            4 => Self::I32(val as i32),
            8 => Self::I64(val),
            _ => panic!(),
        }
    }
    
    pub fn unsigned(val: u64, size: usize) -> Self {
        match size {
            1 => Self::U8(val as u8),
            2 => Self::U16(val as u16),
            4 => Self::U32(val as u32),
            8 => Self::U64(val),
            _ => panic!(),
        }
    }

    pub fn type_of(&self) -> Type {
        match self {
            Self::I8(_) => Type::I8,
            Self::I16(_) => Type::I16,
            Self::I32(_) => Type::I32,
            Self::I64(_) => Type::I64,
            Self::U8(_) => Type::U8,
            Self::U16(_) => Type::U16,
            Self::U32(_) => Type::U32,
            Self::U64(_) => Type::U64,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            Self::I8(_) => 1,
            Self::I16(_) => 2,
            Self::I32(_) => 4,
            Self::I64(_) => 8,
            Self::U8(_) => 1,
            Self::U16(_) => 2,
            Self::U32(_) => 4,
            Self::U64(_) => 8,
        }
    }

    pub fn is(&self, other: isize) -> bool {
        self.as_isize() == other
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Self::I8(_)|Self::I16(_)|Self::I32(_)|Self::I64(_) => true,
            Self::U8(_)|Self::U16(_)|Self::U32(_)|Self::U64(_) => false,
        }
    }

    pub fn as_isize(self) -> isize {
        match self {
            Self::I8(i) => i as isize,
            Self::I16(i) => i as isize,
            Self::I32(i) => i as isize,
            Self::I64(i) => i as isize,
            Self::U8(u) => u as isize,
            Self::U16(u) => u as isize,
            Self::U32(u) => u as isize,
            Self::U64(u) => u as isize,
        }
    }

    pub fn as_usize(self) -> usize {
        match self {
            Self::I8(i) => i as usize,
            Self::I16(i) => i as usize,
            Self::I32(i) => i as usize,
            Self::I64(i) => i as usize,
            Self::U8(u) => u as usize,
            Self::U16(u) => u as usize,
            Self::U32(u) => u as usize,
            Self::U64(u) => u as usize,
        }
    }

    pub fn as_i64(self) -> i64 {
        match self {
            Self::I8(i) => i as i64,
            Self::I16(i) => i as i64,
            Self::I32(i) => i as i64,
            Self::I64(i) => i,
            Self::U8(u) => u as i64,
            Self::U16(u) => u as i64,
            Self::U32(u) => u as i64,
            Self::U64(u) => u as i64,
        }
    }

    pub fn as_u64(self) -> u64 {
        match self {
            Self::I8(i) => i as u64,
            Self::I16(i) => i as u64,
            Self::I32(i) => i as u64,
            Self::I64(i) => i as u64,
            Self::U8(u) => u as u64,
            Self::U16(u) => u as u64,
            Self::U32(u) => u as u64,
            Self::U64(u) => u,
        }
    }
}

impl Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8(_)|Self::I16(_)|Self::I32(_)|Self::I64(_) => write!(f, "{}", self.as_isize()),
            Self::U8(_)|Self::U16(_)|Self::U32(_)|Self::U64(_) => write!(f, "{}", self.as_usize()),
        }
    }
}


impl Neg for IntType {
    type Output = IntType;

    fn neg(self) -> Self::Output {
        if self.is_signed() {
            Self::signed(-self.as_i64(), self.size_of())
        } else {
            panic!()
        }
    }
}

impl Add for IntType {
    type Output = IntType;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() + rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() + rhs.as_u64(), self.size_of())
        }
    }
}

impl Sub for IntType {
    type Output = IntType;
    
    fn sub(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() - rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() - rhs.as_u64(), self.size_of())
        }
    }
}

impl Mul for IntType {
    type Output = IntType;
    
    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() * rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() * rhs.as_u64(), self.size_of())
        }
    }
}

impl Div for IntType {
    type Output = IntType;
    
    fn div(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() / rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() / rhs.as_u64(), self.size_of())
        }
    }
}

impl Rem for IntType {
    type Output = IntType;

    fn rem(self, rhs: IntType) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() % rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() % rhs.as_u64(), self.size_of())
        }
    }
}


impl Not for IntType {
    type Output = IntType;

    fn not(self) -> Self::Output {
        if self.is_signed() {
            Self::signed(!self.as_i64(), self.size_of())
        } else {
            Self::unsigned(!self.as_u64(), self.size_of())
        }
    }
}

impl BitOr for IntType {
    type Output = IntType;

    fn bitor(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() | rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() | rhs.as_u64(), self.size_of())
        }
    }
}

impl BitXor for IntType {
    type Output = IntType;
    
    fn bitxor(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() ^ rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() ^ rhs.as_u64(), self.size_of())
        }
    }
}

impl BitAnd for IntType {
    type Output = IntType;
    
    fn bitand(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() & rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() & rhs.as_u64(), self.size_of())
        }
    }
}

impl Shl for IntType {
    type Output = IntType;
    
    fn shl(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() << rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() << rhs.as_u64(), self.size_of())
        }
    }
}

impl Shr for IntType {
    type Output = IntType;
    
    fn shr(self, rhs: Self) -> Self::Output {
        if self.is_signed() {
            Self::signed(self.as_i64() >> rhs.as_i64(), self.size_of())
        } else {
            Self::unsigned(self.as_u64() >> rhs.as_u64(), self.size_of())
        }
    }
}



#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum FltType {
    F32(f32),
    F64(f64),
}

impl FltType {
    pub fn new(val: f64, size: usize) -> Self {
        match size {
            4 => Self::F32(val as f32),
            8 => Self::F64(val),
            _ => panic!(),
        }
    }

    pub fn type_of(&self) -> Type {
        match self {
            Self::F32(_) => Type::F32,
            Self::F64(_) => Type::F64,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            Self::F32(_) => 4,
            Self::F64(_) => 8,
        }
    }

    pub fn as_f64(self) -> f64 {
        match self {
            Self::F32(f) => f as f64,
            Self::F64(f) => f,
        }
    }
}

impl Display for FltType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F32(_)|Self::F64(_) => write!(f, "{}", self.as_f64()),
        }
    }
}

impl Neg for FltType {
    type Output = FltType;

    fn neg(self) -> Self::Output {
        Self::new(-self.as_f64(), self.size_of())
    }
}

impl Add for FltType {
    type Output = FltType;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(self.as_f64() + rhs.as_f64(), self.size_of())
    }
}

impl Sub for FltType {
    type Output = FltType;
    
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(self.as_f64() - rhs.as_f64(), self.size_of())
    }
}

impl Mul for FltType {
    type Output = FltType;
    
    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(self.as_f64() * rhs.as_f64(), self.size_of())
    }
}

impl Div for FltType {
    type Output = FltType;
    
    fn div(self, rhs: Self) -> Self::Output {
        Self::new(self.as_f64() / rhs.as_f64(), self.size_of())
    }
}

impl Rem for FltType {
    type Output = FltType;

    fn rem(self, rhs: Self) -> Self::Output {
        Self::new(self.as_f64() % rhs.as_f64(), self.size_of())
    }
}
