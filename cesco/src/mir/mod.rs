mod build;
//mod optimise;
mod nums;

use std::fmt::Display;
use std::collections::HashSet;
use crate::hir::ast::Type;
pub use build::destructure;
//pub use optimise::optimise;
pub use nums::*;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SizeInfo {
    pub size: usize,
    pub align: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Offset {
    Reg(usize),
    Imm(isize)
}

impl Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Offset::Reg(i) => write!(f, "_{}", i),
            Offset::Imm(i) => write!(f, "{}", i),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Immediate {
    NullPtr,
    Null(Type),
    Int(IntType),
    Flt(FltType),
    Bool(bool),
    Char(char),
    Str(String),
}

impl Immediate {
    fn type_of(&self) -> Type {
        match self {
            Self::NullPtr => Type::Null,
            Self::Null(t) => Type::OptOf(Box::new(t.clone())),
            Self::Int(i) => i.type_of(),
            Self::Flt(f) => f.type_of(),
            Self::Bool(_) => Type::Bool,
            Self::Char(_) => Type::Char,
            Self::Str(_) => Type::Str,
        }
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NullPtr => write!(f, "nullptr"),
            Self::Null(t) => write!(f, "null({})", t),
            Self::Int(i) => write!(f, "{}", i),
            Self::Flt(fl) => write!(f, "{}", fl),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Char(c) => write!(f, "'{}'", c),
            Self::Str(s) => write!(f, "{:?}", s),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Temporary(usize, Type);

impl Temporary {
    pub fn type_of(&self) -> Type {
        self.1
    }

    pub fn name_of(&self) -> usize {
        self.0
    }

    pub fn size_of(&self) -> usize {
        self.1.size_of()
    }

    pub fn align_of(&self) -> usize {
        self.1.align_of()
    }
}

impl Display for Temporary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_T{}", self.0)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable(String, Type);

impl Variable {
    pub fn type_of(&self) -> Type {
        self.1
    }

    pub fn name_of(&self) -> &String {
        &self.0
    }

    pub fn size_of(&self) -> usize {
        self.1.size_of()
    }

    pub fn align_of(&self) -> usize {
        self.1.align_of()
    }

    pub fn name_of_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}



#[derive(Debug, Clone, PartialEq)]
pub enum TempAddress {
    At(Temporary),
    AtOffsetImm(Temporary, Immediate),
    AtOffsetTemp(Temporary, Temporary),
    AtOffsetVar(Temporary, Variable),
    Deref(Temporary),
    DerefOffsetImm(Temporary, Immediate),
}

impl TempAddress {
    pub fn base(&self) -> &Temporary {
        match self {
            Self::At(var) => var,
            Self::AtOffsetImm(var, _) => var,
            Self::AtOffsetTemp(var, _) => var,
            Self::AtOffsetVar(var, _) => var,
            Self::Deref(var) => var,
            Self::DerefOffsetImm(var, _) => var,
        }
    }

    pub fn type_of(&self) -> Type {
        match self {
            Self::At(var) => var.type_of(),
            Self::AtOffsetImm(var, _) => var.type_of().inner().unwrap().clone(),
            Self::AtOffsetTemp(var, _) => var.type_of().inner().unwrap().clone(),
            Self::AtOffsetVar(var, _) => var.type_of().inner().unwrap().clone(),
            Self::Deref(var) => var.type_of(),
            Self::DerefOffsetImm(var, _) => var.type_of().inner().unwrap().clone(),
        }
    }

    pub fn size_of(&self) -> usize {
        self.type_of().size_of()
    }

    pub fn align_of(&self) -> usize {
        self.type_of().align_of()
    }
}

impl Display for TempAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::At(var) => write!(f, "{}", var),
            Self::AtOffsetImm(var, off) => write!(f, "{}[{}]", var, off),
            Self::AtOffsetTemp(var, off) => write!(f, "{}[{}]", var, off),
            Self::AtOffsetVar(var, off) => write!(f, "{}[{}]", var, off),
            Self::Deref(var) => write!(f, "*{}", var),
            Self::DerefOffsetImm(var, off) => write!(f, "*{}[{}]", var, off),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum VarAddress {
    At(Variable),
    AtOffsetImm(Variable, Immediate),
    AtOffsetTemp(Variable, Temporary),
    AtOffsetVar(Variable, Variable),
    Deref(Variable),
}

impl VarAddress {
    pub fn base(&self) -> &Variable {
        match self {
            Self::At(var) => var,
            Self::AtOffsetImm(var, _) => var,
            Self::AtOffsetTemp(var, _) => var,
            Self::AtOffsetVar(var, _) => var,
            Self::Deref(var) => var,
        }
    }

    pub fn type_of(&self) -> Type {
        match self {
            Self::At(var) => var.type_of(),
            Self::AtOffsetImm(var, _) => var.type_of().inner().unwrap().clone(),
            Self::AtOffsetTemp(var, _) => var.type_of().inner().unwrap().clone(),
            Self::AtOffsetVar(var, _) => var.type_of().inner().unwrap().clone(),
            Self::Deref(var) => var.type_of(),
        }
    }

    pub fn size_of(&self) -> usize {
        self.type_of().size_of()
    }

    pub fn align_of(&self) -> usize {
        self.type_of().align_of()
    }
}

impl Display for VarAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::At(var) => write!(f, "{}", var),
            Self::AtOffsetImm(var, off) => write!(f, "{}[{}]", var, off),
            Self::AtOffsetTemp(var, off) => write!(f, "{}[{}]", var, off),
            Self::AtOffsetVar(var, off) => write!(f, "{}[{}]", var, off),
            Self::Deref(var) => write!(f, "*{}", var),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Dest {
    Temp(TempAddress),
    Addr(VarAddress),
}

impl Dest {
    pub fn type_of(&self) -> Type {
        match self {
            Self::Temp(temp) => temp.type_of(),
            Self::Addr(addr) => addr.type_of(),
        }
    }
}

impl Display for Dest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Temp(temp) => write!(f, "{}", temp),
            Self::Addr(addr) => write!(f, "{}", addr),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Imm(Immediate),
    Temp(TempAddress),
    Var(VarAddress),
}

impl Operand {
    pub fn type_of(&self) -> Type {
        match self {
            Self::Imm(imm) => imm.type_of(),
            Self::Temp(temp) => temp.type_of(),
            Self::Var(var) => var.type_of(),
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(imm) => write!(f, "{}", imm),
            Self::Temp(temp) => write!(f, "{}", temp),
            Self::Var(var) => write!(f, "{}", var),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    FuncBegin(String, Vec<Variable>),
    Jmp0(usize, Operand),
    Jump(usize),
    Label(usize),
    Return,

    // EXPRESSIONS =======================================================

    // math
    Neg(Dest, Operand),
    Add(Dest, Operand, Operand),
    Sub(Dest, Operand, Operand),
    Mul(Dest, Operand, Operand),
    Div(Dest, Operand, Operand),
    Mod(Dest, Operand, Operand),

    // logic
    Not(Dest, Operand),
    And(Dest, Operand, Operand),
    Or (Dest, Operand, Operand),

    // comparison
    Lt(Dest, Operand, Operand),
    Gt(Dest, Operand, Operand),
    Le(Dest, Operand, Operand),
    Ge(Dest, Operand, Operand),
    Eq(Dest, Operand, Operand),
    Ne(Dest, Operand, Operand),

    // bitwise
    BitNot(Dest, Operand),
    BitAnd(Dest, Operand, Operand),
    BitOr (Dest, Operand, Operand),
    BitXor(Dest, Operand, Operand),
    LShift(Dest, Operand, Operand),
    RShift(Dest, Operand, Operand),

    // pointers
    AddrOf(Dest, Operand),

    // fanciness
    Assign    (Dest, Operand),
    Call      (Dest, String, Vec<Operand>),
    LoadArr   (Dest, Vec<Operand>),
    LoadStruct(Dest, String, Vec<(String, Operand)>),
    // ASK:  var x = fallback; if (nullable) { x = *nullable }
    // ., ->, [] all implemented as combinations of pointer arithmetic and deferencing
}

impl Operation {
    pub fn dest(&self) -> Option<&Dest> {
        match self {
            Self::Neg(var, ..) |
            Self::Add(var, ..) |
            Self::Sub(var, ..) |
            Self::Mul(var, ..) |
            Self::Div(var, ..) |
            Self::Mod(var, ..) |
            Self::Not(var, ..) |
            Self::And(var, ..) |
            Self::Or (var, ..) |
            Self::Lt(var, ..) |
            Self::Gt(var, ..) |
            Self::Le(var, ..) |
            Self::Ge(var, ..) |
            Self::Eq(var, ..) |
            Self::Ne(var, ..) |
            Self::BitNot(var, ..) |
            Self::BitAnd(var, ..) |
            Self::BitOr (var, ..) |
            Self::BitXor(var, ..) |
            Self::LShift(var, ..) |
            Self::RShift(var, ..) |
            Self::AddrOf(var, ..) |
            Self::Assign    (var, ..) |
            Self::Call      (var, ..) |
            Self::LoadArr   (var, ..) |
            Self::LoadStruct(var, ..)  => {
                Some(var)
            }
            _ => None
        }
    }

    pub fn dest_mut(&mut self) -> Option<&mut Dest> {
        match self {
            Self::Neg(var, ..) |
            Self::Add(var, ..) |
            Self::Sub(var, ..) |
            Self::Mul(var, ..) |
            Self::Div(var, ..) |
            Self::Mod(var, ..) |
            Self::Not(var, ..) |
            Self::And(var, ..) |
            Self::Or (var, ..) |
            Self::Lt(var, ..) |
            Self::Gt(var, ..) |
            Self::Le(var, ..) |
            Self::Ge(var, ..) |
            Self::Eq(var, ..) |
            Self::Ne(var, ..) |
            Self::BitNot(var, ..) |
            Self::BitAnd(var, ..) |
            Self::BitOr (var, ..) |
            Self::BitXor(var, ..) |
            Self::LShift(var, ..) |
            Self::RShift(var, ..) |
            Self::AddrOf(var, ..) |
            Self::Assign    (var, ..) |
            Self::Call      (var, ..) |
            Self::LoadArr   (var, ..) |
            Self::LoadStruct(var, ..)  => {
                Some(var)
            }
            _ => None
        }
    }

    pub fn ops(&self) -> impl Iterator<Item = &Operand> {
        match self {
            Self::Neg   (_, val) |
            Self::Not   (_, val) |
            Self::BitNot(_, val) |
            Self::AddrOf(_, val) |
            Self::Assign(_, val) => {
                vec![val].into_iter()
            }

            Self::Add(_, left, right) |
            Self::Sub(_, left, right) |
            Self::Mul(_, left, right) |
            Self::Div(_, left, right) |
            Self::Mod(_, left, right) |
            Self::And(_, left, right) |
            Self::Or (_, left, right) |
            Self::Lt(_, left, right) |
            Self::Gt(_, left, right) |
            Self::Le(_, left, right) |
            Self::Ge(_, left, right) |
            Self::Eq(_, left, right) |
            Self::Ne(_, left, right) |
            Self::BitAnd(_, left, right) |
            Self::BitOr (_, left, right) |
            Self::BitXor(_, left, right) |
            Self::LShift(_, left, right) |
            Self::RShift(_, left, right) => {
                vec![left, right].into_iter()
            }

            Self::Call      (_, _, ops) => ops.iter().collect::<Vec<&Operand>>().into_iter(),
            Self::LoadArr   (_, ops) => ops.iter().collect::<Vec<&Operand>>().into_iter(),
            Self::LoadStruct(_, _, ops) => ops.iter().map(|(_, v)| v).collect::<Vec<&Operand>>().into_iter(),
        
            _ => vec![].into_iter()
        }
    }

    pub fn ops_mut(&mut self) -> impl Iterator<Item = &mut Operand> {
        match self {
            Self::Neg   (_, val) |
            Self::Not   (_, val) |
            Self::BitNot(_, val) |
            Self::AddrOf(_, val) |
            Self::Assign(_, val) => {
                vec![val].into_iter()
            }

            Self::Add(_, left, right) |
            Self::Sub(_, left, right) |
            Self::Mul(_, left, right) |
            Self::Div(_, left, right) |
            Self::Mod(_, left, right) |
            Self::And(_, left, right) |
            Self::Or (_, left, right) |
            Self::Lt(_, left, right) |
            Self::Gt(_, left, right) |
            Self::Le(_, left, right) |
            Self::Ge(_, left, right) |
            Self::Eq(_, left, right) |
            Self::Ne(_, left, right) |
            Self::BitAnd(_, left, right) |
            Self::BitOr (_, left, right) |
            Self::BitXor(_, left, right) |
            Self::LShift(_, left, right) |
            Self::RShift(_, left, right) => {
                vec![left, right].into_iter()
            }

            Self::Call      (_, _, ops) => ops.iter_mut().collect::<Vec<&mut Operand>>().into_iter(),
            Self::LoadArr   (_, ops) => ops.iter_mut().collect::<Vec<&mut Operand>>().into_iter(),
            Self::LoadStruct(_, _, ops) => ops.iter_mut().map(|(_, v)| v).collect::<Vec<&mut Operand>>().into_iter(),
        
            _ => vec![].into_iter()
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FuncBegin(name, params) => {
                write!(f, "{}( ", name)?;
                for p in params {
                    write!(f, "{}, ", p)?;
                }
                write!(f, "):")
            }
            Self::Jmp0(label, val) => write!(f, "    jz {}, LB{};", val, label),
            Self::Jump(label) => write!(f, "    jmp LB{};", label),
            Self::Label(label) => write!(f, "LB{}:", label),
            Self::Return => write!(f, "    return;"),

            Self::Neg(target, val) => write!(f, "    {} \t= -{};", target, val),
            Self::Not(target, val) => write!(f, "    {} \t= !{};", target, val),
            Self::BitNot(target, val) => write!(f, "    {} \t= ~{};", target, val),
            Self::AddrOf(target, val) => write!(f, "    {} \t= &{};", target, val),
            Self::Assign(target, val) => write!(f, "    {} \t= {};", target, val),
            Self::Call      (target, name, vals) => {
                write!(f, "    {} \t= {}( ", target, name)?;
                for v in vals {
                    write!(f, "{}, ", v)?;
                }
                write!(f, ");")
            }
            Self::LoadArr   (target, vals) => {
                write!(f, "    {} \t= [ ", target)?;
                for v in vals {
                    write!(f, "{}, ", v)?;
                }
                write!(f, "];")
            }
            Self::LoadStruct(target, name, os) => {
                write!(f, "    {} \t= {}{{ ", target, name)?;
                for (k, v) in os {
                    write!(f, "{}: {}, ", k, v)?;
                }
                write!(f, "}};")
            }
            
            Self::Add(target, left, right) => write!(f, "    {} \t= {} + {};", target, left, right),
            Self::Sub(target, left, right) => write!(f, "    {} \t= {} - {};", target, left, right),
            Self::Mul(target, left, right) => write!(f, "    {} \t= {} * {};", target, left, right),
            Self::Div(target, left, right) => write!(f, "    {} \t= {} / {};", target, left, right),
            Self::Mod(target, left, right) => write!(f, "    {} \t= {} % {};", target, left, right),
            Self::And(target, left, right) => write!(f, "    {} \t= {} && {};", target, left, right),
            Self::Or (target, left, right) => write!(f, "    {} \t= {} || {};", target, left, right),
            Self::Lt(target, left, right) => write!(f, "    {} \t= {} < {};", target, left, right),
            Self::Gt(target, left, right) => write!(f, "    {} \t= {} > {};", target, left, right),
            Self::Le(target, left, right) => write!(f, "    {} \t= {} <= {};", target, left, right),
            Self::Ge(target, left, right) => write!(f, "    {} \t= {} >= {};", target, left, right),
            Self::Eq(target, left, right) => write!(f, "    {} \t= {} == {};", target, left, right),
            Self::Ne(target, left, right) => write!(f, "    {} \t= {} != {};", target, left, right),
            Self::BitAnd(target, left, right) => write!(f, "    {} \t= {} & {};", target, left, right),
            Self::BitOr (target, left, right) => write!(f, "    {} \t= {} | {};", target, left, right),
            Self::BitXor(target, left, right) => write!(f, "    {} \t= {} ^ {};", target, left, right),
            Self::LShift(target, left, right) => write!(f, "    {} \t= {} << {};", target, left, right),
            Self::RShift(target, left, right) => write!(f, "    {} \t= {} >> {};", target, left, right),
        }
    }
}


pub struct GenericUn(pub Dest, pub Operand);
pub struct GenericBin(pub Dest, pub Operand, pub Operand);


#[derive(Debug, Clone, PartialEq)]
pub struct Procedure {
    pub ops: Vec<Operation>,
    pub vars: HashSet<Variable>,
    pub stack: usize
}

impl Display for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for op in &self.ops {
            writeln!(f, "{}", op)?
        }

        Ok(())
    }
}


pub type Program = Vec<Procedure>;
