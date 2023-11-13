use num_derive::FromPrimitive;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, FromPrimitive)]
#[repr(u32)]
pub enum Loc {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
}

impl Loc {
    pub fn is_xmm(&self) -> bool {
        match self {
            Self::Xmm0 => true,
            Self::Xmm1 => true,
            Self::Xmm2 => true,
            Self::Xmm3 => true,
            Self::Xmm4 => true,
            Self::Xmm5 => true,
            Self::Xmm6 => true,
            Self::Xmm7 => true,
            _ => false,
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Target {
    Loc(Loc),
    ImmI(i64),
    ImmF(String),
    ImmId(String),
}

impl Target {
    pub fn is_imm(&self) -> bool {
        match self {
            Self::Loc(_) => false,
            _ => true,
        }
    }

    pub fn is_i(&self) -> bool {
        match self {
            Self::ImmI(_) => true,
            _ => false,
        }
    }

    pub fn is_f(&self) -> bool {
        match self {
            Self::ImmF(_) => true,
            _ => false,
        }
    }

    pub fn is_id(&self) -> bool {
        match self {
            Self::ImmId(_) => true,
            _ => false,
        }
    }

    pub fn is_xmm(&self) -> bool {
        match self {
            Self::Loc(loc) => match loc {
                Loc::Xmm0 => true,
                Loc::Xmm1 => true,
                Loc::Xmm2 => true,
                Loc::Xmm3 => true,
                Loc::Xmm4 => true,
                Loc::Xmm5 => true,
                Loc::Xmm6 => true,
                Loc::Xmm7 => true,
                _ => false,
            }
            Self::ImmI(_) => false,
            Self::ImmF(_) => true,
            Self::ImmId(_) => false,
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operation {
    Neg(Target, Target),
    Add(Target, Target, Target),
    Sub(Target, Target, Target),
    Mul(Target, Target, Target),
    Div(Target, Target, Target),
    Mod(Target, Target, Target),

    Not(Target, Target),
    Or(Target, Target, Target),
    And(Target, Target, Target),

    Label(String),
    Goto(Option<Vec<Operation>>, String),
}