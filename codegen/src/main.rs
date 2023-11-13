use std::fmt::Display;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SizeInfo {
    size: usize,
    align: usize,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Temporary(usize, SizeInfo);

impl Display for Temporary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}T", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Variable(String, SizeInfo);

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Immediate {
    Int(i32),
}

impl Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self::Int(i) = self;

        write!(f, "{}", i)
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
enum AddressMode {
    At(Variable),
    AtOffsetImm(Variable, Immediate),
    AtOffsetTemp(Variable, Temporary),
    AtOffsetVar(Variable, Variable),
    Deref(Variable),
    DerefOffsetImm(Variable, Immediate),
}

impl Display for AddressMode {
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


#[derive(Debug, Clone, PartialEq, Eq)]
enum Operand {
    Imm(Immediate),
    Temp(Temporary),
    Addr(AddressMode),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(imm) => write!(f, "{}", imm),
            Self::Temp(temp) => write!(f, "{}", temp),
            Self::Addr(addr) => write!(f, "{}", addr),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
enum Dest {
    Temp(Temporary),
    Addr(AddressMode),
}

impl Display for Dest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Temp(temp) => write!(f, "{}", temp),
            Self::Addr(addr) => write!(f, "{}", addr),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
enum Operation {
    Jump(String),
    Jmp0(String, Operand),

    Assign(Dest, Operand),

    Add(Dest, Operand, Operand),
    Sub(Dest, Operand, Operand),
    Mul(Dest, Operand, Operand),
    Div(Dest, Operand, Operand),
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Jump(label) => write!(f, "jump -> {};", label),
            Self::Jmp0(label, op) => write!(f, "jz {} ? -> {};", op, label),

            Self::Assign(dest, op) => write!(f, "{:<8} = {};", dest.to_string(), op),

            Self::Add(dest, left, right) => write!(f, "{:<8} = {} + {};", dest.to_string(), left, right),
            Self::Sub(dest, left, right) => write!(f, "{:<8} = {} - {};", dest.to_string(), left, right),
            Self::Mul(dest, left, right) => write!(f, "{:<8} = {} * {};", dest.to_string(), left, right),
            Self::Div(dest, left, right) => write!(f, "{:<8} = {} / {};", dest.to_string(), left, right),
        }
    }
}



macro_rules! dest {
    ((*$id:ident)) => {
        Dest::Addr(AddressMode::Deref(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 })))
    };
    ($id:ident) => {
        Dest::Addr(AddressMode::At(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 })))
    };
    (($id:ident[$idx:ident])) => {
        Dest::Addr(AddressMode::AtOffsetVar(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 }), Variable(stringify!($idx).to_string(), SizeInfo{ size: 0, align: 0 })))
    };
    (($id:ident[($idx:literal)])) => {
        Dest::Addr(AddressMode::AtOffsetTemp(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 }), Temporary($idx, SizeInfo{ size: 0, align: 0 })))
    };
    (($id:ident[$idx:literal])) => {
        Dest::Addr(AddressMode::AtOffsetImm(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 }), Immediate::Int($idx)))
    };
    ($temp:literal) => {
        Dest::Temp(Temporary($temp, SizeInfo{ size: 0, align: 0 }))
    };
}

macro_rules! opnd {
    ((*$id:ident)) => {
        Operand::Addr(AddressMode::Deref(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 })))
    };
    ($id:ident) => {
        Operand::Addr(AddressMode::At(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 })))
    };
    (($id:ident[$idx:ident])) => {
        Operand::Addr(AddressMode::AtOffsetVar(Variable(stringify!($id).to_string(), SizeInfo{ size: 0, align: 0 }), Variable(stringify!($idx).to_string(), SizeInfo{ size: 0, align: 0 })))
    };
    (($temp:literal)) => {
        Operand::Temp(Temporary($temp, SizeInfo{ size: 0, align: 0 }))
    };
    ($imm:literal) => {
        Operand::Imm(Immediate::Int($imm))
    };
}

macro_rules! op {
    ($dest:tt = $val:tt) => {
        Operation::Assign(dest!($dest), opnd!($val))
    };
    ($dest:tt = $left:tt + $right:tt) => {
        Operation::Add(dest!($dest), opnd!($left), opnd!($right))
    };
    ($dest:tt = $left:tt - $right:tt) => {
        Operation::Sub(dest!($dest), opnd!($left), opnd!($right))
    };
    ($dest:tt = $left:tt * $right:tt) => {
        Operation::Mul(dest!($dest), opnd!($left), opnd!($right))
    };
    ($dest:tt = $left:tt / $right:tt) => {
        Operation::Div(dest!($dest), opnd!($left), opnd!($right))
    };
}


fn main() {
    let program = vec![
        op!(y       = 20),
        op!(z       = 10),
        op!(x       = y - z),
        op!((a[10]) = b + c),
    ];

    for op in program {
        println!("{}", op);
    }
}
