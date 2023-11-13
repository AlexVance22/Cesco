mod ldst;
mod ops;

use std::collections::{ HashSet, HashMap };
use std::fmt::{ Write, Display };
use crate::mir::*;


fn mem_keyword(size: usize) -> &'static str {
    match size {
        1 => "BYTE",
        2 => "WORD",
        4 => "DWORD",
        8 => "QWORD",
        _ => unreachable!("size: {}", size)
    }
}

fn fmt_offset(off: isize) -> String {
    match off.cmp(&0) {
        std::cmp::Ordering::Greater => format!("+{}", off),
        std::cmp::Ordering::Equal =>   String::new(),
        std::cmp::Ordering::Less =>    format!("{}", off),
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Register {
    Rax = 0,
    Rbx,
    Rcx,
    Rdx,
    Rdi,
    Rsi,
    R8 ,
    R9 ,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Rax => write!(f, "rax"),
            Register::Rbx => write!(f, "rbx"),
            Register::Rcx => write!(f, "rcx"),
            Register::Rdx => write!(f, "rdx"),
            Register::Rdi => write!(f, "rdi"),
            Register::Rsi => write!(f, "rsi"),
            Register::R8  => write!(f, "r8"),
            Register::R9  => write!(f, "r9"),
            Register::R10 => write!(f, "r10"),
            Register::R11 => write!(f, "r11"),
            Register::R12 => write!(f, "r12"),
            Register::R13 => write!(f, "r13"),
            Register::R14 => write!(f, "r14"),
            Register::R15 => write!(f, "r15"),
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RegAddress {      // loc, size, offset?
    At(Register),
    Deref(Register, usize),
    DerefOffsetImm(Register, usize, isize),
}

impl RegAddress {
    pub fn from_temp(temp: &TempAddress, reg: Register, registry: &Registry) -> Self {
        let size = temp.size_of();

        match temp {
            TempAddress::At(_) => RegAddress::At(reg),
            TempAddress::AtOffsetImm(_, off) => {
                unreachable!()
            }
            TempAddress::AtOffsetTemp(_, off) => {
                unreachable!()
            }
            TempAddress::AtOffsetVar(_, off) => {
                unreachable!()
            }
            TempAddress::Deref(_) => RegAddress::At(reg),
            TempAddress::DerefOffsetImm(_, off) => {
                if let Immediate::Int(i) = off {
                    RegAddress::DerefOffsetImm(reg, temp.size_of(), i.as_isize())
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn base(&self) -> Register {
        match self {
            Self::At(reg) => *reg,
            Self::Deref(reg, ..) => *reg,
            Self::DerefOffsetImm(reg, ..) => *reg,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            Self::At(_) => 8,
            Self::Deref(_, size) => *size,
            Self::DerefOffsetImm(_, size, _) => *size,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MemVar {
    loc: isize,
    size: usize,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemAddress {       // loc, size, offset?
    At(MemVar),
    AtOffsetImm(MemVar, isize),
    AtOffsetTemp(MemVar, Register),
    AtOffsetVar(MemVar, MemVar),
    Deref(MemVar),
}

impl MemAddress {
    pub fn from_var(var: &VarAddress, loc: isize, registry: &Registry) -> Self {
        let size = var.size_of();

        match var {
            VarAddress::At(_) => MemAddress::At(MemVar{ loc, size }),
            VarAddress::AtOffsetImm(_, off) => {
                if let Immediate::Int(i) = off {
                    MemAddress::AtOffsetImm(MemVar{ loc, size }, i.as_isize())
                } else {
                    unreachable!()
                }
            }
            VarAddress::AtOffsetTemp(_, off) => {
                if let Some(RegAddress::At(reg)) = registry.regs.get(&off.name_of()) {
                    MemAddress::AtOffsetTemp(MemVar{ loc, size }, *reg)
                } else {
                    unreachable!()
                }
            }
            VarAddress::AtOffsetVar(_, off) => {
                if let Some(MemAddress::At(mem)) = registry.vars.get(off.name_of()) {
                    MemAddress::AtOffsetVar(MemVar{ loc, size }, *mem)
                } else {
                    unreachable!()
                }
            }
            VarAddress::Deref(_) => MemAddress::At(MemVar{ loc, size }),
        }
    }

    pub fn base(&self) -> isize {
        match self {
            Self::At(var, ..)|
            Self::AtOffsetImm(var, ..)|
            Self::AtOffsetTemp(var, ..)|
            Self::AtOffsetVar(var, ..)|
            Self::Deref(var, ..) => var.loc,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            Self::At(var, ..)|
            Self::AtOffsetImm(var, ..)|
            Self::AtOffsetTemp(var, ..)|
            Self::AtOffsetVar(var, ..)|
            Self::Deref(var, ..) => var.size,
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Address {
    Mem(MemAddress),
    Reg(RegAddress),
}

impl Address {
    fn size_of(&self) -> usize {
        match self {
            Self::Mem(mem) => mem.size_of(),
            Self::Reg(reg) => reg.size_of(),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
enum RightOp {
    Addr(Address),
    Imm(Immediate),
}


const REGS: [Register; 14] = [ Register::Rax,
                               Register::Rbx,
                               Register::Rcx,
                               Register::Rdx,
                               Register::Rdi,
                               Register::Rsi,
                               Register::R8,
                               Register::R9,
                               Register::R10,
                               Register::R11,
                               Register::R12,
                               Register::R13,
                               Register::R14,
                               Register::R15 ];


struct Registry {
    vars: HashMap<String, MemAddress>,
    regs: HashMap<usize, RegAddress>,
    rsp: isize,
    used: HashSet<Register>
}

impl Registry {
    fn new() -> Self {
        Self{
            vars: HashMap::new(),
            regs: HashMap::new(),
            rsp: 0,
            used: HashSet::new(),
        }
    }

    fn alloc(&mut self, var: &Dest) -> Option<Address> {
        match var {
            Dest::Addr(addr) => {
                if let Some(mem) = self.vars.get(addr.base().name_of()) {
                    Some(Address::Mem(MemAddress::from_var(addr, mem.base(), self)))
                } else {
                    let size = addr.size_of();
                    let align = addr.align_of() as isize;
                    self.rsp -= size as isize;

                    self.rsp = if -self.rsp % align == 0 {
                        self.rsp
                    } else {
                        let nsize = -self.rsp + align;
                        -nsize + (nsize % align)
                    };
                    let addrval = MemAddress::from_var(addr, self.rsp, self);
                    self.vars.insert(addr.base().name_of().clone(), addrval);
                    Some(Address::Mem(addrval))
                }
            }
            Dest::Temp(temp) => {
                if let Some(reg) = self.regs.get(&temp.base().name_of()) {
                    Some(Address::Reg(RegAddress::from_temp(temp, reg.base(), self)))
                } else {
                    for reg in REGS {
                        if !self.used.contains(&reg) {
                            self.used.insert(reg);
                            let regval = RegAddress::from_temp(temp, reg, self);
                            return Some(Address::Reg(regval))
                        }
                    }
        
                    None
                }
            }
        }
    }

    fn dealloc(&mut self, var: &Dest) -> Option<Address> {
        match var {
            Dest::Addr(_) => None,
            Dest::Temp(temp) => {
                if let Some(reg) = self.regs.remove(&temp.base().name_of()) {
                    self.used.remove(&reg.base());
                    Some(Address::Reg(reg))
                } else {
                    None
                }
            }
        }
    }

    fn get(&self, var: &Dest) -> Option<Address> {
        match var {
            Dest::Addr(addr) => {
                self.vars.get(addr.base().name_of()).map(|a| Address::Mem(*a))
            }
            Dest::Temp(temp) => {
                self.regs.get(&temp.base().name_of()).map(|a| Address::Reg(*a))
            }
        }
    }

    fn malloc(&mut self, size: usize) -> isize {        
        self.rsp -= size as isize;
        self.rsp
    }

    fn rsp(&self) -> isize {
        self.rsp
    }
}


struct UnOp(Dest, Operand);

struct BinOp(Dest, Operand, Operand);


fn basic_un_op<Op: FnOnce(Register, RightOp) -> String>(registry: &mut Registry, data: UnOp, op: Op) -> String {
    let dest = if let Address::Reg(reg) = registry.alloc(&data.0).unwrap() {
        reg.base()
    } else {
        unreachable!()
    };

    match data.1 {
        Operand::Temp(a) => {
            let a = registry.dealloc(&Dest::Temp(a)).unwrap();
            format!("{}", op(dest, RightOp::Addr(a)))
        }
        Operand::Var(a) => {
            let a = registry.get(&Dest::Addr(a)).unwrap();
            format!("{}", op(dest, RightOp::Addr(a)))
        }
        _ => unreachable!()
    }
}

fn basic_bin_op<Op: FnOnce(Register, RightOp) -> String>(registry: &mut Registry, data: BinOp, op: Op) -> String {
    let dest = if let Address::Reg(reg) = registry.alloc(&data.0).unwrap() {
        reg.base()
    } else {
        unreachable!()
    };

    match (data.1, data.2) {
        (Operand::Temp(a), Operand::Imm(b)) |
        (Operand::Imm(b), Operand::Temp(a)) => {
            let a = registry.dealloc(&Dest::Temp(a)).unwrap();
            format!("{}\n{}", ldst::ld(dest, a), op(dest, RightOp::Imm(b)))
        }
        (Operand::Var(a), Operand::Imm(b)) |
        (Operand::Imm(b), Operand::Var(a)) => {
            let a = registry.get(&Dest::Addr(a)).unwrap();
            format!("{}\n{}", ldst::ld(dest, a), op(dest, RightOp::Imm(b)))
        }
        (Operand::Temp(a), Operand::Var(b)) |
        (Operand::Var(b), Operand::Temp(a)) => {
            let a = registry.dealloc(&Dest::Temp(a)).unwrap();
            let b = registry.get(&Dest::Addr(b)).unwrap();
            format!("{}\n{}", ldst::ld(dest, a), op(dest, RightOp::Addr(b)))
        }
        (Operand::Var(a), Operand::Var(b)) => {
            let a = registry.get(&Dest::Addr(a)).unwrap();
            let b = registry.get(&Dest::Addr(b)).unwrap();
            format!("{}\n{}", ldst::ld(dest, a), op(dest, RightOp::Addr(b)))
        }
        (Operand::Temp(a), Operand::Temp(b)) => {
            let a = registry.dealloc(&Dest::Temp(a)).unwrap();
            let b = registry.dealloc(&Dest::Temp(b)).unwrap();
            format!("{}\n{}", ldst::ld(dest, a), op(dest, RightOp::Addr(b)))
        }
        _ => unreachable!()
    }
}


pub fn generate(program: Program) -> Result<String, std::fmt::Error> {
    let mut res = String::new();

    writeln!(res, "    global main")?;
    writeln!(res, "    section .text")?;

    for proc in program {
        let mut registry = Registry::new();

        for op in proc.ops {
            match op {
                Operation::FuncBegin(name, params) => {
                    writeln!(res, "{}:", name)?;
                    writeln!(res, "    push    rbp")?;
                    writeln!(res, "    mov     rbp, rsp")?;
                    //if params.len() >= 4 {
                    //    writeln!(res, "    mov     QWORD [rbp+24], r9")?;
                    //    registry.place(1, 24);
                    //}
                    //if params.len() >= 3 {
                    //    writeln!(res, "    mov     QWORD [rbp+16], r8")?;
                    //    registry.place(1, 16);
                    //}
                    //if params.len() >= 2 {
                    //    writeln!(res, "    mov     QWORD [rbp+8], rdx")?;
                    //    registry.place(1, 8);
                    //}
                    //if !params.is_empty() {
                    //    writeln!(res, "    mov     QWORD [rbp], rcx")?;
                    //    registry.place(1, 0);
                    //}
                    writeln!(res, "    sub     rsp, {}", proc.stack)?;

                }
                Operation::Jmp0(label, val) => {
                    match val {
                        Operand::Temp(src) => {
                            let src = registry.dealloc(&Dest::Temp(src)).unwrap();
                            //writeln!(res, "    test    {}, {}", src, src)?;
                            writeln!(res, "    jz      .LB{}", label)?;
                        }
                        Operand::Var(src) => {
                            let src = registry.get(&Dest::Addr(src)).unwrap();
                            //writeln!(res, "    test    {}, {}", src, src)?;
                            writeln!(res, "    jz      .LB{}", label)?;
                        }
                        _ => unreachable!()
                    }
                }
                Operation::Jump(label) => {
                    writeln!(res, "    jmp     .LB{}", label)?;
                }
                Operation::Label(label) => {
                    writeln!(res, ".LB{}:", label)?
                }
                Operation::Return => {
                    writeln!(res, "    leave\n    ret")?
                }

                Operation::Neg(var, val) => {
                    writeln!(res, "{}", basic_un_op(&mut registry, UnOp(var, val), ops::neg))?;
                }
                Operation::Add(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::add))?;
                }
                Operation::Sub(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::sub))?;
                }
                Operation::Mul(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::mul))?;
                }
                Operation::Div(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::div))?;
                }
                Operation::Mod(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::rem))?;
                }

                Operation::Not(var, val) => {
                    writeln!(res, "{}", basic_un_op(&mut registry, UnOp(var, val), ops::not))?;
                }
                Operation::Or (var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::or))?;
                }
                Operation::And(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::and))?;
                }

                Operation::Lt(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::lt))?;
                }
                Operation::Gt(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::gt))?;
                }
                Operation::Le(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::le))?;
                }
                Operation::Ge(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::ge))?;
                }

                Operation::BitNot(var, val) => {
                    writeln!(res, "{}", basic_un_op(&mut registry, UnOp(var, val), ops::not))?;
                }
                Operation::BitOr (var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::or))?;
                }
                Operation::BitXor(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::xor))?;
                }
                Operation::BitAnd(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::and))?;
                }
                Operation::LShift(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::shl))?;
                }
                Operation::RShift(var, left, right) => {
                    writeln!(res, "{}", basic_bin_op(&mut registry, BinOp(var, left, right), ops::shr))?;
                }

                Operation::AddrOf(var, val) => {
                    let dest = registry.alloc(&var).unwrap();

                    match val {
                        Operand::Temp(src) => {
                            let src = registry.dealloc(&Dest::Temp(src)).unwrap();
                            writeln!(res, "{}", ldst::mov(dest, src))?
                        }
                        Operand::Var(src) => {
                            let src = registry.get(&Dest::Addr(src)).unwrap();
                            writeln!(res, "{}", ldst::mov(dest, src))?
                        }
                        _ => unreachable!()
                    }
                }
                
                Operation::Assign(var, val) => {
                    let dest = registry.alloc(&var).unwrap();

                    match val {
                        Operand::Temp(src) => {
                            let src = registry.dealloc(&Dest::Temp(src)).unwrap();
                            writeln!(res, "{}", ldst::mov(dest, src))?
                        }
                        Operand::Var(src) => {
                            let src = registry.get(&Dest::Addr(src)).unwrap();
                            writeln!(res, "{}", ldst::mov(dest, src))?
                        }
                        _ => unreachable!()
                    }
                }

                Operation::LoadArr(var, ops) => {
                    let dest = registry.alloc(&var).unwrap();
                    let (rsp, footer) = match dest {
                        Address::Reg(RegAddress::At(reg)) => {
                            let rsp = registry.malloc(dest.size_of());
                            (rsp, format!("    lea     {}, [rbp-{}]\n", reg, rsp))
                        }
                        Address::Mem(MemAddress::At(mem)) => {
                            (mem.loc, String::new())
                        }
                        _ => unreachable!("{:?}", dest)
                    };
                    
                    for (i, op) in ops.into_iter().enumerate() {
                        match op {
                            Operand::Temp(src) => {
                                let dest = Address::Mem(MemAddress::AtOffsetImm(MemVar{ loc: rsp, size: src.size_of() }, i as isize));
                                let src = registry.dealloc(&Dest::Temp(src)).unwrap();
                                writeln!(res, "{}", ldst::mov(dest, src))?;
                            }
                            Operand::Var(src) => {
                                let dest = Address::Mem(MemAddress::AtOffsetImm(MemVar{ loc: rsp, size: src.size_of() }, i as isize));
                                let src = registry.get(&Dest::Addr(src)).unwrap();
                                writeln!(res, "{}", ldst::mov(dest, src))?;
                            }
                            _ => unreachable!()
                        }
                    }

                    writeln!(res, "{}", footer)?;
                }

                _ => ()
            }
        }
    }

    Ok(res)
}
