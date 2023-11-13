use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsp,
    Xmm0,
    Xmm1
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Rax => write!(f, "rax"),
            Register::Rbx => write!(f, "rbx"),
            Register::Rcx => write!(f, "rcx"),
            Register::Rdx => write!(f, "rdx"),
            Register::Rsp => write!(f, "rsp"),
            Register::Xmm0 => write!(f, "xmm0"),
            Register::Xmm1 => write!(f, "xmm1"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Imm(i64),
    Reg(Register),
    Ptr(String),
}

impl Operand {
    pub fn rax() -> Self {
        Self::Reg(Register::Rax)
    }

    pub fn rbx() -> Self {
        Self::Reg(Register::Rbx)
    }

    pub fn rcx() -> Self {
        Self::Reg(Register::Rcx)
    }

    pub fn rdx() -> Self {
        Self::Reg(Register::Rdx)
    }

    pub fn rsp() -> Self {
        Self::Reg(Register::Rsp)
    }

    pub fn xmm0() -> Self {
        Self::Reg(Register::Xmm0)
    }

    pub fn xmm1() -> Self {
        Self::Reg(Register::Xmm1)
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "{i}"),
            Operand::Reg(r) => write!(f, "{r}"),
            Operand::Ptr(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mnemonic {
    Push(Operand),
    Pop(Register),
    Mov(Register, Operand),
    Cmovl(Register, Register),
    Cmovg(Register, Register),
    Cmovle(Register, Register),
    Cmovge(Register, Register),
    Cmove(Register, Register),
    Cmovne(Register, Register),
    Call(String),
    Add(Register, Operand),
    Sub(Register, Operand),
    IMul(Register, Operand),
    IMulImm(Register, Register, i64),
    IDiv(Register),
    Addss(Register, Operand),
    Inc(Register),
    Dec(Register),
    Cmp(Operand, Operand),
    Xor(Register, Operand),
    Test(Register, Register),
    Jmp(String),
    Jz(String),
    Jnl(String),
    Jng(String),
    Jg(String),
    Jl(String),
    Jne(String),
    Je(String),
    Label(String),
}

impl Display for Mnemonic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mnemonic::Push(op) => write!(f, "    push {op}"),
            Mnemonic::Pop(reg) => write!(f, "    pop {reg}"),
            Mnemonic::Mov(reg, op) => write!(f, "    mov {reg}, {op}"),
            Mnemonic::Cmovl(r1, r2) => write!(f, "    cmovl {r1}, {r2}"),
            Mnemonic::Cmovg(r1, r2) => write!(f, "    cmovg {r1}, {r2}"),
            Mnemonic::Cmovle(r1, r2) => write!(f, "    cmovle {r1}, {r2}"),
            Mnemonic::Cmovge(r1, r2) => write!(f, "    cmovge {r1}, {r2}"),
            Mnemonic::Cmove(r1, r2) => write!(f, "    cmove {r1}, {r2}"),
            Mnemonic::Cmovne(r1, r2) => write!(f, "    cmovne {r1}, {r2}"),
            Mnemonic::Call(func) => write!(f, "    call {func}"),
            Mnemonic::Add(reg, op) => write!(f, "    add {reg}, {op}"),
            Mnemonic::Sub(reg, op) => write!(f, "    sub {reg}, {op}"),
            Mnemonic::IMul(reg, op) => write!(f, "    imul {reg}, {op}"),
            Mnemonic::IMulImm(r1, r2, i) => write!(f, "    imul {r1}, {r2}, {i}"),
            Mnemonic::IDiv(reg) => write!(f, "    idiv {reg}"),
            Mnemonic::Addss(reg, op) => write!(f, "    addss {reg}, {op}"),
            Mnemonic::Inc(reg) => write!(f, "    inc {reg}"),
            Mnemonic::Dec(reg) => write!(f, "    dec {reg}"),
            Mnemonic::Cmp(op1, op2) => write!(f, "    cmp {op1}, {op2}"),
            Mnemonic::Xor(reg, op) => write!(f, "    xor {reg}, {op}"),
            Mnemonic::Test(r1, r2) => write!(f, "    test {r1}, {r2}"),
            Mnemonic::Jmp(label) => write!(f, "    jmp {label}"),
            Mnemonic::Jz(label) => write!(f, "    jz {label}"),
            Mnemonic::Jnl(label) => write!(f, "    jnl {label}"),
            Mnemonic::Jng(label) => write!(f, "    jg {label}"),
            Mnemonic::Jg(label) => write!(f, "    jg {label}"),
            Mnemonic::Jl(label) => write!(f, "    jl {label}"),
            Mnemonic::Jne(label) => write!(f, "    jne {label}"),
            Mnemonic::Je(label) => write!(f, "    je {label}"),
            Mnemonic::Label(label) => write!(f, "{label}:"),
        }
    }
}
