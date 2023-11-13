use super::*;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RegDeref {
    At(Register, usize),
    AtOffsetImm(Register, usize, isize),
}


fn ld_reg(dest: Register, src: RegAddress) -> String {
    match src {
        RegAddress::At(reg) => format!("    mov     {dest}, {}", reg),
        RegAddress::Deref(reg, size) => format!("    mov     {dest}, {} [{}]", mem_keyword(size), reg),
        RegAddress::DerefOffsetImm(reg, size, off) => format!("    mov     {dest}, {} [{}{}]", mem_keyword(size), reg, fmt_offset(off)),
    }
}

fn ld_mem(dest: Register, src: MemAddress) -> String {
    match src {
        MemAddress::At(MemVar{ loc, size }) => format!("    mov     {dest}, {} [rsp{}]", mem_keyword(size), fmt_offset(loc)),
        MemAddress::AtOffsetImm(MemVar{ loc, size }, off) => format!("    mov     {dest}, {} [rsp{}]", mem_keyword(size), fmt_offset(loc + off)),
        MemAddress::AtOffsetTemp(MemVar{ loc, size }, off) => format!("    mov     {dest}, {} [rsp+{}{}]", mem_keyword(size), off, fmt_offset(loc)),
        MemAddress::AtOffsetVar(MemVar{ loc, size }, MemVar{ loc: offloc, size: offsize }) => {
            format!("    mov     {dest}, {} [rsp{}]\n    mov     {dest}, {} [rsp+{dest}{}]", mem_keyword(offsize), fmt_offset(offloc), mem_keyword(size), fmt_offset(loc))
        }
        MemAddress::Deref(MemVar{ loc, size }) => {
            format!("    mov     {dest}, QWORD [rsp{}]\n    mov     {dest}, {} [{dest}]", fmt_offset(loc), mem_keyword(size))
        }
    }
}

pub fn ld(dest: Register, src: Address) -> String {
    match src {
        Address::Reg(reg) => ld_reg(dest, reg),
        Address::Mem(mem) => ld_mem(dest, mem),
    }
}


fn st_reg(dest: MemAddress, src: RegAddress) -> String {
    match dest {
        MemAddress::At(MemVar{ loc, size }) => match src {
            RegAddress::At(reg) => format!("    mov     {} [rsp{}], {}", mem_keyword(size), fmt_offset(loc), reg),
            _ => unimplemented!(),
        },
        MemAddress::AtOffsetImm(MemVar{ loc, size }, off) => match src {
            RegAddress::At(reg) => format!("    mov     {} [rsp{}], {}", mem_keyword(size), fmt_offset(loc + off), reg),
            _ => unimplemented!(),
        }
        MemAddress::AtOffsetTemp(MemVar{ loc, size }, off) => match src {
            RegAddress::At(reg) => format!("    mov     {} [rsp{}], {}", mem_keyword(size), fmt_offset(loc), reg),
            _ => unimplemented!(),
        }
        MemAddress::AtOffsetVar(MemVar{ loc, size }, off) => match src {
            RegAddress::At(reg) => format!("    mov     {} [rsp{}], {}", mem_keyword(size), fmt_offset(loc), reg),
            _ => unimplemented!(),
        }
        MemAddress::Deref(MemVar{ loc, size }) => unimplemented!(),
    }
}

fn st_mem(dest: MemAddress, src: MemAddress) -> String {
    match dest {
        MemAddress::At(dest) => match src {
            MemAddress::At(MemVar{ loc, size }) => unimplemented!(),
            MemAddress::AtOffsetImm(MemVar{ loc, size }, off) => unimplemented!(),
            MemAddress::AtOffsetTemp(MemVar{ loc, size }, off) => unimplemented!(),
            MemAddress::AtOffsetVar(MemVar{ loc, size }, off) => unimplemented!(),
            MemAddress::Deref(MemVar{ loc, size }) => unimplemented!(),
        },
        MemAddress::AtOffsetImm(MemVar{ loc, size }, off) => unimplemented!(),
        MemAddress::AtOffsetTemp(MemVar{ loc, size }, off) => unimplemented!(),
        MemAddress::AtOffsetVar(MemVar{ loc, size }, off) => unimplemented!(),
        MemAddress::Deref(MemVar{ loc, size }) => unimplemented!(),
    }
}

pub fn st(dest: MemAddress, src: Address) -> String {
    match src {
        Address::Reg(reg) => st_reg(dest, reg),
        Address::Mem(mem) => st_mem(dest, mem),
    }
}


fn st_deref_reg(dest: RegDeref, src: RegAddress) -> String {
    match dest {
        RegDeref::At(dest, size) => match src {
            RegAddress::At(reg) => format!("    mov     {} [{}], {}", mem_keyword(size), dest, reg),
            _ => unimplemented!(),
        },
        RegDeref::AtOffsetImm(dest, size, off) => match src {
            RegAddress::At(reg) => format!("    mov     {} [{}{}], {}", mem_keyword(size), dest, fmt_offset(off), reg),
            _ => unimplemented!(),
        }
    }
}

fn st_deref_mem(dest: RegDeref, src: MemAddress) -> String {
    match dest {
        RegDeref::At(reg, size) => match src {
            MemAddress::At(MemVar{ loc, size }) => unimplemented!(),
            MemAddress::AtOffsetImm(MemVar{ loc, size }, off) => unimplemented!(),
            MemAddress::AtOffsetTemp(MemVar{ loc, size }, off) => unimplemented!(),
            MemAddress::AtOffsetVar(MemVar{ loc, size }, off) => unimplemented!(),
            MemAddress::Deref(MemVar{ loc, size }) => unimplemented!(),
        },
        RegDeref::AtOffsetImm(reg, size, off) => unimplemented!(),
    }
}

fn st_deref(dest: RegDeref, src: Address) -> String {
    match src {
        Address::Reg(reg) => st_deref_reg(dest, reg),
        Address::Mem(mem) => st_deref_mem(dest, mem),
    }
}


pub fn mov(dest: Address, src: Address) -> String {
    match dest {
        Address::Reg(reg) => match reg {
            RegAddress::At(reg) => ld(reg, src),
            RegAddress::Deref(reg, size) => st_deref(RegDeref::At(reg, size), src),
            RegAddress::DerefOffsetImm(reg, size, off) => st_deref(RegDeref::AtOffsetImm(reg, size, off), src),
        }
        Address::Mem(mem) => st(mem, src),
    }
}
