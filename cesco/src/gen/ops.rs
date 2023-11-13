use super::*;
use std::fmt::Write;


fn basic_op(dest: Register, src: RightOp, op: &'static str) -> String {
    match src {
        RightOp::Imm(Immediate::Int(i)) => format!("    add     {}, {}", dest, i.as_usize()),
        RightOp::Imm(_) => unreachable!(),
        RightOp::Addr(Address::Reg(reg)) => match reg {
            RegAddress::At(reg) =>                                      format!("    {op}     {}, {}", dest, reg),
            RegAddress::Deref(reg, size) =>                      format!("    {op}     {}, {} [{}]", dest, mem_keyword(size), reg),
            RegAddress::DerefOffsetImm(reg, size, off) => format!("    {op}     {}, {} [{}{}]", dest, mem_keyword(size), reg, fmt_offset(off)),
        }
        RightOp::Addr(Address::Mem(mem)) => match mem {
            MemAddress::At(MemVar{ loc, size }) =>                          format!("    {op}     {}, {} [rsp{}]", dest, mem_keyword(size), fmt_offset(loc)),
            MemAddress::AtOffsetImm(MemVar{ loc, size }, off) =>     format!("    {op}     {}, {} [rsp{}]", dest, mem_keyword(size), fmt_offset(loc + off)),
            MemAddress::AtOffsetTemp(MemVar{ loc, size }, off) => format!("    {op}     {}, {} [rsp+{}{}]", dest, mem_keyword(size), off, fmt_offset(loc)),
            MemAddress::AtOffsetVar(MemVar{ loc, size }, MemVar{ loc: offloc, size: offsize }) => {
                let mut res = String::new();
                write!(res, "    push    rbx");
                write!(res, "    mov     rbx, {} [rsp{}]", mem_keyword(offsize), fmt_offset(offloc));
                write!(res, "    {op}     {}, {} [rsp+rbx{}]", dest, mem_keyword(size), fmt_offset(loc));
                write!(res, "    pop     rbx");
                res

            }
            MemAddress::Deref(MemVar{ loc, size }) => {
                let mut res = String::new();
                write!(res, "    push    rbx");
                write!(res, "    mov     rbx, {} [rsp{}]", mem_keyword(size), fmt_offset(loc));
                write!(res, "    {op}     {}, QWORD [rbx]", dest);
                write!(res, "    pop     rbx");
                res
            }
        },
    }
}


pub fn neg(dest: Register, src: RightOp) -> String {
    basic_op(dest, src, "neg")
}

pub fn add(dest: Register, src: RightOp) -> String {
    basic_op(dest, src, "add")
}

pub fn sub(dest: Register, src: RightOp) -> String {
    basic_op(dest, src, "sub")
}

pub fn mul(dest: Register, src: RightOp) -> String {
    String::new()
}

pub fn div(dest: Register, src: RightOp) -> String {
    String::new()
}

pub fn rem(dest: Register, src: RightOp) -> String {
    String::new()
}


pub fn lt(dest: Register, src: RightOp) -> String {
    String::new()
}

pub fn gt(dest: Register, src: RightOp) -> String {
    String::new()
}

pub fn le(dest: Register, src: RightOp) -> String {
    String::new()
}

pub fn ge(dest: Register, src: RightOp) -> String {
    String::new()
}


pub fn not(dest: Register, src: RightOp) -> String {
    basic_op(dest, src, "not ")
}

pub fn or(dest: Register, src: RightOp) -> String {
    basic_op(dest, src, "or ")
}

pub fn xor(dest: Register, src: RightOp) -> String {
    basic_op(dest, src, "xor")
}

pub fn and(dest: Register, src: RightOp) -> String {
    basic_op(dest, src, "and")
}

pub fn shl(dest: Register, src: RightOp) -> String {
    String::new()
}

pub fn shr(dest: Register, src: RightOp) -> String {
    String::new()
}
