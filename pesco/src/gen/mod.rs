mod mnems;

use std::collections::HashMap;
use std::collections::HashSet;
use crate::data::Data;
use crate::parse::Token;
use crate::parse::TokenKind;
pub use mnems::*;

struct Condition {
    kind: TokenKind,
    loc_for_end: usize,
    loc_for_do: usize,
}

fn inter_pushint(r: &mut Vec<Mnemonic>, v: Data) {
    r.push(Mnemonic::Push(Operand::Imm(v.into_int())));
}

fn inter_pushflt(r: &mut Vec<Mnemonic>, v: Data, lits: &mut HashMap<u64, usize>) {
    let f = v.into_flt().to_bits();

    if lits.contains_key(&f) {
        let i = lits.get(&f).unwrap();
        r.push(Mnemonic::Mov(Register::Rax, Operand::Ptr(format!("F{}", *i))));
    } else {
        r.push(Mnemonic::Mov(Register::Rax, Operand::Ptr(format!("F{}", lits.len()))));
        lits.insert(f, lits.len());
    }

    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_pushstr(r: &mut Vec<Mnemonic>, v: Data, lits: &mut HashMap<String, usize>) {
    let s = v.into_str();

    if lits.contains_key(&s) {
        let i = lits.get(&s).unwrap();
        r.push(Mnemonic::Mov(Register::Rax, Operand::Ptr(format!("S{}", *i))));
    } else {
        r.push(Mnemonic::Mov(Register::Rax, Operand::Ptr(format!("S{}", lits.len()))));
        lits.insert(s, lits.len());
    }

    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_pushtrue(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Push(Operand::Imm(1)));
}

fn inter_pushfalse(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Push(Operand::Imm(0)));
}

fn inter_dup(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Push(Operand::rax()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_2dup(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Push(Operand::rax()));
    r.push(Mnemonic::Push(Operand::rbx()));
    r.push(Mnemonic::Push(Operand::rax()));
    r.push(Mnemonic::Push(Operand::rbx()));
}

fn inter_over(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Push(Operand::rax()));
    r.push(Mnemonic::Push(Operand::rbx()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_rot(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rcx));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Push(Operand::rcx()));
    r.push(Mnemonic::Push(Operand::rax()));
    r.push(Mnemonic::Push(Operand::rbx()));
}

fn inter_irot(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rcx));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Push(Operand::rbx()));
    r.push(Mnemonic::Push(Operand::rcx()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_swap(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Push(Operand::rbx()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_pop(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rax));
}

fn inter_add(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Add(Register::Rax, Operand::rbx()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_sub(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Sub(Register::Rax, Operand::rbx()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_mul(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::IMul(Register::Rax, Operand::rbx()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_div(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Xor(Register::Rdx, Operand::rdx()));
    r.push(Mnemonic::IDiv(Register::Rbx));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_addimm(r: &mut Vec<Mnemonic>, v: Data) {
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Add(Register::Rax, Operand::Imm(v.into_int())));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_subimm(r: &mut Vec<Mnemonic>, v: Data) {
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Sub(Register::Rax, Operand::Imm(v.into_int())));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_mulimm(r: &mut Vec<Mnemonic>, v: Data) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::IMulImm(Register::Rax, Register::Rbx, v.into_int()));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_divimm(r: &mut Vec<Mnemonic>, v: Data) {
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Mov(Register::Rbx, Operand::Imm(v.into_int())));
    r.push(Mnemonic::Xor(Register::Rdx, Operand::rdx()));
    r.push(Mnemonic::IDiv(Register::Rbx));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_addf(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Xmm1));
    r.push(Mnemonic::Pop(Register::Xmm0));
    r.push(Mnemonic::Add(Register::Xmm0, Operand::xmm1()));
    r.push(Mnemonic::Push(Operand::xmm0()));
}

fn inter_inc(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Inc(Register::Rax));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_dec(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Dec(Register::Rax));
    r.push(Mnemonic::Push(Operand::rax()));
}

fn inter_lt(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Xor(Register::Rcx, Operand::rcx()));
    r.push(Mnemonic::Mov(Register::Rdx, Operand::Imm(1)));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Cmp(Operand::rax(), Operand::rbx()));
    r.push(Mnemonic::Cmovl(Register::Rcx, Register::Rdx));
    r.push(Mnemonic::Push(Operand::rcx()));
}

fn inter_gt(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Xor(Register::Rcx, Operand::rcx()));
    r.push(Mnemonic::Mov(Register::Rdx, Operand::Imm(1)));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Cmp(Operand::rax(), Operand::rbx()));
    r.push(Mnemonic::Cmovg(Register::Rcx, Register::Rdx));
    r.push(Mnemonic::Push(Operand::rcx()));
}

fn inter_le(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Xor(Register::Rcx, Operand::rcx()));
    r.push(Mnemonic::Mov(Register::Rdx, Operand::Imm(1)));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Cmp(Operand::rax(), Operand::rbx()));
    r.push(Mnemonic::Cmovle(Register::Rcx, Register::Rdx));
    r.push(Mnemonic::Push(Operand::rcx()));
}

fn inter_ge(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Xor(Register::Rcx, Operand::rcx()));
    r.push(Mnemonic::Mov(Register::Rdx, Operand::Imm(1)));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Cmp(Operand::rax(), Operand::rbx()));
    r.push(Mnemonic::Cmovge(Register::Rcx, Register::Rdx));
    r.push(Mnemonic::Push(Operand::rcx()));
}

fn inter_eq(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Xor(Register::Rcx, Operand::rcx()));
    r.push(Mnemonic::Mov(Register::Rdx, Operand::Imm(1)));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Cmp(Operand::rax(), Operand::rbx()));
    r.push(Mnemonic::Cmove(Register::Rcx, Register::Rdx));
    r.push(Mnemonic::Push(Operand::rcx()));
}

fn inter_ne(r: &mut Vec<Mnemonic>) {
    r.push(Mnemonic::Xor(Register::Rcx, Operand::rcx()));
    r.push(Mnemonic::Mov(Register::Rdx, Operand::Imm(1)));
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Cmp(Operand::rax(), Operand::rbx()));
    r.push(Mnemonic::Cmovne(Register::Rcx, Register::Rdx));
    r.push(Mnemonic::Push(Operand::rcx()));
}

fn inter_cmpdo(r: &mut Vec<Mnemonic>, conds: &[Condition], k: Data) {
    r.push(Mnemonic::Pop(Register::Rbx));
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Cmp(Operand::rax(), Operand::rbx()));
    let loc_for_do = conds.last().unwrap().loc_for_do;
    match k.into_int() {
        0 => r.push(Mnemonic::Jnl(format!("cond_{loc_for_do}"))),
        1 => r.push(Mnemonic::Jng(format!("cond_{loc_for_do}"))),
        2 => r.push(Mnemonic::Jg(format!("cond_{loc_for_do}"))),
        3 => r.push(Mnemonic::Jl(format!("cond_{loc_for_do}"))),
        4 => r.push(Mnemonic::Jne(format!("cond_{loc_for_do}"))),
        5 => r.push(Mnemonic::Je(format!("cond_{loc_for_do}"))),
        _ => ()
    }

    // 0 lt -> nl
    // 1 gt -> ng
    // 2 le -> g
    // 3 ge -> l
    // 4 eq -> ne
    // 5 ne -> e
}

fn inter_out(r: &mut Vec<Mnemonic>, links: &mut HashSet<String>) {
    r.push(Mnemonic::Pop(Register::Rcx));
    r.push(Mnemonic::Sub(Register::Rsp, Operand::Imm(32)));
    r.push(Mnemonic::Call("dump".to_string()));
    r.push(Mnemonic::Add(Register::Rsp, Operand::Imm(32)));
    links.insert("stdio".to_string());
}

fn inter_err(r: &mut Vec<Mnemonic>, links: &mut HashSet<String>) {
    r.push(Mnemonic::Pop(Register::Rcx));
    r.push(Mnemonic::Sub(Register::Rsp, Operand::Imm(32)));
    r.push(Mnemonic::Call("dump".to_string()));
    r.push(Mnemonic::Add(Register::Rsp, Operand::Imm(32)));
    links.insert("stdio".to_string());
}

fn inter_puts(r: &mut Vec<Mnemonic>, links: &mut HashSet<String>) {
    r.push(Mnemonic::Pop(Register::Rcx));
    r.push(Mnemonic::Sub(Register::Rsp, Operand::Imm(32)));
    r.push(Mnemonic::Call("puts".to_string()));
    r.push(Mnemonic::Add(Register::Rsp, Operand::Imm(32)));
    links.insert("stdio".to_string());
}

fn inter_if(_r: &[Mnemonic], conds: &mut Vec<Condition>, i: usize) {
    conds.push(Condition {
        kind: TokenKind::KwIf,
        loc_for_end: i,
        loc_for_do: i,
    });
}

fn inter_while(r: &mut Vec<Mnemonic>, conds: &mut Vec<Condition>, i: usize) {
    conds.push(Condition {
        kind: TokenKind::KwWhile,
        loc_for_end: i,
        loc_for_do: i,
    });

    r.push(Mnemonic::Label(format!("addr_{i}")));
}

fn inter_do(r: &mut Vec<Mnemonic>, conds: &[Condition]) {
    r.push(Mnemonic::Pop(Register::Rax));
    r.push(Mnemonic::Test(Register::Rax, Register::Rax));
    let loc_for_do = conds.last().unwrap().loc_for_do;
    r.push(Mnemonic::Jz(format!("cond_{loc_for_do}")));
}

fn inter_end(r: &mut Vec<Mnemonic>, conds: &mut Vec<Condition>) {
    let Condition {
        kind,
        loc_for_end,
        loc_for_do,
    } = conds.pop().unwrap();

    match kind {
        TokenKind::KwIf => {
            r.push(Mnemonic::Label(format!("cond_{loc_for_do}")));
        }
        TokenKind::KwWhile => {
            r.push(Mnemonic::Jmp(format!("addr_{loc_for_end}")));
            r.push(Mnemonic::Label(format!("cond_{loc_for_do}")));
        }
        _ => (),
    }
}


pub struct Program {
    pub instrs: Vec<Mnemonic>,
    pub fltlits: HashMap<u64, usize>,
    pub strlits: HashMap<String, usize>,
    pub links: HashSet<String>,
}


pub fn gen_inter(tokens: Vec<Token>) -> Program {
    let mut res: Vec<Mnemonic> = Vec::new();
    let mut fltlits: HashMap<u64, usize> = HashMap::new();
    let mut strlits: HashMap<String, usize> = HashMap::new();
    let mut links: HashSet<String> = HashSet::new();

    let mut conds: Vec<Condition> = Vec::new();

    for (i, t) in tokens.into_iter().enumerate() {
        match t.kind {
            TokenKind::IntLit => inter_pushint(&mut res, t.val),
            TokenKind::FltLit => inter_pushflt(&mut res, t.val, &mut fltlits),
            TokenKind::StrLit => inter_pushstr(&mut res, t.val, &mut strlits),
            TokenKind::KwTrue => inter_pushtrue(&mut res),
            TokenKind::KwFalse => inter_pushfalse(&mut res),
            TokenKind::OpDup => inter_dup(&mut res),
            TokenKind::Op2Dup => inter_2dup(&mut res),
            TokenKind::OpOver => inter_over(&mut res),
            TokenKind::OpRot => inter_rot(&mut res),
            TokenKind::OpIRot => inter_irot(&mut res),
            TokenKind::OpSwap => inter_swap(&mut res),
            TokenKind::OpPop => inter_pop(&mut res),
            TokenKind::OpAdd => inter_add(&mut res),
            TokenKind::OpSub => inter_sub(&mut res),
            TokenKind::OpMul => inter_mul(&mut res),
            TokenKind::OpDiv => inter_div(&mut res),
            TokenKind::_AddImm => inter_addimm(&mut res, t.val),
            TokenKind::_SubImm => inter_subimm(&mut res, t.val),
            TokenKind::_MulImm => inter_mulimm(&mut res, t.val),
            TokenKind::_DivImm => inter_divimm(&mut res, t.val),
            TokenKind::_OpInc => inter_inc(&mut res),
            TokenKind::_OpDec => inter_dec(&mut res),
            TokenKind::OpLT => inter_lt(&mut res),
            TokenKind::OpGT => inter_gt(&mut res),
            TokenKind::OpLE => inter_le(&mut res),
            TokenKind::OpGE => inter_ge(&mut res),
            TokenKind::OpEQ => inter_eq(&mut res),
            TokenKind::OpNE => inter_ne(&mut res),
            TokenKind::_CmpDo => inter_cmpdo(&mut res, &conds, t.val),
            TokenKind::OpOut => inter_out(&mut res, &mut links),
            TokenKind::OpErr => inter_err(&mut res, &mut links),
            TokenKind::KwPuts => inter_puts(&mut res, &mut links),
            TokenKind::KwIf => inter_if(&res, &mut conds, i),
            TokenKind::KwWhile => inter_while(&mut res, &mut conds, i),
            TokenKind::KwDo => inter_do(&mut res, &conds),
            TokenKind::KwEnd => inter_end(&mut res, &mut conds),

            TokenKind::OpIn => unimplemented!(),
            TokenKind::OpArr => unimplemented!(),
            TokenKind::KwStr => unimplemented!(),
            TokenKind::KwInt => unimplemented!(),
            TokenKind::KwFlt => unimplemented!(),
            TokenKind::Ident => unimplemented!(),

            _ => (),
        }
    }

    Program{ instrs: res, strlits, fltlits, links }
}
