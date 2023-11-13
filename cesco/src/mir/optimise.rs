use std::collections::HashMap;
use super::*;


fn swap_con(op: Operand, cons: &HashMap<usize, Operand>) -> Operand {
    if let Operand::Variable(Variable::Value(n, _, _)) = op {
        if let Some(n) = cons.get(&n) {
            n.clone()
        } else {
            op
        }
    } else {
        op
    }
}

fn log2(n: isize) -> Option<u32> {
    if n & (n - 1) == 0 {
        Some(n.ilog2())
    } else {
        None
    }
}


fn con_fold(op: &Operation, cons: &HashMap<usize, Operand>) -> Operation {
    match op.clone() {
        Operation::Neg(var, val) => {
            let val = swap_con(val, cons);
            
            match val {
                Operand::Imm(Immediate::Int(a)) => Operation::Assign(var, Operand::Imm(Immediate::Int(-a))),
                Operand::Imm(Immediate::Flt(a)) => Operation::Assign(var, Operand::Imm(Immediate::Flt(-a))),
                _ => Operation::Neg(var, val)
            }
        }
        Operation::Add(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (&left, &right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(*a + *b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Flt(*a + *b))),
                (Operand::Var(_), Operand::Imm(Immediate::Int(i))) if i.is(0) => Operation::Assign(var, left),
                (Operand::Imm(Immediate::Int(i)), Operand::Var(_)) if i.is(0) => Operation::Assign(var, left),
                _ => Operation::Add(var, left, right)
            }
        }
        Operation::Sub(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (&left, &right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(*a - *b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Flt(*a - *b))),
                (Operand::Var(_), Operand::Imm(Immediate::Int(i))) if i.is(0) => Operation::Assign(var, left),
                (Operand::Imm(Immediate::Int(i)), Operand::Var(_)) if i.is(0) => Operation::Neg(var, left),
                (Operand::Var(VarAddress::Value(n1, _, s1)|VarAddress::Deref(n1, _, s1)), 
                 Operand::Var(VarAddress::Value(n2, ..)|VarAddress::Deref(n2, ..))) => {
                    if n1 == n2 {
                        Operation::Assign(var, Operand::ImmInt(IntType::signed(0, s1.size)))
                    } else {
                        Operation::Sub(var, left, right)
                    }
                }
                _ => Operation::Sub(var, left, right)
            }
        }
        Operation::Mul(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (&left, &right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(*a * *b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Flt(*a * *b))),
                (Operand::Var(_), Operand::ImmInt(i)) if i.is(0) => Operation::Assign(var, Operand::ImmInt(IntType::signed(0, i.size_of()))),
                (Operand::ImmInt(i), Operand::Var(_)) if i.is(0) => Operation::Assign(var, Operand::ImmInt(IntType::signed(0, i.size_of()))),
                (Operand::Var(_), Operand::ImmInt(i)) if i.is(1) => Operation::Assign(var, left),
                (Operand::ImmInt(i), Operand::Var(_)) if i.is(1) => Operation::Assign(var, right),
                (Operand::Var(_), Operand::ImmInt(i)) if i.is(-1) => Operation::Neg(var, left),
                (Operand::ImmInt(i), Operand::Var(_)) if i.is(-1) => Operation::Neg(var, right),
                (Operand::Var(_), Operand::ImmInt(n)) => {
                    let num = n.as_isize();
                    if num > 0 {
                        if num == 2 {
                            return Operation::Add(var, left.clone(), left)
                        } else if let Some(log) = log2(num) {
                            return Operation::LShift(var, left, Operand::ImmInt(IntType::signed(log as i64, n.size_of())))
                        }
                    }
                    Operation::Mul(var, left, right)
                }
                (Operand::ImmInt(n), Operand::Var(_)) => {
                    let num = n.as_isize();
                    if num > 0 {
                        if num == 2 {
                            return Operation::Add(var, right.clone(), right)
                        } else if let Some(log) = log2(num) {
                            return Operation::LShift(var, right, Operand::ImmInt(IntType::signed(log as i64, n.size_of())))
                        }
                    }
                    Operation::Mul(var, left, right)
                }
                _ => Operation::Mul(var, left, right)
            }
        }
        Operation::Div(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);
            
            match (&left, &right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(*a / *b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Flt(*a / *b))),
                (Operand::ImmInt(i), Operand::Variable(_)) if i.is(0) => Operation::Assign(var, Operand::ImmInt(IntType::signed(0, i.size_of()))),
                (Operand::Variable(_), Operand::ImmInt(i)) if i.is(1) => Operation::Assign(var, left),
                (Operand::Variable(_), Operand::ImmInt(i)) if i.is(-1) => Operation::Neg(var, left),
                (Operand::Variable(_), Operand::ImmInt(n)) => {
                    if n.as_i64() > 0 {
                        if let Some(log) = log2(n.as_isize()) {
                            return Operation::RShift(var, left, Operand::ImmInt(IntType::signed(log as i64, 8)))
                        } else {
                            return Operation::Mul(var, left, Operand::ImmFlt(FltType::new(1.0 / (n.as_i64() as f64), 8)))
                        }
                    }
                    Operation::Div(var, left, right)
                }
                (Operand::Variable(Variable::Value(n1, _, s1)|Variable::Deref(n1, _, s1)),
                 Operand::Variable(Variable::Value(n2, ..)|Variable::Deref(n2, ..))) => {
                    if n1 == n2 {
                        Operation::Assign(var, Operand::ImmInt(IntType::signed(1, s1.size)))
                    } else {
                        Operation::Div(var, left, right)
                    }
                }
                _ => Operation::Div(var, left, right)
            }
        }
        Operation::Mod(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (&left, &right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(*a % *b))),
                _ => Operation::Sub(var, left, right)
            }
        }
    
        Operation::Not(var, val) => {
            let val = swap_con(val, cons);
            
            match val {
                Operand::Imm(Immediate::Bool(a)) => Operation::Assign(var, Operand::Imm(Immediate::Bool(!a))),
                _ => unreachable!()
            }
        }
        Operation::Or (var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::Imm(Immediate::Bool(a)), Operand::Imm(Immediate::Bool(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a || b))),
                (Operand::Var(_), Operand::Imm(Immediate::Bool(true))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(true))),
                (Operand::Imm(Immediate::Bool(true)), Operand::Var(_)) => Operation::Assign(var, Operand::Imm(Immediate::Bool(true))),
                _ => op.clone()
            }
        }
        Operation::And(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::Imm(Immediate::Bool(a)), Operand::Imm(Immediate::Bool(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a && b))),
                (Operand::Var(_), Operand::Imm(Immediate::Bool(true))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(true))),
                (Operand::Imm(Immediate::Bool(true)), Operand::Var(_)) => Operation::Assign(var, Operand::Imm(Immediate::Bool(true))),
                _ => op.clone()
            }
        }
    
        Operation::Lt(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a < b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a < b))),
                _ => op.clone()
            }
        }
        Operation::Gt(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a > b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a > b))),
                _ => op.clone()
            }
        }
        Operation::Le(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a <= b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a <= b))),
                _ => op.clone()
            }
        }
        Operation::Ge(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a >= b))),
                (Operand::Imm(Immediate::Flt(a)), Operand::Imm(Immediate::Flt(b))) => Operation::Assign(var, Operand::Imm(Immediate::Bool(a >= b))),
                _ => op.clone()
            }
        }
        Operation::Eq(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::ImmInt(a),    Operand::ImmInt(b)) =>      Operation::Assign(var, Operand::ImmBool(a == b)),
                (Operand::ImmFlt(a),    Operand::ImmFlt(b)) =>      Operation::Assign(var, Operand::ImmBool(a == b)),
                (Operand::ImmBool(a),  Operand::ImmBool(b)) =>    Operation::Assign(var, Operand::ImmBool(a == b)),
                (Operand::ImmChar(a),  Operand::ImmChar(b)) =>    Operation::Assign(var, Operand::ImmBool(a == b)),
                (Operand::ImmStr(a), Operand::ImmStr(b)) =>   Operation::Assign(var, Operand::ImmBool(a == b)),
                _ => op.clone()
            }
        }
        Operation::Ne(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::ImmInt(a),    Operand::ImmInt(b)) =>      Operation::Assign(var, Operand::ImmBool(a != b)),
                (Operand::ImmFlt(a),    Operand::ImmFlt(b)) =>      Operation::Assign(var, Operand::ImmBool(a != b)),
                (Operand::ImmBool(a),  Operand::ImmBool(b)) =>    Operation::Assign(var, Operand::ImmBool(a != b)),
                (Operand::ImmChar(a),  Operand::ImmChar(b)) =>    Operation::Assign(var, Operand::ImmBool(a != b)),
                (Operand::ImmStr(a), Operand::ImmStr(b)) =>   Operation::Assign(var, Operand::ImmBool(a != b)),
                _ => op.clone()
            }
        }

        Operation::BitNot(var, val) => {
            let val = swap_con(val, cons);

            match val {
                Operand::ImmInt(a) => Operation::Assign(var, Operand::ImmInt(!a)),
                _ => op.clone()
            }
        }
        Operation::BitOr (var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);

            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(a | b))),
                _ => op.clone()
            }
        }
        Operation::BitXor(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);
            
            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(a ^ b))),
                _ => op.clone()
            }
        }
        Operation::BitAnd(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);
            
            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(a & b))),
                _ => op.clone()
            }
        }
        Operation::LShift(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);
            
            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(a << b))),
                _ => op.clone()
            }
        }
        Operation::RShift(var, left, right) => {
            let left = swap_con(left, cons);
            let right = swap_con(right, cons);
            
            match (left, right) {
                (Operand::Imm(Immediate::Int(a)), Operand::Imm(Immediate::Int(b))) => Operation::Assign(var, Operand::Imm(Immediate::Int(a >> b))),
                _ => op.clone()
            }
        }

        Operation::LoadArr   (var, ops) => {
            let ops: Vec<Operand> = ops.into_iter().map(|op| swap_con(op, cons)).collect();

            Operation::LoadArr(var, ops)
        }
        Operation::LoadStruct(var, name, ops) => {
            let ops: Vec<(String, Operand)> = ops.into_iter().map(|(k, v)| (k, swap_con(v, cons))).collect();

            Operation::LoadStruct(var, name, ops)
        }
        Operation::Call      (var, name, ops) => {
            let ops: Vec<Operand> = ops.into_iter().map(|op| swap_con(op, cons)).collect();

            Operation::Call(var, name, ops)
        }

        Operation::Assign(var, op) => Operation::Assign(var, swap_con(op, cons)),

        _ => op.clone()
    }
}

fn pack(proc: &mut Procedure) {
    let mut index = 1;
    
    for op in proc.ops.clone() {
        if let Operation::FuncBegin(_, ops) = op {
            index = ops.len() + 1;
        } else if let Some(Variable::Value(n, _, _)) = op.var() {
            if *n > index {
                let n = *n;
                proc.ops.iter_mut().for_each(|op| {
                    if let Some(v) = op.var_mut() {
                        if v.name_of() == n {
                            *v.name_of_mut() = index;
                        }
                    }

                    for o in op.ops_mut() {
                        if let Operand::Variable(v) = o {
                            match v {
                                Variable::Value(v, ..) => if *v == n { *v = index },
                                Variable::Deref(v, ..) => if *v == n { *v = index },
                                Variable::ValOffset(v, o, ..) => {
                                    if *v == n { 
                                        *v = index;
                                    }
                                    if let Offset::Reg(r) = o {
                                        if *r == n {
                                            *r = index;
                                        }
                                    }
                                }
                                Variable::DerOffset(v, o, ..) => {
                                    if *v == n {
                                        *v = index;
                                    }
                                    if let Offset::Reg(r) = o {
                                        if *r == n {
                                            *r = index;
                                        }
                                    }
                                }
                            }
                        }
                    }
                });
            }
            index += 1;
        }
    }
}


fn op_pass(mut proc: Procedure) -> Procedure {
    let mut cons = HashMap::new();

    for op in &proc.ops {
        if let Operation::Assign(Dest::Addr(VarAddress::At(n, _, _), val @ Operand::Imm(_))) = op {
            cons.insert(*n, val.clone());
        }
    }

    proc.ops = proc.ops.into_iter().map(|op| con_fold(&op, &cons)).collect();

    let mut indices = Vec::new();
    let mut conjumps = Vec::new();

    for (i, op) in proc.ops.iter().enumerate() {
        if let Operation::Assign(v, _) = op {
            if cons.contains_key(&v.name_of()) && !proc.vars.contains(&v.as_val()) && v.name_of() != 0 {
                println!("{}", v);
                indices.push(i)
            }
        } else if let Operation::Jmp0(_, op) = op {
            if !matches!(op, Operand::Variable(..)) {
                conjumps.push(i);
            }
        }
    }

    for i in indices.into_iter().rev() {
        proc.ops.remove(i);
    }
    for i in conjumps.into_iter().rev() {
        proc.ops.remove(i);
    }

    proc
}

fn op_proc(mut proc: Procedure) -> Procedure {
    let mut proc_2 = op_pass(proc.clone());

    while proc != proc_2 {
        proc = proc_2;
        proc_2 = op_pass(proc.clone());
    }

    pack(&mut proc);

    proc
}


pub fn optimise(proc: Program) -> Program {
    proc.into_iter().map(op_proc).collect()
}
