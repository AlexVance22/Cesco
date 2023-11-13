use super::*;
use crate::hir::ast::{ Expression, Statement, Ast };
use std::collections::HashMap;


macro_rules! push_op {
    ($base:ident, $count:ident, $locals:ident, $oper:ident($val:ident)) => { {
        let (ex, op) = create_unary_op($count, $val.val, $locals);
        $base.extend(ex);
        $base.push(Operation::$oper(op.0.clone(), op.1));
        ($base, Operand::Variable(op.0))
    } };
    ($base:ident, $count:ident, $locals:ident, $oper:ident($left:ident, $right:ident)) => { {
        let (ex, op) = create_binary_op($count, $left.val, $right.val, $locals);
        $base.extend(ex);
        $base.push(Operation::$oper(op.0.clone(), op.1, op.2));
        ($base, Operand::Variable(op.0))
    } };
}


type LocalTracker = HashMap<String, Variable>;


fn create_binary_op(count: &mut usize, left: Expression, right: Expression, locals: &LocalTracker) -> (Vec<Operation>, GenericBin) {
    let (mut base, left) = resolve_leaf(left, count, locals);
    let (prev, right) = resolve_leaf(right, count, locals);
    base.extend(prev);
    *count += 1;
    let t = left.type_of();
    let s = t.size_info();
    (base, GenericBin(Dest::Temp(TempAddress::At(Temporary(*count, t))), left, right))
}

fn create_unary_op(count: &mut usize, val: Expression, locals: &LocalTracker) -> (Vec<Operation>, GenericUn) {
    let (prev, val) = resolve_leaf(val, count, locals);
    *count += 1;
    let t = val.type_of();
    let s = t.size_info();
    (prev, GenericUn(Dest::Temp(TempAddress::At(Temporary(*count, t))), val))
}


fn resolve_leaf(expr: Expression, count: &mut usize, locals: &LocalTracker) -> (Vec<Operation>, Operand) {
    match expr {
        Expression::NullPtr => (vec![], Operand::Imm(Immediate::NullPtr)),
        Expression::Null(t) => (vec![], Operand::Imm(Immediate::Null(t))),
        Expression::I8Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I8(i)))),
        Expression::I16Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I16(i)))),
        Expression::I32Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I32(i)))),
        Expression::I64Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I64(i)))),
        Expression::U8Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U8(i)))),
        Expression::U16Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U16(i)))),
        Expression::U32Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U32(i)))),
        Expression::U64Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U64(i)))),
        Expression::F32Lit(f) => (vec![], Operand::Imm(Immediate::Flt(FltType::F32(f)))),
        Expression::F64Lit(f) => (vec![], Operand::Imm(Immediate::Flt(FltType::F64(f)))),
        Expression::BoolLit(b) => (vec![], Operand::Imm(Immediate::Bool(b))),
        Expression::CharLit(c) => (vec![], Operand::Imm(Immediate::Char(c))),
        Expression::StrLit(s) => (vec![], Operand::Imm(Immediate::Str(s))),
        Expression::Identifier(_, id) => (vec![], Operand::Var(VarAddress::At(locals.get(&id).unwrap().clone()))),
        _ => get_leaves(expr, count, locals),
    }
}

fn get_leaves(expr: Expression, count: &mut usize, locals: &LocalTracker) -> (Vec<Operation>, Operand) {
    use Expression as Ex;

    let mut base = Vec::new();

    match expr {
        // literals
        Ex::NullPtr => (vec![], Operand::Imm(Immediate::NullPtr)),
        Ex::Null(t) => (vec![], Operand::Imm(Immediate::Null(t))),
        Ex::I8Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I8(i)))),
        Ex::I16Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I16(i)))),
        Ex::I32Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I32(i)))),
        Ex::I64Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::I64(i)))),
        Ex::U8Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U8(i)))),
        Ex::U16Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U16(i)))),
        Ex::U32Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U32(i)))),
        Ex::U64Lit(i) => (vec![], Operand::Imm(Immediate::Int(IntType::U64(i)))),
        Ex::F32Lit(f) => (vec![], Operand::Imm(Immediate::Flt(FltType::F32(f)))),
        Ex::F64Lit(f) => (vec![], Operand::Imm(Immediate::Flt(FltType::F64(f)))),
        Ex::BoolLit(b) => (vec![], Operand::Imm(Immediate::Bool(b))),
        Ex::CharLit(c) => (vec![], Operand::Imm(Immediate::Char(c))),
        Ex::StrLit(s) => (vec![], Operand::Imm(Immediate::Str(s))),
        Ex::ArrLit(t, exprs) => {
            let mut ops = Vec::new();
            let len = exprs.len();
            for val in exprs {
                let (prev, val) = resolve_leaf(val.val, count, locals);
                base.extend(prev);
                ops.push(val);
            }
            *count += 1;
            base.push(Operation::LoadArr(Dest::Temp(TempAddress::At(Temporary(*count, t.clone()))), ops));
            (base, Operand::Temp(TempAddress::At(Temporary(*count, t))))
        }
        Ex::StructLit(t, name, exprs) => {
            let mut ops = Vec::new();
            for (k, v) in exprs {
                let (prev, val) = resolve_leaf(v.val, count, locals);
                base.extend(prev);
                ops.push((k, val));
            }
            *count += 1;
            base.push(Operation::LoadStruct(Dest::Temp(TempAddress::At(Temporary(*count, t.clone()))), name, ops));
            (base, Operand::Temp(TempAddress::At(Temporary(*count, t))))
        }

        Ex::Identifier(_, id) => (vec![], Operand::Var(VarAddress::At(locals.get(&id).unwrap().clone()))),

        // math
        Ex::Neg(_, val) =>                          push_op!(base, count, locals, Neg(val)),
        Ex::Add(_, left, right) => push_op!(base, count, locals, Add(left, right)),
        Ex::Sub(_, left, right) => push_op!(base, count, locals, Sub(left, right)),
        Ex::Mul(_, left, right) => push_op!(base, count, locals, Mul(left, right)),
        Ex::Div(_, left, right) => push_op!(base, count, locals, Div(left, right)),
        Ex::Mod(_, left, right) => push_op!(base, count, locals, Mod(left, right)),

        // logic
        Ex::Not(val) =>                          push_op!(base, count, locals, Not(val)),
        Ex::Or (left, right) => push_op!(base, count, locals, Or (left, right)),
        Ex::And(left, right) => push_op!(base, count, locals, And(left, right)),

        // comparison
        Ex::Lt(left, right) => push_op!(base, count, locals, Lt(left, right)),
        Ex::Gt(left, right) => push_op!(base, count, locals, Gt(left, right)),
        Ex::Le(left, right) => push_op!(base, count, locals, Le(left, right)),
        Ex::Ge(left, right) => push_op!(base, count, locals, Ge(left, right)),
        Ex::Eq(left, right) => push_op!(base, count, locals, Eq(left, right)),
        Ex::Ne(left, right) => push_op!(base, count, locals, Ne(left, right)),

        // bitwise
        Ex::BitNot(_, val) =>                          push_op!(base, count, locals, BitNot(val)),
        Ex::BitOr (_, left, right) => push_op!(base, count, locals, BitOr (left, right)),
        Ex::BitXor(_, left, right) => push_op!(base, count, locals, BitXor(left, right)),
        Ex::BitAnd(_, left, right) => push_op!(base, count, locals, BitAnd(left, right)),
        Ex::LShift(_, left, right) => push_op!(base, count, locals, LShift(left, right)),
        Ex::RShift(_, left, right) => push_op!(base, count, locals, RShift(left, right)),

        // pointers
        Ex::Deref (_, val) => {
            let (prev, val) = resolve_leaf(val.val, count, locals);
            base.extend(prev);
            match val {
                Operand::Temp(temp) => {
                    if let TempAddress::At(Temporary(v, t)) = temp {
                        (base, Operand::Temp(TempAddress::Deref(Temporary(v, t.into_inner().unwrap()))))
                    } else {
                        unreachable!()
                    }
                }
                Operand::Var(addr) => {
                    if let VarAddress::At(Variable(v, t)) = addr {
                        (base, Operand::Var(VarAddress::Deref(Variable(v, t.into_inner().unwrap()))))
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!()
            }
        }
        Ex::DerefChecked(_, val) => {
            let (prev, val) = resolve_leaf(val.val, count, locals);
            base.extend(prev);
            match val {
                Operand::Temp(temp) => {
                    if let TempAddress::At(Temporary(v, t)) = temp {
                        (base, Operand::Temp(TempAddress::Deref(Temporary(v, t.into_inner().unwrap()))))
                    } else {
                        unreachable!()
                    }
                }
                Operand::Var(addr) => {
                    if let VarAddress::At(Variable(v, t)) = addr {
                        (base, Operand::Var(VarAddress::Deref(Variable(v, t.into_inner().unwrap()))))
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!()
            }
        }
        Ex::AddrOf(_, val) => {
            let (ex, GenericUn(dest, op)) = create_unary_op(count, val.val, locals);
            base.extend(ex);
            let t = Type::RefTo(Box::new(dest.type_of()));
            if let Dest::Temp(temp) = dest {
                base.push(Operation::AddrOf(Dest::Temp(TempAddress::At(Temporary(temp.base().name_of(), t))), op));
                (base, Operand::Temp(TempAddress::At(Temporary(temp.base().name_of(), t))))
            } else {
                unreachable!()
            }
        }

        // fanciness
        Ex::Assign{ ty: _, left, right } => {
            let (ex, GenericBin(_, left, right)) = create_binary_op(count, left.val, right.val, locals);
            base.extend(ex);
            if let Operand::Variable(v) = left {
                base.push(Operation::Assign(v.clone(), right));
                (base, Operand::Variable(v))
            } else {
                unreachable!()
            }
        }
        Ex::Call  { ty, name, args } => {
            let mut ops = Vec::new();
            for a in args {
                let (prev, val) = resolve_leaf(a.val, count, locals);
                base.extend(prev);
                ops.push(val);
            }
            *count += 1;
            let s = ty.size_info();
            base.push(Operation::Call(Variable::Value(*count, ty.clone(), s), name, ops));
            (base, Operand::Variable(Variable::Value(*count, ty, s)))

        }
        Ex::Member{ ty: _, obj: _, member: _ } => {
            todo!()
        }
        Ex::ArrGet{ ty: _, obj, index } => {
            let (prev1, obj) = resolve_leaf(obj.val, count, locals);
            let (prev2, index) = resolve_leaf(index.val, count, locals);
            base.extend(prev1);
            base.extend(prev2);

            let inner = if let Type::ArrOf(t, _) = obj.type_of() {
                *t
            } else {
                unreachable!()
            };

            match obj {
                Operand::Var(VarAddress::At(v, ..)) => match index {
                    Operand::Imm(Immediate::Int(i)) => {
                        (base, Operand::Var(VarAddress::AtOffsetImm(v, Immediate::Int(i))))
                    }
                    Operand::Var(VarAddress::At(n, Type::I8|Type::I16|Type::I32|Type::I64|Type::U8|Type::U16|Type::U32|Type::U64, _)) => {
                        (base, Operand::Var(VarAddress::AtOffsetVar(v, Offset::Reg(n), inner.clone(), inner.size_info())))
                    }
                    _ => unreachable!()
                }
                _ => unreachable!()
            }
        }
    }
}


pub fn lower_expr(expr: Expression, count: &mut usize, locals: &LocalTracker) -> (Vec<Operation>, Operand) {
    get_leaves(expr, count, locals)
}


fn generate_block(block: Vec<Statement>, count: &mut usize, mut locals: LocalTracker, allvars: &mut HashSet<Variable>, conds: &mut usize, end: usize) -> Vec<Operation> {
    let mut result = Vec::new();

    for s in block {
        match s {
            Statement::VarDecl{ decl, val } => {
                let (prev, val) = lower_expr(val.val.clone(), count, &locals);
                result.extend(prev);
                *count += 1;
                let s = decl.datatype.val.size_info();
                result.push(Operation::Assign(Variable::Value(*count, decl.datatype.val.clone(), s), val));
                locals.insert(decl.name.val.clone(), Variable::Value(*count, decl.datatype.val.clone(), s));
                allvars.insert(Variable::Value(*count, decl.datatype.val, s));
            }
            Statement::Return(Some(expr)) => {
                let (prev, val) = lower_expr(expr.val, count, &locals);
                result.extend(prev);
                result.push(Operation::Assign(Dest::Temp(TempAddress::At(Temporary(0, val.type_of()))), val));
                result.push(Operation::Return);
                return result;
            }
            Statement::Break => {
                result.push(Operation::Jump(end));
                return result;
            }

            Statement::If { cond, body } => {
                let label = *conds;
                *conds += 1;
                let (prev, val) = lower_expr(cond.val.clone(), count, &locals);
                result.extend(prev);
                result.push(Operation::Jmp0(label, val));
                result.extend(generate_block(body, count, locals.clone(), allvars, conds, label));
                result.push(Operation::Label(label));
            }
            Statement::While { cond, body } => {
                let label = *conds;
                *conds += 2;
                result.push(Operation::Label(label));
                let (prev, val) = lower_expr(cond.val.clone(), count, &locals);
                result.extend(prev);
                result.push(Operation::Jmp0(label + 1, val));
                result.extend(generate_block(body, count, locals.clone(), allvars, conds, label + 1));
                result.push(Operation::Jump(label));
                result.push(Operation::Label(label + 1));
            }

            Statement::Expression(expr) => {
                let (prev, _) = lower_expr(expr.val.clone(), count, &locals);
                result.extend(prev);
            }

            _ => ()
        }
    }

    result
}


fn get_frame_size(locals: &HashSet<Variable>) -> usize {
    let mut size = 0;

    for var in locals {
        let align = var.type_of().align_of();

        size = if size % align == 0 {
            size
        } else {
            let nsize = size + align;
            nsize - (nsize % align)
        };

        size += var.type_of().size_of();
    }

    if size % 16 == 0 {
        size
    } else {
        let nsize = size + 16;
        nsize - (nsize % 16)
    }
}

pub fn destructure(ast: Ast) -> Vec<Procedure> {
    let mut result = Vec::new();

    for (name, func) in &ast.funcs {
        let mut locals = LocalTracker::new();
        let mut vars = HashSet::new();
        let mut count = 0;
        let mut conds = 0;

        let mut ps = Vec::new();

        for p in &func.params {
            count += 1;
            locals.insert(p.name.val.clone(), Variable(p.name.val.clone(), p.datatype.val.clone()));
            ps.push(Variable(p.name.val, p.datatype.val.clone()));
        }

        let mut ops = generate_block(func.body.clone(), &mut count, locals, &mut vars, &mut conds, 0);
        ops.insert(0, Operation::FuncBegin(name.clone(), ps));

        result.push(Procedure { ops, stack: get_frame_size(&vars), vars });
    }

    result
}
