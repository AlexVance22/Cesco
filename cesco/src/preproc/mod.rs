use crate::parse::ast::{self, Association };
use crate::hir::ast as hir;
use crate::span::{ Span, Item };
use std::collections::HashMap;



fn resolve_default(ty: &ast::Type, types: &HashMap<String, ast::Struct>, span: &Span) -> Result<hir::Expression, ()> {
    use ast::Type::*;

    match ty {
        Null => Ok(hir::Expression::NullPtr),
        I8 => Ok(hir::Expression::I8Lit(0)),
        I16 => Ok(hir::Expression::I16Lit(0)),
        I32 => Ok(hir::Expression::I32Lit(0)),
        I64 => Ok(hir::Expression::I64Lit(0)),
        U8 => Ok(hir::Expression::U8Lit(0)),
        U16 => Ok(hir::Expression::U16Lit(0)),
        U32 => Ok(hir::Expression::U32Lit(0)),
        U64 => Ok(hir::Expression::U64Lit(0)),
        F32 => Ok(hir::Expression::F32Lit(0.0)),
        F64 => Ok(hir::Expression::F64Lit(0.0)),
        Bool => Ok(hir::Expression::BoolLit(false)),
        Char => Ok(hir::Expression::CharLit('\0')),
        Str => Ok(hir::Expression::StrLit(String::new())),
        RefTo(_) => Err(()),
        PtrTo(_) => Ok(hir::Expression::NullPtr),
        OptOf(t) => Ok(hir::Expression::Null(process_type(*t.clone(), types))),
        ArrOf(t, i) => {
            let newt = process_type(*t.clone(), types);
            let items = (0..*i).into_iter().map(|_| resolve_default(t, types, span).unwrap()).map(|v| Item::new(v, span.clone())).collect();
            Ok(hir::Expression::ArrLit(newt, items))
        }
        VecOf(t, i) => {
            let newt = process_type(*t.clone(), types);
            let items = (0..*i).into_iter().map(|_| resolve_default(t, types, span).unwrap()).map(|v| Item::new(v, span.clone())).collect();
            Ok(hir::Expression::ArrLit(newt, items))
        }
        User(s) => {
            if let Some(t) = types.get(s) {
                let mut fields = Vec::new();

                for field in &t.fields {
                    fields.push((field.name.val.clone(), Item::new(resolve_default(&field.datatype.val, types, span)?, span.clone())));
                }

                Ok(hir::Expression::StructLit(hir::Type::User(s.clone(), crate::mir::SizeInfo { size: t.size_of(types), align: t.align_of(types) }), s.clone(), fields))
            } else {
                unreachable!()
            }
        }
    }
}

fn resolve_null(ty: &ast::Type, types: &HashMap<String, ast::Struct>) -> Result<hir::Expression, ()> {
    match ty {
        ast::Type::PtrTo(_) => Ok(hir::Expression::NullPtr),
        ast::Type::OptOf(t) => Ok(hir::Expression::Null(process_type(*t.clone(), types))),
        _ => Err(())
    }
}


fn choose_type(left: &hir::Expression, right: &hir::Expression) -> hir::Type {
    if left.is_lit() && right.is_lvalue() {
        right.type_of()
    } else {
        left.type_of()
    }
}

fn process_expr(expr: Item<ast::Expression>, ast: &ast::Ast, locals: &HashMap<String, (hir::Type, bool)>, hint: Option<&ast::Type>) -> Item<hir::Expression> {
    match expr.val {
        ast::Expression::Default => Item::new(resolve_default(hint.unwrap(), &ast.structs, &expr.span).unwrap(), expr.span),
        ast::Expression::Null => Item::new(resolve_null(hint.unwrap(), &ast.structs).unwrap(), expr.span),

        ast::Expression::Identifier(id) => Item::new(hir::Expression::Identifier(locals.get(&id).unwrap().0.clone(), id), expr.span),

        ast::Expression::IntLit(i) => if let Some(hint) = hint {
            match hint {
                ast::Type::I8 =>  Item::new(hir::Expression::I8Lit(i as i8), expr.span),
                ast::Type::I16 => Item::new(hir::Expression::I16Lit(i as i16), expr.span),
                ast::Type::I32 => Item::new(hir::Expression::I32Lit(i as i32), expr.span),
                ast::Type::I64 => Item::new(hir::Expression::I64Lit(i), expr.span),
                ast::Type::U8 =>  Item::new(hir::Expression::U8Lit(i as u8), expr.span),
                ast::Type::U16 => Item::new(hir::Expression::U16Lit(i as u16), expr.span),
                ast::Type::U32 => Item::new(hir::Expression::U32Lit(i as u32), expr.span),
                ast::Type::U64 => Item::new(hir::Expression::U64Lit(i as u64), expr.span),
                _ => unreachable!()
            }    
        } else {
            Item::new(hir::Expression::I32Lit(i as i32), expr.span)
        }
        ast::Expression::FltLit(f) => if let Some(hint) = hint {
            match hint {
                ast::Type::F32 => Item::new(hir::Expression::F32Lit(f as f32), expr.span),
                ast::Type::F64 => Item::new(hir::Expression::F64Lit(f), expr.span),
                _ => unreachable!()
            }    
        } else {
            Item::new(hir::Expression::F32Lit(f as f32), expr.span)
        }
        ast::Expression::BoolLit(b) =>  Item::new(hir::Expression::BoolLit(b), expr.span),
        ast::Expression::CharLit(c) =>  Item::new(hir::Expression::CharLit(c), expr.span),
        ast::Expression::StrLit(s) => Item::new(hir::Expression::StrLit(s), expr.span),
        ast::Expression::ArrLit(vals) => {
            let vals: Vec<Item<hir::Expression>> = if let Some(ast::Type::ArrOf(t, _)) = hint {
                vals.into_iter().map(|e| process_expr(e, ast, locals, Some(t))).collect()
            } else {
                vals.into_iter().map(|e| process_expr(e, ast, locals, None)).collect()
            };

            if let Some(f) = vals.first() {
                Item::new(hir::Expression::ArrLit(hir::Type::ArrOf(Box::new(f.val.type_of()), vals.len()), vals), expr.span)
            } else {
                Item::new(hir::Expression::ArrLit(process_type(hint.unwrap().clone(), &ast.structs), vals), expr.span)
            }
        }
        ast::Expression::StructLit(name, vals) => {
            let vals = vals.into_iter().map(|(s, e)| (s, process_expr(e, ast, locals, hint))).collect();
            if let Some(hint) = hint {
                Item::new(hir::Expression::StructLit(process_type(hint.clone(), &ast.structs), name, vals), expr.span)
            } else {
                let s = ast.structs.get(&name).unwrap();
                let sizeinfo = crate::mir::SizeInfo{ size: s.size_of(&ast.structs), align: s.align_of(&ast.structs) };
                Item::new(hir::Expression::StructLit(hir::Type::User(name.clone(), sizeinfo), name, vals), expr.span)
            }
        }

        ast::Expression::Neg(val) => {
            let val = process_expr(*val, ast, locals, hint);
            Item::new(hir::Expression::Neg(val.val.type_of(), Box::new(val)), expr.span)
        }
        ast::Expression::Add(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Add(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Sub(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Sub(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Mul(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Mul(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Div(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Div(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Mod(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Mod(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }

        ast::Expression::Not(val) => {
            let val = process_expr(*val, ast, locals, hint);
            Item::new(hir::Expression::Not(Box::new(val)), expr.span)
        }
        ast::Expression::Or (left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Or(Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::And(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::And(Box::new(left), Box::new(right)), expr.span)
        }

        ast::Expression::Lt(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Lt(Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Gt(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Gt(Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Le(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Le(Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Ge(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Ge(Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Eq(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Eq(Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::Ne(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Ne(Box::new(left), Box::new(right)), expr.span)
        }

        ast::Expression::BitNot(val) => {
            let val = process_expr(*val, ast, locals, hint);
            Item::new(hir::Expression::BitNot(val.val.type_of(), Box::new(val)), expr.span)
        }
        ast::Expression::BitOr (left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::BitOr (choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::BitXor(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::BitXor(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::BitAnd(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::BitAnd(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::LShift(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::LShift(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }
        ast::Expression::RShift(left, right) => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::RShift(choose_type(&left.val, &right.val), Box::new(left), Box::new(right)), expr.span)
        }

        ast::Expression::AddrOf(val) => {
            let val = process_expr(*val, ast, locals, hint);
            Item::new(hir::Expression::AddrOf(hir::Type::RefTo(Box::new(val.val.type_of())), Box::new(val)), expr.span)
        }
        ast::Expression::Deref (val) => {
            let val = process_expr(*val, ast, locals, hint);
            if let hir::Type::RefTo(t)|hir::Type::PtrTo(t) = val.val.type_of() {
                Item::new(hir::Expression::Deref(*t, Box::new(val)), expr.span)
            } else {
                unreachable!()
            }
        }

        ast::Expression::Assign{ left, right } => {
            let left = process_expr(*left, ast, locals, hint);
            let right = process_expr(*right, ast, locals, hint);
            Item::new(hir::Expression::Assign{ ty: left.val.type_of(), left: Box::new(left), right: Box::new(right) }, expr.span)
        }
        ast::Expression::Call  { name, args, assoc } => {
            let args: Vec<Item<hir::Expression>> = args.into_iter().map(|a| process_expr(a, ast, locals, None)).collect();

            match assoc {
                ast::Association::Method(_) => {
                    let this = args.first().unwrap().val.type_of();
                    let meth = if let hir::Type::PtrTo(t)|hir::Type::RefTo(t) = this {
                        if let hir::Type::User(meth, _) = *t {
                            meth
                        } else { unreachable!() }
                    } else { unreachable!() };

                    for ((othername, _), otherfunc) in &ast.funcs {
                        if name == *othername {
                            if let ast::Association::Method(t) = &otherfunc.assoc.val {
                                if meth == *t {
                                    return Item::new(hir::Expression::Call{ ty: process_type(otherfunc.rettype.val.clone(), &ast.structs), name: otherfunc.mangle.clone(), args }, expr.span)
                                }
                            }
                        }
                    }

                    unreachable!()
                }
                ast::Association::Static(stat) => {
                    for ((othername, _), otherfunc) in &ast.funcs {
                        if name == *othername {
                            if let ast::Association::Static(t) = &otherfunc.assoc.val {
                                if stat == *t {
                                    return Item::new(hir::Expression::Call{ ty: process_type(otherfunc.rettype.val.clone(), &ast.structs), name: otherfunc.mangle.clone(), args }, expr.span)
                                }
                            }
                        }
                    }

                    unreachable!()
                }
                ast::Association::None => {
                    for ((othername, _), otherfunc) in &ast.funcs {
                        if name == *othername {
                            if let ast::Association::None = &otherfunc.assoc.val {
                                return Item::new(hir::Expression::Call{ ty: process_type(otherfunc.rettype.val.clone(), &ast.structs), name: otherfunc.mangle.clone(), args }, expr.span)
                            }
                        }
                    }

                    unreachable!()
                }
            }
        }
        ast::Expression::Member{ obj, member, deref } => {
            let obj = process_expr(*obj, ast, locals, hint);
            let member = process_expr(*member, ast, locals, hint);
            if let hir::Expression::Identifier(_, name) = &member.val {
                if let hir::Type::User(t, _) = obj.val.type_of() {
                    let ty = process_type(ast.structs.get(&t).unwrap().get(name).unwrap().datatype.val.clone(), &ast.structs);
                    if deref {
                        Item::new(hir::Expression::Member{ ty, obj: Box::new(obj), member: Box::new(member) }, expr.span)
                    } else {
                        let objspan = obj.span.clone();
                        let obj = Item::new(hir::Expression::Deref(obj.val.type_of(), Box::new(obj)), objspan);
                        Item::new(hir::Expression::Member{ ty, obj: Box::new(obj), member: Box::new(member) }, expr.span)
                    }
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        ast::Expression::ArrGet{ obj, index } => {
            let obj = process_expr(*obj, ast, locals, hint);
            let index = process_expr(*index, ast, locals, hint);
            if let hir::Type::ArrOf(t, _) = obj.val.type_of() {
                Item::new(hir::Expression::ArrGet{ ty: *t, obj: Box::new(obj), index: Box::new(index) }, expr.span)
            } else {
                unreachable!()
            }
        }

        _ => unimplemented!()
    }
}


fn process_type(ty: ast::Type, types: &HashMap<String, ast::Struct>) -> hir::Type {
    match ty {
        ast::Type::Null => hir::Type::Null,
        ast::Type::I8 => hir::Type::I8,
        ast::Type::I16 => hir::Type::I16,
        ast::Type::I32 => hir::Type::I32,
        ast::Type::I64 => hir::Type::I64,
        ast::Type::U8 => hir::Type::U8,
        ast::Type::U16 => hir::Type::U16,
        ast::Type::U32 => hir::Type::U32,
        ast::Type::U64 => hir::Type::U64,
        ast::Type::F32 => hir::Type::F32,
        ast::Type::F64 => hir::Type::F64,
        ast::Type::Bool => hir::Type::Bool,
        ast::Type::Char => hir::Type::Char,
        ast::Type::Str => hir::Type::Str,
        ast::Type::User(s) => {
            if let Some(t) = types.get(&s) {
                hir::Type::User(s, crate::mir::SizeInfo{ size: t.size_of(types), align: t.align_of(types) })
            } else {
                unreachable!()
            }
        }
        ast::Type::PtrTo(t) => hir::Type::PtrTo(Box::new(process_type(*t, types))),
        ast::Type::RefTo(t) => hir::Type::RefTo(Box::new(process_type(*t, types))),
        ast::Type::OptOf(t) => hir::Type::OptOf(Box::new(process_type(*t, types))),
        ast::Type::ArrOf(t, s) => hir::Type::ArrOf(Box::new(process_type(*t, types)), s),
        ast::Type::VecOf(t, s) => hir::Type::VecOf(Box::new(process_type(*t, types)), s),
    }
}

fn process_variable(var: ast::Variable, types: &HashMap<String, ast::Struct>) -> hir::Variable {
    hir::Variable{
        name: var.name,
        datatype: Item{ val: process_type(var.datatype.val, types), span: var.datatype.span },
        mutable: var.mutable,
    }
}

fn process_field(field: ast::Field, types: &HashMap<String, ast::Struct>) -> (hir::Field, usize) {
    let size = field.datatype.val.size_of(types);

    (hir::Field{
        access: Item{ val: match field.access.val {
            ast::AccessMod::Private => hir::AccessMod::Private,
            ast::AccessMod::Public => hir::AccessMod::Public,
        }, span: field.access.span },
        name: field.name,
        datatype: Item{ val: process_type(field.datatype.val, types), span: field.datatype.span },

    }, size)
}


fn process_statement(statement: ast::Statement, ast: &ast::Ast, locals: &mut HashMap<String, (hir::Type, bool)>, rettype: &ast::Type) -> hir::Statement {
    match statement {
        ast::Statement::VarDecl{ decl, val } => {
            let val = process_expr(val, ast, locals, Some(&decl.datatype.val));
            let decl = process_variable(decl, &ast.structs);
            locals.insert(decl.name.val.clone(), (decl.datatype.val.clone(), decl.mutable.val));
            hir::Statement::VarDecl{ decl, val }
        }

        ast::Statement::Return(Some(val)) => {
            hir::Statement::Return(Some(process_expr(val, ast, locals, Some(rettype))))
        }
        ast::Statement::Return(None) => {
            hir::Statement::Return(None)
        }
        ast::Statement::Yield(_) => {
            hir::Statement::Empty
            //hir::Statement::Yield(process_expr(val, ast, locals))
        }
        ast::Statement::Break => {
            hir::Statement::Break
        }

        ast::Statement::While{ cond, body } => {
            let mut newbody = Vec::new();
            let mut ls = locals.clone();
            for stmt in body {
                newbody.push(process_statement(stmt, ast, &mut ls, rettype));
            }
            hir::Statement::While{ cond: process_expr(cond, ast, locals, None), body: newbody }
        }
        ast::Statement::If{ cond, body } => {
            let mut newbody = Vec::new();
            let mut ls = locals.clone();
            for stmt in body {
                newbody.push(process_statement(stmt, ast, &mut ls, rettype));
            }
            hir::Statement::If{ cond: process_expr(cond, ast, locals, None), body: newbody }
        }
        ast::Statement::For{ varname, iterable, body } => {
            let mut newbody = Vec::new();
            let mut ls = locals.clone();
            for stmt in body {
                newbody.push(process_statement(stmt, ast, &mut ls, rettype));
            }
            hir::Statement::For{ varname, iterable: process_expr(iterable, ast, locals, None), body: newbody }
        }

        ast::Statement::Expression(expr) => {
            hir::Statement::Expression(process_expr(expr, ast, locals, None))
        }

        ast::Statement::Empty => hir::Statement::Empty,
    }
}


fn process_func(func: ast::Func, ast: &ast::Ast) -> hir::Func {
    let mut res = hir::Func::new();

    let mut locals = HashMap::new();

    res.name = Item{ val: func.mangle, span: func.name.span };

    for p in func.params {
        locals.insert(p.name.val.clone(), (process_type(p.datatype.val.clone(), &ast.structs), p.mutable.val));
        res.params.push(process_variable(p, &ast.structs));
    }
    
    for statement in func.body {
        res.body.push(process_statement(statement, ast, &mut locals, &func.rettype.val))
    }

    res.rettype = Item{ val: process_type(func.rettype.val, &ast.structs), span: func.rettype.span };

    res
}


fn process_struct(strct: ast::Struct, ast: &ast::Ast) -> hir::Struct {
    let mut res = hir::Struct::new();

    res.size = strct.size_of(&ast.structs);

    for f in strct.fields {
        res.fields.push(process_field(f, &ast.structs));
    }

    res
}


pub fn process(ast: ast::Ast) -> hir::Ast {
    let mut res = hir::Ast::new();

    let funcs: Vec<ast::Func> = ast.funcs.clone().into_values().collect();
    let structs: Vec<ast::Struct> = ast.structs.clone().into_values().collect();

    for f in funcs {
        if f.name.val == "main" && f.assoc.val == Association::None {
            res.funcs.insert(f.name.val.clone(), process_func(f, &ast));
        } else {
            res.funcs.insert(f.mangle.clone(), process_func(f, &ast));
        }
    }

    for s in structs {
        res.structs.insert(s.name.val.clone(), process_struct(s, &ast));
    }

    res
}
