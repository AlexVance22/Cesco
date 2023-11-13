mod error;

use std::collections::HashMap;
use crate::hir::ast::*;
pub use error::Error;


fn check_block(block: &Vec<Statement>, funcret: &Type) -> Result<(), Vec<Error>> {
    let mut errors = Vec::new();

    for s in block {
        match s {
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    let t = expr.val.type_of();
                    if t != *funcret {
                        errors.push(Error::BadReturn(expr.span.clone(), funcret.clone(), t))
                    }
                }
            }
            
            Statement::VarDecl{ decl: Variable{ datatype, .. }, val } => {
                if datatype.val != val.val.type_of() && !(datatype.val.is_ptr() && val.val == Expression::NullPtr) {
                    errors.push(Error::BadAssigment(val.span.clone(), datatype.val.clone(), val.val.type_of()));
                }
            }
            Statement::Expression(expr) => {
                if let Expression::Assign{ left, right, ty: _ } = &expr.val {
                    let right_t = right.val.type_of();
                    let left_t =  left.val.type_of();
                    if left_t == right_t {
                        //if !ml {
                        //    errors.push(Error::BadMutation(0, tl));
                        //}
                    } else {
                        errors.push(Error::BadAssigment(right.span.clone(), left_t, right_t));
                    }
                }
            }
            
            Statement::If{ cond, body } => {
                let cond_t = cond.val.type_of();
                if cond_t.is_boolean() || cond_t.is_integral() {
                    if let Err(e) = check_block(body, funcret) {
                        errors.extend(e);
                    }
                } else {
                    errors.push(Error::BadIfCond(cond.span.clone()));
                }
            }
            Statement::While{ cond, body } => {
                let cond_t = cond.val.type_of();
                if cond_t.is_boolean() || cond_t.is_integral() {
                    if let Err(e) = check_block(body, funcret) {
                        errors.extend(e);
                    }
                } else {
                    errors.push(Error::BadWhileCond(cond.span.clone()));
                }
            }
        
            // TODO ======================================================
            //Statement::Yield(expr) => {
            //    match expr.type_of(&locals, ast) {
            //        Ok((t, _)) => {
            //            if t != *funcret {
            //                errors.push(Error::BadYield(0, funcret.clone(), t))
            //            }
            //        }
            //        Err(e) => errors.push(e),
            //    }
            //}
            Statement::For{ body, .. } => {
                if let Err(e) = check_block(body, funcret) {
                    errors.extend(e);
                }
            }

            Statement::Break|Statement::Empty => ()
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}


pub fn check(ast: &Ast) -> Result<(), Vec<Error>> {
    for func in ast.funcs.values() {
        let mut locals = HashMap::new();
        for Variable{ datatype, mutable, name } in &func.params {
            locals.insert(name.clone(), (datatype.clone(), mutable.clone()));
        }

        check_block(&func.body, &func.rettype.val)?;
    }

    Ok(())
}