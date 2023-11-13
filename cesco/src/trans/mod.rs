use crate::hir::ast::*;
use crate::span::Item;
use std::fmt::Write;
use std::collections::HashSet;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Kind {
    Prim,
    User(usize)
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrMonomorph {
    len: usize,
    tyn: String,
    kind: Kind,
}

impl ArrMonomorph {
    fn new(len: usize, tyn: String, kind: Kind) -> Self {
        ArrMonomorph{
            len,
            tyn,
            kind,
        }
    }

    fn get_name(&self) -> String {
        if self.tyn.ends_with('*') {
            format!("__{}_ptr_{}", self.tyn.replace('*', ""), self.len)
        } else {
            format!("__{}_{}", self.tyn, self.len)
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NullMonomorph {
    tyn: String,
    kind: Kind,
}

impl NullMonomorph {
    fn new(tyn: String, kind: Kind) -> Self {
        NullMonomorph{
            tyn,
            kind,
        }
    }

    fn get_name(&self) -> String {
        if self.tyn.ends_with('*') {
            format!("__{}_ptr_Nullable", self.tyn.replace('*', ""))
        } else {
            format!("__{}_Nullable", self.tyn)
        }
    }
}



fn c_type(dt: Type) -> (String, usize) {
    match dt {
        Type::Null => ("void".to_string(), 1),
        Type::U8 => ("uint8_t".to_string(), 1),
        Type::U16 => ("uint16_t".to_string(), 1),
        Type::U32 => ("uint32_t".to_string(), 1),
        Type::U64 => ("uint64_t".to_string(), 1),
        Type::I8 => ("int8_t".to_string(), 1),
        Type::I16 => ("int16_t".to_string(), 1),
        Type::I32 => ("int32_t".to_string(), 1),
        Type::I64 => ("int64_t".to_string(), 1),
        Type::F32 => ("float".to_string(), 1),
        Type::F64 => ("double".to_string(), 1),
        Type::Bool => ("bool".to_string(), 1),
        Type::Char => ("char".to_string(), 1),
        Type::Str => ("char*".to_string(), 1),
        Type::User(s, _) => (s, 1),
        Type::PtrTo(t) => (format!("{}*", c_type(*t).0), 1),
        Type::RefTo(t) => (format!("{}*", c_type(*t).0), 1),
        Type::OptOf(t) => (c_type(*t).0, 1),
        Type::ArrOf(t, i) => (c_type(*t).0, i),
        Type::VecOf(t, i) => (c_type(*t).0, i),
    }
}

fn c_fulltypename(dt: Type, arrs: &mut HashSet<ArrMonomorph>, nulls: &mut HashSet<NullMonomorph>) -> String {
    if let Type::OptOf(t) = dt.clone() {
        let kind = if let Type::User(..) = *t { Kind::User(t.size_of()) } else { Kind::Prim };
        let (typename, arrlen) = c_type(*t);

        if arrlen == 1 {
            let a = NullMonomorph::new(typename, kind);
            nulls.insert(a.clone());
            a.get_name()
        } else {
            let a = ArrMonomorph::new(arrlen, typename, kind);
            arrs.insert(a.clone());
            let b = NullMonomorph::new(a.get_name(), kind);
            nulls.insert(b.clone());
            b.get_name()
        }
    } else {
        let kind = if let Type::User(..) = dt { Kind::User(dt.size_of()) } else { Kind::Prim };
        let (typename, arrlen) = c_type(dt);

        if arrlen == 1 {
            typename
        } else {
            let a = ArrMonomorph::new(arrlen, typename, kind);
            arrs.insert(a.clone());
            a.get_name()
        } 
    }
}

fn c_funcsig(f: &Func, arrs: &mut HashSet<ArrMonomorph>, nulls: &mut HashSet<NullMonomorph>) -> Result<String, std::fmt::Error> {
    let mut res = String::new();
    
    write!(res, "{} {}(", c_fulltypename(f.rettype.val.clone(), arrs, nulls), f.name.val)?;

    for param in &f.params {
        write!(res, "{} {}, ", c_fulltypename(param.datatype.val.clone(), arrs, nulls), param.name.val)?;
    }

    if !f.params.is_empty() {
        res.pop();
        res.pop();
    }

    writeln!(res, ");")?;

    Ok(res)
}


fn dump_expr(expr: Item<Expression>, expand_null: bool) -> Result<String, std::fmt::Error> {
    match expr.val {
        Expression::NullPtr => Ok("NULL".to_string()),
        Expression::Null(ty) => {
            if !expand_null {
                return Ok("NULL".to_string())   
            }

            let kind = if let Type::User(..) = ty { Kind::User(ty.size_of()) } else { Kind::Prim };
            let (typename, arrlen) = c_type(ty);
    
            if arrlen == 1 {
                let a = NullMonomorph::new(typename, kind);
                Ok(format!("{}_new(NULL)", a.get_name()))
            } else {
                let a = ArrMonomorph::new(arrlen, typename, kind);
                let b = NullMonomorph::new(a.get_name(), kind);
                Ok(format!("{}_new(NULL)", b.get_name()))   
            }
        }
        Expression::I8Lit(i) => Ok(format!("{i}")),
        Expression::I16Lit(i) => Ok(format!("{i}")),
        Expression::I32Lit(i) => Ok(format!("{i}")),
        Expression::I64Lit(i) => Ok(format!("{i}")),
        Expression::U8Lit(i) => Ok(format!("{i}")),
        Expression::U16Lit(i) => Ok(format!("{i}")),
        Expression::U32Lit(i) => Ok(format!("{i}")),
        Expression::U64Lit(i) => Ok(format!("{i}")),
        Expression::F32Lit(f) => Ok(format!("{f}")),
        Expression::F64Lit(f) => Ok(format!("{f}")),
        Expression::BoolLit(b) => Ok(format!("{b}")),
        Expression::CharLit(c) => Ok(format!("'{c}'")),
        Expression::StrLit(s) => Ok(format!("{s:?}")),
        Expression::Identifier(_, id) => Ok(id),
        Expression::ArrLit(_, vals) => {
            let mut res = String::new();
            for v in vals {
                write!(res, "{}, ", dump_expr(v, true)?)?;
            }
            Ok(format!("{{ {}}}", res))
        }
        Expression::StructLit(.., vals) => {
            let mut res = String::new();
            for (_, v) in vals {
                write!(res, "{}, ", dump_expr(v, true)?)?;
            }
            Ok(format!("{{ {}}}", res))
        }

        Expression::Neg(_, val) => {
            Ok(format!("-({})", dump_expr(*val, true)?))
        }
        Expression::Add(_, left, right) => {
            Ok(format!("({} + {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Sub(_, left, right) => {
            Ok(format!("({} - {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Mul(_, left, right) => {
            Ok(format!("({} * {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Div(_, left, right) => {
            Ok(format!("({} / {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Mod(_, left, right) => {
            Ok(format!("({} % {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }

        Expression::Not(val) => {
            Ok(format!("!({})", dump_expr(*val, true)?))
        }
        Expression::And(left, right) => {
            Ok(format!("({} && {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Or (left, right) => {
            Ok(format!("({} || {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }

        Expression::Lt(left, right) => {
            Ok(format!("({} < {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Gt(left, right) => {
            Ok(format!("({} > {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Le(left, right) => {
            Ok(format!("({} <= {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Ge(left, right) => {
            Ok(format!("({} >= {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Eq(left, right) => {
            Ok(format!("({} == {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Ne(left, right) => {
            Ok(format!("({} != {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }

        Expression::BitNot(_, val) => {
            Ok(format!("~({})", dump_expr(*val, true)?))
        }
        Expression::BitAnd(_, left, right) => {
            Ok(format!("({} & {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::BitOr (_, left, right) => {
            Ok(format!("({} | {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::BitXor(_, left, right) => {
            Ok(format!("({} ^ {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::LShift(_, left, right) => {
            Ok(format!("({} << {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::RShift(_, left, right) => {
            Ok(format!("({} >> {})", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }

        Expression::Deref (_, val) => Ok(format!("(*{})", dump_expr(*val, true)?)),
        Expression::DerefChecked(_, val) => Ok(format!("(*{})", dump_expr(*val, true)?)),
        Expression::AddrOf(_, val) => Ok(format!("(&{})", dump_expr(*val, true)?)),

        Expression::Assign{ left, right, .. } => {
            Ok(format!("{} = {}", dump_expr(*left, true)?, dump_expr(*right, true)?))
        }
        Expression::Call  { name, args, .. } => {
            let mut res = format!("{}(", name);
            let pop = !args.is_empty();
            for a in args {
                write!(res, "{}, ", dump_expr(a, true)?)?;
            }
            if pop {
                res.pop();
                res.pop();
            }
            write!(res, ")")?;
            Ok(res)
        }
        Expression::Member{ obj, member, .. } => {
            Ok(format!("{}.{}", dump_expr(*obj, true)?, dump_expr(*member, true)?))
        }
        Expression::ArrGet{ obj, index, .. } => {
            Ok(format!("{}.val[{}]", dump_expr(*obj, true)?, dump_expr(*index, true)?))
        }
    }
}

fn dump_block(block: Vec<Statement>, ret: &Type, arrs: &mut HashSet<ArrMonomorph>, nulls: &mut HashSet<NullMonomorph>) -> Result<String, std::fmt::Error> {
    let mut res = String::new();

    for stmt in block {
        match stmt {
            Statement::Break => writeln!(res, "    break;")?,
            Statement::Return(expr) => match expr {
                Some(expr) => {
                    if let Type::OptOf(t) = ret {
                        let kind = if let Type::User(..) = t.as_ref() { Kind::User(t.size_of()) } else { Kind::Prim };
                        let (typename, arrlen) = c_type(*t.clone());
    
                        if arrlen == 1 {
                            let a = NullMonomorph::new(typename, kind);
                            writeln!(res, "    return {}_new(&({}));", a.get_name(), dump_expr(expr, false)?)?;
                        } else {
                            let a = ArrMonomorph::new(arrlen, typename, kind);
                            let b = NullMonomorph::new(a.get_name(), kind);
                            writeln!(res, "    return {}_new(&({}));", b.get_name(), dump_expr(expr, false)?)?;
                        }
                    } else {
                        writeln!(res, "    return {};", dump_expr(expr, false)?)?
                    }
                }
                None => writeln!(res, "    return;")?
            }

            Statement::If{ cond, body } => {
                writeln!(res, "    if ({}) {{", dump_expr(cond, true)?)?;
                write!(res, "{}", dump_block(body, ret, arrs, nulls)?)?;
                writeln!(res, "    }}")?;
            }
            Statement::While{ cond, body } => {
                writeln!(res, "    while ({}) {{", dump_expr(cond, true)?)?;
                write!(res, "{}", dump_block(body, ret, arrs, nulls)?)?;
                writeln!(res, "    }}")?;
            }

            Statement::VarDecl{ decl, val } => {
                writeln!(res, "    {} {} = {};", c_fulltypename(decl.datatype.val, arrs, nulls), decl.name.val, dump_expr(val, true)?)?
            }
            Statement::Expression(expr) => {
                writeln!(res, "    {};", dump_expr(expr, true)?)?;
            }
            _ => ()
        }
    }

    Ok(res)
}



pub fn transpile(tree: Ast, file: &str, root: bool, arrs: &mut HashSet<ArrMonomorph>, nulls: &mut HashSet<NullMonomorph>) -> Result<(String, String), std::fmt::Error> {
    let mut c_header = String::new();
    let mut h_header = String::new();
    let mut c_res = String::new();
    let mut h_res = String::new();

    writeln!(h_header, "#ifndef {n}_H\n#define {n}_H", n = file.to_ascii_uppercase())?;
    writeln!(c_header, "#include <stdint.h>\n#include <stdbool.h>\n#include <string.h>\n#include <Windows.h>\n#include \"monomorphisations.h\"\n")?;
    for i in tree.imports {
        writeln!(c_header, "#include \"{}.h\"", i)?;
        writeln!(h_header, "#include \"{}.h\"", i)?;
    }
    if !root {
        writeln!(c_header, "#include \"{}.h\"", file)?;
    }

    for (name, strct) in &tree.structs {
        writeln!(h_res, "typedef struct {} {{", name)?;
        for (field, _) in &strct.fields {
            writeln!(h_res, "    {} {};", c_fulltypename(field.datatype.val.clone(), arrs, nulls), field.name.val)?;
        }
        writeln!(h_res, "}} {};\n", name)?;
    }

    if root {
        for (name, strct) in &tree.structs {
            writeln!(c_res, "typedef struct {} {{", name)?;
            for (field, _) in &strct.fields {
                writeln!(c_res, "    {} {};", c_fulltypename(field.datatype.val.clone(), arrs, nulls), field.name.val)?;
            }
            writeln!(c_res, "}} {};\n", name)?;
        }
    }

    writeln!(c_res)?;
    writeln!(c_res)?;
    writeln!(h_res)?;
    writeln!(h_res)?;

    for func in tree.funcs.values() {
        write!(c_res, "{}", c_funcsig(func, arrs, nulls)?)?;
        write!(h_res, "{}", c_funcsig(func, arrs, nulls)?)?;
    }

    writeln!(c_res)?;
    writeln!(c_res)?;
    writeln!(h_res)?;
    writeln!(h_res)?;

    for (name, func) in tree.funcs {
        write!(c_res, "{} {}(", c_fulltypename(func.rettype.val.clone(), arrs, nulls), name)?;

        for param in &func.params {
            write!(c_res, "{} {}, ", c_fulltypename(param.datatype.val.clone(), arrs, nulls), param.name.val)?;
        }
        if !func.params.is_empty() {
            c_res.pop();
            c_res.pop();
        }
        writeln!(c_res, ")\n{{\n{}}}\n", dump_block(func.body, &func.rettype.val, arrs, nulls)?)?;
    }

    writeln!(h_res, "#endif")?;

    Ok((format!("{}{}", c_header, c_res), format!("{}{}", h_header, h_res)))
}


pub fn monomophs(arrs: HashSet<ArrMonomorph>, nulls: HashSet<NullMonomorph>) -> Result<(String, String), std::fmt::Error> {
    let mut head = String::new();
    let mut body = String::new();

    writeln!(head, "#ifndef MONOMORPHS_H")?;
    writeln!(head, "#define MONOMORPHS_H")?;
    writeln!(head)?;
    writeln!(body, "#include <stdint.h>\n#include <stdbool.h>\n#include <string.h>\n#include \"monomorphisations.h\"\n")?;

    for a in arrs {
        if let Kind::User(_) = a.kind {
            writeln!(head, "typedef struct {t} {t};", t = a.tyn)?;
        }

        writeln!(head, "typedef struct {n} {{\n    {} val[{}];\n}} {n};\n", a.tyn, a.len, n = a.get_name())?;
        write!(head, "{n} {n}_new(", n = a.get_name())?;
        for i in 0..a.len {
            write!(head, "{} v{}, ", a.tyn, i)?;
        }
        head.pop();
        head.pop();
        writeln!(head, ");")?;
        write!(body, "{n} {n}_new(", n = a.get_name())?;
        for i in 0..a.len {
            write!(body, "{} v{}, ", a.tyn, i)?;
        }
        body.pop();
        body.pop();
        writeln!(body,  ")\n{{\n    {} result;", a.get_name())?;
        for i in 0..a.len {
            writeln!(body, "    result.val[{i}] = v{i};")?;
        }
        writeln!(body, "    return result;\n}}\n")?;
    }
    writeln!(head)?;
    for a in nulls {
        let m_name = a.get_name();
        if let Kind::User(size) = a.kind {
        writeln!(head, "typedef struct {t} {t};\n", t = a.tyn)?;
        writeln!(head, "typedef struct {n} {{\n    bool has;\n    char val[{}];\n}} {n};\n", size, n = m_name)?;
        } else {
        writeln!(head, "typedef struct {n} {{\n    bool has;\n    {} val;\n}} {n};\n", a.tyn, n = m_name)?;
        }
        writeln!(head, "{n} {n}_new({}* val);\n", a.tyn, n = m_name)?;
        writeln!(body, "{n} {n}_new({}* val)",    a.tyn, n = m_name)?;
        writeln!(body, "{{                              ")?;
        writeln!(body, "    {} result;                  ", m_name)?;
        writeln!(body, "    if (val) {{                 ")?;
        writeln!(body, "        result.has = true;      ")?;
        if let Kind::User(size) = a.kind {
        writeln!(body, "        memcpy(&result.val, val, {});", size)?;
        } else {
        writeln!(body, "        result.val = *val;      ")?;
        }
        writeln!(body, "    }} else {{                  ")?;
        writeln!(body, "        result.has = false;     ")?;
        writeln!(body, "    }}                          ")?;
        writeln!(body, "    return result;              ")?;
        writeln!(body, "}}                              ")?;
    }

    writeln!(head, "#endif")?;

    Ok((head, body))
}
