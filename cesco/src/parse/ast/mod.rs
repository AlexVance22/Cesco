pub mod types;
mod expr;

use std::collections::HashMap;
use crate::span::Item;
pub use types::*;
pub use expr::*;


#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum AccessMod {
    #[default]
    Private,
    Public,
}


#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Field {
    pub access:     Item<AccessMod>,
    pub datatype:   Item<Type>,
    pub name:       Item<String>,
}


#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Variable {
    pub mutable:    Item<bool>,
    pub datatype:   Item<Type>,
    pub name:       Item<String>,
}


#[derive(Debug, Default, Clone, PartialEq)]
pub enum Statement {
    #[default]
    Empty,

    VarDecl{ decl: Variable, val: Item<Expression> },
    If     { cond: Item<Expression>, body: Vec<Statement> },
    While  { cond: Item<Expression>, body: Vec<Statement> },
    For    { varname: Item<String>, iterable: Item<Expression>, body: Vec<Statement> },

    Return(Option<Item<Expression>>),
    Yield(Item<Expression>),
    Break,

    Expression(Item<Expression>),
}


#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum Association {
    #[default]
    None,
    Method(String),
    Static(String),
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Func {
    pub name:   Item<String>,
    pub assoc:  Item<Association>,
    pub mangle: String,
    pub params: Vec<Variable>,
    pub rettype: Item<Type>,
    pub body:   Vec<Statement>,
}


pub fn align_to(size: usize, align: usize) -> usize {
    if size % align == 0 {
        size
    } else {
        let nsize = size + align;
        nsize - (nsize % align)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Struct { 
    pub name:   Item<String>,
    pub fields: Vec<Field>,
}

impl Struct {
    pub fn size_of(&self, types: &HashMap<String, Struct>) -> usize {
        let mut size: usize = 0;

        for i in 0..self.fields.len() {
            size += self.fields[i].datatype.val.size_of(types);

            if let Some(f) = self.fields.get(i + 1) {
                let align = f.datatype.val.align_of(types);
                size = align_to(size, align);
            } else {
                let align = self.align_of(types);
                size = align_to(size, align);
            }
        }

        size
    }

    pub fn align_of(&self, types: &HashMap<String, Struct>) -> usize {
        self.fields.iter()
                   .map(|f| f.datatype.val.align_of(types))
                   .max()
                   .unwrap_or(0)
    }

    pub fn get(&self, name: &str) -> Option<&Field> {
        for f in &self.fields {
            if f.name.val == name {
                return Some(f)
            }
        }

        None
    }
}


#[derive(Debug, Default, Clone, PartialEq)]
pub struct Ast {
    pub funcs: HashMap<(String, Association), Func>,
    pub structs: HashMap<String, Struct>,
    pub imports: Vec<String>,
}

impl Ast {
    pub fn new() -> Self {
        Self::default()
    }
}



#[cfg(test)]
#[macro_use]
mod mock {
    
    macro_rules! field {
        (($v:vis, $t:literal)) => {
            Field{ datatype: Type::User($t.to_string()), access: AccessMod::Public, ..Default::default() }
        };
        (($v:vis, $t:literal[$n:literal])) => {
            Field{ datatype: Type::ArrOf(Box::new(Type::User($t.to_string())), $n), access: AccessMod::Public, ..Default::default() }
        };
        (($v:vis, $t:literal($n:literal))) => {
            Field{ datatype: Type::VecOf(Box::new(Type::User($t.to_string())), $n), access: AccessMod::Public, ..Default::default() }
        };

        (($v:vis $t:ident)) => {
            Field{ datatype: Type::$t, access: AccessMod::Public, ..Default::default() }
        };
        (($v:vis $t:ident[$n:literal])) => {
            Field{ datatype: Item::mock(Type::ArrOf(Box::new(Type::$t), $n)), access: Item::mock(AccessMod::Public), ..Default::default() }
        };
        (($v:vis $t:ident($n:literal))) => {
            Field{ datatype: Type::VecOf(Box::new(Type::$t), $n), access: AccessMod::Public, ..Default::default() }
        };
        //============================================================================================================================
        (($t:literal $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::User($t.to_string()), ..Default::default() }
        };
        (($t:literal[$n:literal] $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::ArrOf(Box::new(Type::User($t.to_string())), $n), ..Default::default() }
        };
        (($t:literal($n:literal) $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::VecOf(Box::new(Type::User($t.to_string())), $n), ..Default::default() }
        };
        
        (($t:ident $i:ident)) => {
            Field{ name: Item::mock(stringify!($i).to_string()), datatype: Item::mock(Type::$t), ..Default::default() }
        };
        (($t:ident[$n:literal] $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::ArrOf(Box::new(Type::$t), $n), ..Default::default() }
        };
        (($t:ident($n:literal) $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::VecOf(Box::new(Type::$t), $n), ..Default::default() }
        };
        //============================================================================================================================
        (($v:vis, $t:literal $i:ident)) => {
            Field{ name: Item::mock(stringify!($i).to_string()), datatype: Item::mock(Type::User($t.to_string())), access: Item::mock(AccessMod::Public) }
        };
        (($v:vis, $t:literal[$n:literal] $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::ArrOf(Box::new(Type::User($t.to_string())), $n), access: AccessMod::Public }
        };
        (($v:vis, $t:literal($n:literal) $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::VecOf(Box::new(Type::User($t.to_string())), $n), access: AccessMod::Public }
        };

        (($v:vis $t:ident $i:ident)) => {
            Field{ name: Item::mock(stringify!($i).to_string()), datatype: Item::mock(Type::$t), access: Item::mock(AccessMod::Public) }
        };
        (($v:vis $t:ident[$n:literal] $i:ident)) => {
            Field{ name: stringify!($i).to_string(), datatype: Type::ArrOf(Box::new(Type::$t), $n), access: AccessMod::Public }
        };
        (($v:vis $t:ident($n:literal) $i:ident)) => {
            Field{ name: Item::mock(stringify!($i).to_string()), datatype: Item::mock(Type::VecOf(Box::new(Type::$t), $n)), access: Item::mock(AccessMod::Public) }
        };
        //============================================================================================================================
        ($t:literal) => {
            Field{ datatype: Type::User($t.to_string()), ..Default::default() }
        };
        (($t:literal[$n:literal])) => {
            Field{ datatype: Type::ArrOf(Box::new(Type::User($t.to_string())), $n), ..Default::default() }
        };
        (($t:literal($n:literal))) => {
            Field{ datatype: Type::VecOf(Box::new(Type::User($t.to_string())), $n), ..Default::default() }
        };

        ($t:ident) => {
            Field{ datatype: Item::mock(Type::$t), ..Default::default() }
        };
        (($t:ident[$n:literal])) => {
            Field{ datatype: Type::ArrOf(Box::new(Type::$t), $n), ..Default::default() }
        };
        (($t:ident($n:literal))) => {
            Field{ datatype: Type::VecOf(Box::new(Type::$t), $n), ..Default::default() }
        };
    }

    #[macro_export]
    macro_rules! def_struct {
        ($name:ident) => {
            Struct{ name: stringify!($name).to_string(), fields: Vec::new() }
        };
        ($name:ident, $($fields:tt),*) => {
            Struct{ name: Item::mock(stringify!($name).to_string()), fields: vec![$(field!($fields)),*] }
        };
    }

}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Item;

    #[test]
    fn sizeof_struct() {
        let mut types: HashMap<String, Struct> = HashMap::new();
        
        let case_1 = def_struct!(case_1, U8, I16, U32, U8);
        let case_2 = def_struct!(case_2, F32, U8);
        let case_3 = def_struct!(case_3, I16, (U8[3]));
        let case_4 = def_struct!(case_4, U8, U8, I16, I32);

        println!("{:#?}", case_1);

        types.insert("case_1".to_string(), case_1.clone());
        types.insert("case_2".to_string(), case_2.clone());
        types.insert("case_3".to_string(), case_3.clone());
        types.insert("case_4".to_string(), case_4.clone());

        assert_eq!(case_1.size_of(&types), 12);
        assert_eq!(case_2.size_of(&types), 8);
        assert_eq!(case_3.size_of(&types), 6);
        assert_eq!(case_4.size_of(&types), 8);
    }
}