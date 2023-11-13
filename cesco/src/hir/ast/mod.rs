mod types;
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
    Break,

    Expression(Item<Expression>),
}


#[derive(Debug, Default, Clone, PartialEq)]
pub struct Func {
    pub name:       Item<String>,
    pub params:     Vec<Variable>,
    pub rettype:    Item<Type>,
    pub body:       Vec<Statement>,
}

impl Func {
    pub fn new() -> Self {
        Self::default()
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Struct { 
    pub name:   Item<String>,
    pub fields: Vec<(Field, usize)>,
    pub size:   usize,
}

#[allow(dead_code)]
impl Struct {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn size_of(&self) -> usize {
        self.size
    }

    pub fn align_of(&self) -> usize {
        self.fields.iter()
                   .map(|(_, size)| *size)
                   .max()
                   .unwrap_or(0)
    }

    pub fn get(&self, name: &str) -> Option<&Field> {
        for (field, _) in &self.fields {
            if field.name.val == name {
                return Some(field)
            }
        }

        None
    }
}


#[derive(Debug, Clone, PartialEq, Default)]
pub struct Ast {
    pub funcs: HashMap<String, Func>,
    pub structs: HashMap<String, Struct>,
    pub imports: Vec<String>,
}

impl Ast {
    pub fn new() -> Self {
        Self::default()
    }
}
