#![allow(unused)]

use std::{
    fs,
    collections::{ HashMap, HashSet }
};
use thiserror::Error;
use crate::hir::ast::*;


#[derive(Error, Debug)]
pub enum Error {
    #[error("file '{0}' - cyclical import detected with file '{1}'")]
    Cyclical(String, String),
}



fn graph_generate(graph: &mut HashMap<String, Ast>, imp: &str, mut ancestors: HashSet<String>) -> Result<(), crate::CompilerError> {
    let src = fs::read_to_string(&format!("{}.cco", imp))?;
    ancestors.insert(imp.to_string());

    let tree = crate::gen_ast(&src, imp)?;
    
    for i in &tree.imports {
        if ancestors.contains(i) {
            return Err(Error::Cyclical(imp.to_string(), i.clone()).into())
        }

        graph_generate(graph, i, ancestors.clone())?;
    }

    graph.insert(imp.to_string(), tree);

    Ok(())
}


pub fn dependency_graph(name: &str) -> Result<HashMap<String, Ast>, crate::CompilerError> {
    let mut mod_tree = HashMap::new();

    graph_generate(&mut mod_tree, name, HashSet::new())?;

    Ok(mod_tree)
}