mod span;
mod lex;
mod parse;
mod preproc;
mod hir;
mod import;
mod check;
mod trans;
mod mir;
mod gen;

use std::{
    process::Command,
    collections::HashSet,
};
use thiserror::Error;
//use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};


#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("ERROR - Failed to lex program")]
    Lex(Vec<lex::Error>),
    #[error("ERROR - Failed to parse program")]
    Parse(Vec<parse::Error>),
    #[error("ERROR - Failed to process imports")]
    Import(#[from] import::Error),
    #[error("ERROR - Failed to open file")]
    FileIO(#[from] std::io::Error),
}


fn gen_ast(src: &str, file: &str) -> Result<hir::ast::Ast, CompilerError> {
    let tokens = lex::lex(src, file).map_err(CompilerError::Lex)?;

    //println!("{:#?}", tokens);

    let tree = parse::parse(tokens).map_err(CompilerError::Parse)?;

    //println!("{:#?}", tree);

    Ok(preproc::process(tree))
}


pub fn compile(infile: &str) {
    let fileroot = infile.trim_end_matches(".cco");

    let tree = match import::dependency_graph(fileroot) {
        Ok(res) => {
            println!("INFO - successfully parsed program");
            res
        }
        Err(err) => { 
            eprintln!("ERROR - Failed to parse program '{infile}' with following errors:\n");
            match err {
                CompilerError::Lex(err) => for e in err { eprintln!("{}", e); }
                CompilerError::Parse(err) => for e in err { eprintln!("{}", e); }
                CompilerError::Import(err) => eprintln!("{}", err),
                CompilerError::FileIO(err) => eprintln!("{}", err),
            }
            return
        }
    };

    let mut files = Vec::new();

    for (name, tree) in tree {
        match check::check(&tree) {
            Ok(_) => {
                println!("INFO - successfully verified program");
            },
            Err(err) => { 
                eprintln!("Failed to verify program '{infile}' with following errors:");
                for e in err {
                    eprintln!("{}", e);
                }
            }
        };

        let ir = mir::destructure(tree);

        println!();
        for proc in &ir {
            println!("{}", proc);
        }

        //let ir = mir::optimise(ir);

        println!();
        for proc in &ir {
            println!("{}", proc);
        }

        let asm = gen::generate(ir).unwrap();

        std::fs::write(format!("{}.asm", fileroot), asm).unwrap();

        println!("INFO - assembly to object");

        Command::new("nasm")
            .arg("-f")
            .arg("win64")
            .arg("-o")
            .arg(&format!("{}.obj", name))
            .arg(&format!("{}.asm", name))
            //.status()
            .output()
            .unwrap();

        files.push(format!("{}.obj", name));
    }

    println!("INFO - linking object");

    Command::new("gcc")
        .arg("-nolibc")
        .arg("-o")
        .arg(format!("{}.exe", fileroot))
        .args(&files)
        //.status()
        .output()
        .unwrap();
}


pub fn transpile(infile: &str) {
    let fileroot = infile.trim_end_matches(".cco");

    let tree = match import::dependency_graph(fileroot) {
        Ok(res) => {
            println!("INFO - successfully parsed program");
            res
        }
        Err(err) => { 
            eprintln!("ERROR - Failed to parse program '{infile}' with following errors:\n");
            match err {
                CompilerError::Lex(err) => for e in err { eprintln!("{}", e); }
                CompilerError::Parse(err) => for e in err { eprintln!("{}", e); }
                CompilerError::Import(err) => eprintln!("{}", err),
                CompilerError::FileIO(err) => eprintln!("{}", err),
            }
            return
        }
    };

    let mut mono_arrs = HashSet::new();
    let mut mono_nulls = HashSet::new();
    let mut files = Vec::new();

    for (name, tree) in tree {
        /*
        match check::check(&tree) {
            Ok(_) => {
                println!("INFO - successfully verified program");
            },
            Err(err) => { 
                eprintln!("Failed to verify program '{filename}' with following errors:");
                for e in err {
                    eprintln!("{}", e);
                }
                return
            }
        };
        */

        let root = name == fileroot;
        
        let (c_output, h_output) = match trans::transpile(tree, &name, root, &mut mono_arrs, &mut mono_nulls) {
            Ok(res) => {
                println!("INFO - successfully dumped C transpilation");
                res
            }
            Err(e) => { 
                eprintln!("Failed to dump program '{name}.cco' with following errors:{:#?}", e);
                return
            }
        };

        std::fs::write(format!("{}.c", name), c_output).unwrap();
        if name != fileroot {
            std::fs::write(format!("{}.h", name), h_output).unwrap();
        }

        files.push(format!("{}.c", name));
    }

    let (head, body) = match trans::monomophs(mono_arrs, mono_nulls) {
        Ok(res) => {
            println!("INFO - successfully dumped monomorphisations");
            res
        }
        Err(e) => { 
            eprintln!("Failed to dump monomorphisations with following errors:\n{:#?}", e);
            return
        }
    };

    std::fs::write("monomorphisations.c", body).unwrap();
    std::fs::write("monomorphisations.h", head).unwrap();

    //Command::new("gcc")
    //    .arg("-nolibc")
    //    .arg("-o")
    //    .arg(format!("{}.exe", fileroot))
    //    .args(&files)
    //    .status()
    //    .unwrap();
}
