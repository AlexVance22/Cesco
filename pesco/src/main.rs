mod asm;
mod check;
mod data;
mod eval;
mod gen;
mod op;
mod parse;
mod float;

use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

use parse::TokenKind;

macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

fn gen_ops() -> HashMap<String, TokenKind> {
    map! {
        "+"    .to_string() => TokenKind::OpAdd,
        "-"    .to_string() => TokenKind::OpSub,
        "*"    .to_string() => TokenKind::OpMul,
        "/"    .to_string() => TokenKind::OpDiv,
        "<"    .to_string() => TokenKind::OpLT,
        ">"    .to_string() => TokenKind::OpGT,
        "<="   .to_string() => TokenKind::OpLE,
        ">="   .to_string() => TokenKind::OpGE,
        "=="   .to_string() => TokenKind::OpEQ,
        "!="   .to_string() => TokenKind::OpNE,
        "->"   .to_string() => TokenKind::OpOut,
        "->!"  .to_string() => TokenKind::OpErr,
        "<-"   .to_string() => TokenKind::OpIn,
        "[]"   .to_string() => TokenKind::OpArr,
        "str"  .to_string() => TokenKind::KwStr,
        "int"  .to_string() => TokenKind::KwInt,
        "flt"  .to_string() => TokenKind::KwFlt,
        "dup"  .to_string() => TokenKind::OpDup,
        "2dup" .to_string() => TokenKind::Op2Dup,
        "over" .to_string() => TokenKind::OpOver,
        "rot"  .to_string() => TokenKind::OpRot,
        "irot" .to_string() => TokenKind::OpIRot,
        "swap" .to_string() => TokenKind::OpSwap,
        "pop"  .to_string() => TokenKind::OpPop,
        "if"   .to_string() => TokenKind::KwIf,
        "while".to_string() => TokenKind::KwWhile,
        "do"   .to_string() => TokenKind::KwDo,
        "end"  .to_string() => TokenKind::KwEnd,
        "true" .to_string() => TokenKind::KwTrue,
        "false".to_string() => TokenKind::KwFalse,
        "var"  .to_string() => TokenKind::KwVar,
        "func" .to_string() => TokenKind::KwFunc,
        "puts" .to_string() => TokenKind::KwPuts
    }
}


struct Command {
    output: String,
    input: String,
    int_dir: Option<String>,
    eval: bool,
    _bits: Option<i32>
}


fn parse_command(cmd: Vec<String>) -> Result<Command, String> {
    if cmd.len() < 3 {
        eprintln!("no file argument provided");
        process::exit(1);
    }

    let mut output = String::new();
    let mut input = String::new();
    let mut int_dir = None;
    let mut eval = false;

    let mut i = 0;
    while i < cmd.len() {
        if cmd[i] == "-o" {
            i += 1;
            if i >= cmd.len() {
                eprintln!("no output argument provided");
                process::exit(1);
            }
            output = cmd[i].clone();
            i += 1;
            if i >= cmd.len() {
                eprintln!("no input argument provided");
                process::exit(1);
            }
            input = cmd[i].clone();
        }

        if cmd[i] == "-i" {
            i += 1;
            if i >= cmd.len() {
                eprintln!("no directory provided");
                process::exit(1);
            }
            int_dir = Some(cmd[i].clone());
        }

        if cmd[i] == "-e" || cmd[i] == "--eval" {
            eval = true;
        }

        i += 1;
    }

    Ok(Command {
        output,
        input,
        int_dir,
        eval,
        _bits: None,
    })
}


fn main() {
    let args: Vec<String> = env::args().collect();

    let cmd = parse_command(args).unwrap_or_else(|e| {
        eprintln!("{e}");
        process::exit(1);
    });

    let ops = gen_ops();

    let source = fs::read_to_string(&cmd.input).unwrap_or_else(|_| {
        eprintln!("failed to open file '{}'", cmd.input);
        process::exit(1);
    });

    let out_root = cmd.output.strip_suffix(".exe").unwrap();
    let (f_asm, f_obj) = if let Some(dir) = cmd.int_dir { 
        (format!("{}/{}.asm", dir, out_root),
        format!("{}/{}.obj", dir, out_root))
    } else {
        (format!("{}.asm", out_root),
        format!("{}.obj", out_root))
    };
    let f_exe = format!("{}", cmd.output);

    println!("INFO - parsing program");

    let tokens = parse::tokenise(&source, &ops);

    println!("INFO - checking program");

    check::check(&tokens);

    println!("INFO - optimising AST");

    let tokens = op::op_prog(tokens);

    if cmd.eval {
        eval::eval(&tokens);
    }

    println!("INFO - generating code");

    let mut prog = gen::gen_inter(tokens);

    println!("INFO - optimising assembly");

    prog.instrs = op::op_asm(prog.instrs);

    println!("INFO - dumping assembly");

    let libs: Vec<String> = prog.links.iter().map(|s| format!("{s}.obj")).collect();

    asm::dump(prog, &f_asm);

    println!("INFO - assembly to object");

    process::Command::new("nasm")
        .arg("-f")
        .arg("win64")
        .arg("-o")
        .arg(&f_obj)
        .arg(&f_asm)
        .output()
        .unwrap();

    println!("INFO - linking object");

    process::Command::new("gcc")
        .arg("-nolibc")
        .arg("-o")
        .arg(&f_exe)
        .arg(&f_obj)
        .args(&libs)
        .output()
        .unwrap();
}
