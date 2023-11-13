use crate::gen::Program;
use std::fs;


fn fmt_str(lit: &str) -> String {
    use std::fmt::Write;

    let mut result = String::new();
    let mut token = String::new();

    for c in lit.chars() {
        if c == '\n' || c == '\r' || c == '\t' {
            if !token.is_empty() {
                write!(result, "\"{token}\", ").unwrap();
                token = String::new();
            }
            write!(result, "{}, ", c as u8).unwrap();
        } else {
            token.push(c);
        }
    }

    if !token.is_empty() {
        write!(result, "\"{token}\", ").unwrap();
    }
    
    result
}


pub fn dump(mut prog: Program, filename: &str) {
    use std::io::Write;

    let mut f = fs::File::create(filename).unwrap();

    writeln!(f, "    global main").unwrap();
    writeln!(f, "    section .text").unwrap();
    writeln!(f, "    extern ExitProcess").unwrap();

    let stdio = prog.links.contains("stdio");
    prog.links.remove("stdio");

    if stdio {
        writeln!(f, "    extern init_stdio").unwrap();
        writeln!(f, "    extern puts").unwrap();
        writeln!(f, "    extern dump").unwrap();
    }

    writeln!(f, "main:").unwrap();
    if stdio {
        writeln!(f, "    call init_stdio").unwrap()
    }

    for m in prog.instrs {
        writeln!(f, "{m}").unwrap();
    }

    writeln!(f, "    xor rax, rax").unwrap();
    writeln!(f, "    call ExitProcess").unwrap();
    writeln!(f).unwrap();
    writeln!(f, "    section .data").unwrap();

    for (l, i) in prog.strlits {
        writeln!(f, "S{i}: db {}0", fmt_str(&l)).unwrap()
    }

    for (l, i) in prog.fltlits {
        writeln!(f, "F{i}: dq {l}").unwrap();
    }
}
