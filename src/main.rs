mod lexer;
mod parser;
mod eval;
mod simple_types;

use std::io;
use std::io::Write;

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();
    loop {
        print!("> ");
        stdout.flush().unwrap();
        buf.clear();
        stdin.read_line(&mut buf).unwrap();
        let toks = match lexer::lex(buf.as_str()) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("lex error: {:?}", e);
                continue;
            }
        };
        let instrs = match parser::parse_expr(toks) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("parse error: {:?}", e);
                continue;
            }
        };
        let out = match eval::eval_expr(instrs) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("eval error: {:?}", e);
                continue;
            }
        };
        println!("{:?}", out);
    }
}
