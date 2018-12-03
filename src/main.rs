extern crate lua;

use std::collections::HashMap;
use std::io;
use std::io::Write;

use lua::lexer;
use lua::parser;
use lua::eval;

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();
    let mut env = HashMap::new();
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
        let instrs = match parser::parse_stmt(toks) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("parse error: {:?}", e);
                continue;
            }
        };
        eval::eval_stmt(instrs, &mut env).unwrap();
    }
}
