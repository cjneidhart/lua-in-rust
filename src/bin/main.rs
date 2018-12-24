use std::collections::HashMap;
use std::env::args;
use std::fs::read_to_string;
use std::io;
use std::io::Write;

use lua::eval;
use lua::lexer;
use lua::parser;

fn main() {
    let mut args = args();
    match args.nth(1) {
        None => run_prompt(),
        Some(s) => run_file(s.as_str()),
    }
}

fn run_file(filename: &str) {
    let source = read_to_string(filename).unwrap();
    lua::run_string(source.as_str());
}

fn run_prompt() {
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
        let chunk = match parser::parse_chunk(toks) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("parse error: {:?}", e);
                continue;
            }
        };
        eval::eval_chunk(&chunk, &mut env).unwrap();
    }
}