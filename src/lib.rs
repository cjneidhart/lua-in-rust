pub mod eval;
mod instr;
pub mod lexer;
mod lua_val;
pub mod parser;
mod token;

use std::collections::HashMap;

pub fn run_string(source: &str) {
    let tokens = lexer::lex(source).unwrap();
    let chunk = parser::parse_chunk(tokens).unwrap();
    let mut env = HashMap::new();
    eval::eval_chunk(&chunk, &mut env).unwrap();
}
