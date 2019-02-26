mod instr;
mod lexer;
mod lua_std;
mod lua_val;
mod parser;
mod table;
mod token;
pub mod vm;

pub fn run_string(source: &str) {
    let tokens = lexer::lex(source.as_bytes()).unwrap();
    let chunk = parser::parse_chunk(tokens).unwrap();
    let mut state = vm::State::new();
    state.eval_chunk(chunk).unwrap();
}
