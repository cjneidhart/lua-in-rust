mod instr;
pub mod lexer;
mod lua_std;
mod lua_val;
pub mod parser;
mod table;
mod token;
pub mod vm;

pub fn run_string(source: &str) {
    let tokens = lexer::lex(source).unwrap();
    let chunk = parser::parse_chunk(tokens).unwrap();
    let mut state = vm::State::new();
    state.eval_chunk(chunk).unwrap();
}
