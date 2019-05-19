mod error;
mod instr;
mod lexer;
mod lua_std;
mod lua_val;
mod parser;
mod table;
mod token;
mod vm;

use error::{Error, Result};
use instr::Instr;
use lexer::TokenList;
use lua_val::LuaVal;
use parser::Chunk;
use table::Table;
use token::{Token, TokenType};

pub use vm::State;

pub fn run_string(source: &str) {
    let tokens = lexer::lex(source.as_bytes()).unwrap();
    let chunk = parser::parse_chunk(tokens).unwrap();
    let mut state = vm::State::new();
    state.eval_chunk(chunk).unwrap();
}
