mod error;
mod instr;
mod lexer;
mod lua_std;
mod lua_val;
mod parser;
mod table;
mod token;
mod vm;

use error::{Error, ErrorKind};
use instr::Instr;
use lua_val::LuaVal;
use parser::Chunk;
use table::Table;
use token::{Token, TokenType};
use vm::EvalError;

pub use vm::State;

pub type Result<T> = std::result::Result<T, Error>;

pub fn run_string(source: &str) {
    let chunk = parser::parse_str(source).unwrap();
    let mut state = vm::State::new();
    state.eval_chunk(chunk).unwrap();
}
