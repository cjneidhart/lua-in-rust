mod error;
mod instr;
mod lexer;
mod lua_std;
mod lua_val;
mod object;
mod parser;
mod table;
mod token;
mod vm;

use error::{Error, ErrorKind};
use instr::Instr;
use lua_val::Val;
use object::{GcHeap, ObjectPtr};
use table::Table;
use token::{Token, TokenType};

pub use parser::Chunk;
pub use vm::State;

pub type Result<T> = std::result::Result<T, Error>;

pub fn run_string<S: AsRef<str>>(src: S) {
    let chunk = parser::parse_str(src.as_ref()).unwrap();
    let mut state = vm::State::new();
    state.eval_chunk(chunk).unwrap();
}
