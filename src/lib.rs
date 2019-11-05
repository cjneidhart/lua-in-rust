mod compiler;
mod error;
mod instr;
mod lua_std;
mod lua_val;
mod object;
mod table;
mod vm;

use error::{Error, ErrorKind};
use instr::Instr;
use lua_val::Val;
use object::{GcHeap, Markable, ObjectPtr};
use table::Table;

pub use compiler::Chunk;
pub use vm::State;

pub type Result<T> = std::result::Result<T, Error>;

pub fn run_string<S: AsRef<str>>(src: S) {
    let chunk = compiler::parse_str(src.as_ref()).unwrap();
    let mut state = vm::State::new();
    state.eval_chunk(chunk).unwrap();
}
