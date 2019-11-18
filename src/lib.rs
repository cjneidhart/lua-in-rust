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
pub use lua_val::LuaType;
pub use vm::State;

pub type Result<T> = std::result::Result<T, Error>;

pub fn run_string<S: AsRef<str>>(src: S) -> Result<()> {
    let chunk = compiler::parse_str(src.as_ref()).unwrap();
    let mut state = vm::State::new();
    state.eval_chunk(chunk)
}

pub fn run_file(filename: impl AsRef<std::path::Path>) -> Result<()> {
    use std::fs::read_to_string;
    let text = read_to_string(filename.as_ref()).map_err(Error::from_io_error)?;
    run_string(text)
}
