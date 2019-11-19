//! TODO: Crate-level description

#![warn(future_incompatible)]
#![warn(non_ascii_idents)]
#![warn(rust_2018_idioms)]
#![warn(single_use_lifetimes)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unreachable_pub)]
#![warn(unused)]
#![warn(variant_size_differences)]

mod compiler;
mod error;
mod instr;
mod lua_std;
mod vm;

use compiler::Chunk;
use error::{Error, ErrorKind};
use instr::Instr;

pub use vm::LuaType;
pub use vm::RustFunc;
pub use vm::State;

/// Custom result type for evaluating Lua.
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
