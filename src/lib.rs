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

pub use error::{Error, ErrorKind};
pub use vm::LuaType;
pub use vm::RustFunc;
pub use vm::State;

use compiler::Chunk;
use instr::Instr;

/// Custom result type for evaluating Lua.
pub type Result<T> = std::result::Result<T, Error>;
