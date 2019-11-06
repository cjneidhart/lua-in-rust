//! Functions and types associated with converting source code into bytecode.

mod lexer;
mod parser;
mod token;

use crate::{Instr, Result};

pub use token::{Token, TokenType};

#[derive(Debug, Default, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub number_literals: Vec<f64>,
    pub string_literals: Vec<String>,
    pub num_locals: u8,
}

pub fn parse_str(source: impl AsRef<str>) -> Result<Chunk> {
    parser::parse_str(source.as_ref())
}
