//! Functions and types associated with converting source code into bytecode.

mod lexer;
mod parser;
mod token;

use super::Error;
use super::ErrorKind;
use super::Instr;
use super::Result;

use token::Token;
use token::TokenType;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Chunk {
    pub code: Vec<Instr>,
    pub number_literals: Vec<f64>,
    pub string_literals: Vec<String>,
    pub num_locals: u8,
    pub nested: Vec<Chunk>,
}

pub(super) fn parse_str(source: impl AsRef<str>) -> Result<Chunk> {
    parser::parse_str(source.as_ref())
}
