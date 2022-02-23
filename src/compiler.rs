//! Functions and types associated with converting source code into bytecode.

mod exp_desc;
mod lexer;
mod parser;
mod token;

use super::error;
use super::Instr;
use super::Result;

#[derive(Clone, Debug, Default, PartialEq)]
pub(super) struct Chunk {
    pub(super) code: Vec<Instr>,
    pub(super) number_literals: Vec<f64>,
    pub(super) string_literals: Vec<String>,
    pub(super) num_params: u8,
    pub(super) num_locals: u8,
    pub(super) nested: Vec<Chunk>,
}

pub(super) fn parse_str(source: impl AsRef<str>) -> Result<Chunk> {
    parser::parse_str(source.as_ref())
}
