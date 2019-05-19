use std::fmt::{self, Display, Formatter};

use crate::{Instr, LuaVal, Token, TokenType};

// TODO Make Error a more full-featured types, with
// - line/column numbers
// - backtraces

pub type Result<T> = std::result::Result<T, Error>;

/// A type for all errors that might happen in the execution of a lua program,
/// including during the compilation step.
#[derive(Debug, PartialEq)]
pub enum Error {
    /// The end of input was reached before a string was closed.
    UnclosedString(usize),
    /// An invalid character was read in while outside a string or comment.
    InvalidCharacter(u8, usize),
    /// A number literal was malformed.
    BadNumber(usize),

    /// This feature isn't supported yet.
    Unsupported,
    /// This token was unexpected.
    Unexpected(Token),
    /// A different type of token was expected.
    Expect(TokenType),
    /// The end of input was reached, but more was expected.
    UnexpectedEof,
    /// The chunk has too many number literals.
    TooManyNumbers,
    /// The chunk has too many string literals.
    TooManyStrings,
    /// The chunk has too many local variable identifiers.
    TooManyLocals,
    /// A single expression was too complex.
    // Currently this is only given on an explist which is too long.
    Complexity,

    /// A table assignment was performed with the key `NaN`.
    TableKeyNan,
    /// A table assignment was made with a `nil` key.
    TableKeyNil,
    /// A unary operand was applied to the wrong type of value.
    SingleTypeError(Instr, LuaVal),
    /// A binary operand was applied to values of incompatible types.
    DoubleTypeError(Instr, LuaVal, LuaVal),

    /// This error doesn't have an explicit variant yet.
    Other,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Error::*;
        match self {
            UnclosedString(i) => write!(f, "Unclosed string, starting at offset {}", i),
            InvalidCharacter(c, i) => write!(f, "Invalid character '{}' at offset {}", c, i),
            BadNumber(i) => write!(f, "Malformed number at offset {}", i),
            Unsupported => write!(f, "Sorry, this feature isn't supported yet."),
            Unexpected(tok) => write!(f, "Unexpected token: {:?}", tok),
            Expect(typ) => write!(f, "Expected a token of type {:?}", typ),
            UnexpectedEof => write!(f, "Unexpected End of File"),
            TooManyNumbers => write!(f, "The chunk has too many number literals"),
            TooManyStrings => write!(f, "The chunk has too many string literals"),
            TooManyLocals => write!(f, "The chunk has too many local variable identifiers"),
            Complexity => write!(f, "Encountered an expression which is too complex"),
            TableKeyNan => write!(f, "A table assignment was performed with the key `NaN`"),
            TableKeyNil => write!(f, "A table assignment was made with a `nil` key"),
            SingleTypeError(i, _) => write!(f, "Tried to perform {:?} on an invalid type", i),
            DoubleTypeError(i, ..) => write!(f, "Tried to perform {:?} on invalid types", i),
            Other => write!(f, "I haven't documented this error yet."),
        }
    }
}
