use std::fmt;
use std::io;

use crate::LuaType;

#[derive(Debug)]
pub enum ErrorKind {
    AssertionFail,
    BadNumber,
    Complexity,
    InvalidCharacter,
    Io(io::Error),
    TooManyLocals,
    TooManyNumbers,
    TooManyStrings,
    UnclosedString,
    UnexpectedEof,
    UnexpectedTok,
    UnsupportedFeature,
    TableKeyNan,
    TableKeyNil,
    TypeError(TypeError),
    WithMessage(String),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    line_num: usize,
    column: usize,
}

#[derive(Debug)]
pub enum TypeError {
    Arithmetic(LuaType),
    Comparison(LuaType, LuaType),
    Concat(LuaType),
    FunctionCall(LuaType),
    Length(LuaType),
    TableIndex(LuaType),
}

impl ErrorKind {
    pub fn is_recoverable(&self) -> bool {
        match self {
            ErrorKind::UnclosedString | ErrorKind::UnexpectedEof => true,
            _ => false,
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match self {
            AssertionFail => write!(f, "assertion failed!"),
            BadNumber => write!(f, "malformed number"),
            Complexity => write!(f, "complexity"),
            InvalidCharacter => write!(f, "invalid character"),
            Io(e) => e.fmt(f),
            TooManyLocals => write!(f, "too many local variables"),
            TooManyNumbers => write!(f, "too many literal numbers"),
            TooManyStrings => write!(f, "too many literal strings"),
            UnclosedString => write!(f, "unfinished string"),
            UnexpectedEof => write!(f, "unexpected <eof>"),
            UnexpectedTok => write!(f, "syntax error"),
            UnsupportedFeature => write!(f, "unsupported feature"),
            TableKeyNan => write!(f, "table index was NaN"),
            TableKeyNil => write!(f, "table index was nil"),
            TypeError(e) => e.fmt(f),
            WithMessage(msg) => msg.fmt(f),
        }
    }
}

impl Error {
    pub fn new(kind: ErrorKind, line_num: usize, column: usize) -> Self {
        Error {
            kind,
            line_num,
            column,
        }
    }

    pub fn without_location(kind: ErrorKind) -> Self {
        Error::new(kind, 0, 0)
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn line_num(&self) -> usize {
        self.line_num
    }

    pub fn is_recoverable(&self) -> bool {
        self.kind.is_recoverable()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error {}:{}: {}", self.line_num, self.column, self.kind)
    }
}

impl From<io::Error> for Error {
    fn from(io_err: io::Error) -> Self {
        Error::without_location(ErrorKind::Io(io_err))
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeError::*;
        match self {
            Arithmetic(typ) => write!(f, "attempt to perform arithmetic on a {} value", typ),
            Comparison(type1, type2) => write!(f, "attempt to compare {} with {}", type1, type2),
            Concat(typ) => write!(f, "attempt to concatenate a {} value", typ),
            FunctionCall(typ) => write!(f, "attempt to call a {} value", typ),
            Length(typ) => write!(f, "attempt to get length of a {} value", typ),
            TableIndex(typ) => write!(f, "attempt to index a {} value", typ),
        }
    }
}
