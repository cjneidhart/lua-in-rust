use std::fmt;
use std::io;

use crate::LuaType;

// Types

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    line_num: usize,
    column: usize,
}

#[derive(Debug)]
pub enum ErrorKind {
    AssertionFail,
    Io(io::Error),
    UnsupportedFeature,
    TypeError(TypeError),
    WithMessage(String),
    ArgError(ArgError),
    SyntaxError(SyntaxError),
}

#[derive(Debug)]
pub struct ArgError {
    pub arg_number: isize,
    pub func_name: Option<String>,
    pub expected: Option<LuaType>,
    pub received: Option<LuaType>,
}

#[derive(Debug)]
pub enum SyntaxError {
    BadNumber,
    Complexity,
    InvalidCharacter,
    TooManyLocals,
    TooManyNumbers,
    TooManyStrings,
    UnclosedString,
    UnexpectedEof,
    UnexpectedTok,
}

#[derive(Debug)]
pub enum TypeError {
    Arithmetic(LuaType),
    Comparison(LuaType, LuaType),
    Concat(LuaType),
    FunctionCall(LuaType),
    Length(LuaType),
    TableIndex(LuaType),
    TableKeyNan,
    TableKeyNil,
}

// main impls

impl Error {
    pub fn new(kind: impl Into<ErrorKind>, line_num: usize, column: usize) -> Self {
        Error {
            kind: kind.into(),
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

impl ErrorKind {
    pub fn is_recoverable(&self) -> bool {
        if let Self::SyntaxError(e) = self {
            e.is_recoverable()
        } else {
            false
        }
    }
}

impl SyntaxError {
    pub fn is_recoverable(&self) -> bool {
        match self {
            Self::UnclosedString | Self::UnexpectedEof => true,
            _ => false,
        }
    }
}

// `Display` impls

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.line_num, self.column, self.kind)
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match self {
            AssertionFail => write!(f, "assertion failed!"),
            ArgError(e) => e.fmt(f),
            Io(e) => e.fmt(f),
            SyntaxError(e) => e.fmt(f),
            TypeError(e) => e.fmt(f),
            UnsupportedFeature => write!(f, "unsupported feature"),
            WithMessage(msg) => msg.fmt(f),
        }
    }
}

impl fmt::Display for ArgError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let func_name = match &self.func_name {
            Some(s) => s.as_str(),
            None => "?",
        };
        let extra = match (&self.expected, &self.received) {
            (Some(expected), Some(got)) => format!("{} expected, got {}", expected, got),
            (Some(expected), None) => format!("{} expected, got no value", expected),
            (None, _) => "value expected".into(),
        };

        write!(
            f,
            "bad argument #{} to {} ({})",
            self.arg_number, func_name, extra
        )
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SyntaxError::*;
        match self {
            BadNumber => write!(f, "malformed number"),
            Complexity => write!(f, "complexity"),
            InvalidCharacter => write!(f, "invalid character"),
            TooManyLocals => write!(f, "too many local variables"),
            TooManyNumbers => write!(f, "too many literal numbers"),
            TooManyStrings => write!(f, "too many literal strings"),
            UnclosedString => write!(f, "unfinished string"),
            UnexpectedEof => write!(f, "unexpected <eof>"),
            UnexpectedTok => write!(f, "syntax error"),
        }
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
            TableKeyNan => write!(f, "table index was NaN"),
            TableKeyNil => write!(f, "table index was nil"),
        }
    }
}

// `From` impls

impl From<io::Error> for Error {
    fn from(io_err: io::Error) -> Self {
        Error::without_location(ErrorKind::Io(io_err))
    }
}

impl From<ArgError> for ErrorKind {
    fn from(e: ArgError) -> Self {
        Self::ArgError(e)
    }
}

impl From<SyntaxError> for ErrorKind {
    fn from(e: SyntaxError) -> Self {
        Self::SyntaxError(e)
    }
}

impl From<TypeError> for ErrorKind {
    fn from(e: TypeError) -> Self {
        Self::TypeError(e)
    }
}
