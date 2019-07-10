#[derive(Debug)]
pub enum ErrorKind {
    BadNumber,
    Complexity,
    InvalidCharacter,
    TooManyLocals,
    TooManyNumbers,
    TooManyStrings,
    UnclosedString,
    UnexpectedEof,
    UnexpectedTok,
    UnsupportedFeature,
    TableKeyNan,
    TableKeyNil,
    TypeError,
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    line: usize,
    col: u32,
}

impl ErrorKind {
    pub fn is_recoverable(&self) -> bool {
        match self {
            ErrorKind::UnclosedString | ErrorKind::UnexpectedEof => true,
            _ => false,
        }
    }

    pub fn message(&self) -> &str {
        use ErrorKind::*;
        match self {
            BadNumber => "malformed number",
            Complexity => "complexity",
            InvalidCharacter => "invalid character",
            TooManyLocals => "too many local variables",
            TooManyNumbers => "too many literal numbers",
            TooManyStrings => "too many literal strings",
            UnclosedString => "unfinished string",
            UnexpectedEof => "unexpected <eof>",
            UnexpectedTok => "syntax error",
            UnsupportedFeature => "unsupported feature",
            TableKeyNan => "table index was NaN",
            TableKeyNil => "table index was nil",
            TypeError => "type error",
        }
    }
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Error {
            kind,
            line: 0,
            col: 0,
        }
    }

    pub fn get_msg(&self) -> &str {
        self.kind.message()
    }

    pub fn is_recoverable(&self) -> bool {
        self.kind.is_recoverable()
    }
}
