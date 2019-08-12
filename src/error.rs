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
    line_num: usize,
    column: usize,
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

    pub fn message(&self) -> &str {
        self.kind.message()
    }

    pub fn is_recoverable(&self) -> bool {
        self.kind.is_recoverable()
    }
}
