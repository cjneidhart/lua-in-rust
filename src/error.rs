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
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Error {
            kind,
            line: 0,
            col: 0,
        }
    }

    pub fn is_recoverable(&self) -> bool {
        self.kind.is_recoverable()
    }
}
