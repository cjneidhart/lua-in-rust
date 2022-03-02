use std::ops::Range;

/// A lexical unit in the source code.
#[derive(Debug, PartialEq)]
pub(super) struct Token {
    pub(super) typ: TokenType,
    pub(super) start: usize,
    pub(super) len: u32,
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum TokenType {
    // Keywords
    And, Break, Do, Else, ElseIf, End, False, For, Function, If, In, Local,
    Nil, Not, Or, Repeat, Return, Then, True, Until, While,
    // Operator symbols
    Plus, Minus, Star, Slash, Mod, Caret, Hash,
    // Comparisons
    Equal, NotEqual, LessEqual, GreaterEqual, Less, Greater,
    // L/R stuff
    LParen, LParenLineStart, RParen, LCurly, RCurly, LSquare, RSquare,
    // Other symbols
    Semi, Colon, Comma, Dot, DotDot, DotDotDot, Assign,
    // Others
    Identifier,
    LiteralNumber,
    LiteralHexNumber,
    LiteralString,

    EndOfFile,
}

impl Token {
    pub(super) fn range(&self) -> Range<usize> {
        let start = self.start;
        let end = start + self.len as usize;
        start..end
    }
}
