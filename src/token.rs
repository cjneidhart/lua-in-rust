#[derive(Debug, PartialEq)]
pub struct Token {
    pub typ: TokenType,
    pub start: usize,
    pub len: u32,
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    // Keywords
    And, Break, Do, Else, ElseIf, End, False, For, Function, If, In, Local,
    Nil, Not, Or, Repeat, Return, Then, True, Until, While,
    // Operator symbols
    Plus, Minus, Star, Slash, Mod, Caret, Hash,
    // Comparisons
    Equal, NotEqual, LessEqual, GreaterEqual, Less, Greater,
    // L/R stuff
    LParen, RParen, LCurly, RCurly, LSquare, RSquare,
    // Other symbols
    Semi, Colon, Comma, Dot, DotDot, DotDotDot, Assign,
    // Others
    Identifier,
    LiteralNumber,
    LiteralHexNumber,
    LiteralString,

    // Placeholder
    Print
}

impl Token {
    pub fn new(typ: TokenType, start: usize, len: u32) -> Self {
        Token { typ, start, len }
    }
}
