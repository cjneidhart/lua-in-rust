#[rustfmt::skip]
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
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
    Identifier(String),
    LiteralNumber(f64),
    LiteralString(String),
    Eof,

    // Placeholder
    Print
}
