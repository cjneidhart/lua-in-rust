#[derive(Debug, PartialEq)]
pub enum Instr {
    Print,
    Assign,
    GlobalLookup,
    // values
    PushNil,
    PushBool(bool),
    PushNum(f64),
    PushString(String),
    PushTable,
    // In: table, key, value
    // Out: table
    TableAssign,
    // binary operators
    Add,
    Subtract,
    Multiply,
    Divide,
    Pow,
    Mod,
    Concat,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
    // unary
    Not,
    Length,
    Negate,
}