#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Instr {
    Jump(isize),
    BranchTrue(isize),
    BranchFalse(isize),
    BranchTrueKeep(isize),
    BranchFalseKeep(isize),
    Pop,

    Print,

    // For global variables, the usize is an index into the list of string constants.
    // This string is the variable's name.
    GetGlobal(u8),
    SetGlobal(u8),

    GetLocal(u8),
    SetLocal(u8),

    NewTable,

    // The single parameter is the index of the string literal
    GetField(u8),
    SetField(u8),

    PushNil,
    PushBool(bool),
    PushNum(u8),
    PushString(u8),

    ForPrep(usize),
    // Local to access , how far back to jump
    ForLoop(usize, isize),

    // Function call (number of arguments)
    Call(u8),

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

    // unary
    Not,
    Length,
    Negate,
}
