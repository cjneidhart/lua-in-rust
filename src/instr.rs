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
    GetGlobal(usize),
    SetGlobal(usize),

    GetLocal(usize),
    SetLocal(usize),

    PushNil,
    PushBool(bool),
    PushNum(usize),
    PushString(usize),

    ForPrep(usize),
    // Local to access , how far back to jump
    ForLoop(usize, usize),

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
