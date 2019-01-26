#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Instr {
    Jump(isize),
    BranchTrue(isize),
    BranchFalse(isize),
    BranchTrueKeep(isize),
    BranchFalseKeep(isize),
    Pop,

    Print,
    GetGlobal,
    SetGlobal,
    GetLocal(usize),
    SetLocal(usize),

    PushNil,
    PushBool(bool),
    PushNum(usize),
    PushString(usize),

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
