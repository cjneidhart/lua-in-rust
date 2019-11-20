/// Instr is the instruction which can be read by the VM.
///
/// Many of the variants use an `isize` parameter, as an offset for the VM to
/// jump.
///
/// Several others use a u8 parameter to index either the locals, the number
/// literals, or the string literals.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum Instr {
    /// Move the instruction pointer by the given offset.
    Jump(isize),

    /// Pop a value from the stack. If it's truthy, add the given offset to the
    /// instruction pointer.
    //BranchTrue(isize),

    /// Pop a value from the stack. If it's falsey, add the given offset to the
    /// instruction pointer.
    BranchFalse(isize),

    /// Examine, but do not pop a value from the stack. If it's truthy, add the
    /// given offset to the instruction pointer.
    BranchTrueKeep(isize),

    /// Examine, but do not pop a value from the stack. If it's falsey, add the
    /// given offset to the instruction pointer.
    BranchFalseKeep(isize),

    /// Pop a value from the stack and discard it.
    Pop,

    /// Use the param as an index into the string literal set. Using that
    /// string, index the global table and push onto the stack.
    GetGlobal(u8),

    /// Use the param as an index into the string literal set. Using that
    /// string as a key, pop a value from the stack and assign to the global
    /// table.
    SetGlobal(u8),

    /// Copy the given local to the top of the stack.
    GetLocal(u8),

    /// Pop the value at the top of the stack, and place it in the given local
    /// index.
    SetLocal(u8),

    /// Create a new table and place it on the stack.
    NewTable,

    /// Index the table on the top of the stack, with the string found in the
    /// literal set at the given index.
    GetField(u8),

    /// Assign to a table. The key will be string literal `op1`.
    /// From the top, the stack should contain:
    /// * The new value
    /// * `op0` number of other values
    /// * The table
    SetField(u8, u8),

    /// Assign the value `stack[0]` to the table `stack[-1]` using the string
    /// literal `op0` as the key. Leave the table on the stack afterwards.
    InitField(u8),

    /// Get a value from a table.
    GetTable,

    /// Assign to a table.
    /// From the top, the stack should contain:
    /// * The new value
    /// * The given number of other values, which will be ignored
    /// * The key
    /// * The table
    SetTable(u8),

    /// Push a `nil` value onto the stack.
    PushNil,

    /// Push the given boolean value onto the stack.
    PushBool(bool),

    /// Fetch the number (float) from the literal set at the given index.
    PushNum(u8),

    /// Fetch the string from the literal set at the given index.
    PushString(u8),

    /// Initializes a for loop, which will use the four local slots starting
    /// at `param0`. End the loop by jumping `param1` forward.
    ForPrep(u8, isize),

    /// End a for loop, using the locals starting at the first parameter to
    /// track its progress. If the loop isn't over, jump using the second
    /// parameter.
    ForLoop(u8, isize),

    // Function call (number of arguments)
    Call(u8),

    /// Add the two values on the top of the stack.
    Add,

    /// Subtract the top value on the stack from the second value on the stack.
    Subtract,

    /// Multiply the two values on the top of the stack.
    Multiply,

    /// Divide the second value on the stack by the first.
    Divide,

    /// Raise the second value on the stack to the power of the first.
    Pow,

    /// Take the remainder, after dividing the second value on the stack by the
    /// first.
    Mod,

    /// Concatenate the two values on the top of the stack.
    Concat,

    /// `true` if the second value on the stack is less than the first; `false`
    /// otherwise.
    Less,

    /// `true` if the second value on the stack is less than or equal to the
    /// first; `false` otherwise.
    LessEqual,

    /// `true` if the second value on the stack is greater than the first;
    /// `false` otherwise.
    Greater,

    /// `true` if the second value on the stack is greater than or equal to the
    /// first; `false` otherwise.
    GreaterEqual,

    /// `true` if and only if the two values at the top of the stack are equal.
    Equal,

    /// `true` if and only if the two values at the top of the stack are not
    /// equal.
    NotEqual,

    /// `true` if the value at the top of the stack is `false` or `nil`,
    /// `false` otherwise.
    Not,

    /// Applies the length operator (`#`) to the value at the top of the stack.
    Length,

    /// Applies the unary negation operator to the value at the top of the
    /// stack.
    Negate,

    /// Return from the chunk.
    Return,

    /// Create a closure from a Chunk and push it onto the stack.
    Closure(u8),
}
