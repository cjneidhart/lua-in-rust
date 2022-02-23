//! This module holds enums which describe the different types of Lua
//! expressions.

#[derive(Debug)]
pub(super) enum ExpDesc {
    Prefix(PrefixExp),
    Other,
}

/// A "prefix expression" is an expression which could be followed by certain
/// extensions and still be a valid expression.
#[derive(Clone, Debug)]
pub(super) enum PrefixExp {
    /// One of the variants of `PlaceExp`
    Place(PlaceExp),
    /// A function call, and the number of arguments
    FunctionCall(u8),
    /// An expression wrapped in parentheses
    Parenthesized,
}

/// This represents an expression which can appear on the left-hand side of an assignment.
/// Also called an "lvalue" in other languages.
#[derive(Clone, Debug)]
pub(super) enum PlaceExp {
    /// A local variable, and its index in the list of locals
    Local(u8),
    /// A global variable, and its index in the list of string literals
    Global(u8),
    /// A table index, with `[` and `]`
    TableIndex,
    /// A field access, and the index of the field's identifier in the list of
    /// string literals
    FieldAccess(u8),
}

impl From<PrefixExp> for ExpDesc {
    fn from(exp: PrefixExp) -> Self {
        Self::Prefix(exp)
    }
}

impl From<PlaceExp> for PrefixExp {
    fn from(exp: PlaceExp) -> Self {
        Self::Place(exp)
    }
}
