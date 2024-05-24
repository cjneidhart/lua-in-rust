use super::object::{ObjectPtr, StringPtr};
use super::Chunk;
use super::Markable;
use super::Result;
use super::State;
use super::Table;

use std::fmt;
use std::hash::{Hash, Hasher};

pub type RustFunc = fn(&mut State) -> Result<u8>;

#[derive(Clone, Default)]
pub(super) enum Val {
    #[default]
    Nil,
    Bool(bool),
    Num(f64),
    Str(StringPtr),
    RustFn(RustFunc),
    Obj(ObjectPtr),
}
use Val::*;

impl Val {
    pub(super) fn as_lua_function(&self) -> Option<Chunk> {
        if let Obj(o) = self {
            o.as_lua_function()
        } else {
            None
        }
    }

    pub(super) fn as_num(&self) -> Option<f64> {
        match self {
            Num(f) => Some(*f),
            _ => None,
        }
    }

    pub(super) fn as_string(&self) -> Option<&str> {
        if let Str(s) = self {
            Some(s.as_str())
        } else {
            None
        }
    }

    pub(super) fn as_table(&mut self) -> Option<&mut Table> {
        if let Obj(o) = self {
            o.as_table()
        } else {
            None
        }
    }

    pub(super) fn truthy(&self) -> bool {
        !matches!(self, Nil | Bool(false))
    }

    /// Returns the value's type.
    pub(super) fn typ(&self) -> LuaType {
        match self {
            Nil => LuaType::Nil,
            Bool(_) => LuaType::Boolean,
            Num(_) => LuaType::Number,
            RustFn(_) => LuaType::Function,
            Str(_) => LuaType::String,
            Obj(o) => o.typ(),
        }
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => b.fmt(f),
            Num(n) => n.fmt(f),
            RustFn(func) => write!(f, "<function: {:p}>", func),
            Obj(o) => o.fmt(f),
            Str(s) => s.fmt(f),
        }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => b.fmt(f),
            Num(n) => n.fmt(f),
            Obj(o) => o.fmt(f),
            _ => write!(f, "{:#?}", self),
        }
    }
}

/// This is very dangerous, since f64 doesn't implement Eq.
impl Eq for Val {}

impl Hash for Val {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            Nil => (),
            Bool(b) => b.hash(hasher),
            Obj(o) => o.hash(hasher),
            Num(n) => {
                debug_assert!(!n.is_nan(), "Can't hash NaN");
                let mut bits = n.to_bits();
                if bits == 1 << 63 {
                    bits = 0;
                }
                bits.hash(hasher);
            },
            RustFn(func) => {
                let f: *const RustFunc = func;
                f.hash(hasher);
            },
            Str(s) => s.hash(hasher),
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Val) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Num(a), Num(b)) => a == b,
            (RustFn(a), RustFn(b)) => {
                let x: *const RustFunc = a;
                let y: *const RustFunc = b;
                x == y
            }
            (Obj(a), Obj(b)) => a == b,
            (Str(a), Str(b)) => StringPtr::eq_physical(a, b),
            _ => false,
        }
    }
}

impl Markable for Val {
    fn mark_reachable(&self) {
        match self {
            Obj(o) => o.mark_reachable(),
            Str(s) => s.mark_reachable(),
            _ => (),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum LuaType {
    Nil,
    Boolean,
    Number,
    String,
    Table,
    Function,
}

impl LuaType {
    pub fn as_str(&self) -> &'static str {
        use LuaType::*;
        match self {
            Nil => "nil",
            Boolean => "boolean",
            Number => "number",
            String => "string",
            Table => "table",
            Function => "function",
        }
    }
}

impl fmt::Display for LuaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}
