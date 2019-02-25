use std::cell::RefCell;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::table::Table;
use crate::vm::State;

type RustFunc = fn(&mut State) -> u8;

#[derive(Clone)]
pub enum LuaVal {
    Nil,
    Bool(bool),
    Number(f64),
    LuaString(Rc<String>),
    RustFn(RustFunc),
    Tbl(Rc<RefCell<Table>>),
}
use LuaVal::*;

impl LuaVal {
    pub fn new_table() -> Self {
        Tbl(Rc::new(RefCell::new(Table::default())))
    }

    pub fn truthy(&self) -> bool {
        match self {
            Nil | Bool(false) => false,
            _ => true,
        }
    }

    pub fn type_string(&self) -> String {
        let s = match self {
            Nil => "nil",
            Bool(_) => "boolean",
            Number(_) => "number",
            LuaString(_) => "string",
            RustFn(_) => "function",
            Tbl(_) => "table",
        };
        s.to_string()
    }
}

impl Debug for LuaVal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => Debug::fmt(b, f),
            Number(n) => Debug::fmt(n, f),
            LuaString(s) => Debug::fmt(s, f),
            RustFn(func) => write!(f, "<RustFn@{:p}>", func),
            Tbl(t) => write!(f, "<Table@{:p}>", t.as_ref()),
        }
    }
}

impl Display for LuaVal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => Display::fmt(b, f),
            Number(n) => Display::fmt(n, f),
            LuaString(s) => Display::fmt(s, f),
            RustFn(func) => write!(f, "<RustFn@{:p}>", func),
            Tbl(t) => write!(f, "<Table@{:p}>", t.as_ref()),
        }
    }
}

/// This is very dangerous, since f64 doesn't implement Eq.
impl Eq for LuaVal {}

impl Hash for LuaVal {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match self {
            Nil => (),
            Bool(b) => b.hash(hasher),
            Number(n) => {
                debug_assert!(!n.is_nan(), "Can't hash NaN");
                let mut bits = n.to_bits();
                if bits == 1 << 63 {
                    bits = 0;
                }
                bits.hash(hasher);
            }
            LuaString(s) => s.hash(hasher),
            RustFn(func) => {
                let f = func as *const RustFunc;
                f.hash(hasher);
            }
            Tbl(t) => {
                let ptr = t.as_ref().as_ptr();
                ptr.hash(hasher);
            }
        }
    }
}

impl PartialEq for LuaVal {
    fn eq(&self, other: &LuaVal) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Number(a), Number(b)) => a == b,
            (LuaString(a), LuaString(b)) => a == b,
            (RustFn(a), RustFn(b)) => {
                let x = a as *const RustFunc;
                let y = b as *const RustFunc;
                x == y
            }
            (Tbl(a), Tbl(b)) => {
                let x = a.as_ref().as_ptr();
                let y = b.as_ref().as_ptr();
                x == y
            }
            _ => false,
        }
    }
}
