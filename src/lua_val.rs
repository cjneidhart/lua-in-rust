use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use crate::vm::State;

#[derive(Clone)]
pub enum LuaVal {
    Nil,
    Bool(bool),
    Number(f64),
    LuaString(Rc<String>),
    RustFn(fn(&mut State) -> u8),
}

impl LuaVal {
    pub fn truthy(&self) -> bool {
        match self {
            LuaVal::Nil | LuaVal::Bool(false) => false,
            _ => true,
        }
    }
}

impl Debug for LuaVal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LuaVal::*;
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => Debug::fmt(b, f),
            Number(n) => Debug::fmt(n, f),
            LuaString(s) => Debug::fmt(s, f),
            RustFn(func) => write!(f, "RustFn@{:p}", func),
        }
    }
}

impl Display for LuaVal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LuaVal::*;
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => Display::fmt(b, f),
            Number(n) => Display::fmt(n, f),
            LuaString(s) => Display::fmt(s, f),
            RustFn(func) => write!(f, "{:p}", func),
        }
    }
}

impl PartialEq for LuaVal {
    fn eq(&self, other: &LuaVal) -> bool {
        use LuaVal::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Number(a), Number(b)) => a == b,
            (LuaString(a), LuaString(b)) => a == b,
            _ => false,
        }
    }
}
