use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum LuaVal {
    Nil,
    Bool(bool),
    Number(f64),
    LuaString(String),
    //Table(LuaTable),
    // Function(LuaFunc),
}

impl LuaVal {
    pub fn truthy(&self) -> bool {
        match self {
            LuaVal::Nil | LuaVal::Bool(false) => false,
            _ => true,
        }
    }
}

impl Display for LuaVal {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use self::LuaVal::*;
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => Display::fmt(b, f),
            Number(n) => Display::fmt(n, f),
            LuaString(s) => Display::fmt(s, f),
        }
    }
}
