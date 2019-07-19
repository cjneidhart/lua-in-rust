use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::object::Markable;
use crate::ObjectPtr;
use crate::State;

type RustFunc = fn(&mut State) -> u8;

#[derive(Clone)]
pub enum Val {
    Nil,
    Bool(bool),
    Num(f64),
    Str(Rc<String>),
    RustFn(RustFunc),
    Obj(ObjectPtr),
}
use Val::*;

impl Val {
    pub fn as_num(&self) -> Option<f64> {
        match self {
            Num(f) => Some(*f),
            _ => None,
        }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Nil | Bool(false) => false,
            _ => true,
        }
    }

    pub fn type_string(&self) -> &str {
        match self {
            Nil => "nil",
            Bool(_) => "boolean",
            Num(_) => "Num",
            Str(_) => "string",
            RustFn(_) => "function",
            Obj(o) => o.raw.type_string(),
        }
    }
}

impl Debug for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => Debug::fmt(b, f),
            Num(n) => Debug::fmt(n, f),
            Str(s) => Debug::fmt(s, f),
            RustFn(func) => write!(f, "<function: {:p}>", func),
            Obj(o) => Debug::fmt(o, f),
        }
    }
}

impl Default for Val {
    fn default() -> Self {
        Nil
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => Display::fmt(b, f),
            Num(n) => Display::fmt(n, f),
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
            Str(s) => s.hash(hasher),
            Obj(o) => o.hash(hasher),
            Num(n) => {
                debug_assert!(!n.is_nan(), "Can't hash NaN");
                let mut bits = n.to_bits();
                if bits == 1 << 63 {
                    bits = 0;
                }
                bits.hash(hasher);
            }
            RustFn(func) => {
                let f = func as *const RustFunc;
                f.hash(hasher);
            }
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Val) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Num(a), Num(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            (RustFn(a), RustFn(b)) => {
                let x = a as *const RustFunc;
                let y = b as *const RustFunc;
                x == y
            }
            (Obj(a), Obj(b)) => a == b,
            _ => false,
        }
    }
}

impl Markable for Val {
    fn mark_reachable(&self) {
        if let Obj(o) = self {
            o.mark_reachable();
        }
    }
}
