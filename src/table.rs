use crate::{EvalError, LuaVal};

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct Table {
    map: HashMap<LuaVal, LuaVal>,
}

impl Table {
    pub fn get(&self, key: &LuaVal) -> LuaVal {
        match key {
            LuaVal::Nil => LuaVal::Nil,
            LuaVal::Number(n) if n.is_nan() => LuaVal::Nil,
            _ => self.map.get(key).cloned().unwrap_or_default(),
        }
    }

    pub fn insert(&mut self, key: LuaVal, value: LuaVal) -> Result<(), EvalError> {
        match key {
            LuaVal::Nil => Err(EvalError::TableKeyNil),
            LuaVal::Number(n) if n.is_nan() => Err(EvalError::TableKeyNan),
            _ => {
                self.map.insert(key, value);
                Ok(())
            }
        }
    }
}
