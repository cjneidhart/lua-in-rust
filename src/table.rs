use crate::lua_val::LuaVal;
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct Table {
    map: HashMap<LuaVal, LuaVal>,
}

impl Table {
    pub fn get(&self, key: &LuaVal) -> LuaVal {
        self.map.get(key).cloned().unwrap_or(LuaVal::Nil)
    }

    pub fn insert(&mut self, key: LuaVal, value: LuaVal) {
        self.map.insert(key, value);
    }
}
