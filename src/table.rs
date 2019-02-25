use crate::lua_val::LuaVal;
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct Table {
    map: HashMap<LuaVal, LuaVal>,
}

impl Table {
    pub fn insert(&mut self, key: LuaVal, value: LuaVal) {
        self.map.insert(key, value);
    }
}
