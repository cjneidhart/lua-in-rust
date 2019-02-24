use crate::lua_val::LuaVal;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct LuaTable {
    map: HashMap<LuaVal, LuaVal>,
}
