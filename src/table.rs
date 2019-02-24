use crate::lua_val::LuaVal;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Table {
    map: HashMap<LuaVal, LuaVal>,
}
