use eval::LuaVal;
use std::collections::HashMap;

#[derive(Clone, Debug, Hash)]
pub struct LuaTable {
    arr: Vec<LuaVal>,
    map: HashMap<LuaVal, LuaVal>,
    // metatable: LuaVal,
}