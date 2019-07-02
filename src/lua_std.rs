//! Lua's Standard Library

use crate::{LuaVal, State};

pub fn init(state: &mut State) {
    let globals = &mut state.globals;
    globals.insert("puts".to_string(), LuaVal::RustFn(print));
}

fn print(state: &mut State) -> u8 {
    let mut iter = state.locals.drain(..);
    if let Some(val) = iter.next() {
        print!("{}", val);
        for val in iter {
            print!("\t{}", val);
        }
        println!();
    }
    0
}
