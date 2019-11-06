//! Lua's Standard Library

use crate::ErrorKind;
use crate::Result;
use crate::State;
use crate::Val;

pub fn init(state: &mut State) {
    let globals = &mut state.globals;
    let mut global_insert = |name: &str, func| {
        globals.insert(name.to_string(), Val::RustFn(func));
    };
    global_insert("assert", lua_assert);
    global_insert("puts", lua_print);
}

fn lua_assert(state: &mut State) -> Result<u8> {
    if let Some(val) = state.locals.get(0) {
        if val.truthy() {
            Ok(0)
        } else {
            Err(state.error(ErrorKind::AssertionFail))
        }
    } else {
        Err(state.error(ErrorKind::TypeError))
    }
}

fn lua_print(state: &mut State) -> Result<u8> {
    let mut iter = state.locals.drain(..);
    if let Some(val) = iter.next() {
        print!("{}", val);
        for val in iter {
            print!("\t{}", val);
        }
        println!();
    }
    Ok(0)
}
