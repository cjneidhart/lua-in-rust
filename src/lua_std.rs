//! Lua's Standard Library

use crate::ErrorKind;
use crate::Result;
use crate::State;
use crate::Val;

pub fn init(state: &mut State) {
    let mut global_insert = |name, func| {
        state.set_global(name, Val::RustFn(func));
    };
    global_insert("assert", lua_assert);
    global_insert("print", lua_print);
}

fn lua_assert(state: &mut State) -> Result<u8> {
    // TODO fail with different message if assert receives no args.
    if state.get_top() >= 1 && state.to_boolean(1) {
        Ok(0)
    } else {
        Err(state.error(ErrorKind::AssertionFail))
    }
}

fn lua_print(state: &mut State) -> Result<u8> {
    let range = 1..=state.get_top();
    let mut strings = range.map(|i| state.to_string(i as isize));
    if let Some(s) = strings.next() {
        print!("{}", s);
        for s in strings {
            print!("\t{}", s);
        }
    }
    println!();
    Ok(0)
}
