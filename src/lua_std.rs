//! Lua's Standard Library

use super::ErrorKind;
use super::Result;
use super::State;

pub(super) fn init(state: &mut State) {
    let mut add = |name, func| {
        state.push_rust_fn(func);
        state.set_global(name);
    };
    add("assert", lua_assert);
    add("print", lua_print);
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
