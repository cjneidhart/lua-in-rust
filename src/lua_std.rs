//! Lua's Standard Library

use super::Error;
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
    add("type", lua_type);
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

fn lua_type(state: &mut State) -> Result<u8> {
    if state.get_top() == 0 {
        let message = "bad argument #1 to 'type' (value expected)".to_string();
        return Err(Error::new(ErrorKind::WithMessage(message), 0, 0));
    }

    let typ = state.typ(1);
    let typ_string = typ.to_string();
    state.pop(1);
    state.push_string(typ_string);

    Ok(1)
}
