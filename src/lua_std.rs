//! Lua's Standard Library

use super::ErrorKind;
use super::State;

pub(super) fn init(state: &mut State) {
    let mut add = |name, func| {
        state.push_rust_fn(func);
        state.set_global(name);
    };

    // Issues an error when the value of its first argument is false; otherwise,
    // returns all its arguments. `message` is an error message; when absent,
    // it defaults to "assertion failed!".
    add("assert", |state| {
        state.check_any(1)?;
        if state.to_boolean(1) {
            state.remove(1);
            Ok(state.get_top() as u8)
        } else {
            state.remove(1);
            if state.get_top() == 0 {
                Err(state.error(ErrorKind::AssertionFail))
            } else {
                let s = state.to_string(1);
                Err(state.error(ErrorKind::WithMessage(s)))
            }
        }
    });

    // Receives any number of arguments, and prints their values to `stdout`.
    add("print", |state| {
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
    });

    // Returns the type of its only argument, coded as a string.
    add("type", |state| {
        state.check_any(1)?;
        let typ = state.typ(1);
        let type_str = typ.to_string();
        state.pop(state.get_top() as isize);
        state.push_string(type_str);
        Ok(1)
    });
}
