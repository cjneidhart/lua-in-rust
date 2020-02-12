//! Lua's Standard Library

use super::error::ErrorKind;
use super::LuaType;
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

    add("ipairs", |state| {
        state.check_type(1, LuaType::Table)?;
        state.set_top(1);
        state.push_rust_fn(|state| {
            state.check_type(1, LuaType::Table)?;
            state.check_type(2, LuaType::Number)?;
            state.set_top(2);
            let old_index = state.to_number(2).unwrap();
            let new_index = old_index + 1.0;
            state.pop(1); // pop the old number
            state.push_number(new_index);
            state.get_table(1)?;
            if state.to_boolean(-1) {
                state.push_number(new_index);
                state.replace(1); // Replaces the table with the index
                Ok(2)
            } else {
                state.set_top(0);
                state.push_nil();
                Ok(1)
            }
        });
        // Swap the table and function
        state.push_value(1);
        state.remove(1);
        // Push the initial index
        state.push_number(0.0);
        Ok(3)
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

    // unpack(list)
    //
    // Returns list[1], list[2], ... list[#list]. The Lua version can take
    // additional arguments to return only part of the list, but that isn't
    // supported yet.
    add("unpack", |state| {
        state.check_type(1, LuaType::Table)?;
        let mut i = 1.0;
        loop {
            state.push_number(i);
            state.get_table(1)?;
            if let LuaType::Nil = state.typ(-1) {
                state.pop(1);
                break;
            } else {
                i += 1.0;
            }
        }
        Ok(i as u8 - 1)
    });
}
