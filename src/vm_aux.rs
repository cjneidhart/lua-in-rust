//! More functions on State.
//!
//! Analagous to the Lua header `lauxlib.h`, which contains all the `luaL_*`
//! functions.

use std::fs::File;
use std::path::Path;

use crate::error::ArgError;
use crate::error::ErrorKind;
use crate::lua_std;
use crate::LuaType;
use crate::Result;
use crate::State;

impl State {
    pub fn check_any(&mut self, arg_number: isize) -> Result<()> {
        assert!(arg_number != 0);
        if self.get_top() < arg_number.abs() as usize {
            let e = ArgError {
                arg_number,
                func_name: None,
                expected: None,
                received: None,
            };
            Err(self.error(ErrorKind::ArgError(e)))
        } else {
            Ok(())
        }
    }

    pub fn check_type(&mut self, arg_number: isize, expected_type: LuaType) -> Result<()> {
        assert!(arg_number != 0);
        if self.get_top() < arg_number.abs() as usize {
            let e = ArgError {
                arg_number,
                func_name: None,
                expected: Some(expected_type),
                received: None,
            };
            return Err(self.error(ErrorKind::ArgError(e)));
        }
        let received_type = self.typ(arg_number);
        if received_type != expected_type {
            let e = ArgError {
                arg_number,
                func_name: None,
                expected: Some(expected_type),
                received: Some(received_type),
            };
            return Err(self.error(ErrorKind::ArgError(e)));
        }
        Ok(())
    }

    /// Loads and runs the given file.
    pub fn do_file(&mut self, filename: impl AsRef<Path>) -> Result<()> {
        self.load_file(filename)?;
        self.call(0, 0)
    }

    /// Loads and runs the given string.
    pub fn do_string(&mut self, s: impl AsRef<str>) -> Result<()> {
        self.load_string(s)?;
        self.call(0, 0)
    }

    /// Loads a file as a Lua chunk. This function uses `load` to load the chunk
    /// in the file named `filename`.
    ///
    /// This function only loads the chunk; it does not run it.
    pub fn load_file(&mut self, filename: impl AsRef<Path>) -> Result<()> {
        let mut reader = File::open(filename)?;
        self.load(&mut reader)
    }

    /// Opens all standard Lua libraries.
    pub fn open_libs(&mut self) {
        lua_std::open_libs(self)
    }
}
