# lua-in-rust
The Lua programming language, implemented in Rust.

### Overview
The code is primarily grouped into three modules:
- `compiler` deals with parsing lua code and converting it into bytecode.
    - `lexer` converts Lua source code into tokens.
    - `parser` converts those tokens into bytecode.
    - `exp_desc` and `token` are type definitions.
- `vm` is the largest module. It deals with actually evaluating lua code, and
  the Rust API.
    - `vm` itself holds the core functionality of the interpreter.
    - `frame` deals with evaluating bytecode.
    - `lua_val` defines the type for values in the VM.
    - `object` deals with garbage collection.
    - `table` implements Lua tables.
- `lua_std` is where any functions in the lua standard library are implemented.
- Other modules:
    - `error` defines the `Error` type used throughout the crate.
    - `instr` defines the VM's instruction set.
    - `lib` and `main` are the basic entrypoints for the library/interpreter.

### Goals
The goals, in rough order of priority, are:
- [x] Basic comparisons and equality
- [x] `and`/`or` expressions with proper short-circuiting
- [x] Basic strings
- [x] `if`/`else`/`elseif`
- [x] `while` and `repeat ... until` loops
- [x] Local variables with proper scoping
- [x] Numeric `for` loops
- [x] Multiple assignment
- [x] Single-line comments
- [x] Function calls
- [x] Function definition
- [x] Tables
- [x] Garbage Collection
- [x] Full table literals
- [ ] Multiple return values
- [ ] `break` and `continue`
- [ ] Interned strings
- [ ] Unparenthesized function calls
- [ ] Better error messages
- [ ] Lua's `next` function
- [ ] Generic `for` loops
- [ ] Metatables
- [ ] Separate array part of tables for integer keys
- [ ] Lua's standard library
- [ ] A Rust API to parallel Lua's C API
- [ ] Coroutines
- [ ] Multi-line comments
- [ ] Use actual bytecode with variable-length instructions
- [ ] Separate `luac` executable

### Building
Like the real Lua, this project currently has zero dependencies.
Just run `cargo run` in the root to launch the interpreter.

### Debug options
There are a few environment options which enable debug features.
These are all disabled by default.
To enable an option, just set it in the environment before compiling
(e.g. `export LUA_DEBUG_VM=1; cargo build`).
For details on a debug option, look in the corresponding module.

The options are:
- `LUA_DEBUG_PARSER`
- `LUA_DEBUG_VM`
- `LUA_DEBUG_GC`
