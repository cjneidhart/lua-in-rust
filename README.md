# lua-in-rust
The Lua programming language, implemented in Rust.

### Overview
The code is primarily grouped into three modules:
- `compiler` deals with parsing lua code and converting it into bytecode.
- `vm` is the largest module. It deals with actually evaluating lua code, and
  the Rust API.
- `lua_std` is where any functions in the lua standard library are implemented.

### Goals

- [x] Basic comparisons and equality
- [x] `and`/`or` expressions with proper short-circuiting
- [x] Basic strings
- [x] `if`/`else`/`elseif`
- [x] `while` and `repeat` loops
- [x] Local variables with proper scoping
- [x] Numeric `for` loops
- [x] Multiple assignment
- [x] Single-line comments
- [x] Function calls
- [x] Function definition
- [x] Tables
- [x] Garbage Collection
- [ ] Interned strings
- [ ] Full table literals
- [ ] Unparenthesized function calls
- [ ] Generic `for` loops
- [ ] Metatables
- [ ] Lua's standard library
- [ ] A Rust API to parallel Lua's C API
- [ ] Coroutines
- [ ] Multi-line comments
- [ ] Use actual bytecode with variable-length instructions
- [ ] Separate `luac` executable
- [ ] Feature parity with Lua 5.1

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
