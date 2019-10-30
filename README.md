# lua-in-rust
The Lua programming language, implemented in Rust.

Goals

- [x] Basic comparisons and equality
- [x] `and`/`or` expressions with proper short-circuiting
- [x] Basic strings
- [x] `if`/`else`/`elseif`
- [x] `while` and `repeat` loops
- [x] Local variables with proper scoping
- [x] Numeric `for` loops
- [x] Multiple assignment
- [x] Single-line comments
- [ ] Function calls
- [ ] Function definition
- [ ] Tables
- [ ] Garbage Collection
- [ ] Metatables
- [ ] Lua's standard library
- [ ] A Rust API to parallel Lua's C API
- [ ] Coroutines
- [ ] Feature parity with Lua 5.1
- [ ] Multi-line comments
- [ ] Separate `luac` executable


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
