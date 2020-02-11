//! This module provides the `State` struct, which handles the primary
//! components of the VM.

mod frame;
mod lua_val;
mod object;
mod table;

pub use lua_val::LuaType;
pub use lua_val::RustFunc;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;

use super::compiler;
use super::error::ArgError;
use super::error::TypeError;
use super::lua_std;
use super::Chunk;
use super::Error;
use super::ErrorKind;
use super::Instr;
use super::Result;

use frame::Frame;
use lua_val::Val;
use object::{GcHeap, Markable};
use table::Table;

/// The main interface into the Lua VM.
pub struct State {
    /// The global environment. This may be changed to an actual Table in the future.
    globals: HashMap<String, Val>,
    /// The main stack which stores values.
    stack: Vec<Val>,
    /// The bottom index of the current frame in the stack.
    stack_bottom: usize,
    /// The heap which holds any garbage-collected Objects.
    heap: GcHeap,
}

// Important note on how the stack is tracked:
// A State uses a single stack for all local variables, temporary values,
// function arguments, and function return values. Both Lua frames and Rust
// frames use this stack. `self.stack_bottom` refers to the first value in the
// stack which belongs to the current frame. Note that Rust functions access
// the stack using 1-based indexing, but Lua code uses 0-based indexing.

impl Markable for State {
    fn mark_reachable(&self) {
        for val in self.globals.values() {
            val.mark_reachable();
        }
        for val in &self.stack {
            val.mark_reachable();
        }
    }
}

impl State {
    const GC_INITIAL_THRESHOLD: usize = 20;

    /// Creates a new, independent state. This corresponds to the `lua_newstate`
    /// function in the C API.
    pub fn new() -> Self {
        let mut me = Self::empty();
        me.open_libs();
        me
    }

    /// Creates a new state without opening any of the standard libs.
    /// The global namespace of this state is entirely empty.
    pub fn empty() -> Self {
        Self {
            globals: HashMap::new(),
            stack: Vec::new(),
            stack_bottom: 0,
            heap: GcHeap::with_threshold(Self::GC_INITIAL_THRESHOLD),
        }
    }

    /// Calls a function.
    ///
    /// To call a function you must use the following protocol: first, the
    /// function to be called is pushed onto the stack; then, the arguments to
    /// the function are pushed in direct order; that is, the first argument is
    /// pushed first. Finally you call `lua_call`; `num_args` is the number of
    /// arguments that you pushed onto the stack. All arguments and the function
    /// value are popped from the stack when the function is called. The
    /// function results are pushed onto the stack when the function returns.
    /// The number of results is adjusted to `num_ret_expected`. The function
    /// results are pushed onto the stack in direct order (the first result is
    /// pushed first), so that after the call the last result is on the top of
    /// the stack.
    pub fn call(&mut self, num_args: u8, num_ret_expected: u8) -> Result<()> {
        assert!(num_ret_expected <= 1, "Can't return multiple values yet.");
        let idx = self.stack.len() - num_args as usize - 1;
        let func_val = self.stack.remove(idx);
        let num_ret_actual = if let Val::RustFn(f) = func_val {
            let old_stack_bottom = self.stack_bottom;
            self.stack_bottom = idx;
            let num_ret_actual = f(self)?;
            assert!(num_ret_actual <= 1, "Can't return multiple values yet.");
            self.stack.truncate(idx + num_ret_actual as usize);
            self.stack_bottom = old_stack_bottom;
            num_ret_actual
        } else if let Some(chunk) = func_val.as_lua_function() {
            self.eval_chunk(chunk, num_args)?
        } else {
            return Err(self.type_error(TypeError::FunctionCall(func_val.typ())));
        };
        match (num_ret_expected, num_ret_actual) {
            (1, 0) => self.push_nil(),
            (0, 1) => {
                self.stack.pop().unwrap();
            }
            _ => (),
        }
        Ok(())
    }

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
        let idx = self.convert_idx(arg_number);
        let received_type = self.stack[idx].typ();
        if self.stack[idx].typ() != expected_type {
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

    /// Pops `n` values from the stack, concatenates them, and pushes the
    /// result. If `n` is 1, the result is the single value on the stack (that
    /// is, the function does nothing); if `n` is 0, the result is the empty
    /// string.
    pub fn concat(&mut self, n: usize) -> Result<()> {
        assert!(n == 2, "Can only concatenate two at a time for now");
        let r = self.pop_val();
        let l = self.pop_val();
        if let Some(l) = l.as_string() {
            if let Some(r) = r.as_string() {
                let mut s = String::new();
                s.push_str(l);
                s.push_str(r);
                self.push_string(s);
                Ok(())
            } else {
                Err(self.type_error(TypeError::Concat(r.typ())))
            }
        } else {
            Err(self.type_error(TypeError::Concat(l.typ())))
        }
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

    /// Pushes onto the stack the value of the global `name`.
    pub fn get_global(&mut self, name: &str) {
        let val = self.globals.get(name).cloned().unwrap_or_default();
        self.stack.push(val);
    }

    /// Returns the index of the top element in the stack. Because indices start
    /// at 1, this result is equal to the number of elements in the stack (and
    /// so 0 means an empty stack).
    pub fn get_top(&self) -> usize {
        self.stack.len() - self.stack_bottom
    }

    /// Calls `reader` to produce source code, then parses that code and returns
    /// the chunk. If the code is syntactically invalid, but could be valid if
    /// more code was appended, then `reader` will be called again. A common use
    /// for this function is for `reader` to query the user for a line of input.
    pub fn load(&mut self, reader: &mut impl io::Read) -> Result<()> {
        let mut buffer = String::new();
        reader.read_to_string(&mut buffer)?;
        compiler::parse_str(&buffer).map(|chunk| {
            self.push_chunk(chunk);
        })
    }

    /// Loads a file as a Lua chunk. This function uses `load` to load the chunk
    /// in the file named `filename`.
    ///
    /// This function only loads the chunk; it does not run it.
    pub fn load_file(&mut self, filename: impl AsRef<Path>) -> Result<()> {
        let mut reader = fs::File::open(filename)?;
        self.load(&mut reader)
    }

    /// Loads a string as a Lua chunk. This function uses `load` to load the
    /// chunk in the string `s`.
    pub fn load_string(&mut self, s: impl AsRef<str>) -> Result<()> {
        let c = compiler::parse_str(s)?;
        self.push_chunk(c);
        Ok(())
    }

    /// Creates a new empty table and pushes it onto the stack.
    pub fn new_table(&mut self) {
        self.check_heap();
        let t = self.heap.new_table();
        self.stack.push(Val::Obj(t));
    }

    pub fn open_libs(&mut self) {
        lua_std::init(self)
    }

    /// Pops `n` elements from the stack.
    pub fn pop(&mut self, n: isize) {
        assert!(
            n <= self.get_top() as isize,
            "Tried to pop too many elements"
        );
        for _ in 0..n {
            self.pop_val();
        }
    }

    /// Pushes a boolean onto the stack.
    pub fn push_boolean(&mut self, b: bool) {
        self.stack.push(Val::Bool(b));
    }

    /// Pushes a `nil` value onto the stack.
    pub fn push_nil(&mut self) {
        self.stack.push(Val::Nil);
    }

    /// Pushes a number with value `n` onto the stack.
    pub fn push_number(&mut self, n: f64) {
        self.stack.push(Val::Num(n));
    }

    /// Pushes a Rust function onto the stack.
    pub fn push_rust_fn(&mut self, f: RustFunc) {
        self.stack.push(Val::RustFn(f));
    }

    /// Pushes the given string onto the stack.
    pub fn push_string(&mut self, s: String) {
        self.check_heap();
        let obj = self.heap.new_string(s);
        self.stack.push(Val::Obj(obj));
    }

    /// Pushes a copy of the element at the given index onto the stack.
    pub fn push_value(&mut self, i: isize) {
        // TODO: figure out what lua does when index is invalid
        let val = self.at_index(i);
        self.stack.push(val);
    }

    pub fn remove(&mut self, i: isize) {
        let idx = self.convert_idx(i);
        self.stack.remove(idx);
    }

    /// Pops a value from the stack, then replaces the value at the given index
    /// with that value.
    pub fn replace(&mut self, i: isize) {
        let idx = self.convert_idx(i);
        let val = self.stack.pop().unwrap();
        self.stack[idx] = val;
    }

    /// Pops a value from the stack and sets it as the new value of global
    /// `name`.
    pub fn set_global(&mut self, name: &str) {
        let val = self.pop_val();
        self.globals.insert(name.to_string(), val);
    }

    /// Returns whether the value at the given index is not `false` or `nil`.
    pub fn to_boolean(&self, idx: isize) -> bool {
        let val = self.at_index(idx);
        val.truthy()
    }

    /// Attempts to convert the value at the given index to a number.
    pub fn to_number(&self, idx: isize) -> Result<f64> {
        let i = self.convert_idx(idx);
        let val = &self.stack[i];
        val.as_num()
            .ok_or_else(|| self.type_error(TypeError::Arithmetic(val.typ())))
    }

    /// Converts the value at the given index to a string.
    pub fn to_string(&self, idx: isize) -> String {
        let i = self.convert_idx(idx);
        self.stack[i].to_string()
    }

    /// Returns the type of the value in the given acceptable index.
    pub fn typ(&self, idx: isize) -> LuaType {
        self.at_index(idx).typ()
    }

    /// Allocate every string in `strs` on the heap.
    /// The `Val`s returned are always strings.
    fn alloc_strings<I, S>(&mut self, strs: I) -> Vec<Val>
    where
        I: std::iter::IntoIterator<Item = S>,
        S: ToString,
    {
        use std::iter::FromIterator;
        Vec::from_iter(strs.into_iter().map(|s| {
            self.check_heap();
            Val::Obj(self.heap.new_string(s.to_string()))
        }))
    }

    /// Get the value at the given index. Panics if out of bounds.
    fn at_index(&self, idx: isize) -> Val {
        let i = self.convert_idx(idx);
        self.stack[i].clone()
    }

    /// Perform a garbage-collection cycle, if necessary.
    fn check_heap(&mut self) {
        self.check_heap_with(|| {});
    }

    /// Perform a garbage-collection cycle, if necessary. `mark_others` should
    /// mark any other objects which the `State` does not know about.
    fn check_heap_with(&mut self, mark_others: impl FnOnce()) {
        if self.heap.is_full() {
            self.mark_reachable();
            mark_others();
            self.heap.collect();
        }
    }

    /// Given a relative index, convert it to an absolute index to the stack.
    fn convert_idx(&self, fake_idx: isize) -> usize {
        let stack_top = self.stack.len() as isize;
        let stack_bottom = self.stack_bottom as isize;
        let stack_len = stack_top - stack_bottom;
        if fake_idx > 0 && fake_idx <= stack_len {
            (fake_idx - 1 + stack_bottom) as usize
        } else if fake_idx < 0 && fake_idx >= -stack_len {
            (stack_top + fake_idx) as usize
        } else {
            panic!("index out of bounds");
        }
    }

    pub fn error(&self, kind: ErrorKind) -> Error {
        // TODO actually find position
        let pos = 0;
        let column = 0;
        Error::new(kind, pos, column)
    }

    fn eval_chunk(&mut self, chunk: Chunk, num_args: u8) -> Result<u8> {
        let strings = self.alloc_strings(&chunk.string_literals);
        let old_stack_bottom = self.stack_bottom;
        self.stack_bottom = self.stack.len() - num_args as usize;

        match num_args.cmp(&chunk.num_params) {
            Ordering::Less => {
                for _ in num_args..chunk.num_params {
                    self.push_nil();
                }
            }
            Ordering::Greater => {
                self.pop((num_args - chunk.num_params) as isize);
            }
            Ordering::Equal => (),
        }

        for _ in 0..(chunk.num_locals) {
            self.push_nil();
        }

        let mut frame = Frame::new(chunk, strings);
        let num_vals_returned = frame.eval(self)?;
        match num_vals_returned {
            0 => {
                self.stack.truncate(self.stack_bottom);
                self.stack_bottom = old_stack_bottom;
            }
            1 => {
                let ret_val = self.pop_val();
                self.stack.truncate(self.stack_bottom);
                self.stack_bottom = old_stack_bottom;
                self.stack.push(ret_val);
            }
            _ => {
                panic!("Can't handle multiple return values");
            }
        }
        Ok(num_vals_returned)
    }

    /// Pop a value from the stack
    fn pop_val(&mut self) -> Val {
        self.stack.pop().unwrap()
    }

    fn push_chunk(&mut self, chunk: Chunk) {
        self.check_heap();
        let obj = self.heap.new_lua_fn(chunk);
        self.stack.push(Val::Obj(obj));
    }

    fn type_error(&self, e: TypeError) -> Error {
        self.error(ErrorKind::TypeError(e))
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::compiler::parse_str;
    use super::lua_val::Val;
    use super::Chunk;
    use super::Instr::*;
    use super::State;

    #[test]
    fn vm_test01() {
        let mut state = State::new();
        let input = parse_str("a = 1").unwrap();
        state.eval_chunk(input, 0).unwrap();
        assert_eq!(Val::Num(1.0), *state.globals.get("a").unwrap());
    }

    #[test]
    fn vm_test02() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![
                PushString(1),
                PushString(2),
                Concat,
                SetGlobal(0),
                Return(0),
            ],
            string_literals: vec!["key".to_string(), "a".to_string(), "b".to_string()],
            ..Chunk::default()
        };
        state.eval_chunk(input, 0).unwrap();
        let val = state.globals.get("key").unwrap();
        assert_eq!("ab".to_string(), val.as_string().unwrap());
    }

    #[test]
    fn vm_test04() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushNum(0), PushNum(0), Equal, SetGlobal(0), Return(0)],
            number_literals: vec![2.5],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
        };
        state.eval_chunk(input, 0).unwrap();
        assert_eq!(Val::Bool(true), *state.globals.get("a").unwrap());
    }

    #[test]
    fn vm_test05() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![
                PushBool(true),
                BranchFalseKeep(2),
                Pop,
                PushBool(false),
                SetGlobal(0),
                Return(0),
            ],
            string_literals: vec!["key".to_string()],
            ..Chunk::default()
        };
        state.eval_chunk(input, 0).unwrap();
        assert_eq!(Val::Bool(false), *state.globals.get("key").unwrap());
    }

    #[test]
    fn vm_test06() {
        let mut state = State::new();
        let code = vec![
            PushBool(true),
            BranchFalse(3),
            PushNum(0),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
        };
        state.eval_chunk(chunk, 0).unwrap();
        assert_eq!(Val::Num(5.0), *state.globals.get("a").unwrap());
    }

    #[test]
    fn vm_test07() {
        let mut state = State::new();
        let code = vec![
            PushNum(0),
            PushNum(0),
            Less,
            BranchFalse(2),
            PushBool(true),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![2.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
        };
        state.eval_chunk(chunk, 0).unwrap();
        assert!(state.globals.get("a").is_none());
    }

    #[test]
    fn vm_test08() {
        let code = vec![
            PushNum(2), // a = 2
            SetGlobal(0),
            GetGlobal(0), // a <0
            PushNum(0),
            Less,
            BranchFalse(5),
            GetGlobal(0),
            PushNum(1),
            Add,
            SetGlobal(0),
            Jump(-9),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 10.0, 0.0],
            string_literals: vec!["a".to_string()],
            ..Chunk::default()
        };
        let mut state = State::new();
        state.eval_chunk(chunk, 0).unwrap();
    }

    #[test]
    fn vm_test09() {
        // local a = 1
        // while a < 10 do
        //   a = a + 1
        // end
        // x = a
        let code = vec![
            PushNum(0),
            SetLocal(0),
            GetLocal(0),
            PushNum(1),
            Less,
            BranchFalse(5),
            GetLocal(0),
            PushNum(2),
            Add,
            SetLocal(0),
            Jump(-9),
            GetLocal(0),
            SetGlobal(0),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 10.0, 1.0],
            string_literals: vec!["x".to_string()],
            num_locals: 1,
            ..Chunk::default()
        };
        let mut state = State::new();
        state.eval_chunk(chunk, 0).unwrap();
        assert_eq!(Val::Num(10.0), *state.globals.get("x").unwrap());
    }

    #[test]
    fn vm_test10() {
        let code = vec![
            // For loop control variables
            PushNum(0), // start = 6
            PushNum(1), // limit = 2
            PushNum(1), // step = 2
            // Start loop
            ForPrep(0, 3),
            PushNum(0),
            SetGlobal(0), // a = 2
            // End loop
            ForLoop(0, -3),
            Return(0),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![6.0, 2.0],
            string_literals: vec!["a".to_string()],
            num_locals: 4,
            ..Chunk::default()
        };
        let mut state = State::new();
        state.eval_chunk(chunk, 0).unwrap();
        assert!(state.globals.get("a").is_none());
    }

    #[test]
    fn vm_test11() {
        let text = "
            a = 0
            for i = 1, 3 do
                a = a + i
            end";
        let chunk = parse_str(&text).unwrap();
        let mut state = State::new();
        state.eval_chunk(chunk, 0).unwrap();
        let a = state.globals.get("a").unwrap().as_num().unwrap();
        assert_eq!(a, 6.0);
    }
}
