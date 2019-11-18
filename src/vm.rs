//! This module provides the `State` struct, which handles the primary
//! components of the VM.

mod frame;

use std::collections::HashMap;
use std::io;

use crate::compiler;
use crate::lua_std;
use crate::Chunk;
use crate::Error;
use crate::ErrorKind;
use crate::GcHeap;
use crate::Markable;
use crate::Result;
use crate::Val;

use frame::Frame;

#[derive(Default)]
pub struct State {
    pub globals: HashMap<String, Val>,
    stack: Vec<Val>,
    stack_bottom: usize,
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
    pub fn new() -> Self {
        let mut me = State::default();
        lua_std::init(&mut me);
        me
    }

    pub fn call(&mut self, num_args: u8, num_results: u8) -> Result<()> {
        assert!(num_results == 0, "Can't return values from functions yet.");
        let idx = self.stack.len() - num_args as usize - 1;
        let func_val = self.stack.remove(idx);
        if let Val::RustFn(f) = func_val {
            let old_stack_bottom = self.stack_bottom;
            self.stack_bottom = idx;
            let re = f(self).map(|n| {
                assert!(n == 0, "Can't return values from functions yet.");
            });
            self.stack.truncate(self.stack_bottom);
            self.stack_bottom = old_stack_bottom;
            re
        } else if let Some(chunk) = func_val.as_lua_function() {
            self.eval_chunk(chunk)
        } else {
            // TODO: handle this
            panic!("Tried to call something not a function.");
        }
    }

    /// Pops `n` values from the stack, concatenates them, and pushes the
    /// result. If `n` is 1, the result is the single value on the stack (that
    /// is, the function does nothing); if `n` is 0, the result is the empty
    /// string.
    pub fn concat(&mut self, n: usize) -> Result<()> {
        assert!(n == 2, "Can only concatenate two at a time for now");
        let r = self.pop_val();
        let l = self.pop_val();
        if let (Some(l), Some(r)) = (l.as_string(), r.as_string()) {
            let mut s = String::new();
            s.push_str(l);
            s.push_str(r);
            self.push_string(s);
            Ok(())
        } else {
            Err(self.error(ErrorKind::TypeError))
        }
    }

    pub fn create_table(&mut self, size: usize) {
        assert!(size == 0, "Pre-allocating tables is unsupported.");
        self.check_heap();
        let t = self.heap.new_table();
        self.stack.push(Val::Obj(t));
    }

    pub fn get_global(&self, s: &str) -> Val {
        self.globals.get(s).cloned().unwrap_or_default()
    }

    pub fn get_top(&mut self) -> usize {
        self.stack.len() - self.stack_bottom
    }

    /// Calls `reader` to produce source code, then parses that code and returns
    /// the chunk. If the code is syntactically invalid, but could be valid if
    /// more code was appended, then `reader` will be called again. A common use
    /// for this function is for `reader` to query the user for a line of input.
    pub fn load<F>(mut reader: F) -> Result<Chunk>
    where
        F: FnMut(&mut String) -> io::Result<usize>,
    {
        let mut buffer = String::new();
        loop {
            match reader(&mut buffer) {
                Ok(_) => (),
                Err(e) => {
                    return Err(Error::from_io_error(e));
                }
            }
            match compiler::parse_str(&buffer) {
                Err(ref e) if e.is_recoverable() => {
                    continue;
                }
                result => {
                    return result;
                }
            }
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

    /// Pops a value from the stack, then replaces the value at the given index
    /// with that value.
    pub fn replace(&mut self, i: isize) {
        let idx = self.convert_idx(i);
        let val = self.stack.pop().unwrap();
        self.stack[idx] = val;
    }

    pub fn set_global(&mut self, name: &str, val: Val) {
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
        self.stack[i].as_num().ok_or_else(|| self.type_error())
    }

    /// Converts the value at the given index to a string.
    pub fn to_string(&self, idx: isize) -> String {
        let i = self.convert_idx(idx);
        self.stack[i].to_string()
    }

    pub fn eval_chunk(&mut self, chunk: Chunk) -> Result<()> {
        let strings = self.alloc_strings(&chunk.string_literals);
        let old_stack_bottom = self.stack_bottom;
        self.stack_bottom = self.stack.len();
        for _ in 0..(chunk.num_locals) {
            self.stack.push(Val::Nil);
        }
        let mut frame = Frame::new(chunk, strings);
        frame.eval(self)?;
        self.stack_bottom = old_stack_bottom;
        self.stack.truncate(old_stack_bottom);
        Ok(())
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

    /// Pop a value from the stack
    fn pop_val(&mut self) -> Val {
        self.stack.pop().unwrap()
    }

    fn type_error(&self) -> Error {
        self.error(ErrorKind::TypeError)
    }
}

#[cfg(test)]
mod tests {
    use super::State;

    use crate::compiler::parse_str;
    use crate::Chunk;
    use crate::Instr::*;
    use crate::Val;

    #[test]
    fn vm_test01() {
        let mut state = State::new();
        let input = parse_str("a = 1").unwrap();
        state.eval_chunk(input).unwrap();
        assert_eq!(Val::Num(1.0), *state.globals.get("a").unwrap());
    }

    #[test]
    fn vm_test02() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushString(1), PushString(2), Concat, SetGlobal(0), Return],
            number_literals: vec![],
            string_literals: vec!["key".to_string(), "a".to_string(), "b".to_string()],
            nested: vec![],
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
        let val = state.globals.get("key").unwrap();
        assert_eq!("ab".to_string(), val.as_string().unwrap());
    }

    #[test]
    fn vm_test04() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushNum(0), PushNum(0), Equal, SetGlobal(0), Return],
            number_literals: vec![2.5],
            string_literals: vec!["a".to_string()],
            nested: vec![],
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
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
                Return,
            ],
            number_literals: vec![],
            string_literals: vec!["key".to_string()],
            nested: vec![],
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
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
            Return,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            nested: vec![],
            num_locals: 0,
        };
        state.eval_chunk(chunk).unwrap();
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
            Return,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![2.0],
            string_literals: vec!["a".to_string()],
            nested: vec![],
            num_locals: 0,
        };
        state.eval_chunk(chunk).unwrap();
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
            Return,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 10.0, 0.0],
            string_literals: vec!["a".to_string()],
            nested: vec![],
            num_locals: 0,
        };
        let mut state = State::new();
        state.eval_chunk(chunk).unwrap();
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
            Return,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 10.0, 1.0],
            string_literals: vec!["x".to_string()],
            nested: vec![],
            num_locals: 1,
        };
        let mut state = State::new();
        state.eval_chunk(chunk).unwrap();
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
            Return,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![6.0, 2.0],
            string_literals: vec!["a".to_string()],
            nested: vec![],
            num_locals: 4,
        };
        let mut state = State::new();
        state.eval_chunk(chunk).unwrap();
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
        state.eval_chunk(chunk).unwrap();
        let a = state.globals.get("a").unwrap().as_num().unwrap();
        assert_eq!(a, 6.0);
    }
}
