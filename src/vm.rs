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

use frame::{Action, Frame};

#[derive(Default)]
pub struct State {
    pub globals: HashMap<String, Val>,
    // This field is only used by external functions.
    pub locals: Vec<Val>,
    frames: Vec<Frame>,
    heap: GcHeap,
}

impl Markable for State {
    fn mark_reachable(&self) {
        let globals_iter = self.globals.values();
        let locals_iter = self.locals.iter();
        for val in globals_iter.chain(locals_iter) {
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

    fn error(&mut self, kind: ErrorKind) -> Error {
        // TODO actually find position
        let pos = 0;
        let column = 0;
        Error::new(kind, pos, column)
    }

    pub fn eval_chunk(&mut self, chunk: Chunk) -> Result<()> {
        let mut frame = Frame::new(chunk);
        loop {
            match frame.eval()? {
                Action::AllocTable => {
                    if self.heap.is_full() {
                        self.mark_reachable();
                        frame.mark_reachable();
                        self.heap.collect();
                    }
                    let val = Val::Obj(self.heap.new_table());
                    frame.stack.push(val);
                }
                Action::Call(num_args) => {
                    self.locals = frame.stack.split_off(frame.stack.len() - num_args as usize);
                    let f = frame.stack.pop().unwrap();
                    if let Val::RustFn(func) = f {
                        self.frames.push(frame);
                        func(self);
                        frame = self.frames.pop().unwrap();
                        frame.stack.append(&mut self.locals);
                    } else {
                        return Err(self.error(ErrorKind::TypeError));
                    }
                }
                Action::GetGlobal(i) => {
                    let key = &frame.chunk.string_literals[i as usize];
                    let val = self.globals.get(key).cloned().unwrap_or(Val::Nil);
                    frame.stack.push(val);
                }
                Action::SetGlobal(i) => {
                    let key = frame.chunk.string_literals[i as usize].clone();
                    let val = frame.stack.pop().unwrap();
                    self.globals.insert(key, val);
                }
                Action::Return => {
                    return Ok(());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

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
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
        assert_eq!(
            Val::Str(Rc::new("ab".to_string())),
            *state.globals.get("key").unwrap()
        );
    }

    #[test]
    fn vm_test04() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushNum(0), PushNum(0), Equal, SetGlobal(0), Return],
            number_literals: vec![2.5],
            string_literals: vec!["a".to_string()],
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
