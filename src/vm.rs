//! This modules provides the `State` struct, which handles the primary
//! components of the VM.

use std::collections::HashMap;
use std::io;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::rc::Rc;

use crate::compiler;
use crate::lua_std;
use crate::Chunk;
use crate::Error;
use crate::ErrorKind;
use crate::GcHeap;
use crate::Instr;
use crate::Result;
use crate::Val;

#[derive(Default)]
pub struct State {
    pub globals: HashMap<String, Val>,
    // This field is only used by external functions.
    pub locals: Vec<Val>,
    heap: GcHeap,
}

impl State {
    pub fn new() -> Self {
        let mut me = State::default();
        lua_std::init(&mut me);
        me
    }

    /// Calls `reader` to produce source code, then parses that code and returns
    /// the chunk. If the code is syntactically invalid, but could be valid if
    /// more code was appended, then reader will be called again. A common use
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

    fn err(&mut self, kind: ErrorKind) -> Error {
        let pos = 0;
        let column = 0;
        Error::new(kind, pos, column)
    }

    pub fn eval_chunk(&mut self, chunk: Chunk) -> Result<()> {
        let mut stack = Vec::new();
        for _ in 0..chunk.num_locals {
            stack.push(Val::Nil);
        }

        let len = chunk.code.len();
        let mut ip = 0isize;
        while ip < len as isize {
            let instr = chunk.code[ip as usize];
            if option_env!("LUA_DEBUG_VM").is_some() {
                println!("{:4}    {:?}", ip, instr);
            }
            ip += 1;
            match instr {
                Instr::Pop => {
                    stack.pop();
                }
                Instr::Jump(offset) => {
                    ip += offset;
                }

                Instr::BranchFalse(offset) | Instr::BranchFalseKeep(offset) => {
                    let val = stack.pop().unwrap();
                    if !val.truthy() {
                        ip += offset;
                    }
                    if let Instr::BranchFalseKeep(_) = instr {
                        stack.push(val);
                    }
                }
                Instr::BranchTrue(offset) | Instr::BranchTrueKeep(offset) => {
                    let val = stack.pop().unwrap();
                    if val.truthy() {
                        ip += offset;
                    }
                    if let Instr::BranchTrueKeep(_) = instr {
                        stack.push(val);
                    }
                }

                Instr::Call(num_args) => {
                    self.locals = stack.split_off(stack.len() - num_args as usize);
                    let func = stack.pop().unwrap();
                    if let Val::RustFn(f) = func {
                        f(self);
                        stack.push(Val::Nil);
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                }

                Instr::GetLocal(i) => stack.push(stack[i as usize].clone()),
                Instr::SetLocal(i) => stack[i as usize] = stack.pop().unwrap(),

                Instr::GetGlobal(i) => {
                    let name = &chunk.string_literals[i as usize];
                    let val = match self.globals.get(name) {
                        Some(val) => val.clone(),
                        None => Val::Nil,
                    };
                    stack.push(val);
                }
                Instr::SetGlobal(i) => {
                    let val = stack.pop().unwrap();
                    let name = chunk.string_literals[i as usize].clone();
                    self.globals.insert(name, val);
                }

                Instr::ForPrep(local_slot, body_length) => {
                    let step = stack.pop().unwrap();
                    let end = stack.pop().unwrap();
                    let start = stack.pop().unwrap();

                    let (start, end, step) = get_numeric_for_initializers(start, end, step)
                        .ok_or_else(|| self.err(ErrorKind::TypeError))?;

                    if check_numeric_for_condition(start, end, step) {
                        // The condition was true; set up the locals and start
                        // the loop.
                        let mut local_slot = local_slot as usize;
                        for num in [start, end, step, start].iter() {
                            stack[local_slot] = Val::Num(*num);
                            local_slot += 1;
                        }
                    } else {
                        // Skip the loop.
                        ip += body_length;
                    }
                }
                Instr::ForLoop(local_slot_u8, offset) => {
                    let local_slot = local_slot_u8 as usize;
                    let var = &stack[local_slot];
                    let limit = &stack[local_slot + 1];
                    let step = &stack[local_slot + 2];

                    // We know these values are numbers.
                    let limit = limit.as_num().unwrap();
                    let step = step.as_num().unwrap();
                    let var = var.as_num().unwrap() + step;

                    if check_numeric_for_condition(var, limit, step) {
                        let var = Val::Num(var);
                        let usable_var = var.clone();
                        stack[local_slot] = var;
                        stack[local_slot + 3] = usable_var;
                        ip += offset;
                    }
                }

                // Literals
                Instr::PushNil => stack.push(Val::Nil),
                Instr::PushBool(b) => stack.push(Val::Bool(b)),
                Instr::PushNum(i) => stack.push(Val::Num(chunk.number_literals[i as usize])),
                Instr::PushString(i) => {
                    let val = Val::Str(Rc::new(get_string(&chunk, i as usize)));
                    stack.push(val);
                }

                // Arithmetic
                Instr::Add => eval_float_float(<f64 as Add>::add, instr, &mut stack)?,
                Instr::Subtract => eval_float_float(<f64 as Sub>::sub, instr, &mut stack)?,
                Instr::Multiply => eval_float_float(<f64 as Mul>::mul, instr, &mut stack)?,
                Instr::Divide => eval_float_float(<f64 as Div>::div, instr, &mut stack)?,
                Instr::Mod => eval_float_float(<f64 as Rem>::rem, instr, &mut stack)?,
                Instr::Pow => eval_float_float(f64::powf, instr, &mut stack)?,

                // Equality
                Instr::Equal => {
                    let e2 = stack.pop().unwrap();
                    let e1 = stack.pop().unwrap();
                    stack.push(Val::Bool(e1 == e2));
                }
                Instr::NotEqual => {
                    let e2 = stack.pop().unwrap();
                    let e1 = stack.pop().unwrap();
                    stack.push(Val::Bool(e1 != e2));
                }

                // Order comparison
                Instr::Less => eval_float_bool(<f64 as PartialOrd>::lt, instr, &mut stack)?,
                Instr::Greater => eval_float_bool(<f64 as PartialOrd>::gt, instr, &mut stack)?,
                Instr::LessEqual => eval_float_bool(<f64 as PartialOrd>::le, instr, &mut stack)?,
                Instr::GreaterEqual => eval_float_bool(<f64 as PartialOrd>::ge, instr, &mut stack)?,

                // String concatenation
                Instr::Concat => {
                    let v2 = stack.pop().unwrap();
                    let v1 = stack.pop().unwrap();
                    if let (Val::Str(s1), Val::Str(s2)) = (&v1, &v2) {
                        let mut new_string = String::clone(s1);
                        new_string += s2;
                        stack.push(Val::Str(Rc::new(new_string)));
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                }

                // Unary
                Instr::Negate => {
                    let e = stack.pop().unwrap();
                    if let Val::Num(n) = e {
                        stack.push(Val::Num(-n));
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                }
                Instr::Not => {
                    let e = stack.pop().unwrap();
                    stack.push(Val::Bool(!e.truthy()));
                }

                Instr::NewTable => {
                    let obj_ptr = self.heap.new_table(&stack[..]);
                    let val = Val::Obj(obj_ptr);
                    stack.push(val);
                }

                Instr::GetField(i) => {
                    let mut t = stack.pop().unwrap();
                    if let Some(t) = t.as_table() {
                        let key = Val::Str(Rc::new(get_string(&chunk, i as usize)));
                        let val = t.get(&key);
                        stack.push(val.clone());
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                }

                Instr::SetField(stack_offset, literal_id) => {
                    let v = stack.pop().unwrap();
                    let index = stack.len() - stack_offset as usize - 1;
                    let mut t = stack.remove(index);
                    if let Some(t) = t.as_table() {
                        let key = Val::Str(Rc::new(get_string(&chunk, literal_id as usize)));
                        t.insert(key, v)?;
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                }
                Instr::InitField(i) => {
                    let v = stack.pop().unwrap();
                    let mut t = stack.pop().unwrap();
                    if let Some(t) = t.as_table() {
                        let key = Val::Str(Rc::new(get_string(&chunk, i as usize)));
                        t.insert(key, v)?;
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                    stack.push(t);
                }

                Instr::GetTable => {
                    let key = stack.pop().unwrap();
                    let mut t = stack.pop().unwrap();
                    if let Some(t) = t.as_table() {
                        let val = t.get(&key);
                        stack.push(val.clone());
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                }

                Instr::SetTable(offset) => {
                    let val = stack.pop().unwrap();
                    let index = stack.len() - offset as usize - 2;
                    let mut t = stack.remove(index);
                    let key = stack.remove(index);
                    if let Some(t) = t.as_table() {
                        t.insert(key, val)?;
                    } else {
                        return Err(self.err(ErrorKind::TypeError));
                    }
                }

                Instr::Print => {
                    let e = stack.pop().unwrap();
                    println!("{}", e);
                }

                _ => panic!("We don't support {:?} yet.", instr),
            }
        }

        Ok(())
    }
}

fn get_string(chunk: &Chunk, i: usize) -> String {
    chunk.string_literals[i].clone()
}

/// Evaluate a function of 2 floats which returns a bool.
///
/// Take 2 values from the stack, pass them to `f`, and push the returned value
/// onto the stack. Returns an `EvalError` if anything goes wrong.
fn eval_float_bool<F>(f: F, _instr: Instr, stack: &mut Vec<Val>) -> Result<()>
where
    F: FnOnce(&f64, &f64) -> bool,
{
    let v2 = stack.pop().unwrap();
    let v1 = stack.pop().unwrap();
    if let (Val::Num(n1), Val::Num(n2)) = (&v1, &v2) {
        stack.push(Val::Bool(f(n1, n2)));
        return Ok(());
    }

    Err(Error::new(ErrorKind::TypeError, 0, 0))
}

/// Evaluate a function of 2 floats which returns a float.
///
/// Take 2 values from the stack, pass them to `f`, and push the returned value
/// onto the stack. Returns an `EvalError` if anything goes wrong.
fn eval_float_float<F>(f: F, _instr: Instr, stack: &mut Vec<Val>) -> Result<()>
where
    F: FnOnce(f64, f64) -> f64,
{
    let v2 = stack.pop().unwrap();
    let v1 = stack.pop().unwrap();
    if let (&Val::Num(n1), &Val::Num(n2)) = (&v1, &v2) {
        stack.push(Val::Num(f(n1, n2)));
        return Ok(());
    }

    // This has to be outside the `if let` to avoid borrow issues.
    Err(Error::new(ErrorKind::TypeError, 0, 0))
}

/// Get all three values as numbers.
fn get_numeric_for_initializers(start: Val, limit: Val, step: Val) -> Option<(f64, f64, f64)> {
    match (start, limit, step) {
        (Val::Num(start), Val::Num(limit), Val::Num(step)) => Some((start, limit, step)),
        _ => None,
    }
}

/// Helper to evaluate the condition of a numeric `for` loop.
///
/// If `step` is positive, return `var <= limit`. If `step` is not positive,
/// return `var >= limit`.
fn check_numeric_for_condition(var: f64, limit: f64, step: f64) -> bool {
    if step > 0.0 {
        var <= limit
    } else if step <= 0.0 {
        var >= limit
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instr::Instr::*;

    #[test]
    fn vm_test01() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushNum(0), SetGlobal(0)],
            number_literals: vec![1.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
        assert_eq!(Val::Num(1.0), *state.globals.get("a").unwrap());
    }

    #[test]
    fn vm_test02() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushString(1), PushString(2), Concat, SetGlobal(0)],
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
            code: vec![PushNum(0), PushNum(0), Equal, SetGlobal(0)],
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
        let code = vec![PushBool(true), BranchFalse(3), PushNum(0), SetGlobal(0)];
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
            BranchFalse(3),
            PushBool(true),
            SetGlobal(0),
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
            BranchFalse(43243),
            GetGlobal(0),
            PushNum(1),
            Add,
            SetGlobal(0),
            Jump(-9),
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
        let chunk = compiler::parse_str(&text).unwrap();
        let mut state = State::new();
        state.eval_chunk(chunk).unwrap();
        let a = state.globals.get("a").unwrap().as_num().unwrap();
        assert_eq!(a, 6.0);
    }
}
