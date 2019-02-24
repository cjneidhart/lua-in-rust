//! This modules provides the `State` struct, which handles the primary
//! components of the VM.

use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::rc::Rc;
use std::result;

use crate::instr::Instr;
use crate::lua_std;
use crate::lua_val::LuaVal::{self, *};
use crate::parser::Chunk;

#[derive(Debug)]
pub enum EvalError {
    StackError,
    SingleTypeError(Instr, LuaVal),
    DoubleTypeError(Instr, LuaVal, LuaVal),
    Other,
}

type Result<T> = result::Result<T, EvalError>;

#[derive(Default)]
pub struct State {
    pub globals: HashMap<String, LuaVal>,
    // This field is only used by external functions.
    pub locals: Vec<LuaVal>,
}

impl State {
    pub fn new() -> Self {
        let mut me = State::default();
        lua_std::init(&mut me);
        me
    }

    pub fn eval_chunk(&mut self, chunk: Chunk) -> Result<()> {
        let mut stack = Vec::new();
        for _ in 0..chunk.num_locals {
            stack.push(Nil);
        }

        let len = chunk.code.len();
        let mut ip = 0isize;
        while ip < len as isize {
            let instr = chunk.code[ip as usize];
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
                    if let RustFn(f) = func {
                        f(self);
                        stack.push(Nil);
                    } else {
                        return Err(EvalError::Other);
                    }
                }

                Instr::GetLocal(i) => stack.push(stack[i as usize].clone()),
                Instr::SetLocal(i) => stack[i as usize] = stack.pop().unwrap(),

                Instr::GetGlobal(i) => {
                    let name = &chunk.string_literals[i as usize];
                    let val = match self.globals.get(name) {
                        Some(val) => val.clone(),
                        None => Nil,
                    };
                    stack.push(val);
                }
                Instr::SetGlobal(i) => {
                    let val = stack.pop().unwrap();
                    let name = chunk.string_literals[i as usize].clone();
                    self.globals.insert(name, val);
                }

                Instr::ForPrep(local_slot) => {
                    stack[local_slot + 2] = stack.pop().unwrap();
                    stack[local_slot + 1] = stack.pop().unwrap();
                    let starting_val = stack.pop().unwrap();
                    stack[local_slot + 3] = starting_val.clone();
                    stack[local_slot] = starting_val;
                }
                Instr::ForLoop(local_slot, offset) => {
                    let mut next_val = None;
                    match (
                        &stack[local_slot],
                        &stack[local_slot + 1],
                        &stack[local_slot + 2],
                    ) {
                        (Number(current_ref), Number(stop), Number(step)) => {
                            let current = *current_ref + step;
                            if (*step > 0.0 && current <= *stop)
                                || (*step <= 0.0 && current >= *stop)
                            {
                                next_val = Some(Number(current));
                            }
                        }
                        _ => return Err(EvalError::Other),
                    }
                    if let Some(x) = next_val {
                        stack[local_slot] = x.clone();
                        stack[local_slot + 3] = x;
                        ip += offset;
                    }
                }

                // Literals
                Instr::PushNil => stack.push(Nil),
                Instr::PushBool(b) => stack.push(Bool(b)),
                Instr::PushNum(i) => stack.push(Number(chunk.number_literals[i as usize])),
                Instr::PushString(i) => {
                    let s = chunk.string_literals[i as usize].clone();
                    stack.push(LuaString(Rc::new(s)));
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
                    stack.push(Bool(e1 == e2));
                }
                Instr::NotEqual => {
                    let e2 = stack.pop().unwrap();
                    let e1 = stack.pop().unwrap();
                    stack.push(Bool(e1 != e2));
                }

                // Order comparison
                Instr::Less => eval_float_bool(<f64 as PartialOrd>::lt, instr, &mut stack)?,
                Instr::Greater => eval_float_bool(<f64 as PartialOrd>::gt, instr, &mut stack)?,
                Instr::LessEqual => eval_float_bool(<f64 as PartialOrd>::le, instr, &mut stack)?,
                Instr::GreaterEqual => eval_float_bool(<f64 as PartialOrd>::ge, instr, &mut stack)?,

                // String concatenation
                Instr::Concat => attempt_concat(&mut stack)?,

                // Unary
                Instr::Negate => {
                    let e = safe_pop(&mut stack)?;
                    if let Number(n) = e {
                        stack.push(Number(-n));
                    } else {
                        return Err(EvalError::SingleTypeError(instr, e));
                    }
                }
                Instr::Not => {
                    let e = safe_pop(&mut stack)?;
                    stack.push(Bool(!e.truthy()));
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

fn attempt_concat(stack: &mut Vec<LuaVal>) -> Result<()> {
    let v2 = safe_pop(stack)?;
    let v1 = safe_pop(stack)?;
    if let (LuaString(s1), LuaString(s2)) = (&v1, &v2) {
        let mut new_string = String::clone(s1);
        new_string.push_str(s2.as_str());
        stack.push(LuaString(Rc::new(new_string)));
        return Ok(());
    }

    Err(EvalError::DoubleTypeError(Instr::Concat, v1, v2))
}

/// Evaluate a function of 2 floats which returns a bool.
///
/// Take 2 values from the stack, pass them to `f`, and push the returned value
/// onto the stack. Returns an `EvalError` if anything goes wrong.
fn eval_float_bool<F>(f: F, instr: Instr, stack: &mut Vec<LuaVal>) -> Result<()>
where
    F: FnOnce(&f64, &f64) -> bool,
{
    let v2 = safe_pop(stack)?;
    let v1 = safe_pop(stack)?;
    if let (Number(n1), Number(n2)) = (&v1, &v2) {
        stack.push(Bool(f(n1, n2)));
        return Ok(());
    }

    Err(EvalError::DoubleTypeError(instr, v1, v2))
}

/// Evaluate a function of 2 floats which returns a float.
///
/// Take 2 values from the stack, pass them to `f`, and push the returned value
/// onto the stack. Returns an `EvalError` if anything goes wrong.
fn eval_float_float<F>(f: F, instr: Instr, stack: &mut Vec<LuaVal>) -> Result<()>
where
    F: FnOnce(f64, f64) -> f64,
{
    let v2 = safe_pop(stack)?;
    let v1 = safe_pop(stack)?;
    if let (&Number(n1), &Number(n2)) = (&v1, &v2) {
        stack.push(Number(f(n1, n2)));
        return Ok(());
    }

    // This has to be outside the `if let` to avoid borrow issues.
    Err(EvalError::DoubleTypeError(instr, v1, v2))
}

/// Pop from the top of the stack, or return a EvalError.
fn safe_pop(stack: &mut Vec<LuaVal>) -> Result<LuaVal> {
    stack.pop().ok_or(EvalError::StackError)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instr::Instr::*;

    #[test]
    fn test1() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushNum(0), SetGlobal(0)],
            number_literals: vec![1.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
        assert_eq!(Number(1.0), *state.globals.get("a").unwrap());
    }

    #[test]
    fn test2() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushString(1), PushString(2), Concat, SetGlobal(0)],
            number_literals: vec![],
            //string_literals: vec![],
            string_literals: vec!["key".to_string(), "a".to_string(), "b".to_string()],
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
        assert_eq!(
            LuaString(Rc::new("ab".to_string())),
            *state.globals.get("key").unwrap()
        );
    }

    #[test]
    fn test4() {
        let mut state = State::new();
        let input = Chunk {
            code: vec![PushNum(0), PushNum(0), Equal, SetGlobal(0)],
            number_literals: vec![2.5],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        state.eval_chunk(input).unwrap();
        assert_eq!(Bool(true), *state.globals.get("a").unwrap());
    }

    #[test]
    fn test5() {
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
        assert_eq!(Bool(false), *state.globals.get("key").unwrap());
    }

    #[test]
    fn test6() {
        let mut state = State::new();
        let code = vec![PushBool(true), BranchFalse(3), PushNum(0), SetGlobal(0)];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
            num_locals: 0,
        };
        state.eval_chunk(chunk).unwrap();
        assert_eq!(Number(5.0), *state.globals.get("a").unwrap());
    }

    #[test]
    fn test7() {
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
    fn test8() {
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
    fn test9() {
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
        assert_eq!(Number(10.0), *state.globals.get("x").unwrap());
    }
}