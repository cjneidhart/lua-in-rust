use std::collections::HashMap;
use std::ops::{Div, Mul, Rem, Sub};

use crate::instr::Instr;
use crate::lua_val::{LuaVal, LuaVal::*};
use crate::parser::Chunk;

pub type GlobalEnv = HashMap<String, LuaVal>;

#[derive(Debug)]
pub enum EvalError {
    StackError,
    SingleTypeError(Instr, LuaVal),
    DoubleTypeError(Instr, LuaVal, LuaVal),
    Other,
}

pub fn eval_chunk(input: &Chunk, env: &mut GlobalEnv) -> Result<(), EvalError> {
    let mut stack = Vec::<LuaVal>::new();
    let mut ip = 0;
    let len = input.code.len();
    while ip < len {
        use self::Instr::*;
        let instr = input.code[ip];
        ip += 1;
        match instr {
            Pop => {
                safe_pop(&mut stack)?;
            }
            Jump(offset) => {
                if offset < 0 {
                    ip -= (-offset) as usize;
                } else {
                    ip += offset as usize;
                }
            }

            BranchFalse(offset) => {
                let val = safe_pop(&mut stack)?;
                if !val.truthy() {
                    ip += offset as usize;
                }
            }
            BranchTrue(offset) => {
                let val = safe_pop(&mut stack)?;
                if val.truthy() {
                    ip += offset as usize;
                }
            }
            BranchFalseKeep(offset) => {
                let val = safe_pop(&mut stack)?;
                if !val.truthy() {
                    ip += offset as usize;
                }
                stack.push(val);
            }
            BranchTrueKeep(offset) => {
                let val = safe_pop(&mut stack)?;
                if val.truthy() {
                    ip += offset as usize;
                }
                stack.push(val);
            }

            Print => {
                let e = stack.pop().unwrap();
                println!("{}", e);
            }
            Assign => {
                let val = stack.pop().unwrap();
                let name = stack.pop().unwrap();
                if let LuaVal::LuaString(s) = name {
                    env.insert(s, val);
                } else {
                    return Err(EvalError::DoubleTypeError(Instr::Assign, name, val));
                }
            }

            GlobalLookup => {
                let name = stack.pop().unwrap();
                if let LuaVal::LuaString(s) = name {
                    let val = env.get(&s).unwrap_or(&LuaVal::Nil);
                    stack.push(val.clone());
                } else {
                    return Err(EvalError::SingleTypeError(instr, name));
                }
            }

            // Literals
            PushNil => stack.push(Nil),
            PushBool(b) => stack.push(Bool(b)),
            PushNum(i) => stack.push(Number(input.number_literals[i])),
            PushString(i) => stack.push(LuaString(input.string_literals[i].clone())),

            // Arithmetic
            Add => eval_float_float(<f64 as std::ops::Add>::add, instr, &mut stack)?,
            Subtract => eval_float_float(<f64 as Sub>::sub, instr, &mut stack)?,
            Multiply => eval_float_float(<f64 as Mul>::mul, instr, &mut stack)?,
            Divide => eval_float_float(<f64 as Div>::div, instr, &mut stack)?,
            Mod => eval_float_float(<f64 as Rem>::rem, instr, &mut stack)?,
            Pow => eval_float_float(f64::powf, instr, &mut stack)?,

            // Equality
            Equal => {
                let e2 = stack.pop().unwrap();
                let e1 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Bool(n1 == n2)),
                    (Bool(b1), Bool(b2)) => stack.push(Bool(b1 == b2)),
                    _ => panic!(),
                }
            }
            NotEqual => {
                let e2 = stack.pop().unwrap();
                let e1 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Bool(n1 != n2)),
                    (Bool(b1), Bool(b2)) => stack.push(Bool(b1 != b2)),
                    _ => panic!(),
                }
            }

            // Order comparison
            Less => eval_float_bool(<f64 as PartialOrd<f64>>::lt, instr, &mut stack)?,
            Greater => eval_float_bool(<f64 as PartialOrd<f64>>::gt, instr, &mut stack)?,
            LessEqual => eval_float_bool(<f64 as PartialOrd<f64>>::le, instr, &mut stack)?,
            GreaterEqual => eval_float_bool(<f64 as PartialOrd<f64>>::ge, instr, &mut stack)?,

            // String concatenation
            Concat => attempt_concat(&mut stack)?,

            // Unary
            Negate => {
                let e = safe_pop(&mut stack)?;
                if let Number(n) = e {
                    stack.push(Number(-n));
                } else {
                    return Err(EvalError::SingleTypeError(instr, e));
                }
            }
            Not => {
                let e = safe_pop(&mut stack)?;
                stack.push(Bool(!e.truthy()));
            }

            _ => panic!("We don't support that instruction yet."),
        }
    }

    Ok(())
}

fn attempt_concat(stack: &mut Vec<LuaVal>) -> Result<(), EvalError> {
    let v2 = safe_pop(stack)?;
    let v1 = safe_pop(stack)?;
    if let (LuaString(s1), LuaString(s2)) = (&v1, &v2) {
        stack.push(LuaString(s1.clone() + s2));
        return Ok(());
    }

    Err(EvalError::DoubleTypeError(Instr::Concat, v1, v2))
}

/// Evaluate a function of 2 floats which returns a bool.
///
/// Take 2 values from the stack, pass them to `f`, and push the returned value
/// onto the stack. Returns an `EvalError` if anything goes wrong.
fn eval_float_bool<F>(f: F, instr: Instr, stack: &mut Vec<LuaVal>) -> Result<(), EvalError>
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
fn eval_float_float<F>(f: F, instr: Instr, stack: &mut Vec<LuaVal>) -> Result<(), EvalError>
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
fn safe_pop(stack: &mut Vec<LuaVal>) -> Result<LuaVal, EvalError> {
    stack.pop().ok_or(EvalError::StackError)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instr::Instr::*;

    #[test]
    fn test1() {
        let mut env = HashMap::new();
        let input = Chunk {
            code: vec![PushString(0), PushNum(0), Assign],
            number_literals: vec![1.0],
            string_literals: vec!["a".to_string()],
        };
        eval_chunk(&input, &mut env).unwrap();
        assert_eq!(1, env.len());
        assert_eq!(LuaVal::Number(1.0), *env.get("a").unwrap());
    }

    #[test]
    fn test2() {
        let mut env = HashMap::new();
        let input = Chunk {
            code: vec![PushString(0), PushString(1), PushString(2), Concat, Assign],
            number_literals: vec![],
            //string_literals: vec![],
            string_literals: vec!["key".to_string(), "a".to_string(), "b".to_string()],
        };
        eval_chunk(&input, &mut env).unwrap();
        assert_eq!(1, env.len());
        assert_eq!(
            LuaVal::LuaString("ab".to_string()),
            *env.get("key").unwrap()
        );
    }

    #[test]
    fn test4() {
        let mut env = HashMap::new();
        let input = Chunk {
            code: vec![PushString(0), PushNum(0), PushNum(0), Equal, Assign],
            number_literals: vec![2.5],
            string_literals: vec!["a".to_string()],
        };
        eval_chunk(&input, &mut env).unwrap();
        assert_eq!(1, env.len());
        assert_eq!(LuaVal::Bool(true), *env.get("a").unwrap());
    }

    #[test]
    fn test5() {
        let mut env = HashMap::new();
        let input = Chunk {
            code: vec![
                PushString(0),
                PushBool(true),
                BranchFalseKeep(2),
                Pop,
                PushBool(false),
                Instr::Assign,
            ],
            number_literals: vec![],
            string_literals: vec!["key".to_string()],
        };
        eval_chunk(&input, &mut env).unwrap();
        assert_eq!(1, env.len());
        assert_eq!(Bool(false), *env.get("key").unwrap());
    }

    #[test]
    fn test6() {
        let mut env = HashMap::new();
        let code = vec![
            PushBool(true),
            BranchFalse(3),
            PushString(0),
            PushNum(0),
            Instr::Assign,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![5.0],
            string_literals: vec!["a".to_string()],
        };
        eval_chunk(&chunk, &mut env).unwrap();
        assert_eq!(1, env.len());
        assert_eq!(Number(5.0), *env.get("a").unwrap());
    }

    #[test]
    fn test7() {
        let mut env = HashMap::new();
        let code = vec![
            PushNum(0),
            PushNum(0),
            Less,
            BranchFalse(3),
            PushString(0),
            PushBool(true),
            Instr::Assign,
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![2.0],
            string_literals: vec!["a".to_string()],
        };
        eval_chunk(&chunk, &mut env).unwrap();
        assert_eq!(0, env.len());
    }

    #[test]
    fn test8() {
        //let mut env = HashMap::new();
        let code = vec![
            PushString(0),
            PushNum(2),
            Assign,
            PushString(0),
            GlobalLookup,
            PushNum(0),
            Less,
            BranchFalse(43243),
            PushString(0),
            PushString(0),
            GlobalLookup,
            PushNum(1),
            Add,
            Assign,
            Jump(-12),
        ];
        let chunk = Chunk {
            code,
            number_literals: vec![1.0, 10.0, 0.0],
            string_literals: vec!["a".to_string()],
        };
        eval_chunk(&chunk, &mut HashMap::new()).unwrap();
    }
}
