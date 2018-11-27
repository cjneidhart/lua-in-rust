use std::ops::{Div, Mul, Rem, Sub};

use simple_types::Instr;

#[derive(Clone, Debug, PartialEq)]
pub enum LuaVal {
    Nil,
    Bool(bool),
    Number(f64),
    LuaString(String),
    //Table(LuaTable),
    // Function(LuaFunc),
}
use self::LuaVal::*;

impl LuaVal {
    pub fn truthy(&self) -> bool {
        match self {
            Nil | Bool(false) => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    StackError,
    SingleTypeError(Instr, LuaVal),
    DoubleTypeError(Instr, LuaVal, LuaVal),
    Other,
}

pub fn eval_expr(input: Vec<Instr>) -> Result<LuaVal, EvalError> {
    let mut stack = Vec::new();
    for instr in input.into_iter() {
        use self::Instr::*;
        match instr {
            // Literals
            PushNil => stack.push(Nil),
            PushBool(b) => stack.push(Bool(b)),
            PushNum(n) => stack.push(Number(n)),
            PushString(s) => stack.push(LuaString(s)),

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
            Less => eval_float_bool(<f64 as PartialOrd<f64>>::gt, instr, &mut stack)?,
            Greater => eval_float_bool(<f64 as PartialOrd<f64>>::gt, instr, &mut stack)?,
            LessEqual => eval_float_bool(<f64 as PartialOrd<f64>>::le, instr, &mut stack)?,
            GreaterEqual => eval_float_bool(<f64 as PartialOrd<f64>>::ge, instr, &mut stack)?,

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

            _ => panic!(),
        }
    }

    safe_pop(&mut stack)
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

fn safe_pop(stack: &mut Vec<LuaVal>) -> Result<LuaVal, EvalError> {
    stack.pop().ok_or(EvalError::StackError)
}

#[cfg(test)]
mod tests {
    use simple_types::Instr::*;
    use super::*;

    #[test]
    fn test1() {
        let input = vec![
            PushNum(2.0),
            PushNum(1.0),
            Add
        ];
        assert_eq!(eval_expr(input).unwrap(), LuaVal::Number(3.0));
    }

    #[test]
    fn test2() {
        let input = vec![PushNum(2.0), PushNum(1.0), Subtract];
        assert_eq!(eval_expr(input).unwrap(), LuaVal::Number(1.0));
    }

    #[test]
    fn test3() {
        let input = vec![PushNum(3.5), PushNum(2.0), Multiply];
        check_it(input, LuaVal::Number(7.0));
    }

    #[test]
    fn test4() {
        let input = vec![PushNum(2.0), PushNum(2.0), Equal];
        check_it(input, LuaVal::Bool(true));
    }

    fn check_it(input: Vec<Instr>, output: LuaVal) {
        assert_eq!(eval_expr(input).unwrap(), output);
    }
}
