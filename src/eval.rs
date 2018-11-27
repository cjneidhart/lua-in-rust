use std::hash::Hash;
use simple_types::Instr;

#[derive(Clone, Debug)]
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

pub fn eval_expr(input: Vec<Instr>) -> LuaVal {
    let mut stack = Vec::new();
    for instr in input.into_iter() {
        match instr {
            Instr::PushNil => stack.push(Nil),
            Instr::PushBool(b) => stack.push(Bool(b)),
            Instr::PushNum(n) => stack.push(Number(n)),
            Instr::PushString(s) => stack.push(LuaString(s)),
            Instr::Add => {
                let e1 = stack.pop().unwrap();
                let e2 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Number(n1 + n2)),
                    _ => panic!(),
                }
            }
            Instr::Subtract => {
                let e1 = stack.pop().unwrap();
                let e2 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Number(n1 - n2)),
                    _ => panic!(),
                }
            }
            Instr::Multiply => {
                let e1 = stack.pop().unwrap();
                let e2 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Number(n1 * n2)),
                    _ => panic!(),
                }
            }
            Instr::Divide => {
                let e1 = stack.pop().unwrap();
                let e2 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Number(n1 / n2)),
                    _ => panic!(),
                }
            }
            Instr::Mod => {
                let e1 = stack.pop().unwrap();
                let e2 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Number(n1 % n2)),
                    _ => panic!(),
                }
            }
            Instr::Pow => {
                let e2 = stack.pop().unwrap();
                let e1 = stack.pop().unwrap();
                match (e1, e2) {
                    (Number(n1), Number(n2)) => stack.push(Number(n1.powf(n2))),
                    _ => panic!(),
                }
            }
            Instr::Negate => {
                let e = stack.pop().unwrap();
                match e {
                    Number(n) => stack.push(Number(-n)),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    stack.pop().unwrap()
}