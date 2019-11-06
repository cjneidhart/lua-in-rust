use std::ops;

use crate::Chunk;
use crate::Error;
use crate::ErrorKind;
use crate::Instr;
use crate::Markable;
use crate::Result;
use crate::Val;

/// A `Frame` represents a single stack-frame of a Lua function.
pub struct Frame {
    /// The chunk being executed
    pub chunk: Chunk,
    /// The stack of local variables and temp values
    pub stack: Vec<Val>,
    /// The index of the next (not current) instruction
    ip: usize,
    /// The chunk's literal strings, pre-allocated; guaranteed to be strings.
    string_literals: Vec<Val>,
}

/// Although `Frame` manages most of the core evaluation, sometimes it needs to
/// access the global `State`. When that happens, it returns an `Action` to the
/// `State`, indicating what it needs.
pub enum Action {
    /// Get the given string from this frame and look up the global variable of
    /// the same name.
    GetGlobal(u8),
    /// Get the given string from this frame, and the value ontop of this
    /// frame's stack. Use those to assign to a global variable.
    SetGlobal(u8),
    /// Allocate a new string. Only the VM can do this, since it might trigger
    /// a garbage collection.
    AllocString(String),
    /// Allocate an empty table. Only the VM can do this, since it might trigger
    /// a garbage collection.
    AllocTable,
    /// Call another function. The single parameter to this variant is how many
    /// arguments are given. That many arguments will be ontop of the frame's
    /// stack. Just under the arguments is the function to be called.
    Call(u8),
    /// The `Frame` has finished evaluating.
    Return,
}

impl Frame {
    /// Create a new Frame.
    pub fn new(chunk: Chunk, string_literals: Vec<Val>) -> Self {
        let mut stack = Vec::new();
        for _ in 0..(chunk.num_locals) {
            stack.push(Val::Nil);
        }
        let ip = 0;
        Self {
            chunk,
            stack,
            ip,
            string_literals,
        }
    }

    /// Start evaluating instructions from the current position.
    pub fn eval(&mut self) -> Result<Action> {
        loop {
            let inst = self.get_instr();
            match inst {
                // General control flow
                Instr::Pop => {
                    self.pop_val();
                }
                Instr::Jump(offset) => self.jump(offset),
                Instr::BranchFalse(offset) => {
                    if !self.pop_val().truthy() {
                        self.jump(offset);
                    }
                }
                Instr::BranchTrue(offset) => {
                    if self.pop_val().truthy() {
                        self.jump(offset);
                    }
                }
                Instr::BranchFalseKeep(offset) => {
                    let val = self.pop_val();
                    if !val.truthy() {
                        self.jump(offset);
                    }
                    self.stack.push(val);
                }
                Instr::BranchTrueKeep(offset) => {
                    let val = self.pop_val();
                    if val.truthy() {
                        self.jump(offset);
                    }
                    self.stack.push(val);
                }

                // Local variables
                Instr::GetLocal(i) => {
                    let val = self.stack[i as usize].clone();
                    self.stack.push(val);
                }
                Instr::SetLocal(i) => {
                    self.stack[i as usize] = self.stack.pop().unwrap();
                }

                // Actions which require help from the `State`.
                Instr::GetGlobal(i) => {
                    return Ok(Action::GetGlobal(i));
                }
                Instr::SetGlobal(i) => {
                    return Ok(Action::SetGlobal(i));
                }
                Instr::Call(num_args) => {
                    return Ok(Action::Call(num_args));
                }
                Instr::NewTable => {
                    return Ok(Action::AllocTable);
                }
                Instr::Return => {
                    return Ok(Action::Return);
                }

                // Literals
                Instr::PushNil => self.stack.push(Val::Nil),
                Instr::PushBool(b) => self.stack.push(Val::Bool(b)),
                Instr::PushNum(i) => {
                    let n = self.get_number_constant(i);
                    self.stack.push(Val::Num(n));
                }
                Instr::PushString(i) => {
                    let s = self.get_string_constant(i);
                    self.stack.push(s);
                }

                // Arithmetic
                Instr::Add => self.eval_float_float(<f64 as ops::Add>::add)?,
                Instr::Subtract => self.eval_float_float(<f64 as ops::Sub>::sub)?,
                Instr::Multiply => self.eval_float_float(<f64 as ops::Mul>::mul)?,
                Instr::Divide => self.eval_float_float(<f64 as ops::Div>::div)?,
                Instr::Mod => self.eval_float_float(<f64 as ops::Rem>::rem)?,
                Instr::Pow => self.eval_float_float(f64::powf)?,

                // Equality
                Instr::Equal => {
                    let val2 = self.pop_val();
                    let val1 = self.pop_val();
                    self.stack.push(Val::Bool(val1 == val2));
                }
                Instr::NotEqual => {
                    let val2 = self.pop_val();
                    let val1 = self.pop_val();
                    self.stack.push(Val::Bool(val1 != val2));
                }

                // Orderings
                Instr::Less => self.eval_float_bool(<f64 as PartialOrd>::lt)?,
                Instr::Greater => self.eval_float_bool(<f64 as PartialOrd>::gt)?,
                Instr::LessEqual => self.eval_float_bool(<f64 as PartialOrd>::le)?,
                Instr::GreaterEqual => self.eval_float_bool(<f64 as PartialOrd>::ge)?,

                // `for` loops
                Instr::ForLoop(slot, offset) => self.instr_for_loop(slot, offset),
                // TODO remove this cast.
                Instr::ForPrep(slot, len) => self.instr_for_prep(slot, len as usize)?,

                // Unary
                Instr::Negate => self.instr_negate()?,
                Instr::Not => self.instr_not(),

                // Manipulating tables
                Instr::GetField(i) => self.instr_get_field(i)?,
                Instr::GetTable => self.instr_get_table()?,
                Instr::InitField(i) => self.instr_init_field(i)?,
                Instr::SetField(offset, i) => self.instr_set_field(offset, i)?,
                Instr::SetTable(offset) => self.instr_set_table(offset)?,

                // Misc.
                Instr::Concat => {
                    let s = self.instr_concat()?;
                    return Ok(Action::AllocString(s));
                }
                Instr::Print => {
                    println!("{}", self.pop_val());
                }

                _ => panic!("Unsupported: {:?}", inst),
            }
        }
    }

    // Helper methods

    fn error(&mut self, _kind: ErrorKind) -> Error {
        unimplemented!();
    }

    fn eval_float_bool(&mut self, f: impl Fn(&f64, &f64) -> bool) -> Result<()> {
        let n2 = self.pop_num()?;
        let n1 = self.pop_num()?;
        self.stack.push(Val::Bool(f(&n1, &n2)));
        Ok(())
    }

    fn eval_float_float(&mut self, f: impl Fn(f64, f64) -> f64) -> Result<()> {
        let n2 = self.pop_num()?;
        let n1 = self.pop_num()?;
        self.stack.push(Val::Num(f(n1, n2)));
        Ok(())
    }

    /// Get the instruction at the instruction pointer, and advance the
    /// instruction pointer accordingly.
    fn get_instr(&mut self) -> Instr {
        let i = self.chunk.code[self.ip];
        self.ip += 1;
        i
    }

    fn get_number_constant(&self, i: u8) -> f64 {
        self.chunk.number_literals[i as usize]
    }

    fn get_string_constant(&self, i: u8) -> Val {
        self.string_literals[i as usize].clone()
    }

    fn jump(&mut self, offset: isize) {
        self.ip = self.ip.wrapping_add(offset as usize);
    }

    fn pop_num(&mut self) -> Result<f64> {
        self.pop_val().as_num().ok_or_else(|| self.type_error())
    }

    fn pop_val(&mut self) -> Val {
        self.stack.pop().unwrap()
    }

    fn type_error(&mut self) -> Error {
        self.error(ErrorKind::TypeError)
    }

    // Instruction-specific methods

    /// Pop two values from the stack and concatenate them. Instead of pushing
    /// the result to the stack immediately, this function returns the `String`
    /// so the VM can properly allocate it.
    fn instr_concat(&mut self) -> Result<String> {
        let val2 = self.pop_val();
        let val1 = self.pop_val();
        if let (Some(s1), Some(s2)) = (val1.as_string(), val2.as_string()) {
            let mut new_str = String::new();
            new_str.push_str(s1);
            new_str.push_str(s2);
            Ok(new_str)
        } else {
            Err(self.type_error())
        }
    }

    fn instr_for_prep(&mut self, local_slot: u8, body_length: usize) -> Result<()> {
        let step = self.pop_num()?;
        let end = self.pop_num()?;
        let start = self.pop_num()?;
        if check_numeric_for_condition(start, end, step) {
            let mut local_slot = local_slot as usize;
            for &n in &[start, end, step, start] {
                self.stack[local_slot] = Val::Num(n);
                local_slot += 1;
            }
        } else {
            self.ip += body_length;
        }
        Ok(())
    }

    fn instr_for_loop(&mut self, local_slot: u8, offset: isize) {
        let slot = local_slot as usize;
        let mut var = self.stack[slot].as_num().unwrap();
        let limit = self.stack[slot + 1].as_num().unwrap();
        let step = self.stack[slot + 2].as_num().unwrap();
        var += step;
        if check_numeric_for_condition(var, limit, step) {
            self.stack[slot] = Val::Num(var);
            self.stack[slot + 3] = Val::Num(var);
            self.jump(offset);
        }
    }

    fn instr_get_field(&mut self, field_id: u8) -> Result<()> {
        let mut tbl_val = self.pop_val();
        if let Some(t) = tbl_val.as_table() {
            let key = self.get_string_constant(field_id);
            let val = t.get(&key);
            self.stack.push(val.clone());
            Ok(())
        } else {
            Err(self.error(ErrorKind::TypeError))
        }
    }

    fn instr_get_table(&mut self) -> Result<()> {
        let key = self.pop_val();
        let mut tbl = self.pop_val();
        if let Some(t) = tbl.as_table() {
            self.stack.push(t.get(&key));
            Ok(())
        } else {
            Err(self.error(ErrorKind::TypeError))
        }
    }

    fn instr_init_field(&mut self, i: u8) -> Result<()> {
        let val = self.pop_val();
        let mut tbl = self.pop_val();
        let t = tbl.as_table().unwrap();
        let key = self.get_string_constant(i);
        t.insert(key, val)?;
        self.stack.push(tbl);
        Ok(())
    }

    fn instr_negate(&mut self) -> Result<()> {
        let n = self.pop_num()?;
        self.stack.push(Val::Num(-n));
        Ok(())
    }

    fn instr_not(&mut self) {
        let b = self.pop_val().truthy();
        self.stack.push(Val::Bool(!b));
    }

    fn instr_set_field(&mut self, stack_offset: u8, field_id: u8) -> Result<()> {
        let val = self.pop_val();
        let idx = self.stack.len() - stack_offset as usize - 1;
        let mut tbl = self.stack.remove(idx);
        if let Some(t) = tbl.as_table() {
            let key = self.get_string_constant(field_id);
            t.insert(key, val)?;
            Ok(())
        } else {
            Err(self.error(ErrorKind::TypeError))
        }
    }

    fn instr_set_table(&mut self, offset: u8) -> Result<()> {
        let val = self.pop_val();
        let index = self.stack.len() - offset as usize - 2;
        let mut tbl = self.stack.remove(index);
        let key = self.stack.remove(index);
        if let Some(t) = tbl.as_table() {
            t.insert(key, val)?;
            Ok(())
        } else {
            Err(self.error(ErrorKind::TypeError))
        }
    }
}

impl Markable for Frame {
    fn mark_reachable(&self) {
        let iter = self.stack.iter().chain(&self.string_literals);
        for val in iter {
            val.mark_reachable()
        }
    }
}

fn check_numeric_for_condition(var: f64, limit: f64, step: f64) -> bool {
    if step > 0.0 {
        var <= limit
    } else if step <= 0.0 {
        var >= limit
    } else {
        false
    }
}
