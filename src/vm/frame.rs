use std::ops;

use super::Chunk;
use super::ErrorKind;
use super::Instr;
use super::Markable;
use super::Result;
use super::State;
use super::Val;

/// A `Frame` represents a single stack-frame of a Lua function.
#[derive(Default)]
pub(super) struct Frame {
    /// The chunk being executed
    chunk: Chunk,
    /// The index of the next (not current) instruction
    ip: usize,
    /// The chunk's literal strings, pre-allocated; guaranteed to be strings.
    string_literals: Vec<Val>,
}

impl Frame {
    /// Create a new Frame.
    pub(super) fn new(chunk: Chunk, string_literals: Vec<Val>) -> Self {
        let ip = 0;
        Self {
            chunk,
            ip,
            string_literals,
        }
    }

    /// Jump forward/back by `offset` instructions.
    fn jump(&mut self, offset: isize) {
        self.ip = self.ip.wrapping_add(offset as usize);
    }

    /// Get the instruction at the instruction pointer, and advance the
    /// instruction pointer accordingly.
    fn get_instr(&mut self) -> Instr {
        let i = self.chunk.code[self.ip];
        self.ip += 1;
        i
    }

    fn get_nested_chunk(&mut self, i: u8) -> Chunk {
        self.chunk.nested[i as usize].clone()
    }

    fn get_number_constant(&self, i: u8) -> f64 {
        self.chunk.number_literals[i as usize]
    }

    fn get_string_constant(&self, i: u8) -> Val {
        self.string_literals[i as usize].clone()
    }

    /// Start evaluating instructions from the current position.
    pub(super) fn eval(&mut self, state: &mut State) -> Result<()> {
        loop {
            let inst = self.get_instr();
            if option_env!("LUA_DEBUG_VM").is_some() {
                println!("{:?}", inst);
            }
            match inst {
                // General control flow
                Instr::Pop => {
                    state.pop_val();
                }
                Instr::Jump(offset) => self.jump(offset),
                Instr::BranchFalse(ofst) => state.instr_branch(self, false, ofst, false),
                Instr::BranchFalseKeep(ofst) => state.instr_branch(self, false, ofst, true),
                //Instr::BranchTrue(ofst) => state.instr_branch(self, true, ofst, false),
                Instr::BranchTrueKeep(ofst) => state.instr_branch(self, true, ofst, true),

                // Local variables
                Instr::GetLocal(i) => state.instr_get_local(i),
                Instr::SetLocal(i) => state.instr_set_local(i),

                Instr::GetGlobal(i) => state.instr_get_global(self, i),
                Instr::SetGlobal(i) => state.instr_set_global(self, i),

                // Functions
                Instr::Closure(i) => state.instr_closure(self, i),
                Instr::Call(num_args) => state.call(num_args, 0)?,
                Instr::Return => {
                    return Ok(());
                }

                // Literals
                Instr::PushNil => state.push_nil(),
                Instr::PushBool(b) => state.push_boolean(b),
                Instr::PushNum(i) => {
                    let n = self.get_number_constant(i);
                    state.push_number(n);
                }
                Instr::PushString(i) => {
                    // TODO Optimize this so we don't have to clone every time.
                    let s = self.chunk.string_literals[i as usize].clone();
                    state.push_string(s);
                }

                // Arithmetic
                Instr::Add => state.eval_float_float(<f64 as ops::Add>::add)?,
                Instr::Subtract => state.eval_float_float(<f64 as ops::Sub>::sub)?,
                Instr::Multiply => state.eval_float_float(<f64 as ops::Mul>::mul)?,
                Instr::Divide => state.eval_float_float(<f64 as ops::Div>::div)?,
                Instr::Mod => state.eval_float_float(<f64 as ops::Rem>::rem)?,
                Instr::Pow => state.eval_float_float(f64::powf)?,

                // Equality
                Instr::Equal => {
                    let val2 = state.pop_val();
                    let val1 = state.pop_val();
                    state.push_boolean(val1 == val2);
                }
                Instr::NotEqual => {
                    let val2 = state.pop_val();
                    let val1 = state.pop_val();
                    state.push_boolean(val1 != val2);
                }

                // Orderings
                Instr::Less => state.eval_float_bool(<f64 as PartialOrd>::lt)?,
                Instr::Greater => state.eval_float_bool(<f64 as PartialOrd>::gt)?,
                Instr::LessEqual => state.eval_float_bool(<f64 as PartialOrd>::le)?,
                Instr::GreaterEqual => state.eval_float_bool(<f64 as PartialOrd>::ge)?,

                // `for` loops
                Instr::ForLoop(slot, offset) => state.instr_for_loop(self, slot, offset)?,
                Instr::ForPrep(slot, len) => state.instr_for_prep(self, slot, len)?,

                // Unary
                Instr::Negate => state.instr_negate()?,
                Instr::Not => state.instr_not(),

                // Manipulating tables
                Instr::NewTable => state.create_table(0),
                Instr::GetField(i) => state.instr_get_field(self, i)?,
                Instr::GetTable => state.instr_get_table()?,
                Instr::InitField(i) => state.instr_init_field(self, i)?,
                Instr::SetField(offset, i) => state.instr_set_field(self, offset, i)?,
                Instr::SetTable(offset) => state.instr_set_table(offset)?,

                // Misc.
                Instr::Concat => {
                    state.concat(2)?;
                }

                _ => panic!("Unsupported: {:?}", inst),
            }
        }
    }
}

// Instruction-specific methods
impl State {
    /// Pop a value. If its truthiness matches `cond`, jump with `offset`.
    /// If `keep_cond`, then push the value back after jumping.
    fn instr_branch(&mut self, frame: &mut Frame, cond: bool, offset: isize, keep_cond: bool) {
        let val = self.pop_val();
        let truthy = val.truthy();
        if cond == truthy {
            frame.jump(offset);
        }
        if keep_cond {
            self.stack.push(val);
        }
    }

    fn instr_closure(&mut self, frame: &mut Frame, i: u8) {
        self.check_heap();
        let chunk = frame.get_nested_chunk(i);
        let obj = self.heap.new_lua_fn(chunk);
        self.stack.push(Val::Obj(obj));
    }

    fn instr_for_prep(&mut self, frame: &mut Frame, local: u8, body_len: isize) -> Result<()> {
        let step = self.pop_num()?;
        let end = self.pop_num()?;
        let start = self.pop_num()?;
        if check_numeric_for_condition(start, end, step) {
            let mut local_slot = local as usize + self.stack_bottom;
            for &n in &[start, end, step, start] {
                self.stack[local_slot] = Val::Num(n);
                local_slot += 1;
            }
        } else {
            frame.jump(body_len);
        }
        Ok(())
    }

    fn instr_for_loop(&mut self, frame: &mut Frame, local_slot: u8, offset: isize) -> Result<()> {
        let slot = local_slot as usize + self.stack_bottom;
        let mut var = self.stack[slot].as_num().unwrap();
        let limit = self.stack[slot + 1].as_num().unwrap();
        let step = self.stack[slot + 2].as_num().unwrap();
        var += step;
        if check_numeric_for_condition(var, limit, step) {
            self.stack[slot] = Val::Num(var);
            self.stack[slot + 3] = Val::Num(var);
            frame.jump(offset);
        }
        Ok(())
    }

    fn instr_get_field(&mut self, frame: &mut Frame, field_id: u8) -> Result<()> {
        let mut tbl_val = self.pop_val();
        if let Some(t) = tbl_val.as_table() {
            let key = frame.get_string_constant(field_id);
            let val = t.get(&key);
            self.stack.push(val.clone());
            Ok(())
        } else {
            Err(self.type_error())
        }
    }

    fn instr_get_global(&mut self, frame: &Frame, string_num: u8) {
        let s = &frame.chunk.string_literals[string_num as usize];
        self.get_global(s);
    }

    fn instr_get_local(&mut self, local_num: u8) {
        let i = local_num as usize + self.stack_bottom;
        let val = self.stack[i].clone();
        self.stack.push(val);
    }

    fn instr_get_table(&mut self) -> Result<()> {
        let key = self.pop_val();
        let mut tbl = self.pop_val();
        if let Some(t) = tbl.as_table() {
            self.stack.push(t.get(&key));
            Ok(())
        } else {
            Err(self.type_error())
        }
    }

    fn instr_init_field(&mut self, frame: &Frame, i: u8) -> Result<()> {
        let val = self.pop_val();
        let mut tbl = self.pop_val();
        let t = tbl.as_table().unwrap();
        let key = frame.get_string_constant(i);
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

    fn instr_set_field(&mut self, frame: &Frame, stack_offset: u8, field_id: u8) -> Result<()> {
        let val = self.pop_val();
        let idx = self.stack.len() - stack_offset as usize - 1;
        let mut tbl = self.stack.remove(idx);
        if let Some(t) = tbl.as_table() {
            let key = frame.get_string_constant(field_id);
            t.insert(key, val)?;
            Ok(())
        } else {
            Err(self.error(ErrorKind::TypeError))
        }
    }

    fn instr_set_global(&mut self, frame: &Frame, string_num: u8) {
        let s = frame.get_string_constant(string_num);
        let val = self.pop_val();
        if let Some(s) = s.as_string() {
            self.globals.insert(s.into(), val);
        } else {
            // TODO handle this better
            panic!("Tried to index globals with something other than a string.");
        }
    }

    fn instr_set_local(&mut self, local_num: u8) {
        let val = self.pop_val();
        let i = local_num as usize + self.stack_bottom;
        self.stack[i] = val;
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

    // Helper methods

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

    fn pop_num(&mut self) -> Result<f64> {
        self.pop_val().as_num().ok_or_else(|| self.type_error())
    }
}

impl Markable for Frame {
    fn mark_reachable(&self) {
        for val in &self.string_literals {
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
