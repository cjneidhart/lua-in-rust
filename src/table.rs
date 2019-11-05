use crate::Error;
use crate::ErrorKind;
use crate::Markable;
use crate::Result;
use crate::Val;

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct Table {
    map: HashMap<Val, Val>,
}

impl Table {
    pub fn get(&self, key: &Val) -> Val {
        match key {
            Val::Nil => Val::Nil,
            Val::Num(n) if n.is_nan() => Val::Nil,
            _ => self.map.get(key).cloned().unwrap_or_default(),
        }
    }

    pub fn insert(&mut self, key: Val, value: Val) -> Result<()> {
        match key {
            Val::Nil => Err(Error::new(ErrorKind::TableKeyNil, 0, 0)),
            Val::Num(n) if n.is_nan() => Err(Error::new(ErrorKind::TableKeyNan, 0, 0)),
            _ => {
                self.map.insert(key, value);
                Ok(())
            }
        }
    }
}

impl Markable for Table {
    fn mark_reachable(&self) {
        for (k, v) in &self.map {
            k.mark_reachable();
            v.mark_reachable();
        }
    }
}
