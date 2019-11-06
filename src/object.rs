//! An `Object` is some data which:
//! - Has an unknown lifetime
//! - May have references to other `Object`s
//!
//! Because of this, it needs to be garbage collected.

use std::cell::Cell;
use std::fmt::{self, Display};
use std::ops::Drop;
use std::ptr::{self, NonNull};

use crate::Table;

/// A wrapper around the `LuaVal`s which need to be garbage-collected.
struct WrappedObject {
    /// The value this object holds.
    raw: RawObject,
    /// The next object in the heap.
    next: *mut WrappedObject,
    /// A flag used in garbage-collection. This is behind a `Cell` so that
    /// we can alter the keys of a table.
    color: Cell<Color>,
}

enum RawObject {
    Table(Table),
    Str(String),
}

impl RawObject {
    pub fn type_string(&self) -> &'static str {
        match self {
            RawObject::Str(_) => "string",
            RawObject::Table(_) => "table",
        }
    }
}

/// The internal pointer type objects use to point to each other.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ObjectPtr {
    ptr: NonNull<WrappedObject>,
}

impl ObjectPtr {
    pub fn as_string(&self) -> Option<&str> {
        match &self.deref().raw {
            RawObject::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_table(&mut self) -> Option<&mut Table> {
        match &mut self.deref_mut().raw {
            RawObject::Table(t) => Some(t),
            _ => None,
        }
    }

    pub fn type_string(self) -> &'static str {
        self.deref().raw.type_string()
    }

    fn deref(&self) -> &WrappedObject {
        unsafe { self.ptr.as_ref() }
    }

    fn deref_mut(&mut self) -> &mut WrappedObject {
        unsafe { self.ptr.as_mut() }
    }
}

impl Display for ObjectPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.deref().raw {
            RawObject::Str(s) => Display::fmt(s, f),
            RawObject::Table(_) => write!(f, "table: {:p}", self.ptr),
        }
    }
}

#[derive(Clone, Copy)]
enum Color {
    Unmarked,
    Reachable,
}

/// A collection of objects which need to be garbage-collected.
pub struct GcHeap {
    /// The start of the linked list which contains every Object.
    start: *mut WrappedObject,
    /// The number of objects currently in the heap.
    size: usize,
    /// When the heap grows this large, run the GC.
    threshold: usize,
}

impl GcHeap {
    /// Run the garbage-collector.
    /// Make sure you mark all the roots before calling this function.
    pub fn collect(&mut self) {
        if option_env!("LUA_DEBUG_GC").is_some() {
            println!("Running garbage collector");
            println!("Initial size: {}", self.size);
        }

        let mut next_ptr_ref = &mut self.start;
        while !next_ptr_ref.is_null() {
            // From right-to-left, this unsafe block means:
            // - deref the reference (safe) to get a pointer
            // - deref the pointer (unsafe) to get an Object
            // - make a mutable reference to that Object
            let next_obj = unsafe { &mut **next_ptr_ref };
            match next_obj.color.get() {
                Color::Reachable => {
                    // Reset its color.
                    next_obj.color.set(Color::Unmarked);
                    next_ptr_ref = &mut next_obj.next;
                }
                Color::Unmarked => {
                    let boxed = unsafe { Box::from_raw(*next_ptr_ref) };
                    *next_ptr_ref = boxed.next;
                    self.size -= 1;
                }
            }
        }
        self.threshold = self.size * 2;
    }

    pub fn is_full(&self) -> bool {
        self.size >= self.threshold
    }

    pub fn new_string(&mut self, s: String) -> ObjectPtr {
        let raw = RawObject::Str(s);
        self.new_obj_from_raw(raw)
    }

    pub fn new_table(&mut self) -> ObjectPtr {
        let raw = RawObject::Table(Table::default());
        self.new_obj_from_raw(raw)
    }

    fn new_obj_from_raw(&mut self, raw: RawObject) -> ObjectPtr {
        let new_object = WrappedObject {
            next: self.start,
            color: Cell::new(Color::Unmarked),
            raw,
        };
        let boxed = Box::new(new_object);
        let raw_ptr = Box::into_raw(boxed);
        // Pointers from Box::into_raw are guaranteed to not be null.
        let obj_ptr = ObjectPtr {
            ptr: NonNull::new(raw_ptr).unwrap(),
        };

        self.start = raw_ptr;
        self.size += 1;

        obj_ptr
    }
}

impl Default for GcHeap {
    fn default() -> Self {
        Self {
            start: ptr::null_mut(),
            size: 0,
            threshold: 20,
        }
    }
}

impl Drop for GcHeap {
    fn drop(&mut self) {
        let mut next_ptr = self.start;
        while !next_ptr.is_null() {
            let boxed = unsafe { Box::from_raw(next_ptr) };
            next_ptr = boxed.next;
            // Now the boxed object is dropped.
        }
    }
}

pub trait Markable {
    /// Mark this item and the references it contains as reachable.
    fn mark_reachable(&self);
}

impl Markable for WrappedObject {
    fn mark_reachable(&self) {
        if let Color::Unmarked = self.color.get() {
            self.color.set(Color::Reachable);
            self.raw.mark_reachable();
        }
    }
}

impl Markable for RawObject {
    fn mark_reachable(&self) {
        match self {
            RawObject::Str(_) => (),
            RawObject::Table(tbl) => tbl.mark_reachable(),
        }
    }
}

impl Markable for ObjectPtr {
    fn mark_reachable(&self) {
        self.deref().mark_reachable()
    }
}
