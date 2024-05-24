//! An `Object` is some data which:
//! - Has an unknown lifetime
//! - May have references to other `Object`s
//!
//! Because of this, it needs to be garbage collected.

use std::borrow::Borrow;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::ops::Drop;
use std::ptr::{self, NonNull};

use super::Chunk;
use super::LuaType;
use super::Table;

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
    // Wrap this in a box to reduce the memory usage. Minimal performance impact
    // because functions are rarely accessed.
    LuaFn(Box<Chunk>),
    Table(Table),
}

impl RawObject {
    #[must_use]
    pub(super) const fn typ(&self) -> LuaType {
        match self {
            RawObject::LuaFn(_) => LuaType::Function,
            RawObject::Table(_) => LuaType::Table,
        }
    }
}

/// The internal pointer type objects use to point to each other.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) struct ObjectPtr {
    ptr: NonNull<WrappedObject>,
}

impl ObjectPtr {
    pub(super) fn as_lua_function(self) -> Option<Chunk> {
        match &self.deref().raw {
            RawObject::LuaFn(chunk) => Some((**chunk).clone()),
            _ => None,
        }
    }

    pub(super) fn as_table(&mut self) -> Option<&mut Table> {
        match &mut self.deref_mut().raw {
            RawObject::Table(t) => Some(t),
            _ => None,
        }
    }

    pub(super) fn typ(self) -> LuaType {
        self.deref().raw.typ()
    }

    fn deref(&self) -> &WrappedObject {
        unsafe { self.ptr.as_ref() }
    }

    fn deref_mut(&mut self) -> &mut WrappedObject {
        unsafe { self.ptr.as_mut() }
    }
}

impl fmt::Display for ObjectPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.deref().raw {
            RawObject::LuaFn(_) => write!(f, "function: {:p}", self.ptr),
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
pub(super) struct GcHeap {
    /// The start of the linked list which contains every Object.
    start: *mut WrappedObject,
    /// The number of objects currently in the heap.
    size: usize,
    /// When the heap grows this large, run the GC.
    threshold: usize,
    /// The collection of interned Strings.
    strings: HashSet<StringPtr>,
}

impl GcHeap {
    /// Create a new heap, with the given initial threshold.
    pub(super) fn with_threshold(threshold: usize) -> Self {
        Self {
            start: ptr::null_mut(),
            size: 0,
            threshold,
            strings: HashSet::new(),
        }
    }

    /// Run the garbage-collector.
    /// Make sure you mark all the roots before calling this function.
    pub(super) fn collect(&mut self) {
        if option_env!("LUA_DEBUG_GC").is_some() {
            println!("Running garbage collector");
            println!("Initial size: {}", self.size);
        }

        let mut next_ptr_ref = &mut self.start;
        while !next_ptr_ref.is_null() {
            // From right-to-left, this unsafe block means:
            // - deref the reference (safe) to get a pointer
            // - deref the pointer (unsafe) to get a WrappedObject
            // - make a mutable reference to that WrappedObject
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

        let mut strings_to_remove = Vec::new();
        for ptr in &self.strings {
            let val = unsafe { ptr.0.as_ref() };
            match val.color.get() {
                Color::Reachable => {
                    val.color.set(Color::Unmarked);
                }
                Color::Unmarked => {
                    strings_to_remove.push(ptr.clone());
                }
            }
        }
        for ptr in strings_to_remove {
            self.strings.remove(&ptr);
            let _boxed = unsafe { Box::from_raw(ptr.0.as_ptr()) };
        }

        self.threshold = self.size * 2;
    }

    #[must_use]
    pub(super) const fn is_full(&self) -> bool {
        self.size >= self.threshold
    }

    pub(super) fn new_lua_fn(&mut self, chunk: Chunk, mark: impl FnOnce()) -> ObjectPtr {
        let raw = RawObject::LuaFn(Box::new(chunk));
        self.new_obj_from_raw(raw, mark)
    }

    pub(super) fn new_string(&mut self, s: String, mark: impl FnOnce()) -> StringPtr {
        if let Some(ptr) = self.strings.get(s.as_str()) {
            ptr.clone()
        } else {
            if self.is_full() {
                mark();
                self.collect();
            }
            let boxed = Box::new(MarkedString {
                data: s,
                color: Cell::new(Color::Unmarked),
            });
            let ptr = StringPtr(NonNull::new(Box::into_raw(boxed)).unwrap());
            self.strings.insert(ptr.clone());
            ptr
        }
    }

    pub(super) fn new_table(&mut self, mark: impl FnOnce()) -> ObjectPtr {
        let raw = RawObject::Table(Table::default());
        self.new_obj_from_raw(raw, mark)
    }

    fn new_obj_from_raw(&mut self, raw: RawObject, mark: impl FnOnce()) -> ObjectPtr {
        if self.is_full() {
            mark();
            self.collect();
        }
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

impl Drop for GcHeap {
    fn drop(&mut self) {
        let mut next_ptr = self.start;
        while !next_ptr.is_null() {
            let boxed = unsafe { Box::from_raw(next_ptr) };
            next_ptr = boxed.next;
            // Now the boxed object is dropped.
        }
        for ptr in self.strings.drain() {
            let _boxed = unsafe { Box::from_raw(ptr.0.as_ptr()) };
        }
    }
}

/// An item is `Markable` if it can be marked as reachable, and thus it and
/// anything it references will not be collected by the GC.
pub(super) trait Markable {
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
            RawObject::LuaFn(_) => (),
            RawObject::Table(tbl) => tbl.mark_reachable(),
        }
    }
}

impl Markable for ObjectPtr {
    fn mark_reachable(&self) {
        self.deref().mark_reachable()
    }
}

/// This impl is mainly for any `Vec<Val>`s we use.
impl<T: Markable> Markable for [T] {
    fn mark_reachable(&self) {
        for val in self {
            val.mark_reachable();
        }
    }
}

/// This is just for the `globals` field of `State`. It can be removed once
/// globals are stored in a normal `Table`.
impl<K, V: Markable> Markable for HashMap<K, V> {
    fn mark_reachable(&self) {
        for val in self.values() {
            val.mark_reachable();
        }
    }
}

struct MarkedString {
    data: String,
    color: Cell<Color>,
}

/// Wrapper type around a pointer to a String.
/// Behaves like a String for the purposes of Hashing and Equality.
#[derive(Clone, Debug)]
pub(crate) struct StringPtr(NonNull<MarkedString>);

impl StringPtr {
    #[must_use]
    pub(crate) fn eq_physical(a: &Self, b: &Self) -> bool {
        a.0 == b.0
    }

    #[must_use]
    pub(crate) fn as_str(&self) -> &str {
        &(unsafe { self.0.as_ref() }).data
    }
}

impl Borrow<str> for StringPtr {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Hash for StringPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl PartialEq for StringPtr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}
impl Eq for StringPtr {}

impl Markable for StringPtr {
    fn mark_reachable(&self) {
        unsafe { self.0.as_ref().color.set(Color::Reachable) }
    }
}

impl fmt::Display for StringPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}
