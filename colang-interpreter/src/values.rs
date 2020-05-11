//! Representing and handling values in CO.

use colang::program::FieldId;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

/// A value in CO.
///
/// Can represent a value of any type and value category. Lvalues are represented as pointers,
/// so that they could be modified.
#[derive(Clone)]
pub enum Value {
    Lvalue(Lvalue),
    Rvalue(Rvalue),
}

impl Value {
    /// Attempts to treat a value as an lvalue.
    ///
    /// Panics if the value is an rvalue.
    pub fn into_lvalue(self) -> Lvalue {
        match self {
            Value::Lvalue(value) => value,
            Value::Rvalue(_) => panic!("Rvalue accessed as lvalue."),
        }
    }

    /// Treats the value as an rvalue.
    ///
    /// This operation is always safe. Any modification to the returned rvalue will not be
    /// propagated to `self` directly.
    pub fn into_rvalue(self) -> Rvalue {
        match self {
            Value::Lvalue(value) => value.detach(),
            Value::Rvalue(value) => value,
        }
    }
}

/// An lvalue in CO: a value with a determined location in memory.
///
/// Lvalues in the interpreter are represented as mutable pointers (`Rc<RefCell<T>>`) to rvalues.
#[derive(Clone)]
pub struct Lvalue(Rc<RefCell<Rvalue>>);

impl Lvalue {
    /// Creates a new lvalue initialized with the value of `rvalue`.
    pub fn store(rvalue: Rvalue) -> Lvalue {
        Lvalue(Rc::new(RefCell::new(rvalue)))
    }

    /// Clones an lvalue to a new, independent location in memory.
    ///
    /// This is equivalent to `Lvalue::store(value.detach())`.
    pub fn clone_contents(&self) -> Lvalue {
        Lvalue::store(self.0.borrow().clone())
    }

    /// Clones the underlying rvalue, returning an independent copy of it.
    pub fn detach(&self) -> Rvalue {
        self.0.borrow().clone()
    }

    /// Borrows the underlying rvalue immutably.
    pub fn borrow(&self) -> Ref<Rvalue> {
        self.0.borrow()
    }

    /// Borrows the underlying rvalue mutably.
    pub fn borrow_mut(&self) -> RefMut<Rvalue> {
        self.0.borrow_mut()
    }

    /// Checks if `other` is an lvalue identifying the same named location.
    pub fn is_same(&self, other: &Lvalue) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

/// An rvalue in CO.
///
/// Every value that exists in the program belongs to this type.
///
/// Some types that are distinct in the compilation phase (currently only `[char]` and `string`)
/// are represented by the same "runtime type" in the interpreter, which makes them
/// indistinguishable without context.
pub enum Rvalue {
    /// `int` type in CO directly maps into Rust `i32`.
    Int(i32),

    /// `bool` type in CO directly maps into Rust `bool`.
    Bool(bool),

    /// `char` type in CO directly maps into Rust `char`.
    Char(u8),

    /// A CO array instance.
    ///
    /// CO arrays are growable arrays (`Vec<T>`) of individually referencable elements (`Lvalue`),
    /// implicitly hidden by a pointer (`Rc<RefCell<T>>`).
    Array(Rc<RefCell<Vec<Lvalue>>>),

    /// A CO pointer instance.
    ///
    /// Pointers in CO represent a mutable location in memory (`Lvalue`), unless they are `null`,
    /// in which case the `Option` is `None`.
    Pointer(Option<Lvalue>),

    /// A CO struct instance.
    ///
    /// CO structs are collections of fields referencable by their `FieldId`s, where each of the
    /// fields is an individually referencable value (`Lvalue`).
    Struct(HashMap<FieldId, Lvalue>),

    /// A value of a CO expression of `void` type.
    ///
    /// This value can only occur when evaluating an expression in a void context. Its propagation
    /// is restricted to minimum.
    Void,
}

impl Rvalue {
    /// Creates a character array rvalue from an `&str`.
    ///
    /// Note that there is no distinction in the interpreter between `[char]` and `string`.
    pub fn from_str(contents: &str) -> Rvalue {
        let chars = contents
            .as_bytes()
            .iter()
            .map(|ch| Lvalue::store(Rvalue::Char(*ch)))
            .collect();
        Rvalue::Array(Rc::new(RefCell::new(chars)))
    }

    /// Asserts that the value has type `int` and extracts the underlying integer value.
    pub fn as_int(&self) -> i32 {
        match self {
            Rvalue::Int(x) => *x,
            _ => panic_wrong_type("int", self.type_()),
        }
    }

    /// Asserts that the value has type `bool` and extracts the underlying boolean value.
    pub fn as_bool(&self) -> bool {
        match self {
            Rvalue::Bool(b) => *b,
            _ => panic_wrong_type("bool", self.type_()),
        }
    }

    /// Asserts that the value has type `char` and extracts the underlying character value.
    pub fn as_char(&self) -> u8 {
        match self {
            Rvalue::Char(c) => *c,
            _ => panic_wrong_type("char", self.type_()),
        }
    }

    /// Asserts that the value is a character array representing a valid UTF-8 string and extracts
    /// that string.
    pub fn as_utf8_string(&self) -> String {
        match self {
            Rvalue::Array(v) => {
                let chars = v
                    .borrow()
                    .iter()
                    .map(|ch| match *ch.borrow() {
                        Rvalue::Char(ch) => ch,
                        _ => panic_wrong_type("char", ch.borrow().type_()),
                    })
                    .collect();
                String::from_utf8(chars).expect("string is not a valid UTF-8 sequence")
            }
            _ => panic_wrong_type("array", self.type_()),
        }
    }

    /// Asserts that the value is an array and extracts the underlying value.
    pub fn into_array(self) -> Rc<RefCell<Vec<Lvalue>>> {
        match self {
            Rvalue::Array(v) => v,
            _ => panic_wrong_type("array", self.type_()),
        }
    }

    /// Asserts that the value is a pointer and extracts the underlying value.
    pub fn into_pointer(self) -> Option<Lvalue> {
        match self {
            Rvalue::Pointer(p) => p,
            _ => panic_wrong_type("pointer", self.type_()),
        }
    }

    /// Asserts that the value is a struct instance and accesses the underlying field map.
    pub fn as_struct(&self) -> &HashMap<FieldId, Lvalue> {
        match self {
            Rvalue::Struct(fields) => fields,
            _ => panic_wrong_type("struct", self.type_()),
        }
    }

    /// Provides quick-and-dirty information about the value type in runtime.
    ///
    /// This is _not_ meant to be an actual RTTI solution, it is only meant to be used in
    /// type mismatch panics, which should not occur under normal circumstances.
    pub fn type_(&self) -> &'static str {
        match self {
            Rvalue::Int(_) => "int",
            Rvalue::Bool(_) => "bool",
            Rvalue::Char(_) => "char",
            Rvalue::Array(_) => "array",
            Rvalue::Pointer(_) => "pointer",
            Rvalue::Struct(_) => "struct",
            Rvalue::Void => "void",
        }
    }
}

impl Clone for Rvalue {
    fn clone(&self) -> Self {
        match self {
            Rvalue::Int(x) => Rvalue::Int(*x),
            Rvalue::Bool(b) => Rvalue::Bool(*b),
            Rvalue::Char(c) => Rvalue::Char(*c),
            Rvalue::Array(v) => Rvalue::Array(Rc::clone(v)),
            Rvalue::Pointer(p) => Rvalue::Pointer(p.clone()),
            Rvalue::Struct(fields) => Rvalue::Struct(
                fields
                    .iter()
                    .map(|(id, lvalue)| (id.clone(), lvalue.clone_contents()))
                    .collect(),
            ),
            Rvalue::Void => Rvalue::Void,
        }
    }
}

fn panic_wrong_type(expected_type: &str, actual_type: &str) -> ! {
    panic!(
        "Runtime type mismatch: expected `{}`, got `{}`",
        expected_type, actual_type
    )
}
