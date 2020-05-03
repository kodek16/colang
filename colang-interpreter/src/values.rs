use crate::errors::RuntimeError;
use crate::panic_wrong_type;
use crate::RunResult;
use colang::program::FieldId;
use colang::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Lvalue(Lvalue),
    Rvalue(Rvalue),
}

impl Value {
    pub fn into_lvalue(self) -> Lvalue {
        match self {
            Value::Lvalue(value) => value,
            Value::Rvalue(_) => panic!("Rvalue accessed as lvalue."),
        }
    }

    pub fn into_rvalue(self) -> Rvalue {
        match self {
            Value::Lvalue(value) => (*value.borrow()).clone(),
            Value::Rvalue(value) => value,
        }
    }
}

#[derive(Clone)]
pub struct Lvalue(Rc<RefCell<Rvalue>>);

impl Lvalue {
    pub fn store(rvalue: Rvalue) -> Lvalue {
        Lvalue(Rc::new(RefCell::new(rvalue)))
    }

    pub fn clone_contents(&self) -> Lvalue {
        Lvalue::store(self.0.borrow().clone())
    }

    pub fn detach(&self) -> Rvalue {
        self.0.borrow().clone()
    }

    pub fn borrow(&self) -> impl Deref<Target = Rvalue> + '_ {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> impl DerefMut<Target = Rvalue> + '_ {
        self.0.borrow_mut()
    }

    /// Checks if `other` is an lvalue identifying the same named location.
    pub fn is_same(&self, other: &Lvalue) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

/// Every value that exists in the program belongs to this type.
pub enum Rvalue {
    Int(i32),
    Bool(bool),
    Char(u8),
    Array(Rc<RefCell<Vec<Lvalue>>>),
    Pointer(Option<Lvalue>),
    Struct(HashMap<FieldId, Lvalue>),
    Void,
}

impl Rvalue {
    pub fn new_string(contents: &str) -> Rvalue {
        let chars = contents
            .as_bytes()
            .iter()
            .map(|ch| Lvalue::store(Rvalue::Char(*ch)))
            .collect();
        Rvalue::Array(Rc::new(RefCell::new(chars)))
    }

    pub fn as_int(&self) -> i32 {
        match self {
            Rvalue::Int(x) => *x,
            _ => panic_wrong_type("int", self.type_()),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Rvalue::Bool(b) => *b,
            _ => panic_wrong_type("bool", self.type_()),
        }
    }

    pub fn as_char(&self) -> u8 {
        match self {
            Rvalue::Char(c) => *c,
            _ => panic_wrong_type("char", self.type_()),
        }
    }

    pub fn into_array(self) -> Rc<RefCell<Vec<Lvalue>>> {
        match self {
            Rvalue::Array(v) => v,
            _ => panic_wrong_type("array", self.type_()),
        }
    }

    pub fn as_utf8_string(&self) -> String {
        match self {
            Rvalue::Array(v) => {
                let chars = v
                    .borrow()
                    .iter()
                    .map(|ch| match *ch.borrow() {
                        Rvalue::Char(ch) => ch,
                        _ => panic_wrong_type("char", ch.detach().type_()),
                    })
                    .collect();
                String::from_utf8(chars).expect("string is not a valid UTF-8 sequence")
            }
            _ => panic_wrong_type("array", self.type_()),
        }
    }

    pub fn into_pointer(self) -> Option<Lvalue> {
        match self {
            Rvalue::Pointer(p) => p,
            _ => panic_wrong_type("pointer", self.type_()),
        }
    }

    pub fn into_pointer_unwrap(self, location: Option<SourceOrigin>) -> RunResult<Lvalue> {
        match self.into_pointer() {
            Some(pointer) => Ok(pointer),
            None => Err(RuntimeError::new(
                "Attempt to dereference null pointer",
                location,
            )),
        }
    }

    pub fn into_pointer_to_self(self, location: Option<SourceOrigin>) -> RunResult<Lvalue> {
        match self.into_pointer() {
            Some(pointer) => Ok(pointer),
            None => Err(RuntimeError::new("`self` is null in method", location)),
        }
    }

    pub fn as_struct(&self) -> &HashMap<FieldId, Lvalue> {
        match self {
            Rvalue::Struct(fields) => fields,
            _ => panic_wrong_type("struct", self.type_()),
        }
    }

    /// A quick-and-dirty information source about value types in runtime.
    /// This is _not_ meant to be an actual RTTI solution, it is only meant
    /// to be used in type mismatch panics, which should not occur under
    /// normal circumstances.
    pub fn type_(&self) -> &'static str {
        use Rvalue::*;
        match self {
            Int(_) => "int",
            Bool(_) => "bool",
            Char(_) => "char",
            Array(_) => "array",
            Pointer(_) => "pointer",
            Struct(_) => "struct",
            Void => "void",
        }
    }
}

impl Clone for Rvalue {
    fn clone(&self) -> Self {
        use Rvalue::*;
        match self {
            Int(x) => Rvalue::Int(*x),
            Bool(b) => Rvalue::Bool(*b),
            Char(c) => Rvalue::Char(*c),
            Array(v) => Rvalue::Array(Rc::clone(v)),
            Pointer(p) => Rvalue::Pointer(p.clone()),
            Struct(fields) => {
                let fields_copy = fields
                    .iter()
                    .map(|(id, lvalue)| (id.clone(), lvalue.clone_contents()))
                    .collect();
                Rvalue::Struct(fields_copy)
            }
            Void => Rvalue::Void,
        }
    }
}
