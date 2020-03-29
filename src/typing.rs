//! CO types and their properties are defined in this module.

use std::cell::RefCell;
use std::rc::Rc;

// Numeric types plan:
// int, int8, int16, int64, int128
// float, double

#[derive(Debug, PartialEq)]
pub struct Type {
    kind: TypeKind,
    name: String,
}

impl Type {
    /// A convenience method for constructing managed error type instances.
    /// Error type is not bound to the registry.
    pub fn error() -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type {
            kind: TypeKind::Error,
            name: "<error>".to_string(),
        }))
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn kind(&self) -> &TypeKind {
        &self.kind
    }

    pub fn is_error(&self) -> bool {
        match self.kind {
            TypeKind::Error => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeKind {
    Void,
    Int,
    Bool,

    Array(Rc<RefCell<Type>>),

    /// An invalid type. It can never appear in a valid program.
    Error,
}

#[derive(Debug)]
pub struct TypeRegistry {
    void: Rc<RefCell<Type>>,
    int: Rc<RefCell<Type>>,
    bool: Rc<RefCell<Type>>,
}

impl TypeRegistry {
    /// Initialize a new registry. There should be only one instance of a `TypeRegistry`
    /// present in a program.
    pub fn new() -> TypeRegistry {
        let void = Rc::new(RefCell::new(Type {
            kind: TypeKind::Void,
            name: "void".to_string(),
        }));
        let int = Rc::new(RefCell::new(Type {
            kind: TypeKind::Int,
            name: "int".to_string(),
        }));
        let bool = Rc::new(RefCell::new(Type {
            kind: TypeKind::Bool,
            name: "bool".to_string(),
        }));

        TypeRegistry { void, int, bool }
    }

    pub fn void(&self) -> &Rc<RefCell<Type>> {
        &self.void
    }

    pub fn int(&self) -> &Rc<RefCell<Type>> {
        &self.int
    }

    pub fn bool(&self) -> &Rc<RefCell<Type>> {
        &self.bool
    }

    pub fn primitive_types(&self) -> Vec<&Rc<RefCell<Type>>> {
        vec![&self.void, &self.int, &self.bool]
    }
}
