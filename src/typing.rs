//! CO types and their properties are defined in this module.

use std::cell::RefCell;
use std::collections::HashMap;
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

    /// If `self` is an array type, returns the type of elements.
    pub fn element_type(&self, registry: &TypeRegistry) -> Option<Rc<RefCell<Type>>> {
        match self.kind {
            TypeKind::Array(ref element_kind) => Some(Rc::clone(&registry.instances[element_kind])),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Void,
    Int,
    Bool,

    Array(Box<TypeKind>),

    /// An invalid type. It can never appear in a valid program.
    Error,
}

#[derive(Debug)]
pub struct TypeRegistry {
    /// All type kinds actually used in the program have an instantiation that can
    /// be accessed through this collection.
    instances: HashMap<TypeKind, Rc<RefCell<Type>>>,
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

        let mut instances = HashMap::new();
        instances.insert(TypeKind::Void, void);
        instances.insert(TypeKind::Int, int);
        instances.insert(TypeKind::Bool, bool);

        TypeRegistry { instances }
    }

    pub fn void(&self) -> &Rc<RefCell<Type>> {
        &self.instances[&TypeKind::Void]
    }

    pub fn int(&self) -> &Rc<RefCell<Type>> {
        &self.instances[&TypeKind::Int]
    }

    pub fn bool(&self) -> &Rc<RefCell<Type>> {
        &self.instances[&TypeKind::Bool]
    }

    pub fn array_of(&mut self, element_type: &Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
        let element_type_name = element_type.borrow().name.clone();
        let element_type_kind = element_type.borrow().kind.clone();
        if element_type_kind == TypeKind::Error {
            return Type::error();
        }

        let array_type_kind = TypeKind::Array(Box::new(element_type_kind));

        let array_type = self
            .instances
            .entry(array_type_kind.clone())
            .or_insert_with(|| {
                Rc::new(RefCell::new(Type {
                    kind: array_type_kind,
                    name: format!("[{}]", element_type_name),
                }))
            });
        Rc::clone(array_type)
    }

    pub fn primitive_types(&self) -> Vec<&Rc<RefCell<Type>>> {
        vec![self.void(), self.int(), self.bool()]
    }
}
