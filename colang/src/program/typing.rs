//! CO types and their properties are defined in this module.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Numeric types plan:
// int, int8, int16, int64, int128
// float, double

#[derive(Debug)]
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

    /// If `self` is a pointer type, returns the type of target.
    pub fn pointer_target_type(&self, registry: &TypeRegistry) -> Option<Rc<RefCell<Type>>> {
        match self.kind {
            TypeKind::TemplateInstance(TypeTemplateKind::Pointer, ref type_parameters) => {
                Some(Rc::clone(&registry.types[&type_parameters[0]]))
            }
            _ => None,
        }
    }

    /// If `self` is an array type, returns the type of elements.
    pub fn array_element_type(&self, registry: &TypeRegistry) -> Option<Rc<RefCell<Type>>> {
        match self.kind {
            TypeKind::TemplateInstance(TypeTemplateKind::Array, ref type_parameters) => {
                Some(Rc::clone(&registry.types[&type_parameters[0]]))
            }
            _ => None,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Void,
    Int,
    Bool,

    TemplateInstance(TypeTemplateKind, Vec<TypeKind>),

    /// An invalid type. It can never appear in a valid program.
    Error,
}

pub struct TypeRegistry {
    /// All type kinds actually used in the program have an instantiation that can
    /// be accessed through this collection.
    types: HashMap<TypeKind, Rc<RefCell<Type>>>,

    /// Same as `types`, but for templates.
    templates: HashMap<TypeTemplateKind, Rc<RefCell<TypeTemplate>>>,
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

        let mut types = HashMap::new();
        types.insert(TypeKind::Void, void);
        types.insert(TypeKind::Int, int);
        types.insert(TypeKind::Bool, bool);

        let mut templates = HashMap::new();
        templates.insert(TypeTemplateKind::Array, create_array_template());
        templates.insert(TypeTemplateKind::Pointer, create_pointer_template());

        TypeRegistry { types, templates }
    }

    pub fn void(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeKind::Void]
    }

    pub fn int(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeKind::Int]
    }

    pub fn bool(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeKind::Bool]
    }

    pub fn array(&self) -> &Rc<RefCell<TypeTemplate>> {
        &self.templates[&TypeTemplateKind::Array]
    }

    pub fn array_of(&mut self, element_type: &Type) -> Rc<RefCell<Type>> {
        Rc::clone(self.array())
            .borrow()
            .instantiate(vec![element_type], self)
    }

    pub fn pointer(&self) -> &Rc<RefCell<TypeTemplate>> {
        &self.templates[&TypeTemplateKind::Pointer]
    }

    pub fn pointer_to(&mut self, target_type: &Type) -> Rc<RefCell<Type>> {
        Rc::clone(self.pointer())
            .borrow()
            .instantiate(vec![target_type], self)
    }

    pub fn primitive_types(&self) -> Vec<&Rc<RefCell<Type>>> {
        vec![self.void(), self.int(), self.bool()]
    }
}

pub struct TypeTemplate {
    kind: TypeTemplateKind,
    name: String,
    instantiate: Box<dyn Fn(Vec<&Type>, &mut TypeRegistry) -> Rc<RefCell<Type>>>,
}

impl TypeTemplate {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn kind(&self) -> &TypeTemplateKind {
        &self.kind
    }

    pub fn instantiate(
        &self,
        type_parameters: Vec<&Type>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<Type>> {
        (*self.instantiate)(type_parameters, registry)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum TypeTemplateKind {
    Array,
    Pointer,
}

pub fn create_array_template() -> Rc<RefCell<TypeTemplate>> {
    Rc::new(RefCell::new(TypeTemplate {
        kind: TypeTemplateKind::Array,
        name: "<array>".to_string(),
        instantiate: Box::new(|mut types, registry| {
            assert_eq!(types.len(), 1);
            let element_type = types.pop().unwrap();

            let element_type_kind = element_type.kind.clone();
            if element_type_kind == TypeKind::Error {
                return Type::error();
            }

            let array_type_kind =
                TypeKind::TemplateInstance(TypeTemplateKind::Array, vec![element_type_kind]);

            let array_type = registry
                .types
                .entry(array_type_kind.clone())
                .or_insert_with(|| {
                    Rc::new(RefCell::new(Type {
                        kind: array_type_kind,
                        name: format!("[{}]", element_type.name),
                    }))
                });
            Rc::clone(array_type)
        }),
    }))
}

pub fn create_pointer_template() -> Rc<RefCell<TypeTemplate>> {
    Rc::new(RefCell::new(TypeTemplate {
        kind: TypeTemplateKind::Pointer,
        name: "<pointer>".to_string(),
        instantiate: Box::new(|mut types, registry| {
            assert_eq!(types.len(), 1);
            let target_type = types.pop().unwrap();

            let target_type_kind = target_type.kind.clone();
            if target_type_kind == TypeKind::Error {
                return Type::error();
            }

            let pointer_type_kind =
                TypeKind::TemplateInstance(TypeTemplateKind::Pointer, vec![target_type_kind]);

            let pointer_type = registry
                .types
                .entry(pointer_type_kind.clone())
                .or_insert_with(|| {
                    Rc::new(RefCell::new(Type {
                        kind: pointer_type_kind,
                        name: format!("&{}", target_type.name),
                    }))
                });
            Rc::clone(pointer_type)
        }),
    }))
}
