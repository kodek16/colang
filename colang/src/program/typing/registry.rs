use crate::program::typing::templates::{ProtoTypeParameter, TypeTemplate, TypeTemplateId};
use crate::program::typing::types::{Type, TypeCompleteness, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A singleton storage for all types present in the program.
pub struct TypeRegistry {
    /// All types used in the program have are stored in this collection, and can be accessed
    /// through their IDs.
    pub(in crate::program::typing) types: HashMap<TypeId, Rc<RefCell<Type>>>,

    /// Same as `types`, but for type templates.
    pub(in crate::program::typing) templates: HashMap<TypeTemplateId, Rc<RefCell<TypeTemplate>>>,
}

impl TypeRegistry {
    /// Initialize a new registry. There should be only one instance of a `TypeRegistry`
    /// present in a program.
    pub fn new() -> TypeRegistry {
        let mut registry = TypeRegistry {
            types: HashMap::new(),
            templates: HashMap::new(),
        };

        // Strictly speaking, some primitive types that have internal methods are not
        // fully complete yet, but this does not really matter since their methods are also
        // added on program initialization.
        Type::new(
            "void".to_string(),
            TypeId::Void,
            None,
            TypeCompleteness::FullyComplete,
            &mut registry,
        );
        Type::new(
            "int".to_string(),
            TypeId::Int,
            None,
            TypeCompleteness::FullyComplete,
            &mut registry,
        );
        Type::new(
            "bool".to_string(),
            TypeId::Bool,
            None,
            TypeCompleteness::FullyComplete,
            &mut registry,
        );
        Type::new(
            "char".to_string(),
            TypeId::Char,
            None,
            TypeCompleteness::FullyComplete,
            &mut registry,
        );
        Type::new(
            "string".to_string(),
            TypeId::String,
            None,
            TypeCompleteness::FullyComplete,
            &mut registry,
        );

        create_array_template(&mut registry);
        create_pointer_template(&mut registry);

        // Array is marked fully complete once its internal methods are initialized later.
        registry.mark_fully_complete(&Rc::clone(registry.pointer()).borrow().base_type());

        registry
    }

    pub fn lookup(&self, type_id: &TypeId) -> &Rc<RefCell<Type>> {
        &self.types[type_id]
    }

    pub fn void(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Void]
    }

    pub fn int(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Int]
    }

    pub fn bool(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Bool]
    }

    pub fn char(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::Char]
    }

    pub fn string(&self) -> &Rc<RefCell<Type>> {
        &self.types[&TypeId::String]
    }

    pub fn array(&self) -> &Rc<RefCell<TypeTemplate>> {
        &self.templates[&TypeTemplateId::Array]
    }

    pub fn array_of(&mut self, element_type: &Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
        Rc::clone(self.array())
            .borrow()
            .instantiate(vec![element_type], self, None)
            .unwrap()
    }

    pub fn pointer(&self) -> &Rc<RefCell<TypeTemplate>> {
        &self.templates[&TypeTemplateId::Pointer]
    }

    pub fn pointer_to(&mut self, target_type: &Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
        Rc::clone(self.pointer())
            .borrow()
            .instantiate(vec![target_type], self, None)
            .unwrap()
    }

    /// Basic types are non-template internal types.
    pub fn basic_types(&self) -> Vec<&Rc<RefCell<Type>>> {
        vec![
            self.void(),
            self.int(),
            self.bool(),
            self.char(),
            self.string(),
        ]
    }

    pub fn all_types(&self) -> impl Iterator<Item = &Rc<RefCell<Type>>> {
        self.types.values()
    }

    pub fn all_user_defined_types(&self) -> impl Iterator<Item = &Rc<RefCell<Type>>> {
        self.types
            .values()
            .filter(|type_| type_.borrow().is_user_defined())
    }

    /// Unregisters (hides) a previously registered type from the registry.
    /// This type will not appear in calls to `all_user_defined`, but there still might remain
    /// references to it from other types. If such references from types that have not been removed
    /// exist in a program, it should be considered invalid.
    pub fn remove_type(&mut self, type_: &Type) {
        self.types.remove(&type_.type_id).expect(&format!(
            "Cannot remove type `{}` from the program: it is not registered",
            type_.name,
        ));
    }

    /// Marks a previously incomplete type as complete (but with possibly incomplete dependencies).
    pub fn mark_complete_without_deps(&mut self, type_: &Rc<RefCell<Type>>) {
        type_.borrow_mut().completeness = TypeCompleteness::CompleteWithoutDeps;
    }

    /// Marks a type as fully complete (with all dependencies also fully complete).
    pub fn mark_fully_complete(&mut self, type_: &Rc<RefCell<Type>>) {
        type_.borrow_mut().completeness = TypeCompleteness::FullyComplete;
    }
}

fn create_array_template(registry: &mut TypeRegistry) -> Rc<RefCell<TypeTemplate>> {
    let type_parameters = vec![ProtoTypeParameter {
        name: "T".to_string(),
        definition_site: None,
    }];

    TypeTemplate::new(
        TypeTemplateId::Array,
        "<array>".to_string(),
        type_parameters,
        None,
        Some(Box::new(|type_args| {
            format!("[{}]", type_args[0].borrow().name)
        })),
        registry,
    )
}

fn create_pointer_template(registry: &mut TypeRegistry) -> Rc<RefCell<TypeTemplate>> {
    let type_parameters = vec![ProtoTypeParameter {
        name: "T".to_string(),
        definition_site: None,
    }];

    TypeTemplate::new(
        TypeTemplateId::Pointer,
        "<pointer>".to_string(),
        type_parameters,
        None,
        Some(Box::new(|type_args| {
            format!("&{}", type_args[0].borrow().name)
        })),
        registry,
    )
}
