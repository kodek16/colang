//! CO types and their properties are defined in this module.

use crate::program::{Function, InternalFunctionTag, Program, SymbolId, Variable};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Numeric types plan:
// int, int8, int16, int64, int128
// float, double

#[derive(Debug)]
pub struct Type {
    type_id: TypeId,
    name: String,
    fields: Vec<Rc<RefCell<Variable>>>,
}

impl Type {
    /// Creates a new user-defined struct type and registers it with the program.
    pub fn new_struct(name: String, program: &mut Program) -> Rc<RefCell<Type>> {
        let id = program.symbol_ids_mut().next_id();
        let type_id = TypeId::Struct(id);
        let type_ = Type {
            type_id: type_id.clone(),
            name,
            fields: vec![],
        };
        let type_ = Rc::new(RefCell::new(type_));
        program.types_mut().types.insert(type_id, Rc::clone(&type_));
        type_
    }

    /// A convenience method for constructing managed error type instances.
    /// Error type is not bound to the registry.
    pub fn error() -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type {
            type_id: TypeId::Error,
            name: "<error>".to_string(),
            fields: vec![],
        }))
    }

    pub(crate) fn add_field(&mut self, field: Rc<RefCell<Variable>>) {
        self.fields.push(field)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_id(&self) -> &TypeId {
        &self.type_id
    }

    pub fn fields(&self) -> impl Iterator<Item = &Rc<RefCell<Variable>>> {
        self.fields.iter()
    }

    pub fn is_error(&self) -> bool {
        match self.type_id {
            TypeId::Error => true,
            _ => false,
        }
    }

    pub fn is_user_defined(&self) -> bool {
        self.symbol_id().is_some()
    }

    /// If the type is user-defined, returns its symbol id.
    pub fn symbol_id(&self) -> Option<SymbolId> {
        match self.type_id {
            TypeId::Struct(symbol_id) => Some(symbol_id),
            _ => None,
        }
    }

    /// If `self` is an instantiation of a template, retrieve the source template and
    /// type parameters.
    pub fn uninstantiate(
        &self,
        registry: &TypeRegistry,
    ) -> Option<(Rc<RefCell<TypeTemplate>>, Vec<Rc<RefCell<Type>>>)> {
        match self.type_id {
            TypeId::TemplateInstance(ref template_id, ref type_parameters) => {
                let template = Rc::clone(&registry.templates[template_id]);
                let type_parameters: Vec<_> = type_parameters
                    .iter()
                    .map(|parameter_type_id| Rc::clone(&registry.types[parameter_type_id]))
                    .collect();
                Some((template, type_parameters))
            }
            _ => None,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self.type_id {
            TypeId::TemplateInstance(TypeTemplateId::Pointer, _) => true,
            _ => false,
        }
    }

    /// If `self` is a pointer type, returns the type of target.
    pub fn pointer_target_type(&self, registry: &TypeRegistry) -> Option<Rc<RefCell<Type>>> {
        match self.type_id {
            TypeId::TemplateInstance(TypeTemplateId::Pointer, ref type_parameters) => {
                Some(Rc::clone(&registry.types[&type_parameters[0]]))
            }
            _ => None,
        }
    }

    /// If `self` is an array type, returns the type of elements.
    pub fn array_element_type(&self, registry: &TypeRegistry) -> Option<Rc<RefCell<Type>>> {
        match self.type_id {
            TypeId::TemplateInstance(TypeTemplateId::Array, ref type_parameters) => {
                Some(Rc::clone(&registry.types[&type_parameters[0]]))
            }
            _ => None,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeId {
    Void,
    Int,
    Bool,
    Char,
    String,

    TemplateInstance(TypeTemplateId, Vec<TypeId>),

    /// A struct defined in the program.
    Struct(SymbolId),

    /// An invalid type. It can never appear in a valid program.
    Error,
}

pub struct TypeRegistry {
    /// All type ids actually used in the program have an instantiation that can
    /// be accessed through this collection.
    types: HashMap<TypeId, Rc<RefCell<Type>>>,

    /// Same as `types`, but for templates.
    templates: HashMap<TypeTemplateId, Rc<RefCell<TypeTemplate>>>,
}

impl TypeRegistry {
    /// Initialize a new registry. There should be only one instance of a `TypeRegistry`
    /// present in a program.
    pub fn new() -> TypeRegistry {
        let void = Rc::new(RefCell::new(Type {
            type_id: TypeId::Void,
            name: "void".to_string(),
            fields: vec![],
        }));
        let int = Rc::new(RefCell::new(Type {
            type_id: TypeId::Int,
            name: "int".to_string(),
            fields: vec![],
        }));
        let bool = Rc::new(RefCell::new(Type {
            type_id: TypeId::Bool,
            name: "bool".to_string(),
            fields: vec![],
        }));
        let char = Rc::new(RefCell::new(Type {
            type_id: TypeId::Char,
            name: "char".to_string(),
            fields: vec![],
        }));
        let string = Rc::new(RefCell::new(Type {
            type_id: TypeId::String,
            name: "string".to_string(),
            fields: vec![],
        }));

        let mut types = HashMap::new();
        types.insert(TypeId::Void, void);
        types.insert(TypeId::Int, int);
        types.insert(TypeId::Bool, bool);
        types.insert(TypeId::Char, char);
        types.insert(TypeId::String, string);

        let mut templates = HashMap::new();
        templates.insert(TypeTemplateId::Array, create_array_template());
        templates.insert(TypeTemplateId::Pointer, create_pointer_template());

        TypeRegistry { types, templates }
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

    pub fn array_of(&mut self, element_type: &Type) -> Rc<RefCell<Type>> {
        Rc::clone(self.array())
            .borrow()
            .instantiate(vec![element_type], self)
    }

    pub fn pointer(&self) -> &Rc<RefCell<TypeTemplate>> {
        &self.templates[&TypeTemplateId::Pointer]
    }

    pub fn pointer_to(&mut self, target_type: &Type) -> Rc<RefCell<Type>> {
        Rc::clone(self.pointer())
            .borrow()
            .instantiate(vec![target_type], self)
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
}

pub struct TypeTemplate {
    type_template_id: TypeTemplateId,
    name: String,
    instantiate: Box<dyn Fn(Vec<&Type>, &mut TypeRegistry) -> Rc<RefCell<Type>>>,
    method_templates: HashMap<
        String,
        Box<dyn Fn(Vec<&Rc<RefCell<Type>>>, &mut Program) -> Rc<RefCell<Function>>>,
    >,
}

impl TypeTemplate {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_template_id(&self) -> &TypeTemplateId {
        &self.type_template_id
    }

    pub fn instantiate(
        &self,
        type_parameters: Vec<&Type>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<Type>> {
        (*self.instantiate)(type_parameters, registry)
    }

    pub fn method_template(
        &self,
        type_parameters: Vec<&Rc<RefCell<Type>>>,
        method_name: &str,
        program: &mut Program,
    ) -> Option<Rc<RefCell<Function>>> {
        self.method_templates
            .get(method_name)
            .map(|method_template| (*method_template)(type_parameters, program))
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum TypeTemplateId {
    Array,
    Pointer,
}

pub fn create_array_template() -> Rc<RefCell<TypeTemplate>> {
    let mut method_templates: HashMap<
        String,
        Box<dyn Fn(Vec<&Rc<RefCell<Type>>>, &mut Program) -> Rc<RefCell<Function>>>,
    > = HashMap::new();

    method_templates.insert(
        "push".to_string(),
        create_array_internal_method_template(
            InternalFunctionTag::ArrayPush,
            crate::program::internal::create_array_push_method,
        ),
    );
    method_templates.insert(
        "pop".to_string(),
        create_array_internal_method_template(
            InternalFunctionTag::ArrayPop,
            crate::program::internal::create_array_pop_method,
        ),
    );
    method_templates.insert(
        "len".to_string(),
        create_array_internal_method_template(
            InternalFunctionTag::ArrayLen,
            crate::program::internal::create_array_len_method,
        ),
    );
    method_templates.insert(
        "index".to_string(),
        create_array_internal_method_template(
            InternalFunctionTag::ArrayIndex,
            crate::program::internal::create_array_index_method,
        ),
    );

    Rc::new(RefCell::new(TypeTemplate {
        type_template_id: TypeTemplateId::Array,
        name: "<array>".to_string(),
        instantiate: Box::new(|mut types, registry| {
            assert_eq!(types.len(), 1);
            let element_type = types.pop().unwrap();

            let element_type_id = element_type.type_id.clone();
            if element_type_id == TypeId::Error {
                return Type::error();
            }

            let array_type_id =
                TypeId::TemplateInstance(TypeTemplateId::Array, vec![element_type_id]);

            let array_type = registry
                .types
                .entry(array_type_id.clone())
                .or_insert_with(|| {
                    Rc::new(RefCell::new(Type {
                        type_id: array_type_id,
                        name: format!("[{}]", element_type.name),
                        fields: vec![],
                    }))
                });
            Rc::clone(array_type)
        }),
        method_templates,
    }))
}

pub fn create_pointer_template() -> Rc<RefCell<TypeTemplate>> {
    Rc::new(RefCell::new(TypeTemplate {
        type_template_id: TypeTemplateId::Pointer,
        name: "<pointer>".to_string(),
        instantiate: Box::new(|mut types, registry| {
            assert_eq!(types.len(), 1);
            let target_type = types.pop().unwrap();

            let target_type_id = target_type.type_id.clone();
            if target_type_id == TypeId::Error {
                return Type::error();
            }

            let pointer_type_id =
                TypeId::TemplateInstance(TypeTemplateId::Pointer, vec![target_type_id]);

            let pointer_type = registry
                .types
                .entry(pointer_type_id.clone())
                .or_insert_with(|| {
                    Rc::new(RefCell::new(Type {
                        type_id: pointer_type_id,
                        name: format!("&{}", target_type.name),
                        fields: vec![],
                    }))
                });
            Rc::clone(pointer_type)
        }),
        method_templates: HashMap::new(),
    }))
}

fn create_array_internal_method_template(
    tag: impl Fn(TypeId) -> InternalFunctionTag + 'static,
    internal_method_constructor: impl Fn(&Rc<RefCell<Type>>, &mut TypeRegistry) -> Function + 'static,
) -> Box<dyn Fn(Vec<&Rc<RefCell<Type>>>, &mut Program) -> Rc<RefCell<Function>>> {
    Box::new(move |mut types, program| {
        assert_eq!(types.len(), 1);
        let element_type = types.pop().unwrap();

        let internal_function_tag = tag(element_type.borrow().type_id().clone());

        if program
            .internal_functions
            .contains_key(&internal_function_tag)
        {
            Rc::clone(&program.internal_functions[&internal_function_tag])
        } else {
            let method = Rc::new(RefCell::new(internal_method_constructor(
                element_type,
                &mut program.types,
            )));
            program
                .internal_functions
                .insert(internal_function_tag, Rc::clone(&method));
            method
        }
    })
}
