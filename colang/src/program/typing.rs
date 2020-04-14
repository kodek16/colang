//! CO types and their properties are defined in this module.

use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{Function, Program, SymbolId, Variable};
use crate::scope::Scope;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Numeric types plan:
// int, int8, int16, int64, int128
// float, double

pub struct Type {
    pub type_id: TypeId,
    name: String,
    definition_site: Option<InputSpan>,

    fields: Vec<Rc<RefCell<Variable>>>,
    methods: Vec<Rc<RefCell<Function>>>,
    scope: Scope,
}

impl Type {
    /// Creates a new user-defined struct type and registers it with the program.
    pub fn new_struct(
        name: String,
        definition_site: InputSpan,
        program: &mut Program,
    ) -> Rc<RefCell<Type>> {
        let type_id = TypeId::Struct(program.symbol_ids_mut().next_id());
        Type::new(name, type_id, Some(definition_site), program.types_mut())
    }

    fn new(
        name: String,
        type_id: TypeId,
        definition_site: Option<InputSpan>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<Type>> {
        let type_ = Type {
            type_id: type_id.clone(),
            name,
            definition_site,
            fields: vec![],
            methods: vec![],
            scope: Scope::new_for_type(),
        };
        let type_ = Rc::new(RefCell::new(type_));
        registry.types.insert(type_id, Rc::clone(&type_));
        type_
    }

    /// A convenience method for constructing managed error type instances.
    /// Error type is not bound to the registry.
    pub fn error() -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type {
            type_id: TypeId::Error,
            name: "<error>".to_string(),
            definition_site: None,
            fields: vec![],
            methods: vec![],
            scope: Scope::new_for_type(),
        }))
    }

    #[must_use]
    pub fn add_field(&mut self, field: Rc<RefCell<Variable>>) -> Result<(), CompilationError> {
        self.fields.push(Rc::clone(&field));
        self.scope.add_variable(field)
    }

    #[must_use]
    pub fn add_method(&mut self, method: Rc<RefCell<Function>>) -> Result<(), CompilationError> {
        self.methods.push(Rc::clone(&method));
        self.scope.add_function(method)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn definition_site(&self) -> Option<InputSpan> {
        self.definition_site
    }

    pub fn fields(&self) -> impl Iterator<Item = &Rc<RefCell<Variable>>> {
        self.fields.iter()
    }

    pub fn methods(&self) -> impl Iterator<Item = &Rc<RefCell<Function>>> {
        self.methods.iter()
    }

    pub fn lookup_field(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Variable>>, CompilationError> {
        self.scope.lookup_variable(name, reference_location)
    }

    pub fn lookup_method(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Function>>, CompilationError> {
        self.scope.lookup_function(name, reference_location)
    }

    pub fn is_error(&self) -> bool {
        match self.type_id {
            TypeId::Error => true,
            _ => false,
        }
    }

    pub fn is_user_defined(&self) -> bool {
        match self.type_id {
            TypeId::Struct(_) => true,
            TypeId::TemplateInstance(TypeTemplateId::Struct(_), _) => true,
            _ => false,
        }
    }

    /// Replaces all dependencies this type has on type parameters with concrete type arguments.
    pub fn instantiate(
        &self,
        type_arguments: &HashMap<TypeId, TypeId>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<Type>> {
        if let Some(type_argument) = type_arguments.get(&self.type_id) {
            Rc::clone(registry.lookup(type_argument))
        } else {
            match &self.type_id {
                TypeId::TemplateInstance(template_id, args) => {
                    let template = Rc::clone(&registry.templates[template_id]);
                    let instantiated_arguments: Vec<_> = args
                        .iter()
                        .map(|argument_id| {
                            let argument_type = Rc::clone(registry.lookup(argument_id));
                            let instantiated_type =
                                argument_type.borrow().instantiate(type_arguments, registry);
                            instantiated_type
                        })
                        .collect();

                    let result = template
                        .borrow()
                        .instantiate(instantiated_arguments.iter().collect(), registry, None)
                        .expect(&format!(
                            "Failed to instantiate type template `{}`",
                            template.borrow().name
                        ));
                    result
                }
                other => Rc::clone(registry.lookup(other)),
            }
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

    /// Template type parameter that will be substituted for a concrete type during instantiation.
    /// The second element of the tag is the index of the parameter.
    TypeParameter(TypeTemplateId, usize),

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
        let mut registry = TypeRegistry {
            types: HashMap::new(),
            templates: HashMap::new(),
        };

        Type::new("void".to_string(), TypeId::Void, None, &mut registry);
        Type::new("int".to_string(), TypeId::Int, None, &mut registry);
        Type::new("bool".to_string(), TypeId::Bool, None, &mut registry);
        Type::new("char".to_string(), TypeId::Char, None, &mut registry);
        Type::new("string".to_string(), TypeId::String, None, &mut registry);

        create_array_template(&mut registry);
        create_pointer_template(&mut registry);

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
}

pub struct TypeTemplate {
    type_template_id: TypeTemplateId,
    name: String,
    definition_site: Option<InputSpan>,

    /// If the template has a quirky naming pattern (like arrays or pointers),
    /// this field allows to specify it.
    name_template: Box<dyn Fn(Vec<&Rc<RefCell<Type>>>) -> String>,

    /// The placeholder types that will be substituted with type arguments in fields and methods
    /// during instantiation.
    type_parameters: Vec<Rc<RefCell<Type>>>,

    /// This is a full-fledged type that will be used as the source of copying during instantiation.
    base_type: Rc<RefCell<Type>>,
}

/// Type parameters are constructed at the same time as the template using this information
/// collected beforehand.
pub struct ProtoTypeParameter {
    pub name: String,
    pub definition_site: Option<InputSpan>,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum TypeTemplateId {
    Array,
    Pointer,

    Struct(SymbolId),
}

impl TypeTemplate {
    pub fn new_struct_template(
        name: String,
        type_parameters: Vec<ProtoTypeParameter>,
        definition_site: InputSpan,
        program: &mut Program,
    ) -> Rc<RefCell<TypeTemplate>> {
        let id = program.symbol_ids_mut().next_id();
        let type_template_id = TypeTemplateId::Struct(id);

        TypeTemplate::new(
            type_template_id.clone(),
            name,
            type_parameters,
            Some(definition_site),
            None,
            program.types_mut(),
        )
    }

    fn new(
        type_template_id: TypeTemplateId,
        name: String,
        type_parameters: Vec<ProtoTypeParameter>,
        definition_site: Option<InputSpan>,
        name_template: Option<Box<dyn Fn(Vec<&Rc<RefCell<Type>>>) -> String>>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<TypeTemplate>> {
        let type_parameters: Vec<_> = type_parameters
            .into_iter()
            .enumerate()
            .map(|(index, type_param)| {
                let type_parameter = Rc::new(RefCell::new(Type {
                    type_id: TypeId::TypeParameter(type_template_id.clone(), index),
                    name: type_param.name,
                    definition_site: type_param.definition_site,
                    fields: vec![],
                    methods: vec![],
                    scope: Scope::new_for_type(),
                }));
                registry.types.insert(
                    type_parameter.borrow().type_id.clone(),
                    Rc::clone(&type_parameter),
                );
                type_parameter
            })
            .collect();

        let name_template: Box<dyn Fn(Vec<&Rc<RefCell<Type>>>) -> String> =
            if let Some(name_template) = name_template {
                name_template
            } else {
                let template_name = name.clone();
                Box::new(move |type_arguments| {
                    let type_argument_names: Vec<_> = type_arguments
                        .iter()
                        .map(|type_arg| type_arg.borrow().name.clone())
                        .collect();
                    format!("{}<{}>", template_name, type_argument_names.join(", "))
                })
            };

        let base_type = Rc::new(RefCell::new(Type {
            type_id: TypeId::TemplateInstance(
                type_template_id.clone(),
                type_parameters
                    .iter()
                    .map(|type_param| type_param.borrow().type_id.clone())
                    .collect(),
            ),
            name: (*name_template)(type_parameters.iter().collect()),
            definition_site,
            fields: vec![],
            methods: vec![],
            scope: Scope::new_for_type(),
        }));
        registry
            .types
            .insert(base_type.borrow().type_id.clone(), Rc::clone(&base_type));

        let type_template = Rc::new(RefCell::new(TypeTemplate {
            type_template_id: type_template_id.clone(),
            name,
            definition_site,
            name_template,
            type_parameters,
            base_type,
        }));

        registry
            .templates
            .insert(type_template_id, Rc::clone(&type_template));

        type_template
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_template_id(&self) -> &TypeTemplateId {
        &self.type_template_id
    }

    pub fn definition_site(&self) -> Option<InputSpan> {
        self.definition_site
    }

    pub fn type_parameters(&self) -> impl Iterator<Item = &Rc<RefCell<Type>>> {
        self.type_parameters.iter()
    }

    pub fn base_type(&self) -> &Rc<RefCell<Type>> {
        &self.base_type
    }

    pub fn instantiate(
        &self,
        type_arguments: Vec<&Rc<RefCell<Type>>>,
        registry: &mut TypeRegistry,
        location: Option<InputSpan>,
    ) -> Result<Rc<RefCell<Type>>, CompilationError> {
        if type_arguments.len() != self.type_parameters.len() {
            return Err(CompilationError::wrong_number_of_type_template_arguments(
                self.name(),
                self.type_parameters.len(),
                type_arguments.len(),
                location.expect(
                    "Synthetic instantiation of type template with wrong number of type arguments",
                ),
            ));
        }

        if type_arguments
            .iter()
            .any(|type_arg| type_arg.borrow().is_error())
        {
            return Ok(Type::error());
        }

        let type_argument_ids: Vec<_> = type_arguments
            .iter()
            .map(|type_arg| type_arg.borrow().type_id.clone())
            .collect();

        let type_argument_map: HashMap<TypeId, TypeId> = self
            .type_parameters
            .iter()
            .map(|type_param| type_param.borrow().type_id.clone())
            .zip(type_argument_ids.iter().map(TypeId::clone))
            .collect();

        let concrete_type_id =
            TypeId::TemplateInstance(self.type_template_id.clone(), type_argument_ids);
        let concrete_type_name = (*self.name_template)(type_arguments);

        if let Some(preexisting_instantiation) = registry.types.get(&concrete_type_id) {
            return Ok(Rc::clone(preexisting_instantiation));
        }

        let concrete_type = Rc::new(RefCell::new(Type {
            type_id: concrete_type_id.clone(),
            name: concrete_type_name,
            definition_site: self.definition_site,
            fields: vec![],
            methods: vec![],
            scope: Scope::new_for_type(),
        }));
        registry
            .types
            .insert(concrete_type_id.clone(), Rc::clone(&concrete_type));

        for field in self.base_type.borrow().fields.iter() {
            let instantiated_field = Rc::new(RefCell::new(field.borrow().instantiate_field(
                concrete_type_id.clone(),
                &type_argument_map,
                registry,
            )));
            concrete_type
                .borrow_mut()
                .add_field(instantiated_field)
                .expect("Name collision on type template instantiation");
        }

        for method in self.base_type.borrow().methods.iter() {
            let instantiated_method =
                method
                    .borrow()
                    .instantiate(concrete_type_id.clone(), &type_argument_map, registry);
            concrete_type
                .borrow_mut()
                .add_method(instantiated_method)
                .expect("Name collision on type template instantiation");
        }

        Ok(Rc::clone(&concrete_type))
    }
}

pub fn create_array_template(registry: &mut TypeRegistry) -> Rc<RefCell<TypeTemplate>> {
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

pub fn create_pointer_template(registry: &mut TypeRegistry) -> Rc<RefCell<TypeTemplate>> {
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
