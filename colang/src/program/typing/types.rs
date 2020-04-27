use crate::errors::CompilationError;
use crate::program::typing::registry::TypeRegistry;
use crate::program::typing::templates::TypeTemplateId;
use crate::program::{Function, Program, SymbolId, Variable};
use crate::scope::{FieldEntity, MethodEntity, Scope, TypeScope};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const MAX_TYPE_INSTANTIATION_DEPTH: usize = 64;

/// A type in CO.
///
/// Every value has a certain type, and every expression has a type since it evaluates to a value.
///
/// Types can be divided into groups in multiple ways:
/// - User-defined vs. internal: a type defined in the source program is said to be "user-defined",
///   while types defined internally by the compiler are "internal". Standard library types are
///   also considered "user-defined", since they are backed by source code.
///
/// - Template instances vs. normal types: a type can be an instance of a type template
///   (see `TypeTemplate`). Instantiation (creating a new type from a template) happens in a
///   few stages.
///
/// - `void` vs. all other types: as an exception, there can be no values of the type `void`.
///   Expressions can have type `void` only if they appear in a "void context".
pub struct Type {
    /// A unique identifier for types that is immutable and hashable.
    pub type_id: TypeId,
    pub name: String,
    pub definition_site: Option<SourceOrigin>,

    fields: Vec<Rc<RefCell<Variable>>>,
    methods: Vec<Rc<RefCell<Function>>>,
    scope: TypeScope,

    /// Types can be in one of a few states, see `TypeCompleteness` for details.
    pub(in crate::program::typing) completeness: TypeCompleteness,
}

/// Represents a state a type is in. There are a few states through which a type passes during
/// compilation: every consequent state carries more information.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(in crate::program::typing) enum TypeCompleteness {
    /// The very basic level of information: the compiler knows a type exists and knows its name.
    /// Fields and methods are not yet analyzed.
    Incomplete,

    /// Fields and methods (and their types) of a type are known, but their types may not be
    /// complete.
    CompleteWithoutDeps,

    /// Fields and methods are known, and all the types they refer to are fully complete. Type
    /// arguments, if any, are also fully complete.
    FullyComplete,
}

impl Type {
    /// Creates a new user-defined struct type and registers it with the program.
    pub fn new_struct(
        name: String,
        definition_site: SourceOrigin,
        program: &mut Program,
    ) -> Rc<RefCell<Type>> {
        let type_id = TypeId::Struct(program.symbol_ids_mut().next_id());
        Type::new(
            name,
            type_id,
            Some(definition_site),
            TypeCompleteness::Incomplete,
            program.types_mut(),
        )
    }

    /// Creates an instance of the error type.
    ///
    /// Error type is not bound to the registry, so there can exist multiple distinct instances
    /// of it. Error type can not be present in a valid program.
    pub fn error() -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type {
            type_id: TypeId::Error,
            name: "<error>".to_string(),
            definition_site: None,
            fields: vec![],
            methods: vec![],
            scope: Scope::new(),
            completeness: TypeCompleteness::FullyComplete,
        }))
    }

    /// Creates a new type and registers it with `registry`.
    ///
    /// There must be no existing types with the same `type_id`.
    pub(in crate::program::typing) fn new(
        name: String,
        type_id: TypeId,
        definition_site: Option<SourceOrigin>,
        completeness: TypeCompleteness,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<Type>> {
        let type_ = Type {
            type_id: type_id.clone(),
            name,
            definition_site,
            fields: vec![],
            methods: vec![],
            scope: Scope::new(),
            completeness,
        };
        let type_ = Rc::new(RefCell::new(type_));
        let existing = registry.types.insert(type_id.clone(), Rc::clone(&type_));
        if existing.is_some() {
            panic!(
                "Attempt to create a duplicate type with the same type ID: {:?}",
                type_id
            )
        }
        type_
    }

    /// Adds a field to the list of type members.
    #[must_use]
    pub fn add_field(&mut self, field: Rc<RefCell<Variable>>) -> Result<(), CompilationError> {
        self.fields.push(Rc::clone(&field));
        self.scope.add(FieldEntity(field))
    }

    /// Adds a method to the list of type members.
    #[must_use]
    pub fn add_method(&mut self, method: Rc<RefCell<Function>>) -> Result<(), CompilationError> {
        self.methods.push(Rc::clone(&method));
        self.scope.add(MethodEntity(method))
    }

    /// Looks up a field in the type member scope.
    pub fn lookup_field(
        &self,
        name: &str,
        reference_location: SourceOrigin,
    ) -> Result<Rc<RefCell<Variable>>, CompilationError> {
        self.scope.lookup::<FieldEntity>(name, reference_location)
    }

    /// Looks up a method in the type member scope.
    pub fn lookup_method(
        &self,
        name: &str,
        reference_location: SourceOrigin,
    ) -> Result<Rc<RefCell<Function>>, CompilationError> {
        self.scope.lookup::<MethodEntity>(name, reference_location)
    }

    pub fn fields(&self) -> impl Iterator<Item = &Rc<RefCell<Variable>>> {
        self.fields.iter()
    }

    pub fn methods(&self) -> impl Iterator<Item = &Rc<RefCell<Function>>> {
        self.methods.iter()
    }

    pub fn is_user_defined(&self) -> bool {
        match self.type_id {
            TypeId::Struct(_) => true,
            TypeId::TemplateInstance(TypeTemplateId::Struct(_), _) => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self.type_id {
            TypeId::Void => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self.type_id {
            TypeId::Int => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self.type_id {
            TypeId::Bool => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self.type_id {
            TypeId::String => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self.type_id {
            TypeId::Error => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self.type_id {
            TypeId::TemplateInstance(TypeTemplateId::Array, _) => true,
            _ => false,
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

    /// Checks if any of the transitive type arguments of this type are unfilled type parameter
    /// placeholders.
    pub fn depends_on_type_parameter_placeholders(&self) -> bool {
        fn check(type_id: &TypeId) -> bool {
            match type_id {
                TypeId::TemplateInstance(_, ref type_arguments) => type_arguments.iter().any(check),
                TypeId::TypeParameter(_, _) => true,
                _ => false,
            }
        }

        check(&self.type_id)
    }

    /// Substitutes a set of types in the "signature" of `type_` with other types.
    ///
    /// "Signature" here is defined as the type itself and the signatures of all type arguments,
    /// if `type_` is a template instance.
    ///
    /// No guarantees are made about the completeness of any new types that may be created as
    /// a result of this operation: typically they are incomplete.
    ///
    /// This function is typically used as a first stage of instantiation: type parameter
    /// placeholders for a template base type are substituted with actual type arguments.
    pub fn substitute(
        type_: &Rc<RefCell<Type>>,
        substitutions: &HashMap<TypeId, TypeId>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<Type>> {
        let type_id = &type_.borrow().type_id;
        if let Some(target_id) = substitutions.get(type_id) {
            Rc::clone(registry.lookup(target_id))
        } else {
            match type_id {
                TypeId::TemplateInstance(template_id, type_arguments) => {
                    let template = Rc::clone(&registry.templates[template_id]);
                    let arguments_after_substitution: Vec<_> = type_arguments
                        .iter()
                        .map(|argument_id| {
                            let argument_type = Rc::clone(registry.lookup(argument_id));
                            Type::substitute(&argument_type, substitutions, registry)
                        })
                        .collect();

                    let result = template
                        .borrow()
                        .instantiate(
                            arguments_after_substitution.iter().collect(),
                            registry,
                            None,
                        )
                        .expect(&format!(
                            "Failed to instantiate type template `{}`",
                            template.borrow().name
                        ));
                    result
                }
                _ => Rc::clone(type_),
            }
        }
    }

    /// Ensures that `type_` is fully complete, that is, all of its transitive "dependency" types
    /// are complete.
    ///
    /// This function assumes that the base types for all type templates in the program are already
    /// complete.
    ///
    /// This function performs one of the stages of template instantiation: copying fields and
    /// methods from the base type to the instantiated type. As all transitive dependencies also
    /// have to be instantiated, this function may encounter an infinite loop is the transitive
    /// dependency set is infinite. To prevent that, once a maximum instantiation depth is reached,
    /// it is assumed that the transitive dependency set is infinite, and an error is produced,
    /// since the type set in a program has to be finite. The instantiation stack presumed to be
    /// a beginning of an infinite chain is returned as an error in that case.
    pub fn ensure_is_fully_complete(
        type_: Rc<RefCell<Type>>,
        registry: &mut TypeRegistry,
    ) -> Result<(), Vec<Rc<RefCell<Type>>>> {
        let mut type_stack = Vec::new();

        fn process(
            current_type: Rc<RefCell<Type>>,
            stack: &mut Vec<Rc<RefCell<Type>>>,
            registry: &mut TypeRegistry,
        ) -> Result<(), ()> {
            stack.push(Rc::clone(&current_type));

            if stack.len() > MAX_TYPE_INSTANTIATION_DEPTH {
                return Err(());
            }

            if current_type.borrow().completeness == TypeCompleteness::Incomplete {
                Type::complete_from_base_type(Rc::clone(&current_type), registry);
            }

            if current_type.borrow().completeness == TypeCompleteness::CompleteWithoutDeps {
                let dependencies = current_type
                    .borrow()
                    .type_dependencies_from_global_interface(registry);
                for dependency in dependencies {
                    if !stack.contains(&dependency) {
                        process(Rc::clone(&dependency), stack, registry)?;
                    }
                }
            }

            registry.mark_fully_complete(&current_type);
            stack.pop();
            Ok(())
        }

        let result = process(type_, &mut type_stack, registry);
        result.map_err(|_| type_stack)
    }

    /// For an incomplete type instantiated from a template, completes the instantiation by
    /// copying fields and methods from the base type.
    fn complete_from_base_type(type_: Rc<RefCell<Type>>, registry: &mut TypeRegistry) {
        if type_.borrow().completeness != TypeCompleteness::Incomplete {
            panic!(
                "Attempted to complete type `{}`, which is in an unexpected state {:?}",
                type_.borrow().name,
                type_.borrow().completeness,
            );
        }

        let type_id = type_.borrow().type_id.clone();
        let type_name = type_.borrow().name.clone();

        match &type_id {
            TypeId::TemplateInstance(template_id, _) => {
                let template = Rc::clone(&registry.templates[template_id]);
                let template = template.borrow();

                let base_type = template.base_type().borrow();

                if base_type.completeness == TypeCompleteness::Incomplete {
                    panic!(
                        "Attempted to complete type `{}` from its base type `{}`, but the base type is not complete yet",
                        type_name, base_type.name
                    )
                }

                let own_type_arguments = type_.borrow().actual_type_arguments(registry);
                for field in base_type.fields.iter() {
                    let instantiated_field =
                        Rc::new(RefCell::new(field.borrow().instantiate_field(
                            type_id.clone(),
                            &own_type_arguments,
                            registry,
                        )));
                    // Name collisions have already been reported for the base type.
                    let _ = type_.borrow_mut().add_field(instantiated_field);
                }

                for method in base_type.methods.iter() {
                    let instantiated_method = Function::instantiate_interface(
                        Rc::clone(&method),
                        type_id.clone(),
                        &own_type_arguments,
                        registry,
                    );
                    // Name collisions have already been reported for the base type.
                    let _ = type_.borrow_mut().add_method(instantiated_method);
                }

                registry.mark_complete_without_deps(&type_);
            }
            _ => panic!(
                "Cannot use base type to complete type `{}`: not a template instance",
                type_name
            ),
        }
    }

    /// For an instance of a type template, creates a mapping from type parameter IDs to actual
    /// type argument IDs.
    fn actual_type_arguments(&self, registry: &TypeRegistry) -> HashMap<TypeId, TypeId> {
        match &self.type_id {
            TypeId::TemplateInstance(template_id, type_arg_ids) => {
                let template = Rc::clone(&registry.templates[template_id]);
                let type_parameter_ids: Vec<_> = template
                    .borrow()
                    .type_parameters
                    .iter()
                    .map(|type_param| type_param.borrow().type_id.clone())
                    .collect();

                type_parameter_ids
                    .iter()
                    .zip(type_arg_ids.iter())
                    .map(|(a, p)| (a.clone(), p.clone()))
                    .collect()
            }
            _ => panic!(
                "Concrete type `{}` is not a template instance, and thus has no type arguments",
                self.name
            ),
        }
    }

    /// Collects the types that are "visible" from the "global" definition of a type, that is,
    /// disregarding the method bodies. This includes the fields' types, types from methods'
    /// signatures, and, for template instances, type arguments.
    fn type_dependencies_from_global_interface(
        &self,
        registry: &TypeRegistry,
    ) -> Vec<Rc<RefCell<Type>>> {
        let mut result = Vec::new();
        for field in &self.fields {
            result.push(Rc::clone(&field.borrow().type_));
        }
        for method in &self.methods {
            result.push(Rc::clone(&method.borrow().return_type));
            for parameter in &method.borrow().parameters {
                result.push(Rc::clone(&parameter.borrow().type_));
            }
        }
        if let TypeId::TemplateInstance(_, type_arg_ids) = &self.type_id {
            for type_arg_id in type_arg_ids {
                let type_arg = Rc::clone(&registry.lookup(type_arg_id));
                result.push(type_arg)
            }
        }

        result
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}

/// A plain, hashable, unique identifier for types.
///
/// Types can be looked up from `TypeRegistry` using their IDs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeId {
    Void,
    Int,
    Bool,
    Char,
    String,

    TemplateInstance(TypeTemplateId, Vec<TypeId>),

    /// A user-defined struct.
    Struct(SymbolId),

    /// Template type parameter that will be substituted for a concrete type during instantiation.
    /// The second element of the tag is the index of the parameter.
    TypeParameter(TypeTemplateId, usize),

    /// An invalid type. It can never appear in a valid program.
    Error,
}
