use crate::errors::CompilationError;
use crate::program::typing::registry::TypeRegistry;
use crate::program::typing::templates::TypeTemplateId;
use crate::program::{Field, Function, Program, SymbolId, TypeTemplate};
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

    /// For types instantiated from a template, this is the data about the instantiation.
    pub instantiation_data: Option<TypeInstantiationData>,

    pub fields: Vec<Rc<RefCell<Field>>>,
    pub methods: Vec<Rc<RefCell<Function>>>,

    pub(crate) scope: TypeScope,

    /// Types can be in one of a few states, see `TypeCompleteness` for details.
    pub(in crate::program::typing) completeness: TypeCompleteness,
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

/// Contains information about the instantiation process performed for some type.
#[derive(Clone)]
pub struct TypeInstantiationData {
    pub template: Rc<RefCell<TypeTemplate>>,
    pub type_arguments: Vec<Rc<RefCell<Type>>>,
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
        program.types_mut().register(Type {
            name,
            type_id,
            definition_site: Some(definition_site),
            instantiation_data: None,
            fields: Vec::new(),
            methods: Vec::new(),
            scope: TypeScope::new(),
            completeness: TypeCompleteness::Incomplete,
        })
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
            instantiation_data: None,
            fields: vec![],
            methods: vec![],
            scope: Scope::new(),
            completeness: TypeCompleteness::FullyComplete,
        }))
    }

    /// Adds a field to the list of type members.
    #[must_use]
    pub fn add_field(&mut self, field: Rc<RefCell<Field>>) -> Result<(), CompilationError> {
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
    ) -> Result<Rc<RefCell<Field>>, CompilationError> {
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
    pub fn pointer_target_type(&self) -> Option<Rc<RefCell<Type>>> {
        if self.is_pointer() {
            Some(Rc::clone(
                &self.instantiation_data.as_ref().unwrap().type_arguments[0],
            ))
        } else {
            None
        }
    }

    /// If `self` is an array type, returns the type of elements.
    pub fn array_element_type(&self) -> Option<Rc<RefCell<Type>>> {
        if self.is_array() {
            Some(Rc::clone(
                &self.instantiation_data.as_ref().unwrap().type_arguments[0],
            ))
        } else {
            None
        }
    }

    /// Checks if any of the transitive type arguments of this type are unfilled type parameter
    /// placeholders.
    pub fn depends_on_type_parameter_placeholders(&self) -> bool {
        fn check(type_id: &TypeId) -> bool {
            match type_id {
                TypeId::TemplateInstance(_, ref type_arg_ids) => type_arg_ids.iter().any(check),
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
            match type_.borrow().instantiation_data {
                Some(ref instantiation_data) => {
                    let template = &instantiation_data.template;
                    let arguments_after_substitution: Vec<_> = instantiation_data
                        .type_arguments
                        .iter()
                        .map(|type_arg| Type::substitute(type_arg, substitutions, registry))
                        .collect();

                    TypeTemplate::instantiate(
                        Rc::clone(&template),
                        arguments_after_substitution,
                        registry,
                        None,
                    )
                    .expect(&format!(
                        "Failed to instantiate type template `{}`",
                        template.borrow().name
                    ))
                }
                None => Rc::clone(type_),
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
                    .type_dependencies_from_global_interface();
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
        let instantiation_data = type_.borrow().instantiation_data.clone();

        match instantiation_data {
            Some(instantiation_data) => {
                let template = instantiation_data.template.borrow();
                let base_type = template.base_type().borrow();

                if base_type.completeness == TypeCompleteness::Incomplete {
                    panic!(
                        "Attempted to complete type `{}` from its base type `{}`, but the base type is not complete yet",
                        type_name, base_type.name
                    )
                }

                let own_type_arguments = type_.borrow().actual_type_arguments();
                for field in base_type.fields.iter() {
                    let instantiated_field = Rc::new(RefCell::new(field.borrow().instantiate(
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
            None => panic!(
                "Cannot use base type to complete type `{}`: not a template instance",
                type_name
            ),
        }
    }

    /// Creates a mapping from type parameter IDs to actual type argument IDs.
    ///
    /// Assumes this type is a template instance.
    fn actual_type_arguments(&self) -> HashMap<TypeId, TypeId> {
        match self.instantiation_data {
            Some(ref instantiation_data) => {
                let type_parameter_ids: Vec<_> = instantiation_data
                    .template
                    .borrow()
                    .type_parameters
                    .iter()
                    .map(|type_param| type_param.borrow().type_id.clone())
                    .collect();

                let type_argument_ids: Vec<_> = instantiation_data
                    .type_arguments
                    .iter()
                    .map(|type_arg| type_arg.borrow().type_id.clone())
                    .collect();

                type_parameter_ids
                    .into_iter()
                    .zip(type_argument_ids.into_iter())
                    .collect()
            }
            None => panic!(
                "Concrete type `{}` is not a template instance, and thus has no type arguments",
                self.name
            ),
        }
    }

    /// Collects the types that are "visible" from the "global" definition of a type, that is,
    /// disregarding the method bodies. This includes the fields' types, types from methods'
    /// signatures, and, for template instances, type arguments.
    fn type_dependencies_from_global_interface(&self) -> Vec<Rc<RefCell<Type>>> {
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

        if let Some(ref instantiation_data) = self.instantiation_data {
            for type_argument in &instantiation_data.type_arguments {
                result.push(Rc::clone(&type_argument))
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
