use crate::errors::{self, CompilationError};
use crate::program::typing::registry::TypeRegistry;
use crate::program::typing::types::{Type, TypeId, TypeInstantiationData, TypeInstantiationStatus};
use crate::program::{Program, SymbolId, TypeRef};
use crate::scope::TypeScope;
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// A type template in CO: a type definition with type placeholders that can be substituted for
/// concrete types.
pub struct TypeTemplate {
    /// The name of the type template.
    pub name: String,

    /// A unique identifier of the type template.
    pub type_template_id: TypeTemplateId,

    /// The location in program source code where the type template is defined.
    ///
    /// This can be `None` only for internal type templates.
    pub definition_site: Option<SourceOrigin>,

    /// The placeholder types that are substituted with type arguments in fields and methods
    /// during instantiation.
    pub type_parameters: Vec<Rc<RefCell<Type>>>,

    /// If the template has a quirky naming pattern (like arrays or pointers),
    /// this field allows to specify it.
    name_template: Box<dyn Fn(Vec<&Rc<RefCell<Type>>>) -> String>,

    /// This is a full-fledged type that will be used as the source of copying during instantiation.
    base_type: Rc<RefCell<Type>>,
}

/// Information about a to-be-created type parameter.
///
/// Type parameters are constructed at the same time as the template using this information
/// collected beforehand.
pub struct ProtoTypeParameter {
    pub name: String,
    pub definition_site: Option<SourceOrigin>,
}

/// A plain, hashable, unique identifier for type templates.
///
/// Type templates can be looked up from `TypeRegistry` using their IDs.
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
        definition_site: SourceOrigin,
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

    pub(in crate::program::typing) fn new(
        type_template_id: TypeTemplateId,
        name: String,
        type_parameters: Vec<ProtoTypeParameter>,
        definition_site: Option<SourceOrigin>,
        name_template: Option<Box<dyn Fn(Vec<&Rc<RefCell<Type>>>) -> String>>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<TypeTemplate>> {
        let type_parameters: Vec<_> = type_parameters
            .into_iter()
            .enumerate()
            .map(|(index, type_param)| {
                registry.register(Type {
                    name: type_param.name,
                    type_id: TypeId::TypeParameter(type_template_id.clone(), index),
                    definition_site: type_param.definition_site,
                    instantiation_data: None,
                    fields: Vec::new(),
                    methods: Vec::new(),
                    implemented_traits: Vec::new(),
                    scope: TypeScope::new(),
                    instantiation_status: TypeInstantiationStatus::FullyComplete,
                })
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

        let base_type_id = TypeId::TemplateInstance(
            type_template_id.clone(),
            type_parameters
                .iter()
                .map(|type_param| type_param.borrow().type_id.clone())
                .collect(),
        );

        let base_type = registry.register(Type {
            name: (*name_template)(type_parameters.iter().collect()),
            type_id: base_type_id,
            definition_site,
            // instantiation_data is filled later after `TypeTemplate` is actually created.
            instantiation_data: None,
            fields: Vec::new(),
            methods: Vec::new(),
            implemented_traits: Vec::new(),
            scope: TypeScope::new(),
            instantiation_status: TypeInstantiationStatus::DepsMayNeedInstantiation,
        });

        let type_template = Rc::new(RefCell::new(TypeTemplate {
            name,
            definition_site,
            name_template,
            base_type: Rc::clone(&base_type),
            type_template_id: type_template_id.clone(),
            type_parameters: type_parameters.clone(),
        }));

        base_type.borrow_mut().instantiation_data = Some(TypeInstantiationData {
            template: Rc::clone(&type_template),
            type_arguments: type_parameters,
        });

        let existing = registry
            .templates
            .insert(type_template_id.clone(), Rc::clone(&type_template));
        if existing.is_some() {
            panic!(
                "Attempt to create a duplicate type template with the same type template ID: {:?}",
                type_template_id,
            )
        }
        type_template
    }

    pub fn base_type(&self) -> &Rc<RefCell<Type>> {
        &self.base_type
    }

    /// Creates an instance of the template after checking the type arguments.
    pub fn instantiate(
        template: Rc<RefCell<TypeTemplate>>,
        type_arguments: Vec<TypeRef>,
        registry: &mut TypeRegistry,
        location: SourceOrigin,
    ) -> Result<Rc<RefCell<Type>>, Vec<CompilationError>> {
        if type_arguments.len() != template.borrow().type_parameters.len() {
            return Err(vec![errors::wrong_number_of_type_template_arguments(
                &template.borrow(),
                type_arguments.len(),
                location,
            )]);
        }

        let mut bound_violations = Vec::new();
        for (type_argument, type_parameter) in type_arguments
            .iter()
            .zip(template.borrow().type_parameters.iter())
        {
            for trait_bound in &type_parameter.borrow().implemented_traits {
                if !type_argument
                    .borrow()
                    .implemented_traits
                    .contains(trait_bound)
                {
                    bound_violations.push(errors::type_argument_violates_trait_bound(
                        &type_argument.borrow(),
                        &template.borrow(),
                        &type_parameter.borrow(),
                        &trait_bound.borrow(),
                        type_argument.reference_location().unwrap(),
                    ));
                }
            }
        }

        if !bound_violations.is_empty() {
            return Err(bound_violations);
        }

        Ok(TypeTemplate::instantiate_unchecked(
            template,
            type_arguments
                .into_iter()
                .map(|argument| argument.into())
                .collect(),
            registry,
        ))
    }

    /// Creates an instance of the template without checking type arguments.
    ///
    /// This method should only be used when the caller is certain that `type_arguments` conform
    /// to the template signature.
    pub fn instantiate_unchecked(
        template: Rc<RefCell<TypeTemplate>>,
        type_arguments: Vec<Rc<RefCell<Type>>>,
        registry: &mut TypeRegistry,
    ) -> Rc<RefCell<Type>> {
        let instantiation_data = TypeInstantiationData {
            template: Rc::clone(&template),
            type_arguments: type_arguments.clone(),
        };

        let template = template.borrow();

        if type_arguments
            .iter()
            .any(|type_arg| type_arg.borrow().is_error())
        {
            return Type::error();
        }

        let type_argument_ids: Vec<_> = type_arguments
            .iter()
            .map(|type_arg| type_arg.borrow().type_id.clone())
            .collect();

        let concrete_type_id =
            TypeId::TemplateInstance(template.type_template_id.clone(), type_argument_ids);

        if let Some(preexisting_instantiation) = registry.types.get(&concrete_type_id) {
            return Rc::clone(preexisting_instantiation);
        }

        let result = registry.register(Type {
            name: (*template.name_template)(type_arguments.iter().collect()),
            type_id: concrete_type_id,
            definition_site: template.definition_site,
            instantiation_data: Some(instantiation_data),
            fields: Vec::new(),
            methods: Vec::new(),
            implemented_traits: template.base_type.borrow().implemented_traits.clone(),
            scope: TypeScope::new(),
            instantiation_status: TypeInstantiationStatus::NeedsInstantiation,
        });
        result
    }
}
