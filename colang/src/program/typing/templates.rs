use crate::errors::CompilationError;
use crate::program::typing::registry::TypeRegistry;
use crate::program::typing::types::{Type, TypeCompleteness, TypeId};
use crate::program::{Program, SymbolId};
use crate::source::{InputSpan, SourceOrigin};
use std::cell::RefCell;
use std::rc::Rc;

pub struct TypeTemplate {
    pub type_template_id: TypeTemplateId,
    pub name: String,
    pub definition_site: Option<SourceOrigin>,

    /// The placeholder types that will be substituted with type arguments in fields and methods
    /// during instantiation.
    pub type_parameters: Vec<Rc<RefCell<Type>>>,

    /// If the template has a quirky naming pattern (like arrays or pointers),
    /// this field allows to specify it.
    name_template: Box<dyn Fn(Vec<&Rc<RefCell<Type>>>) -> String>,

    /// This is a full-fledged type that will be used as the source of copying during instantiation.
    base_type: Rc<RefCell<Type>>,
}

/// Type parameters are constructed at the same time as the template using this information
/// collected beforehand.
pub struct ProtoTypeParameter {
    pub name: String,
    pub definition_site: Option<SourceOrigin>,
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
                Type::new(
                    type_param.name,
                    TypeId::TypeParameter(type_template_id.clone(), index),
                    type_param.definition_site,
                    TypeCompleteness::FullyComplete,
                    registry,
                )
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
        let base_type = Type::new(
            (*name_template)(type_parameters.iter().collect()),
            base_type_id,
            definition_site,
            TypeCompleteness::Incomplete,
            registry,
        );

        let type_template = Rc::new(RefCell::new(TypeTemplate {
            type_template_id: type_template_id.clone(),
            name,
            definition_site,
            name_template,
            type_parameters,
            base_type,
        }));

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

    pub fn instantiate(
        &self,
        type_arguments: Vec<&Rc<RefCell<Type>>>,
        registry: &mut TypeRegistry,
        location: Option<InputSpan>,
    ) -> Result<Rc<RefCell<Type>>, CompilationError> {
        if type_arguments.len() != self.type_parameters.len() {
            return Err(CompilationError::wrong_number_of_type_template_arguments(
                &self,
                type_arguments.len(),
                SourceOrigin::Plain(location.expect(
                    "Synthetic instantiation of type template with wrong number of type arguments",
                )),
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

        let concrete_type_id =
            TypeId::TemplateInstance(self.type_template_id.clone(), type_argument_ids);

        if let Some(preexisting_instantiation) = registry.types.get(&concrete_type_id) {
            return Ok(Rc::clone(preexisting_instantiation));
        }

        Ok(Type::new(
            (*self.name_template)(type_arguments),
            concrete_type_id,
            self.definition_site,
            TypeCompleteness::Incomplete,
            registry,
        ))
    }
}
