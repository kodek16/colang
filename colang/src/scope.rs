//! Named entity visibility hierarchy management.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::errors::CompilationError;
use crate::program::{Function, Type, TypeTemplate, Variable};
use crate::source::{InputSpan, SourceOrigin};

pub enum NamedEntity {
    Variable(Rc<RefCell<Variable>>),
    Function(Rc<RefCell<Function>>),
    Type(Rc<RefCell<Type>>),
    TypeTemplate(Rc<RefCell<TypeTemplate>>),
    Field(Rc<RefCell<Variable>>),
    Method(Rc<RefCell<Function>>),
}

impl NamedEntity {
    pub fn kind(&self) -> NamedEntityKind {
        match self {
            NamedEntity::Variable(_) => NamedEntityKind::Variable,
            NamedEntity::Function(_) => NamedEntityKind::Function,
            NamedEntity::Type(_) => NamedEntityKind::Type,
            NamedEntity::TypeTemplate(_) => NamedEntityKind::TypeTemplate,
            NamedEntity::Field(_) => NamedEntityKind::Field,
            NamedEntity::Method(_) => NamedEntityKind::Method,
        }
    }

    pub fn name(&self) -> String {
        match self {
            NamedEntity::Variable(variable) => variable.borrow().name.to_string(),
            NamedEntity::Function(function) => function.borrow().name.to_string(),
            NamedEntity::Type(type_) => type_.borrow().name.to_string(),
            NamedEntity::TypeTemplate(template) => template.borrow().name.to_string(),
            NamedEntity::Field(field) => field.borrow().name.to_string(),
            NamedEntity::Method(method) => method.borrow().name.to_string(),
        }
    }

    pub fn definition_site(&self) -> Option<SourceOrigin> {
        match self {
            NamedEntity::Variable(variable) => {
                variable.borrow().definition_site.map(SourceOrigin::Plain)
            }
            NamedEntity::Function(function) => function.borrow().definition_site,
            NamedEntity::Type(type_) => type_.borrow().definition_site.map(SourceOrigin::Plain),
            NamedEntity::TypeTemplate(template) => {
                template.borrow().definition_site.map(SourceOrigin::Plain)
            }
            NamedEntity::Field(field) => field.borrow().definition_site.map(SourceOrigin::Plain),
            NamedEntity::Method(method) => method.borrow().definition_site,
        }
    }
}

pub enum NamedEntityKind {
    Variable,
    Function,
    Type,
    TypeTemplate,
    Field,
    Method,
}

pub struct Scope {
    // `entities` should always be Some, Option is for interior mutability.
    entities: Option<HashMap<String, NamedEntity>>,

    // `parent` can be None for root scope.
    parent: Option<Box<Scope>>,
}

impl Scope {
    /// Creates a new scope.
    pub fn new() -> Scope {
        Scope {
            entities: Some(HashMap::new()),
            parent: None,
        }
    }

    /// Creates a new "scope frame", pushing the existing frame below it.
    /// Entities in the upper frames can shadow entities with same names in lower frames.
    pub fn push(&mut self) {
        self.parent = Some(Box::new(Scope {
            entities: self.entities.take(),
            parent: self.parent.take(),
        }));
        self.entities = Some(HashMap::new());
    }

    /// Restores previous "scope frame". Has to called after `push`.
    pub fn pop(&mut self) {
        let parent = self
            .parent
            .as_deref_mut()
            .expect("Attempted to pop root scope.");
        self.entities = parent.entities.take();
        self.parent = parent.parent.take();
    }

    #[must_use]
    pub fn add_variable(
        &mut self,
        variable: Rc<RefCell<Variable>>,
    ) -> Result<(), CompilationError> {
        self.add_entity(NamedEntity::Variable(variable))
    }

    #[must_use]
    pub fn add_function(
        &mut self,
        function: Rc<RefCell<Function>>,
    ) -> Result<(), CompilationError> {
        self.add_entity(NamedEntity::Function(function))
    }

    #[must_use]
    pub fn add_type(&mut self, type_: Rc<RefCell<Type>>) -> Result<(), CompilationError> {
        self.add_entity(NamedEntity::Type(type_))
    }

    #[must_use]
    pub fn add_type_template(
        &mut self,
        type_template: Rc<RefCell<TypeTemplate>>,
    ) -> Result<(), CompilationError> {
        self.add_entity(NamedEntity::TypeTemplate(type_template))
    }

    #[must_use]
    pub fn add_field(&mut self, field: Rc<RefCell<Variable>>) -> Result<(), CompilationError> {
        self.add_entity(NamedEntity::Field(field))
    }

    #[must_use]
    pub fn add_method(&mut self, method: Rc<RefCell<Function>>) -> Result<(), CompilationError> {
        self.add_entity(NamedEntity::Method(method))
    }

    pub fn lookup_variable(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Variable>>, CompilationError> {
        self.lookup_entity_kind(
            name,
            reference_location,
            NamedEntityKind::Variable,
            |entity| match entity {
                NamedEntity::Variable(variable) => Some(variable),
                _ => None,
            },
        )
    }

    pub fn lookup_function(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Function>>, CompilationError> {
        self.lookup_entity_kind(
            name,
            reference_location,
            NamedEntityKind::Function,
            |entity| match entity {
                NamedEntity::Function(function) => Some(function),
                _ => None,
            },
        )
    }

    pub fn lookup_type(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Type>>, CompilationError> {
        self.lookup_entity_kind(name, reference_location, NamedEntityKind::Type, |entity| {
            match entity {
                NamedEntity::Type(type_) => Some(type_),
                _ => None,
            }
        })
    }

    pub fn lookup_type_template(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<TypeTemplate>>, CompilationError> {
        self.lookup_entity_kind(
            name,
            reference_location,
            NamedEntityKind::TypeTemplate,
            |entity| match entity {
                NamedEntity::TypeTemplate(type_template) => Some(type_template),
                _ => None,
            },
        )
    }

    pub fn lookup_field(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Variable>>, CompilationError> {
        self.lookup_entity_kind(name, reference_location, NamedEntityKind::Field, |entity| {
            match entity {
                NamedEntity::Field(field) => Some(field),
                _ => None,
            }
        })
    }

    pub fn lookup_method(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Function>>, CompilationError> {
        self.lookup_entity_kind(
            name,
            reference_location,
            NamedEntityKind::Method,
            |entity| match entity {
                NamedEntity::Method(method) => Some(method),
                _ => None,
            },
        )
    }

    /// Convenience method for mutator access.
    fn entities_mut(&mut self) -> &mut HashMap<String, NamedEntity> {
        self.entities.as_mut().unwrap()
    }

    fn lookup_self(&self, name: &str) -> Option<&NamedEntity> {
        self.entities.as_ref().unwrap().get(name)
    }

    fn lookup(&self, name: &str) -> Option<&NamedEntity> {
        self.entities
            .as_ref()
            .unwrap()
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(name)))
    }

    // Generic helper method for looking up different kinds of named entities.
    fn lookup_entity_kind<T>(
        &self,
        name: &str,
        reference_location: InputSpan,
        expected_kind: NamedEntityKind,
        pattern: impl FnOnce(&NamedEntity) -> Option<&Rc<RefCell<T>>>,
    ) -> Result<&Rc<RefCell<T>>, CompilationError> {
        match self.lookup(name) {
            Some(entity) => match pattern(entity) {
                Some(target) => Ok(target),
                None => {
                    let error = CompilationError::named_entity_kind_mismatch(
                        name,
                        expected_kind,
                        entity,
                        SourceOrigin::Plain(reference_location),
                    );
                    Err(error)
                }
            },
            None => Err(CompilationError::named_entity_not_found(
                name,
                expected_kind,
                SourceOrigin::Plain(reference_location),
            )),
        }
    }

    #[must_use]
    fn add_entity(&mut self, entity: NamedEntity) -> Result<(), CompilationError> {
        let name = entity.name();
        let definition_site = entity.definition_site();

        let existing = self.lookup_self(&name);
        match existing {
            Some(existing) => {
                let error = CompilationError::named_entity_already_defined(
                    &name,
                    existing,
                    definition_site
                        .expect(&format!("Name collision for internal entity `{}`", name)),
                );
                Err(error)
            }
            None => {
                self.entities_mut().insert(name.to_string(), entity);
                Ok(())
            }
        }
    }
}
