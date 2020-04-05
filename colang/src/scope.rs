//! Named entity visibility hierarchy management.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::InputSpan;
use crate::errors::{CompilationError, Word};
use crate::program::{Function, Type, Variable};

#[derive(Debug)]
enum NamedEntity {
    Variable(Rc<RefCell<Variable>>),
    Function(Rc<RefCell<Function>>),
    Type(Rc<RefCell<Type>>),
}

impl NamedEntity {
    fn word(&self, scope: &Scope) -> Word {
        match self {
            NamedEntity::Variable(_) => {
                if scope.is_type_scope {
                    Word::Field
                } else {
                    Word::Variable
                }
            }
            NamedEntity::Function(_) => {
                if scope.is_type_scope {
                    Word::Method
                } else {
                    Word::Function
                }
            }
            NamedEntity::Type(_) => Word::Type,
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    // `entities` should always be Some, Option is for interior mutability.
    entities: Option<HashMap<String, NamedEntity>>,

    // `parent` can be None for root scope.
    parent: Option<Box<Scope>>,

    /// Type scopes behave exactly the same as normal scopes, but this flag allows to
    /// produce different error messages specific to type scopes (e.g. variables are reported
    /// as fields, functions as methods).
    is_type_scope: bool,
}

impl Scope {
    /// Creates a new, root scope.
    pub fn new() -> Scope {
        Scope {
            entities: Some(HashMap::new()),
            parent: None,
            is_type_scope: false,
        }
    }

    pub fn new_for_type() -> Scope {
        Scope {
            entities: Some(HashMap::new()),
            parent: None,
            is_type_scope: true,
        }
    }

    /// Creates a new "scope frame", pushing the existing frame below it.
    /// Entities in the upper frames can shadow entities with same names in lower frames.
    pub fn push(&mut self) {
        self.parent = Some(Box::new(Scope {
            entities: self.entities.take(),
            parent: self.parent.take(),
            is_type_scope: self.is_type_scope,
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
        self.add_entity(variable)
    }

    #[must_use]
    pub fn add_function(
        &mut self,
        function: Rc<RefCell<Function>>,
    ) -> Result<(), CompilationError> {
        self.add_entity(function)
    }

    #[must_use]
    pub fn add_type(&mut self, type_: Rc<RefCell<Type>>) -> Result<(), CompilationError> {
        self.add_entity(type_)
    }

    pub fn lookup_variable(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Variable>>, CompilationError> {
        self.lookup_entity_kind(
            name,
            reference_location,
            if self.is_type_scope {
                Word::Field
            } else {
                Word::Variable
            },
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
            if self.is_type_scope {
                Word::Method
            } else {
                Word::Function
            },
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
        self.lookup_entity_kind(
            name,
            reference_location,
            Word::Type,
            |entity| match entity {
                NamedEntity::Type(type_) => Some(type_),
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
        word: Word,
        pattern: impl FnOnce(&NamedEntity) -> Option<&Rc<RefCell<T>>>,
    ) -> Result<&Rc<RefCell<T>>, CompilationError> {
        match self.lookup(name) {
            Some(entity) => match pattern(entity) {
                Some(target) => Ok(target),
                None => {
                    let error = CompilationError::named_entity_kind_mismatch(
                        name,
                        word,
                        entity.word(&self),
                        reference_location,
                    );
                    Err(error)
                }
            },
            None => Err(CompilationError::named_entity_not_found(
                name,
                word,
                reference_location,
            )),
        }
    }

    // Generic method for adding various kinds of entities.
    #[must_use]
    fn add_entity<T: NamedEntityKind>(
        &mut self,
        entity: Rc<RefCell<T>>,
    ) -> Result<(), CompilationError> {
        let name = entity.borrow().name();
        let definition_site = entity.borrow().definition_site();

        let existing = self.lookup_self(&name);
        match existing {
            Some(existing) => {
                let error = CompilationError::named_entity_already_exists(
                    &name,
                    existing.word(&self),
                    definition_site.expect("Name collision for internal entity"),
                );
                Err(error)
            }
            None => {
                self.entities_mut()
                    .insert(name.to_string(), T::to_named(entity));
                Ok(())
            }
        }
    }
}

/// Common behavior for all named entities that can be added to scopes.
trait NamedEntityKind {
    fn name(&self) -> String;
    fn definition_site(&self) -> Option<InputSpan>;
    fn to_named(entity: Rc<RefCell<Self>>) -> NamedEntity;
}

impl NamedEntityKind for Variable {
    fn name(&self) -> String {
        self.name.clone()
    }

    fn definition_site(&self) -> Option<InputSpan> {
        self.definition_site
    }

    fn to_named(variable: Rc<RefCell<Variable>>) -> NamedEntity {
        NamedEntity::Variable(variable)
    }
}

impl NamedEntityKind for Function {
    fn name(&self) -> String {
        self.name().to_string()
    }

    fn definition_site(&self) -> Option<InputSpan> {
        self.definition_site()
    }

    fn to_named(function: Rc<RefCell<Function>>) -> NamedEntity {
        NamedEntity::Function(function)
    }
}

impl NamedEntityKind for Type {
    fn name(&self) -> String {
        self.name().to_string()
    }

    fn definition_site(&self) -> Option<InputSpan> {
        None
    }

    fn to_named(type_: Rc<RefCell<Type>>) -> NamedEntity {
        NamedEntity::Type(type_)
    }
}
