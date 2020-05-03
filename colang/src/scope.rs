//! Named entity visibility hierarchy management.

use crate::errors::CompilationError;
use crate::program::{Field, Function, Type, TypeTemplate, Variable};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

/// A collection of named entities that supports lookup by name.
///
/// Scopes are parametrized by a type `G` that defines the entity kinds allowed to be present
/// in the scope. This way a field cannot be in a free scope, and vice versa.
///
/// All entities in each scope must have unique names: one of the main goals of scopes is to
/// detect and report name conflicts.
///
/// Scopes support nesting: a scope can be nested in another, parent scope. All entities visible
/// in the parent scope are also visible in the nested scope, as long as they are not "shadowed"
/// by entities with the same name defined in the nested scope proper.
pub struct Scope<G: GeneralNamedEntity> {
    // `entities` should always be Some, Option is for interior mutability.
    entities: Option<HashMap<String, G>>,

    // `parent` can be None for root scope.
    parent: Option<Box<Scope<G>>>,
}

/// The scope containing all "free" names that are looked up directly.
pub type FreeScope = Scope<FreeEntity>;

/// A scope associated with a type.
///
/// Names in this scope can be only looked up in the context of a "receiver" value of this type.
pub type TypeScope = Scope<TypeMemberEntity>;

impl<G: GeneralNamedEntity> Scope<G> {
    /// Creates a new scope.
    pub fn new() -> Scope<G> {
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

    /// Restores the previous "scope frame". Has to called after `push`.
    pub fn pop(&mut self) {
        let parent = self
            .parent
            .as_deref_mut()
            .expect("Attempted to pop root scope.");
        self.entities = parent.entities.take();
        self.parent = parent.parent.take();
    }

    /// Adds a new entity to the scope.
    ///
    /// If a name conflict is detected, a `CompilationError` is returned.
    #[must_use]
    pub fn add<S>(&mut self, entity: S) -> Result<(), CompilationError>
    where
        S: SpecificNamedEntity,
        G: From<S>,
    {
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
                self.entities
                    .as_mut()
                    .unwrap()
                    .insert(name.to_string(), entity.into());
                Ok(())
            }
        }
    }

    /// Looks up an entity with a specific name from the scope.
    ///
    /// Expected entity kind must be provided, and if the name corresponds to an entity of a
    /// different kind, an error is returned.
    ///
    /// If the name is unknown, an error is returned as well.
    pub fn lookup<S>(
        &self,
        name: &str,
        reference_location: SourceOrigin,
    ) -> Result<Rc<RefCell<S::Item>>, CompilationError>
    where
        S: SpecificNamedEntity,
        S: TryFrom<G>,
    {
        match self.lookup_chain(name) {
            Some(entity) => match S::try_from(entity.clone()) {
                Ok(target) => Ok(target.item()),
                Err(_) => {
                    let error = CompilationError::named_entity_kind_mismatch(
                        name,
                        S::kind(),
                        entity,
                        reference_location,
                    );
                    Err(error)
                }
            },
            None => Err(CompilationError::named_entity_not_found(
                name,
                S::kind(),
                reference_location,
            )),
        }
    }

    fn lookup_self(&self, name: &str) -> Option<&G> {
        self.entities.as_ref().unwrap().get(name)
    }

    fn lookup_chain(&self, name: &str) -> Option<&G> {
        self.entities.as_ref().unwrap().get(name).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.lookup_chain(name))
        })
    }
}

/// A trait for types directly wrapping a named entity (scope member).
pub trait SpecificNamedEntity: Clone {
    type Item;

    fn kind() -> NamedEntityKind;
    fn item(self) -> Rc<RefCell<Self::Item>>;

    fn name(&self) -> String;
    fn definition_site(&self) -> Option<SourceOrigin>;
}

/// A trait for sum types of `SpecificNamedEntity`, defining allowed entity kinds for a scope.
pub trait GeneralNamedEntity: Clone {
    fn kind(&self) -> NamedEntityKind;

    fn name(&self) -> String;
    fn definition_site(&self) -> Option<SourceOrigin>;
}

// This macro saves a lot of boilerplate, but makes some specific assumptions. Treat with caution.
macro_rules! specific_named_entity_impl {
    ($specific:ty, $general:ty, $variant:path) => {
        impl From<$specific> for $general {
            fn from(entity: $specific) -> Self {
                $variant(entity)
            }
        }

        impl TryFrom<$general> for $specific {
            type Error = ();

            fn try_from(entity: $general) -> Result<Self, Self::Error> {
                match entity {
                    $variant(entity) => Ok(entity),
                    _ => Err(()),
                }
            }
        }
    };
}

/// A simple description of an entity kind used in error messages.
pub enum NamedEntityKind {
    Variable,
    Function,
    Type,
    TypeTemplate,
    Field,
    Method,
}

/// A sum of all allowed type member entity kinds.
#[derive(Clone)]
pub enum TypeMemberEntity {
    Field(FieldEntity),
    Method(MethodEntity),
}

/// An entity kind representing a field of some type.
#[derive(Clone)]
pub struct FieldEntity(pub Rc<RefCell<Field>>);

/// An entity kind representing a method of some type.
#[derive(Clone)]
pub struct MethodEntity(pub Rc<RefCell<Function>>);

impl GeneralNamedEntity for TypeMemberEntity {
    fn kind(&self) -> NamedEntityKind {
        match self {
            TypeMemberEntity::Field(_) => NamedEntityKind::Field,
            TypeMemberEntity::Method(_) => NamedEntityKind::Method,
        }
    }

    fn name(&self) -> String {
        match self {
            TypeMemberEntity::Field(ref field) => field.name(),
            TypeMemberEntity::Method(ref method) => method.name(),
        }
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        match self {
            TypeMemberEntity::Field(ref field) => field.definition_site(),
            TypeMemberEntity::Method(ref method) => method.definition_site(),
        }
    }
}

impl SpecificNamedEntity for FieldEntity {
    type Item = Field;

    fn kind() -> NamedEntityKind {
        NamedEntityKind::Field
    }

    fn item(self) -> Rc<RefCell<Self::Item>> {
        self.0
    }

    fn name(&self) -> String {
        self.0.borrow().name.clone()
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        Some(self.0.borrow().definition_site)
    }
}
specific_named_entity_impl!(FieldEntity, TypeMemberEntity, TypeMemberEntity::Field);

impl SpecificNamedEntity for MethodEntity {
    type Item = Function;

    fn kind() -> NamedEntityKind {
        NamedEntityKind::Function
    }

    fn item(self) -> Rc<RefCell<Self::Item>> {
        self.0
    }

    fn name(&self) -> String {
        self.0.borrow().name.clone()
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        self.0.borrow().definition_site
    }
}
specific_named_entity_impl!(MethodEntity, TypeMemberEntity, TypeMemberEntity::Method);

/// A sum of all entity kinds allowed in the free scope.
#[derive(Clone)]
pub enum FreeEntity {
    Variable(VariableEntity),
    Function(FunctionEntity),
    Type(TypeEntity),
    TypeTemplate(TypeTemplateEntity),
}

/// An entity kind representing a local variable or a function parameter.
#[derive(Clone)]
pub struct VariableEntity(pub Rc<RefCell<Variable>>);

/// An entity kind representing a non-method function.
#[derive(Clone)]
pub struct FunctionEntity(pub Rc<RefCell<Function>>);

/// An entity kind representing a non-template type.
#[derive(Clone)]
pub struct TypeEntity(pub Rc<RefCell<Type>>);

/// An entity kind representing a template type.
#[derive(Clone)]
pub struct TypeTemplateEntity(pub Rc<RefCell<TypeTemplate>>);

impl GeneralNamedEntity for FreeEntity {
    fn kind(&self) -> NamedEntityKind {
        match self {
            FreeEntity::Variable(_) => NamedEntityKind::Variable,
            FreeEntity::Function(_) => NamedEntityKind::Function,
            FreeEntity::Type(_) => NamedEntityKind::Type,
            FreeEntity::TypeTemplate(_) => NamedEntityKind::TypeTemplate,
        }
    }

    fn name(&self) -> String {
        match self {
            FreeEntity::Variable(ref variable) => variable.name(),
            FreeEntity::Function(ref function) => function.name(),
            FreeEntity::Type(ref type_) => type_.name(),
            FreeEntity::TypeTemplate(ref type_template) => type_template.name(),
        }
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        match self {
            FreeEntity::Variable(ref variable) => variable.definition_site(),
            FreeEntity::Function(ref function) => function.definition_site(),
            FreeEntity::Type(ref type_) => type_.definition_site(),
            FreeEntity::TypeTemplate(ref type_template) => type_template.definition_site(),
        }
    }
}

impl SpecificNamedEntity for VariableEntity {
    type Item = Variable;

    fn kind() -> NamedEntityKind {
        NamedEntityKind::Variable
    }

    fn item(self) -> Rc<RefCell<Self::Item>> {
        self.0
    }

    fn name(&self) -> String {
        self.0.borrow().name.clone()
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        self.0.borrow().definition_site
    }
}
specific_named_entity_impl!(VariableEntity, FreeEntity, FreeEntity::Variable);

impl SpecificNamedEntity for FunctionEntity {
    type Item = Function;

    fn kind() -> NamedEntityKind {
        NamedEntityKind::Function
    }

    fn item(self) -> Rc<RefCell<Self::Item>> {
        self.0
    }

    fn name(&self) -> String {
        self.0.borrow().name.clone()
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        self.0.borrow().definition_site
    }
}
specific_named_entity_impl!(FunctionEntity, FreeEntity, FreeEntity::Function);

impl SpecificNamedEntity for TypeEntity {
    type Item = Type;

    fn kind() -> NamedEntityKind {
        NamedEntityKind::Type
    }

    fn item(self) -> Rc<RefCell<Self::Item>> {
        self.0
    }

    fn name(&self) -> String {
        self.0.borrow().name.clone()
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        self.0.borrow().definition_site
    }
}
specific_named_entity_impl!(TypeEntity, FreeEntity, FreeEntity::Type);

impl SpecificNamedEntity for TypeTemplateEntity {
    type Item = TypeTemplate;

    fn kind() -> NamedEntityKind {
        NamedEntityKind::TypeTemplate
    }

    fn item(self) -> Rc<RefCell<Self::Item>> {
        self.0
    }

    fn name(&self) -> String {
        self.0.borrow().name.clone()
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        self.0.borrow().definition_site
    }
}
specific_named_entity_impl!(TypeTemplateEntity, FreeEntity, FreeEntity::TypeTemplate);
