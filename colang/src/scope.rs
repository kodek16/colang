//! Named entity visibility hierarchy management.

use crate::errors::{self, CompilationError};
use crate::program::{Field, Function, Trait, Type, TypeTemplate, Variable};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::fmt::Formatter;
use std::marker::PhantomData;
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

/// Error type for scope lookups.
///
/// `S` is the expected entity kind, and `G` is the set of all possible kinds..
pub enum LookupError<S: SpecificNamedEntity, G: GeneralNamedEntity>
where
    G: From<S>,
{
    FoundDifferentKind(G),
    NotFound {
        name: String,
        phantom: PhantomData<S>,
    },
}

/// Error type for scope add operation.pub
pub struct AddError<G: GeneralNamedEntity> {
    pub existing: G,
}

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
    pub fn add<S>(&mut self, entity: S) -> Result<(), AddError<G>>
    where
        S: SpecificNamedEntity,
        G: From<S>,
    {
        let name = entity.name();

        let existing = self.lookup_self(&name);
        match existing {
            Some(existing) => Err(AddError {
                existing: existing.clone(),
            }),
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
    pub fn lookup<S>(&self, name: &str) -> Result<Rc<RefCell<S::Item>>, LookupError<S, G>>
    where
        S: SpecificNamedEntity,
        G: From<S>,
        S: TryFrom<G>,
    {
        match self.lookup_chain(name) {
            Some(entity) => match S::try_from(entity.clone()) {
                Ok(target) => Ok(target.item()),
                Err(_) => Err(LookupError::FoundDifferentKind(entity.clone())),
            },
            None => Err(LookupError::NotFound {
                name: name.to_string(),
                phantom: PhantomData,
            }),
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

impl<S: SpecificNamedEntity, G: GeneralNamedEntity> LookupError<S, G>
where
    G: From<S>,
{
    /// Creates a `CompilationError` treating the lookup error as coming from a direct lookup.
    ///
    /// A lookup is direct when a name is directly used in an entity context in the source code.
    /// Most lookups are direct.
    pub fn into_direct_lookup_error(self, lookup_location: SourceOrigin) -> CompilationError {
        match self {
            LookupError::FoundDifferentKind(actual) => errors::named_entity_kind_mismatch(
                &actual.name(),
                S::kind(),
                &actual,
                lookup_location,
            ),
            LookupError::NotFound { name, .. } => {
                errors::named_entity_not_found(&name, S::kind(), lookup_location)
            }
        }
    }
}

impl<G: GeneralNamedEntity> AddError<G> {
    /// Creates a `CompilationError` treating the add error as coming from a direct update.
    pub fn into_direct_add_error(self, add_location: SourceOrigin) -> CompilationError {
        errors::named_entity_already_defined(&self.existing.name(), &self.existing, add_location)
    }
}

impl<G: GeneralNamedEntity> fmt::Debug for AddError<G> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("AddError")
            .field("name", &self.existing.name())
            .finish()
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
    Trait,
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
    Trait(TraitEntity),
    Type(TypeEntity),
    TypeTemplate(TypeTemplateEntity),
}

/// An entity kind representing a local variable or a function parameter.
#[derive(Clone)]
pub struct VariableEntity(pub Rc<RefCell<Variable>>);

/// An entity kind representing a non-method function.
#[derive(Clone)]
pub struct FunctionEntity(pub Rc<RefCell<Function>>);

/// An entity kind representing a trait.
#[derive(Clone)]
pub struct TraitEntity(pub Rc<RefCell<Trait>>);

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
            FreeEntity::Trait(_) => NamedEntityKind::Trait,
            FreeEntity::Type(_) => NamedEntityKind::Type,
            FreeEntity::TypeTemplate(_) => NamedEntityKind::TypeTemplate,
        }
    }

    fn name(&self) -> String {
        match self {
            FreeEntity::Variable(ref variable) => variable.name(),
            FreeEntity::Function(ref function) => function.name(),
            FreeEntity::Trait(ref trait_) => trait_.name(),
            FreeEntity::Type(ref type_) => type_.name(),
            FreeEntity::TypeTemplate(ref type_template) => type_template.name(),
        }
    }

    fn definition_site(&self) -> Option<SourceOrigin> {
        match self {
            FreeEntity::Variable(ref variable) => variable.definition_site(),
            FreeEntity::Function(ref function) => function.definition_site(),
            FreeEntity::Trait(ref trait_) => trait_.definition_site(),
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

impl SpecificNamedEntity for TraitEntity {
    type Item = Trait;

    fn kind() -> NamedEntityKind {
        NamedEntityKind::Trait
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
specific_named_entity_impl!(TraitEntity, FreeEntity, FreeEntity::Trait);

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
