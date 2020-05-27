//! Traits in CO define abstractions over types.

use crate::program::typing::types::TypeInstantiationStatus;
use crate::program::{Program, SymbolId, TraitRef, Type, TypeId};
use crate::scope::TypeScope;
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// A trait in CO: abstraction over a class of types with common properties.
pub struct Trait {
    /// The name of the trait.
    pub name: String,

    /// A unique identifier of the trait.
    pub id: TraitId,

    /// The location in program source code where the trait is defined.
    ///
    /// This can be `None` only for internal traits.
    pub definition_site: Option<SourceOrigin>,

    /// The placeholder for the trait implementor type.
    ///
    /// This is the `Self` type for this trait: it serves as an example that the implementor types
    /// have to conform to. It also contains all of the trait methods.
    pub self_type: Rc<RefCell<Type>>,
}

/// A plain, hashable, unique identifier for traits.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitId {
    Plain(SymbolId),
}

impl Trait {
    /// Creates a new user-defined trait.
    pub fn new(
        name: String,
        definition_site: SourceOrigin,
        program: &mut Program,
    ) -> Rc<RefCell<Trait>> {
        let id = TraitId::Plain(program.symbol_ids_mut().next_id());

        let self_type = program.types_mut().register(Type {
            name: String::from("Self"),
            type_id: TypeId::SelfType(id.clone()),
            definition_site: Some(definition_site),
            instantiation_data: None,
            fields: Vec::new(),
            methods: Vec::new(),
            implemented_traits: Vec::new(),
            scope: TypeScope::new(),
            instantiation_status: TypeInstantiationStatus::DepsMayNeedInstantiation,
        });

        let trait_ = Rc::new(RefCell::new(Trait {
            name,
            id,
            definition_site: Some(definition_site),
            self_type: Rc::clone(&self_type),
        }));

        self_type
            .borrow_mut()
            .implemented_traits
            .push(TraitRef::new(Rc::clone(&trait_), definition_site));

        trait_
    }
}

impl PartialEq for Trait {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
