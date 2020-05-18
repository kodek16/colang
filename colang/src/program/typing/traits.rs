//! Traits in CO define abstractions over types.

use crate::program::{Program, SymbolId};
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
}

/// A plain, hashable, unique identifier for traits.
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
        Rc::new(RefCell::new(Trait {
            name,
            id: TraitId::Plain(program.symbol_ids_mut().next_id()),
            definition_site: Some(definition_site),
        }))
    }
}
