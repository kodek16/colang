//! Wrappers for types and related entities that preserve the location of reference.

use crate::program::{Trait, Type};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

/// A wrapper for type that preserves location in code where it was referenced.
pub struct TypeRef {
    type_: Rc<RefCell<Type>>,
    reference_location: Option<SourceOrigin>,
}

impl TypeRef {
    pub fn new(type_: Rc<RefCell<Type>>, reference_location: Option<SourceOrigin>) -> TypeRef {
        TypeRef {
            type_,
            reference_location,
        }
    }

    pub fn reference_location(&self) -> Option<SourceOrigin> {
        self.reference_location
    }
}

impl Deref for TypeRef {
    type Target = Rc<RefCell<Type>>;

    fn deref(&self) -> &Self::Target {
        &self.type_
    }
}

/// A wrapper for trait that preserves location in code where it was referenced.
#[derive(Clone)]
pub struct TraitRef {
    trait_: Rc<RefCell<Trait>>,
    reference_location: SourceOrigin,
}

impl TraitRef {
    pub fn new(trait_: Rc<RefCell<Trait>>, reference_location: SourceOrigin) -> TraitRef {
        TraitRef {
            trait_,
            reference_location,
        }
    }

    pub fn reference_location(&self) -> SourceOrigin {
        self.reference_location
    }
}

impl Deref for TraitRef {
    type Target = Rc<RefCell<Trait>>;

    fn deref(&self) -> &Self::Target {
        &self.trait_
    }
}
