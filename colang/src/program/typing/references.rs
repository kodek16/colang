//! Wrappers for types and related entities that preserve the location of reference.

use crate::program::Trait;
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

/// A wrapper for trait that preserves location in code where it was referenced.
pub struct TraitRef {
    trait_: Rc<RefCell<Trait>>,
    reference_span: SourceOrigin,
}

impl TraitRef {
    pub fn new(trait_: Rc<RefCell<Trait>>, reference_span: SourceOrigin) -> TraitRef {
        TraitRef {
            trait_,
            reference_span,
        }
    }

    pub fn reference_span(&self) -> SourceOrigin {
        self.reference_span
    }
}

impl Deref for TraitRef {
    type Target = Rc<RefCell<Trait>>;

    fn deref(&self) -> &Self::Target {
        &self.trait_
    }
}
