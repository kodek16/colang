//! CO types and their properties are defined in this module.

mod references;
mod registry;
mod templates;
mod traits;
mod types;

pub use references::TraitRef;
pub use registry::{TypeCycleThroughFields, TypeRegistry};
pub use templates::{ProtoTypeParameter, TypeTemplate, TypeTemplateId};
pub use traits::{Trait, TraitId};
pub use types::{Type, TypeId, TypeInstantiationData};
