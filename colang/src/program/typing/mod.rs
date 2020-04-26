//! CO types and their properties are defined in this module.

mod registry;
mod templates;
mod types;

pub use registry::TypeRegistry;
pub use templates::{ProtoTypeParameter, TypeTemplate, TypeTemplateId};
pub use types::{Type, TypeId};
