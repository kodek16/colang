use crate::errors::CompilationError;
use crate::program::{Function, TraitRef, Type};

pub fn trait_method_not_implemented(
    type_: &Type,
    trait_: &TraitRef,
    trait_method: &Function,
) -> CompilationError {
    CompilationError::new(
        "E0054",
        format!(
            "type `{}` is missing implementation of method `{}` from trait `{}`",
            type_.name,
            trait_method.name,
            trait_.borrow().name
        ),
    )
    .with_location(trait_.reference_location())
    .with_subtitle(format!(
        "trait `{}` requires method `{}` which is not implemented",
        trait_.borrow().name,
        trait_method.name
    ))
}
