use crate::errors::CompilationError;
use crate::program::{Trait, Type, TypeTemplate};
use crate::source::SourceOrigin;

pub fn type_argument_violates_trait_bound(
    type_argument: &Type,
    template: &TypeTemplate,
    type_parameter: &Type,
    violated_bound: &Trait,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0056",
        format!(
            "type `{}` does not satisfy bound `{}: {}` of template `{}`",
            type_argument.name, type_parameter.name, violated_bound.name, template.name
        ),
    )
    .with_location(location)
    .with_subtitle(format!(
        "does not satisfy the required bound `{}`",
        violated_bound.name
    ))
}
