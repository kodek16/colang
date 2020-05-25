use crate::ast;
use crate::errors::CompilationError;
use crate::program::Function;
use crate::source::SourceOrigin;

pub fn conflicting_method_from_trait_bounds(
    type_parameter_def: &ast::TypeParameter,
    first_method: &Function,
    second_method: &Function,
) -> CompilationError {
    CompilationError::new(
        "E0055",
        format!(
            "conflicting method definitions implied by trait bounds for `{}`",
            type_parameter_def.name.text
        ),
    )
    .with_location(SourceOrigin::Plain(type_parameter_def.span))
    .with_subtitle(format!(
        "cannot infer the signature of method `{}`",
        first_method.name
    ))
    .with_free_note(format!(
        "at least the following differing signatures are implied:
  {}
  {}
        ",
        first_method.signature(),
        second_method.signature()
    ))
}
