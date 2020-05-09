use crate::errors::CompilationError;
use crate::scope::GeneralNamedEntity;
use crate::source::SourceOrigin;

pub fn named_entity_already_defined<G: GeneralNamedEntity>(
    name: &str,
    existing: &G,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0004",
        format!(
            "{} with name `{}` is already defined in this scope",
            existing.kind().text_with_indefinite_article(),
            name
        ),
    )
    .with_location(location)
    .with_subtitle(format!("attempted to redefine `{}` here", name))
    .maybe_with_bound_note(existing.definition_site(), || {
        format!("`{}` previously defined here", name)
    })
}
