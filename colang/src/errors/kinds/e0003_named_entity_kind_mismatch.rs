use crate::errors::CompilationError;
use crate::scope::{GeneralNamedEntity, NamedEntityKind};
use crate::source::SourceOrigin;

pub fn named_entity_kind_mismatch<G: GeneralNamedEntity>(
    name: &str,
    expected: NamedEntityKind,
    actual: &G,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0003",
        format!(
            "expected `{}` to be {}, but it is {}",
            name,
            expected.text_with_indefinite_article(),
            actual.kind().text_with_indefinite_article(),
        ),
    )
    .with_location(location)
    .with_subtitle(format!(
        "`{}` is used here in {} context",
        name,
        expected.text_with_indefinite_article()
    ))
    .maybe_with_bound_note(actual.definition_site(), || {
        format!(
            "`{}` is defined here as {}",
            actual.name(),
            actual.kind().text_with_indefinite_article(),
        )
    })
}
