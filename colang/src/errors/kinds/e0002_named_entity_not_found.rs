use crate::errors::CompilationError;
use crate::scope::NamedEntityKind;
use crate::source::SourceOrigin;

pub fn named_entity_not_found(
    name: &str,
    kind: NamedEntityKind,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0002",
        format!(
            "no {} with name `{}` could be found in the current scope",
            kind.text(),
            name
        ),
    )
    .with_location(location)
    .with_subtitle(format!("refers to an unknown {}", kind.text()))
}
