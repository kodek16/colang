use crate::ast;
use crate::errors::CompilationError;
use crate::program::Trait;
use crate::source::SourceOrigin;

pub fn field_in_trait(field_def: &ast::FieldDef, trait_: &Trait) -> CompilationError {
    CompilationError::new("E0050", "cannot define fields in traits")
        .with_location(SourceOrigin::Plain(field_def.span))
        .with_subtitle(format!(
            "field `{}` defined in trait `{}`",
            field_def.name.text, trait_.name
        ))
        .maybe_with_bound_note(trait_.definition_site, || {
            format!("trait `{}` defined here", trait_.name)
        })
}
