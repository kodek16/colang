use crate::ast;
use crate::errors::CompilationError;
use crate::program::Trait;
use crate::source::SourceOrigin;

pub fn method_with_body_in_trait(
    method_def: &ast::FunctionDef,
    trait_: &Trait,
) -> CompilationError {
    CompilationError::new("E0051", "trait methods cannot be defined with bodies")
        .with_location(SourceOrigin::Plain(
            method_def.body.as_ref().unwrap().span(),
        ))
        .with_subtitle(format!(
            "method `{}` in trait `{}` defined with a body",
            method_def.name.text, trait_.name
        ))
        .maybe_with_bound_note(trait_.definition_site, || {
            format!("trait `{}` defined here", trait_.name)
        })
}
