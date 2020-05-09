use crate::ast;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn self_not_in_method_signature(
    function_def: &ast::FunctionDef,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0034",
        format!(
            "`self` can only appear as a method parameter, but `{}` is a function",
            function_def.name.text
        ),
    )
    .with_location(location)
    .with_subtitle("`self` not allowed here")
}
