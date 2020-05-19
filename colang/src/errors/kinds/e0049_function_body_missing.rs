use crate::ast;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn function_body_missing(function_def: &ast::FunctionDef) -> CompilationError {
    CompilationError::new(
        "E0049",
        format!(
            "function `{}` must be defined with a body",
            function_def.name.text
        ),
    )
    .with_location(SourceOrigin::Plain(function_def.signature_span))
    .with_subtitle("function body is missing")
}
