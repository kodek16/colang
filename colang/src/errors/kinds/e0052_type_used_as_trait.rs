use crate::ast;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn type_used_as_trait(type_expr: &ast::TypeExpr) -> CompilationError {
    CompilationError::new("E0052", "type cannot be used as a trait")
        .with_location(SourceOrigin::Plain(type_expr.span()))
        .with_subtitle("expected a trait, not a type")
}
