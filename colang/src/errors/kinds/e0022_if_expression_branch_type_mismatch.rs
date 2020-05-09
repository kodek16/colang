use crate::errors::CompilationError;
use crate::program::Expression;
use crate::source::SourceOrigin;

pub fn if_expression_branch_type_mismatch(
    then: &Expression,
    else_: &Expression,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0022",
        format!(
            "`if` expression branches must have same type, but are different: `{}` and `{}`",
            then.type_().borrow().name,
            else_.type_().borrow().name,
        ),
    )
    .with_location(location)
    .with_subtitle("branches have different types")
    .maybe_with_type_explanation(then)
    .maybe_with_type_explanation(else_)
}
