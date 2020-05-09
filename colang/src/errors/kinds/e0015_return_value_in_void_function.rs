use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, Function};

pub fn return_value_in_void_function(
    function: &Function,
    expression: &Expression,
) -> CompilationError {
    CompilationError::new(
        "E0015",
        format!(
            "cannot return a value from function `{}` with no return type",
            function.name
        ),
    )
    .with_location(expression.location())
    .with_subtitle("unexpected value in `return`")
    .with_bound_note(
        function.definition_site.unwrap(),
        format!("function `{}` defined with no return type", function.name),
    )
}
