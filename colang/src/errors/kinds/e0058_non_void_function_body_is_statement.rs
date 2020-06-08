use crate::errors::CompilationError;
use crate::program::StatementKind;
use crate::program::{Function, Statement};

pub fn non_void_function_body_is_statement(
    function: &Function,
    body: &Statement,
) -> CompilationError {
    CompilationError::new(
        "E0058",
        format!(
            "body of the function `{}` with return type `{}` does not specify a return value",
            function.name,
            function.return_type.borrow().name
        ),
    )
    .with_location(body.location())
    // TODO (#3): rework explain_type_... to work with statements
    .with_free_note("consider adding a `return` statement or a terminating expression")
}
