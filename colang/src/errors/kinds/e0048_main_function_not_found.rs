use crate::errors::CompilationError;

pub fn main_function_not_found() -> CompilationError {
    CompilationError::new("E0048", "`main` function not found: you must define one")
}
