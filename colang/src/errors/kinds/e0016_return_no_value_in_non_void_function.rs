use crate::errors::CompilationError;
use crate::program::Function;
use crate::source::SourceOrigin;

pub fn return_no_value_in_non_void_function(
    function: &Function,
    location: SourceOrigin,
) -> CompilationError {
    let return_type = function.return_type.borrow();

    CompilationError::new(
        "E0016",
        format!(
            "`return` must specify a return value of type `{}` in function `{}`",
            return_type.name, function.name
        ),
    )
    .with_location(location)
    .with_subtitle(format!("expected a value of type `{}`", return_type.name))
    .with_bound_note(
        function.definition_site.unwrap(),
        format!(
            "function `{}` defined with return type `{}`",
            function.name, return_type.name
        ),
    )
}
