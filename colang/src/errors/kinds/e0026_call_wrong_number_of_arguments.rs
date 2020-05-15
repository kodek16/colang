use crate::errors::CompilationError;
use crate::program::Function;
use crate::source::SourceOrigin;

pub fn call_wrong_number_of_arguments(
    function: &Function,
    actual_num: usize,
    location: SourceOrigin,
) -> CompilationError {
    // TODO(#2) use more precise argument and parameter spans.
    CompilationError::new(
        "E0026",
        format!(
            "function `{}` expects {} argument(s), not {} as given",
            function.name,
            function.parameters.len(),
            actual_num
        ),
    )
    .with_location(location)
    .with_subtitle(if actual_num > function.parameters.len() {
        "too many arguments"
    } else {
        "too few arguments"
    })
    .maybe_with_bound_note(function.definition_site, || {
        format!(
            "function `{}` defined with {} parameter(s)",
            function.name,
            function.parameters.len()
        )
    })
}
