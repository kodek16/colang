use crate::errors::CompilationError;
use crate::program::TypeTemplate;
use crate::source::SourceOrigin;

pub fn wrong_number_of_type_template_arguments(
    template: &TypeTemplate,
    actual_num: usize,
    location: SourceOrigin,
) -> CompilationError {
    // TODO(#2) use more precise argument and parameter spans.
    CompilationError::new(
        "E0027",
        format!(
            "type template `{}` requires {} type arguments, not {} as given",
            template.name,
            template.type_parameters.len(),
            actual_num
        ),
    )
    .with_location(location)
    .with_subtitle(if actual_num > template.type_parameters.len() {
        "too many type arguments"
    } else {
        "too few type arguments"
    })
    .maybe_with_bound_note(template.definition_site, || {
        format!(
            "type template `{}` defined with {} type parameters",
            template.name,
            template.type_parameters.len()
        )
    })
}
