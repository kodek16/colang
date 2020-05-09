use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn array_elements_type_mismatch(
    first_element: &Expression,
    wrong_type_element: &Expression,
) -> CompilationError {
    let element_type = wrong_type_element.type_().borrow();
    let inferred_type = first_element.type_().borrow();

    CompilationError::new(
        "E0025",
        format!(
            "array element has type `{}`, but it must have the same type `{}` as other elements",
            element_type.name, inferred_type.name
        ),
    )
    .with_location(wrong_type_element.location())
    .with_subtitle(format!("element has type `{}`", element_type.name))
    .maybe_with_type_explanation(wrong_type_element)
    .with_bound_note(
        first_element.location(),
        format!(
            "array type was inferred from the first element as `{}`",
            inferred_type.name
        ),
    )
    .maybe_with_type_explanation(first_element)
}
