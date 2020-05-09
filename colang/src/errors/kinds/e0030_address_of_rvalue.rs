use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn address_of_rvalue(location: SourceOrigin) -> CompilationError {
    CompilationError::new(
        "E0030",
        "cannot take address of an rvalue, only lvalues can be addressed",
    )
    .with_location(location)
    .with_subtitle("expression is an rvalue, but only lvalues have addresses")
}
