use crate::errors::CompilationError;
use crate::program::function::Signature;
use crate::program::{Function, Trait};

pub fn trait_method_signature_mismatch(
    trait_: &Trait,
    method_implementation: &Function,
    actual_signature: &Signature,
    expected_signature: &Signature,
) -> CompilationError {
    CompilationError::new(
        "E0053",
        format!(
            "method `{}` does not match expected signature from trait `{}`",
            method_implementation.name, trait_.name
        ),
    )
    .with_location(
        method_implementation
            .definition_site
            .expect("trait method signature mismatch in internal method"),
    )
    .with_subtitle("method signature is different from the signature defined in trait")
    .with_free_note(format!(
        "expected signature: {}\nactual signature:   {}",
        expected_signature, actual_signature
    ))
}
