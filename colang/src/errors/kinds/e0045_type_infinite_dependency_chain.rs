use crate::errors::CompilationError;
use crate::program::Type;
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub fn type_infinite_dependency_chain(
    source_type: &Type,
    type_chain: Vec<Rc<RefCell<Type>>>,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0045",
        format!(
            "type `{}` causes an infinite type dependency chain",
            source_type.name
        ),
    )
    .with_location(location)
    .with_subtitle("type reference causes an infinite type chain")
    .with_free_note({
        let type_chain: Vec<_> = type_chain
            .iter()
            .take(8)
            .map(|type_| type_.borrow().name.clone())
            .collect();
        let type_chain: String = type_chain.join("\n -> ");
        format!("Type dependency chain:\n    {}\n -> ...", type_chain)
    })
}
