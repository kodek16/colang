use crate::errors::CompilationError;
use crate::program::Function;
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub fn function_infinite_dependency_chain(
    source_function: &Function,
    function_chain: Vec<Rc<RefCell<Function>>>,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0046",
        format!(
            "call to function `{}` causes an infinite function dependency chain",
            source_function.name
        ),
    )
    .with_location(location)
    .with_subtitle("call causes an infinite function instantiation chain")
    .with_free_note({
        // TODO use qualified method names here
        let function_chain: Vec<_> = function_chain
            .iter()
            .take(8)
            .map(|function| function.borrow().name.clone())
            .collect();
        let function_chain: String = function_chain.join("\n -> ");
        format!(
            "Function dependency chain:\n    {}\n -> ...",
            function_chain
        )
    })
}
