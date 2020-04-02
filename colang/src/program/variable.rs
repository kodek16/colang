use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{SymbolId, Type, TypeRegistry};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub definition_site: Option<InputSpan>,
    pub(crate) id: Option<SymbolId>,
    pub(crate) type_: Rc<RefCell<Type>>,
}

impl Variable {
    /// Creates a new variable with a given name and type.
    /// Calling `id()` is invalid before the variable is added to a `Program`.
    pub fn new(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
        types: &TypeRegistry,
    ) -> Result<Variable, CompilationError> {
        if type_ == *types.void() {
            let error = CompilationError::variable_of_type_void(
                definition_site.expect("Internal variable of type `void` defined."),
            );
            return Err(error);
        }

        Ok(Variable {
            name,
            type_,
            definition_site,
            id: None,
        })
    }

    pub fn type_(&self) -> impl Deref<Target = Type> + '_ {
        self.type_.borrow()
    }
}
