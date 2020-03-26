//! Symbol (named entity) visibility hierarchy management.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::InputSpan;
use crate::errors::{CompilationError, Word};
use crate::program::{Program, Variable};
use crate::typing::Type;

/// A named entity in the program.
enum Symbol {
    Variable(Rc<RefCell<Variable>>),
    Type(Rc<RefCell<Type>>),
}

impl Symbol {
    fn word(&self) -> Word {
        match self {
            Symbol::Variable(_) => Word::Variable,
            Symbol::Type(_) => Word::Type,
        }
    }
}

pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

impl Scope {
    /// Creates a new, root scope.
    pub fn new(program: &Program) -> Scope {
        let mut scope = Scope {
            symbols: HashMap::new(),
        };
        scope.add_type(program.int()).unwrap();
        scope.add_type(program.bool()).unwrap();
        scope
    }

    #[must_use]
    pub fn add_variable(
        &mut self,
        variable: &Rc<RefCell<Variable>>,
    ) -> Result<(), CompilationError> {
        let (name, definition_site) = {
            let variable = (**variable).borrow();
            (variable.name.to_string(), variable.definition_site)
        };

        let existing = self.symbols.get(&name);
        match existing {
            Some(existing) => {
                let error = CompilationError::symbol_already_exists(
                    &name,
                    existing.word(),
                    definition_site.expect("Name collision for internal variable"),
                );
                Err(error)
            }
            None => {
                self.symbols
                    .insert(name.to_string(), Symbol::Variable(Rc::clone(variable)));
                Ok(())
            }
        }
    }

    #[must_use]
    pub fn add_type(&mut self, type_: &Rc<RefCell<Type>>) -> Result<(), CompilationError> {
        let name = (**type_).borrow().name();

        if self.symbols.contains_key(&name) {
            // TODO produce actual errors when type collisions become possible (user types).
            panic!("Type name collision");
        }

        self.symbols
            .insert(name.to_string(), Symbol::Type(Rc::clone(type_)));
        Ok(())
    }

    pub fn lookup_variable(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Variable>>, CompilationError> {
        match self.symbols.get(name) {
            Some(Symbol::Variable(variable)) => Ok(&variable),
            Some(other) => {
                let error = CompilationError::symbol_kind_mismatch(
                    name,
                    Word::Variable,
                    other.word(),
                    reference_location,
                );
                Err(error)
            }
            None => {
                let error =
                    CompilationError::symbol_not_found(name, Word::Variable, reference_location);
                Err(error)
            }
        }
    }

    pub fn lookup_type(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Type>>, CompilationError> {
        match self.symbols.get(name) {
            Some(Symbol::Type(type_)) => Ok(type_),
            Some(other) => {
                let error = CompilationError::symbol_kind_mismatch(
                    name,
                    Word::Type,
                    other.word(),
                    reference_location,
                );
                Err(error)
            }
            None => {
                let error =
                    CompilationError::symbol_not_found(name, Word::Type, reference_location);
                Err(error)
            }
        }
    }
}
