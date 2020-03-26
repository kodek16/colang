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
    // `symbols` should always be Some, Option is for interior mutability.
    symbols: Option<HashMap<String, Symbol>>,

    // `parent` can be None for root scope.
    parent: Option<Box<Scope>>,
}

impl Scope {
    /// Creates a new, root scope.
    pub fn new(program: &Program) -> Scope {
        let mut scope = Scope {
            symbols: Some(HashMap::new()),
            parent: None,
        };
        scope.add_type(program.int()).unwrap();
        scope.add_type(program.bool()).unwrap();
        scope
    }

    /// Creates a new "scope frame", pushing the existing frame below it.
    /// Symbols in the upper frames can shadow symbols with same names in lower frames.
    pub fn push(&mut self) {
        self.parent = Some(Box::new(Scope {
            symbols: self.symbols.take(),
            parent: self.parent.take(),
        }));
        self.symbols = Some(HashMap::new());
    }

    /// Restores previous "scope frame". Has to called after `push`.
    pub fn pop(&mut self) {
        let parent = self
            .parent
            .as_deref_mut()
            .expect("Attempted to pop root scope.");
        self.symbols = parent.symbols.take();
        self.parent = parent.parent.take();
    }

    #[must_use]
    pub fn add_variable(
        &mut self,
        variable: &Rc<RefCell<Variable>>,
    ) -> Result<(), CompilationError> {
        let (name, definition_site) = {
            let variable = variable.borrow();
            (variable.name.to_string(), variable.definition_site)
        };

        let existing = self.lookup_self(&name);
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
                self.symbols_mut()
                    .insert(name.to_string(), Symbol::Variable(Rc::clone(variable)));
                Ok(())
            }
        }
    }

    #[must_use]
    pub fn add_type(&mut self, type_: &Rc<RefCell<Type>>) -> Result<(), CompilationError> {
        let name = type_.borrow().name();

        if self.contains_self(&name) {
            // TODO produce actual errors when type collisions become possible (user types).
            panic!("Type name collision");
        }

        self.symbols_mut()
            .insert(name.to_string(), Symbol::Type(Rc::clone(type_)));
        Ok(())
    }

    pub fn lookup_variable(
        &self,
        name: &str,
        reference_location: InputSpan,
    ) -> Result<&Rc<RefCell<Variable>>, CompilationError> {
        match self.lookup(name) {
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
        match self.lookup(name) {
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

    /// Convenience method for mutator access.
    fn symbols_mut(&mut self) -> &mut HashMap<String, Symbol> {
        self.symbols.as_mut().unwrap()
    }

    fn contains_self(&self, name: &str) -> bool {
        self.symbols.as_ref().unwrap().contains_key(name)
    }

    fn lookup_self(&self, name: &str) -> Option<&Symbol> {
        self.symbols.as_ref().unwrap().get(name)
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols
            .as_ref()
            .unwrap()
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.lookup(name)))
    }
}
