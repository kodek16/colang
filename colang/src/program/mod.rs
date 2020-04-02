//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

mod checks;
mod display;
mod expressions;
mod function;
mod instructions;
mod internal;
mod typing;
mod variable;

use private::SymbolImpl;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

/// Every distinct named entity in the program receives a unique ID.
pub type SymbolId = u32;

/// Public trait for all symbols in the program that have a unique ID.
pub trait Symbol {
    fn id(&self) -> SymbolId;
}

#[derive(Debug)]
pub struct Program {
    variables: Vec<Rc<RefCell<Variable>>>,
    user_functions: Vec<Rc<RefCell<Function>>>,
    next_symbol_id: SymbolId,

    types: TypeRegistry,
    internal_functions: HashMap<InternalFunctionTag, Rc<RefCell<Function>>>,

    main_function: Option<Rc<RefCell<Function>>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub(crate) fn new() -> Program {
        Program {
            variables: vec![],
            user_functions: vec![],
            next_symbol_id: 0,
            types: TypeRegistry::new(),
            internal_functions: HashMap::new(),
            main_function: None,
        }
    }

    /// Adds a new variable to the symbol table.
    pub(crate) fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        variable.borrow_mut().set_id(self.next_symbol_id);
        self.next_symbol_id += 1;
        self.variables.push(variable);
    }

    /// Adds a new function to the program. If the function is user-defined,
    /// it is assigned a symbol id.
    pub(crate) fn add_function(&mut self, function: Rc<RefCell<Function>>) {
        let mut function_mut = function.borrow_mut();
        match *function_mut {
            Function::UserDefined(ref mut function_mut) => {
                function_mut.set_id(self.next_symbol_id);
                self.next_symbol_id += 1;
                self.user_functions.push(Rc::clone(&function));
            }
            Function::Internal(ref function_ref) => {
                self.internal_functions
                    .insert(function_ref.tag, Rc::clone(&function));
            }
        }
    }

    /// Mark a function as the "main" function that the program should start
    /// executing from.
    /// Frontend is guaranteed to do this for a valid program.
    pub(crate) fn fill_main_function(&mut self, main_function: Rc<RefCell<Function>>) {
        self.main_function = Some(main_function)
    }

    pub(crate) fn types_mut(&mut self) -> &mut TypeRegistry {
        &mut self.types
    }

    pub(crate) fn internal_function(&self, tag: InternalFunctionTag) -> &Rc<RefCell<Function>> {
        &self.internal_functions[&tag]
    }

    pub fn types(&self) -> &TypeRegistry {
        &self.types
    }

    pub fn main_function(&self) -> impl Deref<Target = Function> + '_ {
        self.main_function
            .as_ref()
            .expect("`main` function has not been specified")
            .borrow()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ValueCategory {
    Lvalue,
    Rvalue,
}

pub use expressions::address::AddressExpr;
pub use expressions::array_from_copy::ArrayFromCopyExpr;
pub use expressions::array_from_elements::ArrayFromElementsExpr;
pub use expressions::block::{BlockBuilder, BlockExpr};
pub use expressions::call::CallExpr;
pub use expressions::deref::DerefExpr;
pub use expressions::if_::IfExpr;
pub use expressions::index::IndexExpr;
pub use expressions::literal::LiteralExpr;
pub use expressions::variable::VariableExpr;
pub use expressions::{Expression, ExpressionKind};

pub use function::{Function, InternalFunction, InternalParameter, Parameter, UserDefinedFunction};
pub use internal::InternalFunctionTag;
pub use typing::{Type, TypeKind, TypeRegistry};
pub use variable::Variable;

pub use instructions::alloc::AllocInstruction;
pub use instructions::assign::AssignInstruction;
pub use instructions::dealloc::DeallocInstruction;
pub use instructions::eval::EvalInstruction;
pub use instructions::read::ReadInstruction;
pub use instructions::return_::ReturnInstruction;
pub use instructions::while_::WhileInstruction;
pub use instructions::write::WriteInstruction;
pub use instructions::Instruction;

pub(crate) use internal::populate_internal_symbols;

mod private {
    use super::{Symbol, SymbolId, Variable};
    use crate::program::UserDefinedFunction;

    /// Private trait that provides the default behavior for initializing symbol IDs.
    pub trait SymbolImpl: Symbol {
        fn id_option(&self) -> &Option<SymbolId>;
        fn id_option_mut(&mut self) -> &mut Option<SymbolId>;

        fn set_id(&mut self, new_id: SymbolId) {
            *self.id_option_mut() = Some(new_id);
        }
    }

    impl<T: SymbolImpl> Symbol for T {
        fn id(&self) -> SymbolId {
            self.id_option()
                .expect("Attempted to access ID of unbound symbol.")
        }
    }

    impl SymbolImpl for Variable {
        fn id_option(&self) -> &Option<SymbolId> {
            &self.id
        }
        fn id_option_mut(&mut self) -> &mut Option<SymbolId> {
            &mut self.id
        }
    }

    impl SymbolImpl for UserDefinedFunction {
        fn id_option(&self) -> &Option<SymbolId> {
            &self.id
        }
        fn id_option_mut(&mut self) -> &mut Option<SymbolId> {
            &mut self.id
        }
    }
}
