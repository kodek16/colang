//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

mod checks;
mod display;
mod expressions;
mod function;
mod instructions;
pub(crate) mod internal;
mod typing;
mod variable;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

/// Every distinct named entity in the program receives a unique ID.
pub type SymbolId = u32;

/// A trait for all symbols in the program that have a unique ID.
pub trait Symbol {
    fn id(&self) -> SymbolId;
}

pub struct SymbolIdRegistry {
    next_id: SymbolId,
}

impl SymbolIdRegistry {
    fn new() -> SymbolIdRegistry {
        SymbolIdRegistry { next_id: 1 }
    }

    fn next_id(&mut self) -> SymbolId {
        let result = self.next_id;
        self.next_id += 1;
        result
    }
}

pub struct Program {
    symbol_ids: SymbolIdRegistry,

    variables: Vec<Rc<RefCell<Variable>>>,
    user_functions: Vec<Rc<RefCell<Function>>>,

    // Somewhat confusingly, this collection only contains functions, not methods. Internal
    // methods should be accessed through the type scope mechanism.
    internal_functions: HashMap<InternalFunctionTag, Rc<RefCell<Function>>>,
    types: TypeRegistry,

    main_function: Option<Rc<RefCell<Function>>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub(crate) fn new() -> Program {
        Program {
            symbol_ids: SymbolIdRegistry::new(),
            variables: vec![],
            user_functions: vec![],
            internal_functions: HashMap::new(),
            types: TypeRegistry::new(),
            main_function: None,
        }
    }

    /// Adds a new variable to the program.
    pub(crate) fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        self.variables.push(variable);
    }

    /// Adds a new function to the program.
    pub(crate) fn add_function(&mut self, function: Rc<RefCell<Function>>) {
        match *function.borrow() {
            Function::UserDefined(_) => {
                self.user_functions.push(Rc::clone(&function));
            }
            Function::Internal(ref internal_function) => {
                self.internal_functions
                    .insert(internal_function.tag.clone(), Rc::clone(&function));
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

    pub(crate) fn symbol_ids_mut(&mut self) -> &mut SymbolIdRegistry {
        &mut self.symbol_ids
    }

    pub(crate) fn internal_function(&self, tag: InternalFunctionTag) -> &Rc<RefCell<Function>> {
        &self.internal_functions.get(&tag).expect(&format!(
            "Couldn't find an internal function instance with tag {:?}",
            tag
        ))
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
pub use expressions::field_access::FieldAccessExpr;
pub use expressions::if_::IfExpr;
pub use expressions::literal::LiteralExpr;
pub use expressions::new::NewExpr;
pub use expressions::variable::VariableExpr;
pub use expressions::{Expression, ExpressionKind};

pub use function::{Function, InternalFunction, InternalParameter, Parameter, UserDefinedFunction};
pub use internal::InternalFunctionTag;
pub use typing::{Type, TypeId, TypeRegistry, TypeTemplate, TypeTemplateId};
pub use variable::Variable;

pub use instructions::alloc::AllocInstruction;
pub use instructions::assign::AssignInstruction;
pub use instructions::dealloc::DeallocInstruction;
pub use instructions::eval::EvalInstruction;
pub use instructions::return_::ReturnInstruction;
pub use instructions::while_::WhileInstruction;
pub use instructions::write::WriteInstruction;
pub use instructions::Instruction;
