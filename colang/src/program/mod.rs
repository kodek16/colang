//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

mod display;
mod expressions;
mod function;
mod instructions;
pub(crate) mod internal;
pub mod transforms;
mod typing;
mod variable;

use std::cell::RefCell;
use std::collections::HashMap;
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

    // All user-defined functions and methods in the program.
    user_functions: Vec<Rc<RefCell<Function>>>,
    types: TypeRegistry,

    // This collection only contains functions, not methods. Internal methods
    // should be accessed through the type scope mechanism.
    internal_functions: HashMap<InternalFunctionTag, Rc<RefCell<Function>>>,

    main_function: Option<Rc<RefCell<Function>>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub(crate) fn new() -> Program {
        Program {
            symbol_ids: SymbolIdRegistry::new(),
            user_functions: vec![],
            types: TypeRegistry::new(),
            internal_functions: HashMap::new(),
            main_function: None,
        }
    }

    /// Adds a new function to the program.
    pub(crate) fn add_function(&mut self, function: Rc<RefCell<Function>>) {
        match function.borrow().id.clone() {
            FunctionId::Internal(tag) => {
                self.internal_functions
                    .insert(tag.clone(), Rc::clone(&function));
            }
            _ => self.user_functions.push(Rc::clone(&function)),
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

    /// All user-defined functions in the program, except unused template method instantiations.
    pub fn all_user_functions(&self) -> impl Iterator<Item = &Rc<RefCell<Function>>> {
        self.user_functions.iter()
    }

    pub fn main_function(&self) -> Rc<RefCell<Function>> {
        Rc::clone(
            self.main_function
                .as_ref()
                .expect("`main` function has not been specified"),
        )
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
pub use expressions::boolean_op::{BooleanOp, BooleanOpExpr};
pub use expressions::call::CallExpr;
pub use expressions::deref::DerefExpr;
pub use expressions::field_access::FieldAccessExpr;
pub use expressions::if_::IfExpr;
pub use expressions::is::IsExpr;
pub use expressions::literal::LiteralExpr;
pub use expressions::new::NewExpr;
pub use expressions::null::NullExpr;
pub use expressions::variable::VariableExpr;
pub use expressions::{Expression, ExpressionKind};

pub use function::{Function, FunctionBody, FunctionId};
pub use internal::InternalFunctionTag;
pub use typing::{ProtoTypeParameter, Type, TypeId, TypeRegistry, TypeTemplate, TypeTemplateId};
pub use variable::{Variable, VariableId};

pub use instructions::assign::AssignInstruction;
pub use instructions::eval::EvalInstruction;
pub use instructions::while_::WhileInstruction;
pub use instructions::write::WriteInstruction;
pub use instructions::return_::ReturnInstruction;
pub use instructions::Instruction;
