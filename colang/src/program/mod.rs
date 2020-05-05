//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

mod display;
mod expressions;
mod field;
pub(crate) mod function;
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

    types: TypeRegistry,

    /// All user-defined functions and methods in the program.
    user_functions: Vec<Rc<RefCell<Function>>>,

    /// All internal functions and internal template method instantiations in the program.
    internal_functions: HashMap<InternalFunctionTag, Rc<RefCell<Function>>>,

    /// All types in the program in the reverse topological order according to their fields.
    /// See `TypeRegistry::all_types_sorted`.
    /// This field is filled in one of the last passes of the analyzer.
    pub(crate) sorted_types: Option<Vec<Rc<RefCell<Type>>>>,

    /// The `main` function that is the entrypoint of the program.
    pub(crate) main_function: Option<Rc<RefCell<Function>>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub(crate) fn new() -> Program {
        Program {
            symbol_ids: SymbolIdRegistry::new(),
            user_functions: vec![],
            types: TypeRegistry::new(),
            sorted_types: None,
            internal_functions: HashMap::new(),
            main_function: None,
        }
    }

    /// Adds (registers) a new function to the program. This method is idempotent: calling it
    /// for an already registered function is guaranteed to produce no effect.
    pub fn add_function(&mut self, function: Rc<RefCell<Function>>) {
        match function.borrow().id.clone() {
            FunctionId::Internal(tag) => {
                self.internal_functions
                    .insert(tag.clone(), Rc::clone(&function));
            }
            _ => {
                // TODO convert 'user_functions` to a HashMap to improve performance of this check.
                if !self.user_functions.contains(&function) {
                    self.user_functions.push(Rc::clone(&function))
                }
            }
        }
    }

    /// Unregisters (hides) a previously registered function. By doing this, the
    /// function will not appear in a call to `all_user_functions`, but there may still be
    /// references to it in the bodies of other functions. If any such references exist in the
    /// preserved functions, the program should be considered invalid.
    ///
    /// If `function` was not previously registers, this operation has no effect.
    pub fn remove_function(&mut self, function: &Function) {
        // TODO convert `user_functions` to a HashMap to enable efficient removals.
        match function.id {
            FunctionId::Internal(ref tag) => {
                self.internal_functions.remove(tag);
            }
            _ => {
                self.user_functions.retain(|f| *f.borrow() != *function);
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

    /// All user-defined functions in the program, except unused template method instantiations.
    pub fn all_user_functions(&self) -> impl Iterator<Item = &Rc<RefCell<Function>>> {
        self.user_functions.iter()
    }

    pub fn sorted_types(&self) -> impl Iterator<Item = &Rc<RefCell<Type>>> {
        self.sorted_types
            .as_ref()
            .expect("types have not been sorted")
            .iter()
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

pub use field::{Field, FieldId};
pub use function::{Function, FunctionId};
pub use internal::InternalFunctionTag;
pub use typing::{
    ProtoTypeParameter, Type, TypeCycleThroughFields, TypeId, TypeInstantiationData, TypeRegistry,
    TypeTemplate, TypeTemplateId,
};
pub use variable::{Variable, VariableId};

pub use instructions::assign::AssignInstruction;
pub use instructions::eval::EvalInstruction;
pub use instructions::read::ReadInstruction;
pub use instructions::return_::ReturnInstruction;
pub use instructions::while_::WhileInstruction;
pub use instructions::write::WriteInstruction;
pub use instructions::Instruction;
