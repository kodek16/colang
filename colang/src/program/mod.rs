//! Intermediate representation for CO programs.
//!
//! This is the interface between the frontend and various backends.

pub mod visitors;

pub(crate) mod expressions;
pub(crate) mod function;
pub(crate) mod internal;

mod display;
mod field;
mod instructions;
mod symbols;
mod typing;
mod values;
mod variable;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use symbols::SymbolIdRegistry;

/// The intermediate representation of a CO program.
///
/// This IR is constructed by the analyzer (a part of the compiler frontend), and passed to
/// a compiler backend for translation or execution.
///
/// A program IR can be thought of as a collection of function linked by function calls and types
/// linked by type references.
///
/// A `Program` is not necessarily valid: when performing error recovery throughout the analyzer,
/// invalid "nodes" are inserted in the program, such as `ErrorExpr`, or error type. Only the
/// `Program` returned on success from the `colang::compile` function is guaranteed to be valid.
pub struct Program {
    pub(crate) symbol_ids: SymbolIdRegistry,
    pub(crate) types: TypeRegistry,

    /// All user-defined functions and methods in the program.
    pub(crate) user_functions: HashMap<FunctionId, Rc<RefCell<Function>>>,

    /// All internal functions and internal template method instantiations in the program.
    pub(crate) internal_functions: HashMap<InternalFunctionTag, Rc<RefCell<Function>>>,

    /// All types in the program in the reverse topological order according to their fields.
    ///
    /// See `TypeRegistry::all_types_sorted`.
    ///
    /// Filled by the analyzer.
    pub(crate) sorted_types: Option<Vec<Rc<RefCell<Type>>>>,

    /// The `main` function that is the entry point of the program.
    ///
    /// Filled by the analyzer.
    pub(crate) main_function: Option<Rc<RefCell<Function>>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub fn new() -> Program {
        Program {
            symbol_ids: SymbolIdRegistry::new(),
            user_functions: HashMap::new(),
            types: TypeRegistry::new(),
            sorted_types: None,
            internal_functions: HashMap::new(),
            main_function: None,
        }
    }

    /// Adds (registers) a new function to the program.
    ///
    /// This method is idempotent: calling it for an already registered function is guaranteed
    /// to produce no effect.
    pub fn add_function(&mut self, function: Rc<RefCell<Function>>) {
        let id = function.borrow().id.clone();
        match id {
            FunctionId::Internal(tag) => {
                self.internal_functions
                    .entry(tag.clone())
                    .or_insert(function);
            }
            _ => {
                self.user_functions.entry(id).or_insert(function);
            }
        }
    }

    /// Unregisters (hides) a previously registered function.
    ///
    /// By doing this, the function will not appear in a call to `all_user_functions`, but
    /// there may still be references to it in the bodies of other functions. If any such references
    /// exist in the preserved functions, the program should be considered invalid.
    ///
    /// If `function` was not previously registered, this operation has no effect.
    pub fn remove_function(&mut self, function: &Function) {
        match function.id {
            FunctionId::Internal(ref tag) => {
                self.internal_functions.remove(tag);
            }
            _ => {
                self.user_functions.remove(&function.id);
            }
        }
    }

    /// Provides immutable access to the type registry.
    pub fn types(&self) -> &TypeRegistry {
        &self.types
    }

    /// Provides mutable access to the type registry.
    pub fn types_mut(&mut self) -> &mut TypeRegistry {
        &mut self.types
    }

    /// Iterates over all types in the program sorted in reverse topological order.
    ///
    /// More details about the exact ordering of types can be found in
    /// `TypeRegistry::all_types_sorted`. This method accesses a cached sorting result.
    pub fn sorted_types(&self) -> impl Iterator<Item = &Rc<RefCell<Type>>> {
        self.sorted_types
            .as_ref()
            .expect("types have not been sorted")
            .iter()
    }

    /// Iterates over all user-defined functions in the program.
    ///
    /// Unused template method instantiations are not included in the program IR.
    pub fn all_user_functions(&self) -> impl Iterator<Item = &Rc<RefCell<Function>>> {
        self.user_functions.values()
    }

    /// Accesses the `main` function: the entry point of the program.
    pub fn main_function(&self) -> Rc<RefCell<Function>> {
        Rc::clone(
            self.main_function
                .as_ref()
                .expect("`main` function has not been specified"),
        )
    }

    /// Provides mutable access to symbol ID registry.
    pub(crate) fn symbol_ids_mut(&mut self) -> &mut SymbolIdRegistry {
        &mut self.symbol_ids
    }

    /// Accesses an internal function directly, bypassing the scope mechanism.
    pub(crate) fn internal_function(&self, tag: InternalFunctionTag) -> &Rc<RefCell<Function>> {
        &self.internal_functions.get(&tag).expect(&format!(
            "Couldn't find an internal function instance with tag {:?}",
            tag
        ))
    }
}

pub use field::{Field, FieldId};
pub use function::{Function, FunctionId};
pub use internal::InternalFunctionTag;
pub use symbols::SymbolId;
pub use typing::{
    ProtoTypeParameter, Type, TypeCycleThroughFields, TypeId, TypeInstantiationData, TypeRegistry,
    TypeTemplate, TypeTemplateId,
};
pub use values::ValueCategory;
pub use variable::{Variable, VariableId};

pub use instructions::assign::AssignInstruction;
pub use instructions::eval::EvalInstruction;
pub use instructions::read::ReadInstruction;
pub use instructions::return_::ReturnInstruction;
pub use instructions::while_::WhileInstruction;
pub use instructions::write::WriteInstruction;
pub use instructions::{Instruction, InstructionKind};

pub use expressions::address::AddressExpr;
pub use expressions::array_from_copy::ArrayFromCopyExpr;
pub use expressions::array_from_elements::ArrayFromElementsExpr;
pub use expressions::block::BlockExpr;
pub use expressions::boolean_op::{BooleanOp, BooleanOpExpr};
pub use expressions::call::CallExpr;
pub use expressions::deref::DerefExpr;
pub use expressions::empty::EmptyExpr;
pub use expressions::error::ErrorExpr;
pub use expressions::field_access::FieldAccessExpr;
pub use expressions::if_::IfExpr;
pub use expressions::is::IsExpr;
pub use expressions::literal::{LiteralExpr, LiteralValue};
pub use expressions::new::NewExpr;
pub use expressions::null::NullExpr;
pub use expressions::variable::VariableExpr;
pub use expressions::{Expression, ExpressionImpl, ExpressionKind};
