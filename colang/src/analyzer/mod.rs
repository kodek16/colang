//! Contains most of analysis phase routines.
//!
//! Analysis phase in the CO compiler handles transforming parsed AST into an intermediate
//! representation (`colang::program::Program`), making sure that it is valid, and reporting
//! any errors caused by the user.
//!
//! Analysis phase is done in multiple _passes_. During each pass, the compiler traverses the
//! program (either through syntax nodes, or through IR entities) and transforms the IR, bringing
//! it closer to the final result. Definitive list of all passes performed by the compiler can
//! be found in `colang::compile`.
//!
//! This module contains the _syntax-driven_ passes implementations. They update the IR by walking
//! through the AST. There are also passes later in the pipeline which do not use the AST directly
//! anymore, they are defined elsewhere.

pub mod basic_types;
pub mod bodies;
pub mod complete_types;
pub mod function_instantiations;
pub mod global_structure;
pub mod trait_wiring;
pub mod type_exprs;

mod trait_exprs;
mod visitor;

pub use visitor::GlobalVisitor;
