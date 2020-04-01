//! Internal symbols follow the same rules (interface) as user-defined
//! symbols, but they have no source-backed definition, and they are
//! instead treated by backends in special ways.

use crate::program::{Function, Program};

#[derive(Debug)]
pub enum InternalFunctionTag {
    Assert,
}

pub fn populate_internal_symbols(program: &mut Program) {}

fn create_assert_function() -> Function {
    unimplemented!()
}
