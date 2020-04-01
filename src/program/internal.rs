//! Internal symbols follow the same rules (interface) as user-defined
//! symbols, but they have no source-backed definition, and they are
//! instead treated by backends in special ways.

use crate::program::{Function, InternalFunction, InternalParameter, Program};
use crate::scope::Scope;
use crate::typing::TypeRegistry;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub enum InternalFunctionTag {
    Assert,
}

pub fn populate_internal_symbols(program: &mut Program, scope: &mut Scope) {
    let assert = create_assert_function(program.types());
    let assert = Rc::new(RefCell::new(assert));
    program.add_function(Rc::clone(&assert));
    scope.add_function(Rc::clone(&assert)).expect(&format!(
        "Couldn't register internal function `{}`",
        assert.borrow().name()
    ));
}

fn create_assert_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "assert".to_string(),
        InternalFunctionTag::Assert,
        vec![InternalParameter {
            name: "fact".to_string(),
            type_: Rc::clone(types.bool()),
        }],
        Rc::clone(types.void()),
    )
}
