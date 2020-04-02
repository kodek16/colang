//! Internal symbols follow the same rules (interface) as user-defined
//! symbols, but they have no source-backed definition, and they are
//! instead treated by backends in special ways.

use crate::program::{Function, InternalFunction, InternalParameter, Program};
use crate::scope::Scope;
use crate::typing::{Type, TypeRegistry};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum InternalFunctionTag {
    Assert,
    AddInt,
    SubInt,
    MulInt,
    LessInt,
    GreaterInt,
    LessEqInt,
    GreaterEqInt,
    EqInt,
    NotEqInt,
}

pub fn populate_internal_symbols(program: &mut Program, scope: &mut Scope) {
    let functions = vec![
        create_assert_function(program.types()),
        create_add_int_function(program.types()),
        create_sub_int_function(program.types()),
        create_mul_int_function(program.types()),
        create_less_int_function(program.types()),
        create_greater_int_function(program.types()),
        create_less_eq_int_function(program.types()),
        create_greater_eq_int_function(program.types()),
        create_eq_int_function(program.types()),
        create_not_eq_int_function(program.types()),
    ];

    for function in functions {
        let function = Rc::new(RefCell::new(function));
        program.add_function(Rc::clone(&function));
        scope.add_function(Rc::clone(&function)).expect(&format!(
            "Couldn't register internal function `{}`",
            function.borrow().name()
        ));
    }
}

fn create_assert_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "assert".to_string(),
        InternalFunctionTag::Assert,
        vec![internal_param("fact", types.bool())],
        Rc::clone(types.void()),
    )
}

fn create_add_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(+)".to_string(),
        InternalFunctionTag::AddInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.int()),
    )
}

fn create_sub_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(-)".to_string(),
        InternalFunctionTag::SubInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.int()),
    )
}

fn create_mul_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(*)".to_string(),
        InternalFunctionTag::MulInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.int()),
    )
}

fn create_less_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(<)".to_string(),
        InternalFunctionTag::LessInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_greater_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(>)".to_string(),
        InternalFunctionTag::GreaterInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_less_eq_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(<=)".to_string(),
        InternalFunctionTag::LessEqInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_greater_eq_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(>=)".to_string(),
        InternalFunctionTag::GreaterEqInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_eq_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(==)".to_string(),
        InternalFunctionTag::EqInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_not_eq_int_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "(!=)".to_string(),
        InternalFunctionTag::NotEqInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.bool()),
    )
}

fn internal_param(name: &str, type_: &Rc<RefCell<Type>>) -> InternalParameter {
    InternalParameter {
        name: name.to_string(),
        type_: Rc::clone(type_),
    }
}
