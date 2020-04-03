//! Internal symbols follow the same rules (interface) as user-defined
//! symbols, but they have no source-backed definition, and they are
//! instead treated by backends in special ways.

use crate::program::{
    Function, InternalFunction, InternalParameter, Program, Type, TypeKind, TypeRegistry,
};
use crate::scope::Scope;
use std::cell::RefCell;
use std::collections::HashMap;
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
    IntAbs,
}

pub fn populate_internal_symbols(
    program: &mut Program,
    scope: &mut Scope,
    type_scopes: &mut HashMap<TypeKind, Scope>,
) {
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

    let int_abs = Rc::new(RefCell::new(create_int_abs_method(program.types())));
    program.add_function(Rc::clone(&int_abs));
    type_scopes
        .entry(program.types.int().borrow().kind().clone())
        .or_insert_with(Scope::new)
        .add_function(int_abs)
        .expect("Couldn't register internal method.")
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

fn create_int_abs_method(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "abs".to_string(),
        InternalFunctionTag::IntAbs,
        vec![internal_param("x", types.int())],
        Rc::clone(types.int()),
    )
}

fn internal_param(name: &str, type_: &Rc<RefCell<Type>>) -> InternalParameter {
    InternalParameter {
        name: name.to_string(),
        type_: Rc::clone(type_),
    }
}
