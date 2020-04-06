//! Internal symbols follow the same rules (interface) as user-defined
//! symbols, but they have no source-backed definition, and they are
//! instead treated by backends in special ways.

use crate::program::{
    Function, InternalFunction, InternalParameter, Program, Type, TypeId, TypeRegistry,
};
use crate::scope::Scope;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
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

    ArrayPush(TypeId),
    ArrayPop(TypeId),
    ArrayLen(TypeId),
}

pub fn populate_internal_symbols(
    program: &mut Program,
    scope: &mut Scope,
    _type_scopes: &mut HashMap<TypeId, Scope>,
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

pub fn create_array_push_method(
    element_type: &Rc<RefCell<Type>>,
    types: &mut TypeRegistry,
) -> Function {
    let array_type = types.array_of(&element_type.borrow());
    let pointer_to_array_type = types.pointer_to(&array_type.borrow());

    InternalFunction::new(
        "push".to_string(),
        InternalFunctionTag::ArrayPush(element_type.borrow().type_id().clone()),
        vec![
            internal_param("self", &pointer_to_array_type),
            internal_param("element", element_type),
        ],
        Rc::clone(types.void()),
    )
}

pub fn create_array_pop_method(
    element_type: &Rc<RefCell<Type>>,
    types: &mut TypeRegistry,
) -> Function {
    let array_type = types.array_of(&element_type.borrow());
    let pointer_to_array_type = types.pointer_to(&array_type.borrow());

    InternalFunction::new(
        "pop".to_string(),
        InternalFunctionTag::ArrayPop(element_type.borrow().type_id().clone()),
        vec![internal_param("self", &pointer_to_array_type)],
        Rc::clone(element_type),
    )
}

pub fn create_array_len_method(
    element_type: &Rc<RefCell<Type>>,
    types: &mut TypeRegistry,
) -> Function {
    let array_type = types.array_of(&element_type.borrow());

    InternalFunction::new(
        "len".to_string(),
        InternalFunctionTag::ArrayLen(element_type.borrow().type_id().clone()),
        vec![internal_param("self", &array_type)],
        Rc::clone(types.int()),
    )
}

fn internal_param(name: &str, type_: &Rc<RefCell<Type>>) -> InternalParameter {
    InternalParameter {
        name: name.to_string(),
        type_: Rc::clone(type_),
    }
}
