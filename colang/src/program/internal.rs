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
    AsciiCode,
    AsciiChar,

    AddInt,
    SubInt,
    MulInt,
    LessInt,
    GreaterInt,
    LessEqInt,
    GreaterEqInt,
    EqInt,
    NotEqInt,
    ReadInt,
    IntToString,

    ReadWord,
    StringIndex,

    ArrayPush(TypeId),
    ArrayPop(TypeId),
    ArrayLen(TypeId),
    ArrayIndex(TypeId),
}

pub fn populate_internal_symbols(
    program: &mut Program,
    scope: &mut Scope,
    type_scopes: &mut HashMap<TypeId, Scope>,
) {
    let visible_functions = vec![
        create_assert_function(program.types()),
        create_ascii_code_function(program.types()),
        create_ascii_char_function(program.types()),
    ];

    let invisible_functions = vec![
        create_add_int_function(program.types()),
        create_sub_int_function(program.types()),
        create_mul_int_function(program.types()),
        create_less_int_function(program.types()),
        create_greater_int_function(program.types()),
        create_less_eq_int_function(program.types()),
        create_greater_eq_int_function(program.types()),
        create_eq_int_function(program.types()),
        create_not_eq_int_function(program.types()),
        create_read_int_function(program.types_mut()),
        create_int_to_string_function(program.types()),
        create_read_word_function(program.types_mut()),
    ];

    let string_methods = vec![create_string_index_method(program.types_mut())];

    // Convert to multi-owned.
    let visible_functions: Vec<_> = visible_functions
        .into_iter()
        .map(|f| Rc::new(RefCell::new(f)))
        .collect();
    let invisible_functions: Vec<_> = invisible_functions
        .into_iter()
        .map(|f| Rc::new(RefCell::new(f)))
        .collect();
    let string_methods: Vec<_> = string_methods
        .into_iter()
        .map(|f| Rc::new(RefCell::new(f)))
        .collect();

    for function in visible_functions
        .iter()
        .chain(invisible_functions.iter())
        .chain(string_methods.iter())
    {
        program.add_function(Rc::clone(&function));
    }

    for function in visible_functions {
        scope.add_function(Rc::clone(&function)).expect(&format!(
            "Couldn't register internal function `{}`",
            function.borrow().name()
        ));
    }

    for method in string_methods {
        type_scopes
            .entry(TypeId::String)
            .or_insert_with(Scope::new_for_type)
            .add_function(Rc::clone(&method))
            .expect(&format!(
                "Couldn't register internal method `string::{}`",
                method.borrow().name()
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

fn create_ascii_code_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "ascii_code".to_string(),
        InternalFunctionTag::AsciiCode,
        vec![internal_param("ch", types.char())],
        Rc::clone(types.int()),
    )
}

fn create_ascii_char_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "ascii_char".to_string(),
        InternalFunctionTag::AsciiChar,
        vec![internal_param("code", types.int())],
        Rc::clone(types.char()),
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

fn create_read_int_function(types: &mut TypeRegistry) -> Function {
    let int = Rc::clone(types.int());
    let pointer_to_int = types.pointer_to(&int.borrow());

    InternalFunction::new(
        "<read>".to_string(),
        InternalFunctionTag::ReadInt,
        vec![internal_param("target", &pointer_to_int)],
        Rc::clone(types.void()),
    )
}

fn create_int_to_string_function(types: &TypeRegistry) -> Function {
    InternalFunction::new(
        "<int as string>".to_string(),
        InternalFunctionTag::IntToString,
        vec![internal_param("value", types.int())],
        Rc::clone(types.string()),
    )
}

fn create_read_word_function(types: &mut TypeRegistry) -> Function {
    let string = Rc::clone(types.string());
    let pointer_to_string = types.pointer_to(&string.borrow());

    InternalFunction::new(
        "<read-word>".to_string(),
        InternalFunctionTag::ReadWord,
        vec![internal_param("target", &pointer_to_string)],
        Rc::clone(types.void()),
    )
}

fn create_string_index_method(types: &mut TypeRegistry) -> Function {
    let char = Rc::clone(types.char());
    let pointer_to_char = types.pointer_to(&char.borrow());

    InternalFunction::new(
        "index".to_string(),
        InternalFunctionTag::StringIndex,
        vec![
            internal_param("self", types.string()),
            internal_param("index", types.int()),
        ],
        pointer_to_char,
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

pub fn create_array_index_method(
    element_type: &Rc<RefCell<Type>>,
    types: &mut TypeRegistry,
) -> Function {
    let array_type = types.array_of(&element_type.borrow());
    let pointer_to_element_type = types.pointer_to(&element_type.borrow());

    InternalFunction::new(
        "index".to_string(),
        InternalFunctionTag::ArrayIndex(element_type.borrow().type_id().clone()),
        vec![
            internal_param("self", &array_type),
            internal_param("index", types.int()),
        ],
        pointer_to_element_type,
    )
}

fn internal_param(name: &str, type_: &Rc<RefCell<Type>>) -> InternalParameter {
    InternalParameter {
        name: name.to_string(),
        type_: Rc::clone(type_),
    }
}
