//! Internal symbols follow the same rules (interface) as user-defined
//! symbols, but they have no source-backed definition, and they are
//! instead treated by backends in special ways.

use crate::program::function::ProtoInternalParameter;
use crate::program::{Function, Program, Type, TypeId, TypeRegistry};
use crate::scope::{FreeScope, FunctionEntity};
use std::cell::RefCell;
use std::rc::Rc;

/// A unique identifier of an internal function.
///
/// Internal functions have to be treated specially by backends. This identifier helps backend
/// understand which function exactly they need to handle.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum InternalFunctionTag {
    Assert,
    AsciiCode,
    AsciiChar,

    AddInt,
    SubInt,
    MulInt,
    DivInt,
    ModInt,
    LessInt,
    GreaterInt,
    LessEqInt,
    GreaterEqInt,
    EqInt,
    NotEqInt,
    IntToString,

    StringAdd,
    StringIndex,
    StringEq,
    StringNotEq,

    ArrayPush(TypeId),
    ArrayPop(TypeId),
    ArrayLen(TypeId),
    ArrayIndex(TypeId),
}

pub fn populate_internal_symbols(program: &mut Program, scope: &mut FreeScope) {
    let visible_functions = vec![
        create_assert_function(program.types()),
        create_ascii_code_function(program.types()),
        create_ascii_char_function(program.types()),
    ];

    let invisible_functions = vec![
        create_add_int_function(program.types()),
        create_sub_int_function(program.types()),
        create_mul_int_function(program.types()),
        create_div_int_function(program.types()),
        create_mod_int_function(program.types()),
        create_less_int_function(program.types()),
        create_greater_int_function(program.types()),
        create_less_eq_int_function(program.types()),
        create_greater_eq_int_function(program.types()),
        create_eq_int_function(program.types()),
        create_not_eq_int_function(program.types()),
        create_int_to_string_function(program.types()),
        // TODO(#6) convert all internal operators to methods. This would involve changing the
        // lookup logic.
        create_string_add_method(program.types()),
        create_string_eq_method(program.types()),
        create_string_not_eq_method(program.types()),
    ];

    let array_methods = create_array_methods(program.types_mut());

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

    for function in visible_functions.iter().chain(invisible_functions.iter()) {
        program.add_function(Rc::clone(&function));
    }

    let array = Rc::clone(&program.types.array().borrow().base_type());
    {
        let mut array = array.borrow_mut();
        for method in array_methods.iter() {
            program.add_function(Rc::clone(&method));
            array.add_method(Rc::clone(&method)).unwrap();
        }
    }

    let mut string = program.types.string().borrow_mut();
    for method in string_methods.iter() {
        string.add_method(Rc::clone(&method)).unwrap();
    }

    for function in visible_functions {
        scope.add(FunctionEntity(Rc::clone(&function))).unwrap();
    }
}

fn create_assert_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "assert".to_string(),
        InternalFunctionTag::Assert,
        vec![internal_param("fact", types.bool())],
        Rc::clone(types.void()),
    )
}

fn create_ascii_code_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "ascii_code".to_string(),
        InternalFunctionTag::AsciiCode,
        vec![internal_param("ch", types.char())],
        Rc::clone(types.int()),
    )
}

fn create_ascii_char_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "ascii_char".to_string(),
        InternalFunctionTag::AsciiChar,
        vec![internal_param("code", types.int())],
        Rc::clone(types.char()),
    )
}

fn create_add_int_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
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
    Function::new_internal(
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
    Function::new_internal(
        "(*)".to_string(),
        InternalFunctionTag::MulInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.int()),
    )
}

fn create_div_int_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "(/)".to_string(),
        InternalFunctionTag::DivInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.int()),
    )
}

fn create_mod_int_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "(%)".to_string(),
        InternalFunctionTag::ModInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.int()),
    )
}

fn create_less_int_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
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
    Function::new_internal(
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
    Function::new_internal(
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
    Function::new_internal(
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
    Function::new_internal(
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
    Function::new_internal(
        "(!=)".to_string(),
        InternalFunctionTag::NotEqInt,
        vec![
            internal_param("lhs", types.int()),
            internal_param("rhs", types.int()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_int_to_string_function(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "<int as string>".to_string(),
        InternalFunctionTag::IntToString,
        vec![internal_param("value", types.int())],
        Rc::clone(types.string()),
    )
}

fn create_string_add_method(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "(+)".to_string(),
        InternalFunctionTag::StringAdd,
        vec![
            internal_param("self", types.string()),
            internal_param("other", types.string()),
        ],
        Rc::clone(types.string()),
    )
}

fn create_string_index_method(types: &mut TypeRegistry) -> Function {
    let char = Rc::clone(types.char());
    let pointer_to_char = types.pointer_to(&char);

    Function::new_internal(
        "index".to_string(),
        InternalFunctionTag::StringIndex,
        vec![
            internal_param("self", types.string()),
            internal_param("index", types.int()),
        ],
        pointer_to_char,
    )
}

fn create_string_eq_method(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "(==)".to_string(),
        InternalFunctionTag::StringEq,
        vec![
            internal_param("self", types.string()),
            internal_param("other", types.string()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_string_not_eq_method(types: &TypeRegistry) -> Function {
    Function::new_internal(
        "(!=)".to_string(),
        InternalFunctionTag::StringNotEq,
        vec![
            internal_param("self", types.string()),
            internal_param("other", types.string()),
        ],
        Rc::clone(types.bool()),
    )
}

fn create_array_methods(types: &mut TypeRegistry) -> Vec<Rc<RefCell<Function>>> {
    let array = Rc::clone(&types.array());
    let array = array.borrow();

    let type_parameter = Rc::clone(&array.type_parameters[0]);
    let array_type = Rc::clone(&array.base_type());
    let pointer_to_array_type = Rc::clone(&types.pointer_to(&array_type));
    let pointer_to_type_parameter = Rc::clone(&types.pointer_to(&type_parameter));

    let methods = vec![
        Function::new_internal(
            "push".to_string(),
            InternalFunctionTag::ArrayPush(type_parameter.borrow().type_id.clone()),
            vec![
                internal_param("self", &pointer_to_array_type),
                internal_param("element", &type_parameter),
            ],
            Rc::clone(types.void()),
        ),
        Function::new_internal(
            "pop".to_string(),
            InternalFunctionTag::ArrayPop(type_parameter.borrow().type_id.clone()),
            vec![internal_param("self", &pointer_to_array_type)],
            Rc::clone(&type_parameter),
        ),
        Function::new_internal(
            "len".to_string(),
            InternalFunctionTag::ArrayLen(type_parameter.borrow().type_id.clone()),
            vec![internal_param("self", &array_type)],
            Rc::clone(types.int()),
        ),
        Function::new_internal(
            "index".to_string(),
            InternalFunctionTag::ArrayIndex(type_parameter.borrow().type_id.clone()),
            vec![
                internal_param("self", &array_type),
                internal_param("index", types.int()),
            ],
            pointer_to_type_parameter,
        ),
    ];

    methods
        .into_iter()
        .map(|m| Rc::new(RefCell::new(m)))
        .collect()
}

fn internal_param(name: &str, type_: &Rc<RefCell<Type>>) -> ProtoInternalParameter {
    ProtoInternalParameter {
        name: name.to_string(),
        type_: Rc::clone(type_),
    }
}
