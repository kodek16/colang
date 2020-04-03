//! Internal symbols implementation.

use crate::{Lvalue, RunResult, Rvalue, Value};

pub fn assert(mut arguments: Vec<Value>) -> RunResult<Value> {
    let value = arguments.remove(0);

    let value = value.into_rvalue().as_bool();
    if !value {
        let error = "Assertion failed";
        return Err(error.into());
    }
    Ok(Value::Rvalue(Rvalue::Void))
}

pub fn add_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Int(lhs + rhs)))
}

pub fn sub_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Int(lhs - rhs)))
}

pub fn mul_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Int(lhs * rhs)))
}

pub fn less_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Bool(lhs < rhs)))
}

pub fn greater_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Bool(lhs > rhs)))
}

pub fn less_eq_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Bool(lhs <= rhs)))
}

pub fn greater_eq_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Bool(lhs >= rhs)))
}

pub fn eq_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Bool(lhs == rhs)))
}

pub fn not_eq_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    Ok(Value::Rvalue(Rvalue::Bool(lhs != rhs)))
}

pub fn int_abs(mut arguments: Vec<Value>) -> RunResult<Value> {
    let x = arguments.pop().unwrap().into_rvalue().as_int();
    let result = if x >= 0 { x } else { -x };
    Ok(Value::Rvalue(Rvalue::Int(result)))
}

pub fn array_push(mut arguments: Vec<Value>) -> RunResult<Value> {
    let element = arguments.pop().unwrap().into_rvalue();
    let array_pointer = arguments
        .pop()
        .unwrap()
        .into_rvalue()
        .into_pointer_to_self()?;

    array_pointer
        .0
        .borrow()
        .clone()
        .into_array()
        .borrow_mut()
        .push(Lvalue::store(element));
    Ok(Value::Rvalue(Rvalue::Void))
}

pub fn array_pop(mut arguments: Vec<Value>) -> RunResult<Value> {
    let array_pointer = arguments
        .pop()
        .unwrap()
        .into_rvalue()
        .into_pointer_to_self()?;
    let result = array_pointer
        .0
        .borrow()
        .clone()
        .into_array()
        .borrow_mut()
        .pop();
    match result {
        Some(result) => {
            let rvalue = result.0.borrow().clone();
            Ok(Value::Rvalue(rvalue))
        }
        None => {
            let error = "Cannot pop from empty array";
            Err(error.into())
        }
    }
}

pub fn array_len(mut arguments: Vec<Value>) -> RunResult<Value> {
    let array = arguments.pop().unwrap().into_rvalue().into_array();
    let result = array.borrow().len();
    Ok(Value::Rvalue(Rvalue::Int(result as i32)))
}
