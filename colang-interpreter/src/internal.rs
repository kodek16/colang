//! Internal symbols implementation.

use crate::errors::RuntimeError;
use crate::{Lvalue, RunResult, Rvalue, Value};
use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

pub fn assert(mut arguments: Vec<Value>) -> RunResult<Value> {
    let value = arguments.remove(0);

    let value = value.into_rvalue().as_bool();
    if !value {
        return Err(RuntimeError::new("Assertion failed", None));
    }
    Ok(Value::Rvalue(Rvalue::Void))
}

pub fn ascii_code(mut arguments: Vec<Value>) -> RunResult<Value> {
    let char = arguments.pop().unwrap().into_rvalue().as_char();
    Ok(Value::Rvalue(Rvalue::Int(char as i32)))
}

pub fn ascii_char(mut arguments: Vec<Value>) -> RunResult<Value> {
    let code = arguments.pop().unwrap().into_rvalue().as_int();
    let result = u8::try_from(code);
    match result {
        Ok(char) => Ok(Value::Rvalue(Rvalue::Char(char))),
        Err(_) => Err(RuntimeError::new(
            format!("\"{}\" is not a valid ASCII code", code),
            None,
        )),
    }
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

pub fn div_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    if rhs != 0 {
        Ok(Value::Rvalue(Rvalue::Int(lhs / rhs)))
    } else {
        Err(RuntimeError::new("Division by zero", None))
    }
}

pub fn mod_int(mut arguments: Vec<Value>) -> RunResult<Value> {
    let rhs = arguments.pop().unwrap().into_rvalue().as_int();
    let lhs = arguments.pop().unwrap().into_rvalue().as_int();
    if rhs != 0 {
        Ok(Value::Rvalue(Rvalue::Int(lhs % rhs)))
    } else {
        Err(RuntimeError::new("Division by zero", None))
    }
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

pub fn int_to_string(mut arguments: Vec<Value>) -> RunResult<Value> {
    let value = arguments.pop().unwrap().into_rvalue().as_int();
    let result = format!("{}", value);
    Ok(Value::Rvalue(Rvalue::new_string(&result)))
}

pub fn array_concat(mut arguments: Vec<Value>) -> RunResult<Value> {
    let other = arguments.pop().unwrap().into_rvalue().into_array();
    let self_ = arguments.pop().unwrap().into_rvalue().into_array();

    let other = other.borrow();
    let self_ = self_.borrow();

    let result: Vec<_> = self_
        .iter()
        .chain(other.iter())
        .map(Lvalue::clone_contents)
        .collect();
    Ok(Value::Rvalue(Rvalue::Array(Rc::new(RefCell::new(result)))))
}

pub fn string_eq(mut arguments: Vec<Value>) -> RunResult<Value> {
    let other = arguments.pop().unwrap().into_rvalue().into_array();
    let self_ = arguments.pop().unwrap().into_rvalue().into_array();

    let other = other.borrow();
    let self_ = self_.borrow();

    let result = if self_.len() == other.len() {
        self_
            .iter()
            .zip(other.iter())
            .all(|(x, y)| x.borrow().as_char() == y.borrow().as_char())
    } else {
        false
    };
    Ok(Value::Rvalue(Rvalue::Bool(result)))
}

pub fn string_not_eq(arguments: Vec<Value>) -> RunResult<Value> {
    if let Value::Rvalue(Rvalue::Bool(result)) = string_eq(arguments)? {
        Ok(Value::Rvalue(Rvalue::Bool(!result)))
    } else {
        panic!("error in `string_eq` internal method");
    }
}

pub fn array_push(mut arguments: Vec<Value>) -> RunResult<Value> {
    let element = arguments.pop().unwrap().into_rvalue();
    let array_pointer = arguments
        .pop()
        .unwrap()
        .into_rvalue()
        .into_pointer_to_self(None)?;

    array_pointer
        .detach()
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
        .into_pointer_to_self(None)?;
    let result = array_pointer.detach().into_array().borrow_mut().pop();
    match result {
        Some(result) => {
            let rvalue = result.detach();
            Ok(Value::Rvalue(rvalue))
        }
        None => Err(RuntimeError::new("Cannot pop from empty array", None)),
    }
}

pub fn array_len(mut arguments: Vec<Value>) -> RunResult<Value> {
    let array = arguments.pop().unwrap().into_rvalue().into_array();
    let result = array.borrow().len();
    Ok(Value::Rvalue(Rvalue::Int(result as i32)))
}

pub fn array_index(mut arguments: Vec<Value>) -> RunResult<Value> {
    let index = arguments.pop().unwrap().into_rvalue().as_int();
    let array = arguments.pop().unwrap().into_rvalue().into_array();
    let array = array.borrow();

    if index < 0 || index >= array.len() as i32 {
        let error = format!(
            "Array index out of bounds: array size is {}, index is {}",
            array.len(),
            index
        );
        return Err(RuntimeError::new(error, None));
    }

    Ok(Value::Rvalue(Rvalue::Pointer(Some(
        array[index as usize].clone(),
    ))))
}
