//! Interpreter backend for CO can run the code right after it is compiled.

mod cin;
mod errors;
mod internal;
mod values;

use crate::errors::RuntimeError;
use crate::values::{Lvalue, Rvalue, Value};
use cin::Cin;
use colang::backends::Backend;
use colang::program::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct InterpreterBackend;

impl Backend for InterpreterBackend {
    fn run(&self, file_name: &str, source: &str, program: Program) -> Result<(), ()> {
        let mut state = State::new();

        let main = program.main_function();
        let result = run_user_function(&main.borrow(), &mut state);
        match result {
            Ok(_) => Ok(()),
            Err(EarlyExit::EarlyReturn(_)) => Ok(()),
            Err(EarlyExit::Error(error)) => {
                error.print_backtrace(file_name, source, main);
                Err(())
            }
        }
    }
}

type RunResult<T> = Result<T, EarlyExit>;

pub enum EarlyExit {
    EarlyReturn(Value),
    Error(RuntimeError),
}

pub struct State {
    variables: HashMap<VariableId, Vec<Lvalue>>,
    cin: Cin,
}

impl State {
    fn new() -> State {
        State {
            variables: HashMap::new(),
            cin: Cin::new(),
        }
    }

    fn push(&mut self, variable_id: VariableId, value: Rvalue) {
        self.variables
            .entry(variable_id)
            .or_default()
            .push(Lvalue::store(value))
    }

    fn pop(&mut self, variable_id: VariableId) {
        self.variables
            .get_mut(&variable_id)
            .expect("variable deallocated before allocation")
            .pop()
            .expect("variable deallocated twice");
    }

    fn get(&self, variable_id: VariableId) -> Value {
        Value::Lvalue(
            self.variables
                .get(&variable_id)
                .expect("variable accessed before allocation")
                .last()
                .expect("variable accessed after deallocation")
                .clone(),
        )
    }
}

fn run_user_function(function: &Function, state: &mut State) -> RunResult<Value> {
    let body = function.body();
    let body = body.borrow();
    run_expression(&body, state)
}

fn run_internal_function(
    function_tag: &InternalFunctionTag,
    arguments: Vec<Value>,
    state: &mut State,
) -> RunResult<Value> {
    use InternalFunctionTag::*;
    match function_tag {
        Assert => internal::assert(arguments),
        AsciiCode => internal::ascii_code(arguments),
        AsciiChar => internal::ascii_char(arguments),
        AddInt => internal::add_int(arguments),
        SubInt => internal::sub_int(arguments),
        MulInt => internal::mul_int(arguments),
        DivInt => internal::div_int(arguments),
        ModInt => internal::mod_int(arguments),
        LessInt => internal::less_int(arguments),
        GreaterInt => internal::greater_int(arguments),
        LessEqInt => internal::less_eq_int(arguments),
        GreaterEqInt => internal::greater_eq_int(arguments),
        EqInt => internal::eq_int(arguments),
        NotEqInt => internal::not_eq_int(arguments),
        ReadInt => internal::read_int(arguments, state),
        IntToString => internal::int_to_string(arguments),
        ReadWord => internal::read_word(arguments, state),
        StringAdd => internal::array_concat(arguments),
        StringIndex => internal::array_index(arguments),
        StringEq => internal::string_eq(arguments),
        StringNotEq => internal::string_not_eq(arguments),
        ArrayPush(_) => internal::array_push(arguments),
        ArrayPop(_) => internal::array_pop(arguments),
        ArrayLen(_) => internal::array_len(arguments),
        ArrayIndex(_) => internal::array_index(arguments),
    }
}

fn run_instruction(instruction: &Instruction, state: &mut State) -> RunResult<()> {
    match instruction {
        Instruction::Write(ref s) => run_write(s, state),
        Instruction::While(ref s) => run_while(s, state),
        Instruction::Assign(ref s) => run_assign(s, state),
        Instruction::Eval(ref s) => run_eval(s, state),
        Instruction::Return(ref s) => run_return(s, state),
    }
}

fn run_write(instruction: &WriteInstruction, state: &mut State) -> RunResult<()> {
    let value = run_expression(&instruction.expression, state)?.into_rvalue();
    print!("{}", value.as_utf8_string());
    Ok(())
}

fn run_while(instruction: &WhileInstruction, state: &mut State) -> RunResult<()> {
    let mut cond = run_expression(&instruction.cond, state)?
        .into_rvalue()
        .as_bool();
    while cond {
        run_instruction(&instruction.body, state)?;
        cond = run_expression(&instruction.cond, state)?
            .into_rvalue()
            .as_bool();
    }
    Ok(())
}

fn run_assign(instruction: &AssignInstruction, state: &mut State) -> RunResult<()> {
    let target = run_expression(&instruction.target, state)?.into_lvalue();
    let new_value = run_expression(&instruction.value, state)?.into_rvalue();
    *target.borrow_mut() = new_value;
    Ok(())
}

fn run_eval(instruction: &EvalInstruction, state: &mut State) -> RunResult<()> {
    let _ = run_expression(&instruction.expression, state)?;
    Ok(())
}

fn run_return(instruction: &ReturnInstruction, state: &mut State) -> RunResult<()> {
    let value = run_expression(&instruction.expression, state)?;
    Err(EarlyExit::EarlyReturn(value))
}

fn run_expression(expression: &Expression, state: &mut State) -> RunResult<Value> {
    match expression.kind() {
        ExpressionKind::Variable(e) => run_variable_expr(e, state),
        ExpressionKind::Literal(e) => run_literal_expr(e, state),
        ExpressionKind::Address(e) => run_address_expr(e, state),
        ExpressionKind::Deref(e) => run_deref_expr(e, state),
        ExpressionKind::New(e) => run_new_expr(e, state),
        ExpressionKind::Is(e) => run_is_expr(e, state),
        ExpressionKind::Null(e) => run_null_expr(e, state),
        ExpressionKind::ArrayFromElements(e) => run_array_from_elements_expr(e, state),
        ExpressionKind::ArrayFromCopy(e) => run_array_from_copy_expr(e, state),
        ExpressionKind::BooleanOp(e) => run_boolean_op_expr(e, state),
        ExpressionKind::Call(e) => run_call_expr(e, state),
        ExpressionKind::FieldAccess(e) => run_field_access_expr(e, state),
        ExpressionKind::If(e) => run_if_expr(e, state),
        ExpressionKind::Block(e) => run_block_expr(e, state),
        ExpressionKind::Empty(_) => Ok(Value::Rvalue(Rvalue::Void)),
        ExpressionKind::Error(_) => panic_error(),
    }
}

fn run_variable_expr(expression: &VariableExpr, state: &State) -> RunResult<Value> {
    let variable_id = expression.variable.borrow().id.clone();
    let result = state.get(variable_id).clone();
    Ok(result)
}

fn run_literal_expr(expression: &LiteralExpr, _: &State) -> RunResult<Value> {
    let value = match expression {
        LiteralExpr::Int(value, _) => Value::Rvalue(Rvalue::Int(*value)),
        LiteralExpr::Bool(value, _) => Value::Rvalue(Rvalue::Bool(*value)),
        LiteralExpr::Char(value, _) => Value::Rvalue(Rvalue::Char(*value)),
        LiteralExpr::String(value, _) => Value::Rvalue(Rvalue::new_string(value)),
    };
    Ok(value)
}

fn run_address_expr(expression: &AddressExpr, state: &mut State) -> RunResult<Value> {
    let lvalue = run_expression(&expression.target, state)?.into_lvalue();
    Ok(Value::Rvalue(Rvalue::Pointer(Some(lvalue))))
}

fn run_deref_expr(expression: &DerefExpr, state: &mut State) -> RunResult<Value> {
    let lvalue = run_expression(&expression.pointer, state)?
        .into_rvalue()
        .into_pointer_unwrap(Some(expression.pointer.location()))?;
    Ok(Value::Lvalue(lvalue))
}

fn run_new_expr(expression: &NewExpr, _: &mut State) -> RunResult<Value> {
    let target = default_value_for_type(&expression.target_type.borrow());
    Ok(Value::Rvalue(Rvalue::Pointer(Some(Lvalue::store(target)))))
}

fn run_is_expr(expression: &IsExpr, state: &mut State) -> RunResult<Value> {
    let lhs = run_expression(&expression.lhs, state)?
        .into_rvalue()
        .into_pointer();
    let rhs = run_expression(&expression.rhs, state)?
        .into_rvalue()
        .into_pointer();

    let result = match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => lhs.is_same(&rhs),
        (None, None) => true,
        _ => false,
    };
    Ok(Value::Rvalue(Rvalue::Bool(result)))
}

fn run_null_expr(_: &NullExpr, _: &mut State) -> RunResult<Value> {
    Ok(Value::Rvalue(Rvalue::Pointer(None)))
}

fn run_array_from_elements_expr(
    expression: &ArrayFromElementsExpr,
    state: &mut State,
) -> RunResult<Value> {
    let elements: RunResult<Vec<Value>> = expression
        .elements
        .iter()
        .map(|element| run_expression(element, state))
        .collect();
    let elements: Vec<Lvalue> = elements?
        .into_iter()
        .map(|value| Lvalue::store(value.into_rvalue()))
        .collect();

    Ok(Value::Rvalue(Rvalue::Array(Rc::new(RefCell::new(
        elements,
    )))))
}

fn run_array_from_copy_expr(expression: &ArrayFromCopyExpr, state: &mut State) -> RunResult<Value> {
    let element = run_expression(&expression.element, state)?.into_rvalue();
    let size = run_expression(&expression.size, state)?
        .into_rvalue()
        .as_int();

    if size <= 0 {
        return Err(RuntimeError::new(
            format!("Attempted to create array of non-positive size: {}", size),
            Some(expression.size.location()),
        ));
    }
    let size = size as usize;

    let array = vec![element; size];
    let array = array.into_iter().map(Lvalue::store).collect();

    Ok(Value::Rvalue(Rvalue::Array(Rc::new(RefCell::new(array)))))
}

fn run_boolean_op_expr(expression: &BooleanOpExpr, state: &mut State) -> RunResult<Value> {
    // Boolean expressions short-circuit as they should.
    let value = match expression.op {
        BooleanOp::And(ref lhs, ref rhs) => {
            run_expression(lhs, state)?.into_rvalue().as_bool()
                && run_expression(rhs, state)?.into_rvalue().as_bool()
        }
        BooleanOp::Or(ref lhs, ref rhs) => {
            run_expression(lhs, state)?.into_rvalue().as_bool()
                || run_expression(rhs, state)?.into_rvalue().as_bool()
        }
        BooleanOp::Not(ref operand) => !run_expression(operand, state)?.into_rvalue().as_bool(),
    };

    Ok(Value::Rvalue(Rvalue::Bool(value)))
}

fn run_call_expr(expression: &CallExpr, state: &mut State) -> RunResult<Value> {
    let function = expression.function.borrow();

    let arguments: RunResult<Vec<Value>> = expression
        .arguments
        .iter()
        .map(|argument| run_expression(argument, state))
        .collect();
    let arguments = arguments?;

    // Currently we make a copy of all arguments for every call, so that they could be later
    // used for error reporting in case an error occurs. Need to benchmark how big of an impact
    // this makes on performance, and consider switching to a faster unwinding model like GDB.
    let arguments_for_backtrace = arguments.clone();

    let result = match function.id {
        FunctionId::Internal(ref tag) => run_internal_function(tag, arguments, state),
        _ => {
            let parameters = function.parameters.iter();

            for (parameter, value) in parameters.zip(arguments.into_iter()) {
                let variable_id = parameter.borrow().id.clone();
                state.push(variable_id, value.into_rvalue())
            }

            let function_result = run_user_function(&expression.function.borrow(), state);

            for parameter in function.parameters.iter() {
                let variable_id = parameter.borrow().id.clone();
                state.pop(variable_id)
            }

            function_result
        }
    };

    match result {
        Ok(value) => Ok(value),
        Err(EarlyExit::EarlyReturn(value)) => Ok(value),
        Err(EarlyExit::Error(error)) => Err(EarlyExit::Error(
            error.annotate_stack_frame(expression, arguments_for_backtrace),
        )),
    }
}

fn run_field_access_expr(expression: &FieldAccessExpr, state: &mut State) -> RunResult<Value> {
    let receiver = run_expression(&expression.receiver, state)?;
    let field_id = expression.field.borrow().id.clone();
    let result = match receiver {
        Value::Lvalue(lvalue) => Value::Lvalue(lvalue.borrow().as_struct()[&field_id].clone()),
        Value::Rvalue(rvalue) => Value::Rvalue(rvalue.as_struct()[&field_id].detach()),
    };
    Ok(result)
}

fn run_if_expr(expression: &IfExpr, state: &mut State) -> RunResult<Value> {
    let cond = run_expression(&expression.cond, state)?
        .into_rvalue()
        .as_bool();
    if cond {
        run_expression(&expression.then, state)
    } else {
        run_expression(&expression.else_, state)
    }
}

fn run_block_expr(block: &BlockExpr, state: &mut State) -> RunResult<Value> {
    for variable in block.local_variables.iter() {
        let variable_id = variable.borrow().id.clone();
        let initial_value = default_value_for_type(&variable.borrow().type_.borrow());
        state.push(variable_id, initial_value);
    }

    for instruction in block.instructions.iter() {
        run_instruction(instruction, state)?;
    }

    let value = run_expression(&block.value, state)?;

    for variable in block.local_variables.iter() {
        let variable_id = variable.borrow().id.clone();
        state.pop(variable_id);
    }

    Ok(value)
}

fn default_value_for_type(type_: &Type) -> Rvalue {
    match type_.type_id {
        TypeId::Void => panic!("Tried to default-initialize a value of type `void`"),
        TypeId::Int => Rvalue::Int(0),
        TypeId::Bool => Rvalue::Bool(false),
        TypeId::Char => Rvalue::Char(0),
        TypeId::String => {
            // There is no distinction between strings and (char) arrays in the interpreter.
            Rvalue::Array(Rc::new(RefCell::new(vec![])))
        }
        TypeId::TemplateInstance(TypeTemplateId::Array, _) => {
            Rvalue::Array(Rc::new(RefCell::new(vec![])))
        }
        TypeId::TemplateInstance(TypeTemplateId::Pointer, _) => Rvalue::Pointer(None),
        TypeId::TemplateInstance(TypeTemplateId::Struct(_), _) => default_value_for_struct(type_),
        TypeId::Struct(_) => default_value_for_struct(type_),
        TypeId::TypeParameter(_, _) => panic!("Type parameter encountered in a compiled program."),
        TypeId::Error => panic_error(),
    }
}

fn default_value_for_struct(type_: &Type) -> Rvalue {
    let fields = type_
        .fields()
        .map(|field| {
            let field = field.borrow();
            let value = default_value_for_type(&field.type_.borrow());
            (field.id.clone(), Lvalue::store(value))
        })
        .collect();
    Rvalue::Struct(fields)
}

fn panic_error() -> ! {
    panic!("Error-containing program passed to interpreter backend.")
}

pub fn panic_wrong_type(expected_type: &str, actual_type: &str) -> ! {
    panic!(
        "Runtime type mismatch: expected `{}`, got `{}`",
        expected_type, actual_type
    )
}
