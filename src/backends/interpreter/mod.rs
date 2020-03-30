//! Interpreter backend for CO can run the code right after it is compiled.

mod cin;

use super::Backend;
use crate::backends::interpreter::cin::Cin;
use crate::program::{
    AllocStmt, ArrayExpr, AssignStmt, BinaryOpExpr, BinaryOperator, BlockExpr, CallExpr,
    DeallocStmt, ExprStmt, Expression, ExpressionKind, Function, IfExpr, IndexExpr, LiteralExpr,
    Program, ReadStmt, Statement, Symbol, SymbolId, VariableExpr, WhileStmt, WriteStmt,
};
use crate::typing::{Type, TypeKind};
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::ops::Deref;
use std::process;
use std::rc::Rc;

pub struct InterpreterBackend;

impl Backend for InterpreterBackend {
    fn run(&self, program: Program) -> Result<(), Box<dyn Error>> {
        let mut state = State::new();

        let main = program.main_function();
        let result = run_function(main, &mut state);
        if let Err(err) = result {
            eprintln!("Error: {}", err);
            process::exit(1);
        }

        Ok(())
    }
}

/// Every value that exists in the program belongs to this type.
#[derive(Clone, Debug)]
enum Value {
    Int(i32),
    Bool(bool),
    Array(Rc<RefCell<Vec<Value>>>),
    Void,
}

impl Value {
    pub fn as_int(&self) -> i32 {
        match self {
            Value::Int(x) => *x,
            _ => panic_wrong_type("int", self.type_()),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic_wrong_type("bool", self.type_()),
        }
    }

    pub fn as_array(self) -> Rc<RefCell<Vec<Value>>> {
        match self {
            Value::Array(v) => v,
            _ => panic_wrong_type("array", self.type_()),
        }
    }

    /// A quick-and-dirty information source about value types in runtime.
    /// This is _not_ meant to be an actual RTTI solution, it is only meant
    /// to be used in type mismatch panics, which should not occur under
    /// normal circumstances.
    pub fn type_(&self) -> &'static str {
        use Value::*;
        match self {
            Int(_) => "int",
            Bool(_) => "bool",
            Array(_) => "array",
            Void => "void",
        }
    }
}

struct State {
    variables: HashMap<SymbolId, Vec<Value>>,
    cin: Cin,
}

impl State {
    fn new() -> State {
        State {
            variables: HashMap::new(),
            cin: Cin::new(),
        }
    }

    fn push(&mut self, variable_id: SymbolId, value: Value) {
        self.variables.entry(variable_id).or_default().push(value)
    }

    fn pop(&mut self, variable_id: SymbolId) {
        self.variables
            .get_mut(&variable_id)
            .expect("variable deallocated before allocation")
            .pop()
            .expect("variable deallocated twice");
    }

    fn get(&self, variable_id: SymbolId) -> &Value {
        self.variables
            .get(&variable_id)
            .expect("variable accessed before allocation")
            .last()
            .expect("variable accessed after deallocation")
    }

    fn update(&mut self, variable_id: SymbolId, new_value: Value) {
        let variable = self
            .variables
            .get_mut(&variable_id)
            .expect("variable accessed before allocation")
            .last_mut()
            .expect("variable accessed after deallocation");

        *variable = new_value;
    }
}

type RunResult<T> = Result<T, Box<dyn Error>>;

fn run_function(function: impl Deref<Target = Function>, state: &mut State) -> RunResult<Value> {
    let body = function.body();
    run_expression(body, state)
}

fn run_statement(statement: &Statement, state: &mut State) -> RunResult<()> {
    match statement {
        Statement::Alloc(ref s) => run_alloc(s, state),
        Statement::Dealloc(ref s) => run_dealloc(s, state),
        Statement::Read(ref s) => run_read(s, state),
        Statement::Write(ref s) => run_write(s, state),
        Statement::While(ref s) => run_while(s, state),
        Statement::Assign(ref s) => run_assign(s, state),
        Statement::Expr(ref s) => run_expr_stmt(s, state),
    }
}

fn run_alloc(statement: &AllocStmt, state: &mut State) -> RunResult<()> {
    let variable_id = statement.variable().id();

    let initial_value = match statement.initializer() {
        Some(initializer) => run_expression(initializer, state)?,
        None => {
            let variable = statement.variable();
            let variable_type = variable.type_().borrow();
            default_value_for_type(&variable_type)
        }
    };

    state.push(variable_id, initial_value);
    Ok(())
}

fn run_dealloc(statement: &DeallocStmt, state: &mut State) -> RunResult<()> {
    let variable_id = statement.variable().id();
    state.pop(variable_id);
    Ok(())
}

fn run_read(statement: &ReadStmt, state: &mut State) -> RunResult<()> {
    let variable_id = statement.variable().id();
    let word = state.cin.read_word()?;
    let new_value: i32 = word
        .parse()
        .map_err(|_| format!("Could not parse `{}` to an integer.", word))?;
    state.update(variable_id, Value::Int(new_value));
    Ok(())
}

fn run_write(statement: &WriteStmt, state: &mut State) -> RunResult<()> {
    let value = run_expression(statement.expression(), state)?;
    match value {
        Value::Int(x) => println!("{}", x),
        _ => panic_wrong_type("int", value.type_()),
    }
    Ok(())
}

fn run_while(statement: &WhileStmt, state: &mut State) -> RunResult<()> {
    let mut cond = run_expression(statement.cond(), state)?.as_bool();
    while cond {
        run_statement(statement.body(), state)?;
        cond = run_expression(statement.cond(), state)?.as_bool();
    }
    Ok(())
}

fn run_assign(statement: &AssignStmt, state: &mut State) -> RunResult<()> {
    let target_id = statement.target().id();
    let new_value = run_expression(statement.value(), state)?;
    state.update(target_id, new_value);
    Ok(())
}

fn run_expr_stmt(statement: &ExprStmt, state: &mut State) -> RunResult<()> {
    let _ = run_expression(statement.expression(), state)?;
    Ok(())
}

fn run_expression(expression: &Expression, state: &mut State) -> RunResult<Value> {
    match &expression.kind {
        ExpressionKind::Variable(e) => run_variable_expr(e, state),
        ExpressionKind::Literal(e) => run_literal_expr(e, state),
        ExpressionKind::BinaryOp(e) => run_binary_op_expr(e, state),
        ExpressionKind::Array(e) => run_array_expr(e, state),
        ExpressionKind::Index(e) => run_index_expr(e, state),
        ExpressionKind::Call(e) => run_call_expr(e, state),
        ExpressionKind::If(e) => run_if_expr(e, state),
        ExpressionKind::Block(e) => run_block_expr(e, state),
        ExpressionKind::Empty => Ok(Value::Void),
        ExpressionKind::Error => panic_error(),
    }
}

fn run_variable_expr(expression: &VariableExpr, state: &State) -> RunResult<Value> {
    let variable_id = expression.variable().id();
    let result = state.get(variable_id).clone();
    Ok(result)
}

fn run_literal_expr(expression: &LiteralExpr, _: &State) -> RunResult<Value> {
    let value = match expression {
        LiteralExpr::Int(value) => Value::Int(*value),
        LiteralExpr::Bool(value) => Value::Bool(*value),
    };
    Ok(value)
}

fn run_binary_op_expr(expression: &BinaryOpExpr, state: &mut State) -> RunResult<Value> {
    let lhs = run_expression(expression.lhs(), state)?.as_int();
    let rhs = run_expression(expression.rhs(), state)?.as_int();

    let result = match expression.operator {
        BinaryOperator::AddInt => Value::Int(lhs + rhs),
        BinaryOperator::SubInt => Value::Int(lhs - rhs),
        BinaryOperator::MulInt => Value::Int(lhs * rhs),
        BinaryOperator::LessInt => Value::Bool(lhs < rhs),
        BinaryOperator::GreaterInt => Value::Bool(lhs > rhs),
        BinaryOperator::LessEqInt => Value::Bool(lhs <= rhs),
        BinaryOperator::GreaterEqInt => Value::Bool(lhs >= rhs),
        BinaryOperator::EqInt => Value::Bool(lhs == rhs),
        BinaryOperator::NotEqInt => Value::Bool(lhs != rhs),
    };
    Ok(result)
}

fn run_array_expr(expression: &ArrayExpr, state: &mut State) -> RunResult<Value> {
    let elements: RunResult<Vec<Value>> = expression
        .elements()
        .map(|element| run_expression(element, state))
        .collect();
    let elements = elements?;

    Ok(Value::Array(Rc::new(RefCell::new(elements))))
}

fn run_index_expr(expression: &IndexExpr, state: &mut State) -> RunResult<Value> {
    let collection = run_expression(expression.collection(), state)?.as_array();
    let collection = collection.borrow();
    let index = run_expression(expression.index(), state)?.as_int();

    if index < 0 || index >= collection.len() as i32 {
        let error = format!(
            "array index out of bounds: array size is {}, index is {}",
            collection.len(),
            index
        );
        return Err(error.into());
    }

    Ok(collection[index as usize].clone())
}

fn run_call_expr(expression: &CallExpr, state: &mut State) -> RunResult<Value> {
    let function = expression.function();
    let parameters = function.parameters();

    let arguments: RunResult<Vec<Value>> = expression
        .arguments()
        .map(|argument| run_expression(argument, state))
        .collect();
    let arguments = arguments?;

    for (parameter, value) in parameters.zip(arguments.into_iter()) {
        let variable_id = parameter.id();
        state.push(variable_id, value)
    }

    let function_result = run_function(expression.function(), state);

    for parameter in function.parameters() {
        let variable_id = parameter.id();
        state.pop(variable_id)
    }

    function_result
}

fn run_if_expr(expression: &IfExpr, state: &mut State) -> RunResult<Value> {
    let cond = run_expression(expression.cond(), state)?.as_bool();
    if cond {
        run_expression(expression.then(), state)
    } else {
        run_expression(expression.else_(), state)
    }
}

fn run_block_expr(block: &BlockExpr, state: &mut State) -> RunResult<Value> {
    for statement in block.statements() {
        run_statement(statement, state)?;
    }

    run_expression(block.final_expr(), state)
}

fn default_value_for_type(type_: &Type) -> Value {
    match type_.kind() {
        TypeKind::Void => panic!("Tried to default-initialize a value of type `void`"),
        TypeKind::Int => Value::Int(0),
        TypeKind::Bool => Value::Bool(false),
        TypeKind::Array(_) => Value::Array(Rc::new(RefCell::new(vec![]))),
        TypeKind::Error => panic_error(),
    }
}

fn panic_error() -> ! {
    panic!("Error-containing program passed to interpreter backend.")
}

fn panic_wrong_type(expected_type: &str, actual_type: &str) -> ! {
    panic!(
        "Runtime type mismatch: expected `{}`, got `{}`",
        expected_type, actual_type
    )
}
