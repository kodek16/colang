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
use std::ops::{Deref, DerefMut};
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

#[derive(Clone)]
enum Value {
    Lvalue(Lvalue),
    Rvalue(Rvalue),
}

impl Value {
    pub fn into_lvalue(self) -> Lvalue {
        match self {
            Value::Lvalue(value) => value,
            Value::Rvalue(_) => panic!("Rvalue accessed as lvalue."),
        }
    }

    pub fn into_rvalue(self) -> Rvalue {
        match self {
            Value::Lvalue(value) => value.borrow().clone(),
            Value::Rvalue(value) => value,
        }
    }
}

#[derive(Clone)]
struct Lvalue(Rc<RefCell<Rvalue>>);

impl Lvalue {
    pub fn store(rvalue: Rvalue) -> Lvalue {
        Lvalue(Rc::new(RefCell::new(rvalue)))
    }

    pub fn borrow(&self) -> impl Deref<Target = Rvalue> + '_ {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> impl DerefMut<Target = Rvalue> + '_ {
        self.0.borrow_mut()
    }
}

/// Every value that exists in the program belongs to this type.
#[derive(Clone)]
enum Rvalue {
    Int(i32),
    Bool(bool),
    Array(Rc<RefCell<Vec<Lvalue>>>),
    Void,
}

impl Rvalue {
    pub fn as_int(&self) -> i32 {
        match self {
            Rvalue::Int(x) => *x,
            _ => panic_wrong_type("int", self.type_()),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Rvalue::Bool(b) => *b,
            _ => panic_wrong_type("bool", self.type_()),
        }
    }

    pub fn into_array(self) -> Rc<RefCell<Vec<Lvalue>>> {
        match self {
            Rvalue::Array(v) => v,
            _ => panic_wrong_type("array", self.type_()),
        }
    }

    /// A quick-and-dirty information source about value types in runtime.
    /// This is _not_ meant to be an actual RTTI solution, it is only meant
    /// to be used in type mismatch panics, which should not occur under
    /// normal circumstances.
    pub fn type_(&self) -> &'static str {
        use Rvalue::*;
        match self {
            Int(_) => "int",
            Bool(_) => "bool",
            Array(_) => "array",
            Void => "void",
        }
    }
}

struct State {
    variables: HashMap<SymbolId, Vec<Lvalue>>,
    cin: Cin,
}

impl State {
    fn new() -> State {
        State {
            variables: HashMap::new(),
            cin: Cin::new(),
        }
    }

    fn push(&mut self, variable_id: SymbolId, value: Rvalue) {
        self.variables
            .entry(variable_id)
            .or_default()
            .push(Lvalue::store(value))
    }

    fn pop(&mut self, variable_id: SymbolId) {
        self.variables
            .get_mut(&variable_id)
            .expect("variable deallocated before allocation")
            .pop()
            .expect("variable deallocated twice");
    }

    fn get(&self, variable_id: SymbolId) -> Value {
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
        Some(initializer) => run_expression(initializer, state)?.into_rvalue(),
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
    let target = run_expression(statement.target(), state)?.into_lvalue();
    let word = state.cin.read_word()?;
    let new_value: i32 = word
        .parse()
        .map_err(|_| format!("Could not parse `{}` to an integer.", word))?;
    *target.borrow_mut() = Rvalue::Int(new_value);
    Ok(())
}

fn run_write(statement: &WriteStmt, state: &mut State) -> RunResult<()> {
    let value = run_expression(statement.expression(), state)?.into_rvalue();
    match value {
        Rvalue::Int(x) => println!("{}", x),
        _ => panic_wrong_type("int", value.type_()),
    }
    Ok(())
}

fn run_while(statement: &WhileStmt, state: &mut State) -> RunResult<()> {
    let mut cond = run_expression(statement.cond(), state)?
        .into_rvalue()
        .as_bool();
    while cond {
        run_statement(statement.body(), state)?;
        cond = run_expression(statement.cond(), state)?
            .into_rvalue()
            .as_bool();
    }
    Ok(())
}

fn run_assign(statement: &AssignStmt, state: &mut State) -> RunResult<()> {
    let target = run_expression(statement.target(), state)?.into_lvalue();
    let new_value = run_expression(statement.value(), state)?.into_rvalue();
    *target.borrow_mut() = new_value;
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
        ExpressionKind::Empty => Ok(Value::Rvalue(Rvalue::Void)),
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
        LiteralExpr::Int(value) => Value::Rvalue(Rvalue::Int(*value)),
        LiteralExpr::Bool(value) => Value::Rvalue(Rvalue::Bool(*value)),
    };
    Ok(value)
}

fn run_binary_op_expr(expression: &BinaryOpExpr, state: &mut State) -> RunResult<Value> {
    let lhs = run_expression(expression.lhs(), state)?
        .into_rvalue()
        .as_int();
    let rhs = run_expression(expression.rhs(), state)?
        .into_rvalue()
        .as_int();

    let result = Value::Rvalue(match expression.operator {
        BinaryOperator::AddInt => Rvalue::Int(lhs + rhs),
        BinaryOperator::SubInt => Rvalue::Int(lhs - rhs),
        BinaryOperator::MulInt => Rvalue::Int(lhs * rhs),
        BinaryOperator::LessInt => Rvalue::Bool(lhs < rhs),
        BinaryOperator::GreaterInt => Rvalue::Bool(lhs > rhs),
        BinaryOperator::LessEqInt => Rvalue::Bool(lhs <= rhs),
        BinaryOperator::GreaterEqInt => Rvalue::Bool(lhs >= rhs),
        BinaryOperator::EqInt => Rvalue::Bool(lhs == rhs),
        BinaryOperator::NotEqInt => Rvalue::Bool(lhs != rhs),
    });
    Ok(result)
}

fn run_array_expr(expression: &ArrayExpr, state: &mut State) -> RunResult<Value> {
    let elements: RunResult<Vec<Value>> = expression
        .elements()
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

fn run_index_expr(expression: &IndexExpr, state: &mut State) -> RunResult<Value> {
    let collection = run_expression(expression.collection(), state)?
        .into_rvalue()
        .into_array();
    let collection = collection.borrow();
    let index = run_expression(expression.index(), state)?
        .into_rvalue()
        .as_int();

    if index < 0 || index >= collection.len() as i32 {
        let error = format!(
            "array index out of bounds: array size is {}, index is {}",
            collection.len(),
            index
        );
        return Err(error.into());
    }

    Ok(Value::Lvalue(collection[index as usize].clone()))
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
        state.push(variable_id, value.into_rvalue())
    }

    let function_result = run_function(expression.function(), state);

    for parameter in function.parameters() {
        let variable_id = parameter.id();
        state.pop(variable_id)
    }

    function_result
}

fn run_if_expr(expression: &IfExpr, state: &mut State) -> RunResult<Value> {
    let cond = run_expression(expression.cond(), state)?
        .into_rvalue()
        .as_bool();
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

fn default_value_for_type(type_: &Type) -> Rvalue {
    match type_.kind() {
        TypeKind::Void => panic!("Tried to default-initialize a value of type `void`"),
        TypeKind::Int => Rvalue::Int(0),
        TypeKind::Bool => Rvalue::Bool(false),
        TypeKind::Array(_) => Rvalue::Array(Rc::new(RefCell::new(vec![]))),
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
