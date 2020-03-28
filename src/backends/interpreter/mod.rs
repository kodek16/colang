//! Interpreter backend for CO can run the code right after it is compiled.

mod cin;

use super::Backend;
use crate::backends::interpreter::cin::Cin;
use crate::program::{
    AssignStmt, BinaryOpExpr, BinaryOperator, BlockExpr, ExprStmt, Expression, IfExpr,
    IntLiteralExpr, Program, ReadStmt, Statement, Symbol, SymbolId, VarDeclStmt, VariableExpr,
    WhileStmt, WriteStmt,
};
use crate::typing::Type;
use std::collections::HashMap;
use std::error::Error;
use std::ops::Deref;
use std::process;

pub struct InterpreterBackend;

impl Backend for InterpreterBackend {
    fn run(&self, program: Program) -> Result<(), Box<dyn Error>> {
        let mut state = State::new();

        let main = program.main_function();
        let main = main.body();

        let result = run_expression(main, &mut state);
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

    pub fn type_(&self) -> &'static str {
        use Value::*;
        match self {
            Int(_) => "int",
            Bool(_) => "bool",
            Void => "void",
        }
    }
}

struct State {
    variables: HashMap<SymbolId, Value>,
    cin: Cin,
}

impl State {
    fn new() -> State {
        State {
            variables: HashMap::new(),
            cin: Cin::new(),
        }
    }
}

type RunResult<T> = Result<T, Box<dyn Error>>;

fn run_statement(statement: &Statement, state: &mut State) -> RunResult<()> {
    match statement {
        Statement::VarDecl(ref s) => run_var_decl(s, state),
        Statement::Read(ref s) => run_read(s, state),
        Statement::Write(ref s) => run_write(s, state),
        Statement::While(ref s) => run_while(s, state),
        Statement::Assign(ref s) => run_assign(s, state),
        Statement::Expr(ref s) => run_expr_stmt(s, state),
    }
}

fn run_var_decl(statement: &VarDeclStmt, state: &mut State) -> RunResult<()> {
    let variable_id = statement.variable().id();

    let initial_value = match statement.initializer() {
        Some(initializer) => run_expression(initializer, state)?,
        None => {
            let variable = statement.variable();
            let variable_type = variable.type_();
            default_value_for_type(variable_type)
        }
    };

    state.variables.insert(variable_id, initial_value);
    Ok(())
}

fn run_read(statement: &ReadStmt, state: &mut State) -> RunResult<()> {
    let variable_id = statement.variable().id();
    let word = state.cin.read_word()?;
    let new_value: i32 = word
        .parse()
        .map_err(|_| format!("Could not parse `{}` to an integer.", word))?;
    state.variables.insert(variable_id, Value::Int(new_value));
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
    state.variables.insert(target_id, new_value);
    Ok(())
}

fn run_expr_stmt(statement: &ExprStmt, state: &mut State) -> RunResult<()> {
    let _ = run_expression(statement.expression(), state)?;
    Ok(())
}

fn run_expression(expression: &Expression, state: &mut State) -> RunResult<Value> {
    match expression {
        Expression::Variable(e) => run_variable_expr(e, state),
        Expression::IntLiteral(e) => run_int_literal_expr(e, state),
        Expression::BinaryOp(e) => run_binary_op_expr(e, state),
        Expression::Call(_e) => unimplemented!(),
        Expression::If(e) => run_if_expr(e, state),
        Expression::Block(e) => run_block_expr(e, state),
        Expression::Empty => Ok(Value::Void),
        Expression::Error => panic_error(),
    }
}

fn run_variable_expr(expression: &VariableExpr, state: &State) -> RunResult<Value> {
    let variable_id = expression.variable().id();
    let result = state.variables.get(&variable_id).unwrap().clone();
    Ok(result)
}

fn run_int_literal_expr(expression: &IntLiteralExpr, _: &State) -> RunResult<Value> {
    Ok(Value::Int(expression.value))
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

fn default_value_for_type(type_: impl Deref<Target = Type>) -> Value {
    match *type_ {
        Type::Int => Value::Int(0),
        Type::Bool => Value::Bool(false),
        Type::Void => panic!("Tried to default-initialize a value of type `void`"),
        Type::Error => panic_error(),
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
