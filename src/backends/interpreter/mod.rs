//! Interpreter backend for CO can run the code right after it is compiled.

mod cin;

use super::Backend;
use crate::backends::interpreter::cin::Cin;
use crate::program::{
    BinaryOpExpr, BinaryOperator, Expression, IntLiteralExpr, Program, ReadStmt, Statement,
    SymbolId, VarDeclStmt, VariableExpr, WriteStmt,
};
use std::collections::HashMap;
use std::error::Error;
use std::process;

pub struct InterpreterBackend;

impl Backend for InterpreterBackend {
    fn run(&self, program: Program) -> Result<(), Box<dyn Error>> {
        let mut state = State::new();

        for statement in program.statements() {
            let result = match statement {
                Statement::VarDecl(ref s) => run_var_decl(s, &mut state),
                Statement::Read(ref s) => run_read(s, &mut state),
                Statement::Write(ref s) => run_write(s, &mut state),
            };
            if let Err(err) = result {
                eprintln!("Error: {}", err);
                process::exit(1);
            }
        }

        Ok(())
    }
}

/// Every value that exists in the program belongs to this type.
type Value = i32;

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

fn run_var_decl(statement: &VarDeclStmt, state: &mut State) -> Result<(), Box<dyn Error>> {
    let variable_id = statement.variable().id();
    let initial_value = statement
        .initializer()
        .map(|i| run_expr(i, state))
        .unwrap_or(0);
    state.variables.insert(variable_id, initial_value);
    Ok(())
}

fn run_read(statement: &ReadStmt, state: &mut State) -> Result<(), Box<dyn Error>> {
    let variable_id = statement.variable().id();
    let word = state.cin.read_word()?;
    let new_value: i32 = word
        .parse()
        .map_err(|_| format!("Could not parse `{}` to an integer.", word))?;
    state.variables.insert(variable_id, new_value);
    Ok(())
}

fn run_write(statement: &WriteStmt, state: &mut State) -> Result<(), Box<dyn Error>> {
    let value = run_expr(statement.expression(), state);
    println!("{}", value);
    Ok(())
}

fn run_expr(expression: &Expression, state: &mut State) -> Value {
    match expression {
        Expression::Variable(e) => run_variable_expr(e, state),
        Expression::IntLiteral(e) => run_int_literal_expr(e, state),
        Expression::BinaryOp(e) => run_binary_op_expr(e, state),
        Expression::Error => panic!("Error-containing program passed to interpreter backend."),
    }
}

fn run_variable_expr(expression: &VariableExpr, state: &State) -> Value {
    let variable_id = expression.variable().id();
    *state.variables.get(&variable_id).unwrap()
}

fn run_int_literal_expr(expression: &IntLiteralExpr, _: &State) -> Value {
    expression.value
}

fn run_binary_op_expr(expression: &BinaryOpExpr, state: &mut State) -> Value {
    let lhs = run_expr(expression.lhs(), state);
    let rhs = run_expr(expression.rhs(), state);

    match expression.operator {
        BinaryOperator::Add => lhs + rhs,
        BinaryOperator::Sub => lhs - rhs,
        BinaryOperator::Mul => lhs * rhs,
    }
}
