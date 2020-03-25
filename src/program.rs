//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Debug)]
pub struct Program {
    variables: Vec<Rc<RefCell<Variable>>>,
    statements: Vec<Statement>,
}

impl Program {
    /// Create a new, empty program.
    pub fn new() -> Program {
        Program {
            variables: vec![],
            statements: vec![],
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    name: String,
}

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
}

#[derive(Debug)]
pub struct VarDeclStmt {
    variable: Weak<RefCell<Variable>>,
    initializer: Expression,
}

#[derive(Debug)]
pub struct ReadStmt {
    variable: Weak<RefCell<Variable>>,
}

#[derive(Debug)]
pub struct WriteStmt {
    expression: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Variable(VariableExpr),
    IntLiteral(IntLiteralExpr),
    Add(AddExpr),
}

#[derive(Debug)]
pub struct VariableExpr {
    variable: Weak<RefCell<Variable>>,
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    value: i32,
}

#[derive(Debug)]
pub struct AddExpr {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}
