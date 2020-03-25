//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Program {
    variables: Vec<Rc<RefCell<Variable>>>,
    statements: Vec<Statement>,
}

impl Program {
    /// Creates a new, empty program.
    pub fn new() -> Program {
        Program {
            variables: vec![],
            statements: vec![],
        }
    }

    /// Adds a new variable to the symbol table.
    pub fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        self.variables.push(variable);
    }

    /// Appends a statement to the end of the program.
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
}

impl Variable {
    /// Creates a new variable with a given name.
    pub fn new(name: String) -> Variable {
        Variable { name }
    }
}

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
}

#[derive(Debug)]
pub struct VarDeclStmt {
    variable: Rc<RefCell<Variable>>,
    initializer: Option<Expression>,
}

impl VarDeclStmt {
    pub fn new(variable: &Rc<RefCell<Variable>>, initializer: Option<Expression>) -> Statement {
        Statement::VarDecl(VarDeclStmt {
            variable: Rc::clone(variable),
            initializer,
        })
    }
}

#[derive(Debug)]
pub struct ReadStmt {
    variable: Rc<RefCell<Variable>>,
}

impl ReadStmt {
    pub fn new(variable: &Rc<RefCell<Variable>>) -> Statement {
        Statement::Read(ReadStmt {
            variable: Rc::clone(variable),
        })
    }
}

#[derive(Debug)]
pub struct WriteStmt {
    expression: Expression,
}

impl WriteStmt {
    pub fn new(expression: Expression) -> Statement {
        Statement::Write(WriteStmt { expression })
    }
}

#[derive(Debug)]
pub enum Expression {
    Variable(VariableExpr),
    IntLiteral(IntLiteralExpr),
    Add(AddExpr),
    Error,
}

#[derive(Debug)]
pub struct VariableExpr {
    variable: Rc<RefCell<Variable>>,
}

impl VariableExpr {
    pub fn new(variable: &Rc<RefCell<Variable>>) -> Expression {
        Expression::Variable(VariableExpr {
            variable: Rc::clone(variable),
        })
    }
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    value: i32,
}

impl IntLiteralExpr {
    pub fn new(value: i32) -> Expression {
        Expression::IntLiteral(IntLiteralExpr { value })
    }
}

#[derive(Debug)]
pub struct AddExpr {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

impl AddExpr {
    pub fn new(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Add(AddExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }
}
