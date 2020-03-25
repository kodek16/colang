//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

/// Every distinct named entity in the program receives a unique ID.
pub type SymbolId = u32;

#[derive(Debug)]
pub struct Program {
    variables: Vec<Rc<RefCell<Variable>>>,
    statements: Vec<Statement>,
    next_variable_id: SymbolId,
}

impl Program {
    /// Creates a new, empty program.
    pub fn new() -> Program {
        Program {
            variables: vec![],
            statements: vec![],
            next_variable_id: 0,
        }
    }

    /// Returns an iterator over program statements.
    pub fn statements<'a>(&'a self) -> impl Iterator<Item = &Statement> + 'a {
        self.statements.iter()
    }

    /// Adds a new variable to the symbol table.
    pub fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        variable.borrow_mut().set_id(self.next_variable_id);
        self.next_variable_id += 1;
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
    id: Option<SymbolId>,
}

impl Variable {
    /// Creates a new variable with a given name.
    /// Calling `id()` is invalid before the variable is added to a `Program`.
    pub fn new(name: String) -> Variable {
        Variable { name, id: None }
    }

    /// A unique ID defined in the context of the entire `Program`.
    pub fn id(&self) -> SymbolId {
        self.id.expect("Attempted to access ID of unbound variable")
    }

    fn set_id(&mut self, new_id: SymbolId) {
        self.id = Some(new_id)
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

    /// Borrow the variable immutably.
    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        (*self.variable).borrow()
    }

    pub fn initializer(&self) -> Option<&Expression> {
        self.initializer.as_ref()
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

    /// Borrow the variable immutably.
    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        (*self.variable).borrow()
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

    pub fn expression(&self) -> &Expression {
        &self.expression
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

    /// Borrow the variable immutably.
    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        (*self.variable).borrow()
    }
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    pub value: i32,
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

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}
