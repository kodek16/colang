//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

use crate::ast::InputSpan;
use crate::typing::Type;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

/// Every distinct named entity in the program receives a unique ID.
pub type SymbolId = u32;

#[derive(Debug)]
pub struct Program {
    variables: Vec<Rc<RefCell<Variable>>>,
    types: Vec<Rc<RefCell<Type>>>,
    statements: Vec<Statement>,
    next_symbol_id: SymbolId,

    // Internal types can be accessed bypassing the scope mechanism.
    // Their canonical instances are referenced here.
    int_type: Rc<RefCell<Type>>,
    bool_type: Rc<RefCell<Type>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub fn new() -> Program {
        let int_type = Rc::new(RefCell::new(Type::Int));
        let bool_type = Rc::new(RefCell::new(Type::Bool));

        Program {
            int_type: Rc::clone(&int_type),
            bool_type: Rc::clone(&bool_type),

            variables: vec![],
            types: vec![int_type, bool_type],
            statements: vec![],
            next_symbol_id: 0,
        }
    }

    /// Returns an iterator over program statements.
    pub fn statements<'a>(&'a self) -> impl Iterator<Item = &Statement> + 'a {
        self.statements.iter()
    }

    /// Adds a new variable to the symbol table.
    pub fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        variable.borrow_mut().set_id(self.next_symbol_id);
        self.next_symbol_id += 1;
        self.variables.push(variable);
    }

    /// Appends a statement to the end of the program.
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    /// The canonical `int` type reference.
    pub fn int(&self) -> &Rc<RefCell<Type>> {
        &self.int_type
    }

    /// The canonical `bool` type reference.
    pub fn bool(&self) -> &Rc<RefCell<Type>> {
        &self.bool_type
    }
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub type_: Rc<RefCell<Type>>,
    pub definition_site: Option<InputSpan>,
    id: Option<SymbolId>,
}

impl Variable {
    /// Creates a new variable with a given name and type.
    /// Calling `id()` is invalid before the variable is added to a `Program`.
    pub fn new(
        name: String,
        type_: &Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
    ) -> Variable {
        Variable {
            name,
            type_: Rc::clone(type_),
            definition_site,
            id: None,
        }
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
    BinaryOp(BinaryOpExpr),
    Error,
}

impl Expression {
    pub fn type_(&self, program: &Program) -> Rc<RefCell<Type>> {
        use Expression::*;
        match self {
            Variable(e) => Rc::clone(&e.variable.borrow().type_),
            IntLiteral(_) => Rc::clone(program.int()),
            BinaryOp(e) => {
                let type_ = match e.operator {
                    BinaryOperator::AddInt => program.int(),
                    BinaryOperator::SubInt => program.int(),
                    BinaryOperator::MulInt => program.int(),
                };
                Rc::clone(type_)
            }
            Error => Type::error(),
        }
    }
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
pub enum BinaryOperator {
    AddInt,
    SubInt,
    MulInt,
}

#[derive(Debug)]
pub struct BinaryOpExpr {
    pub operator: BinaryOperator,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

impl BinaryOpExpr {
    pub fn new(operator: BinaryOperator, lhs: Expression, rhs: Expression) -> Expression {
        Expression::BinaryOp(BinaryOpExpr {
            operator,
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
