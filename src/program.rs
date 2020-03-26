//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

use crate::ast::InputSpan;
use crate::errors::CompilationError;
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
    pub definition_site: Option<InputSpan>,
    type_: Rc<RefCell<Type>>,
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

    pub fn type_(&self) -> impl Deref<Target = Type> + '_ {
        self.type_.borrow()
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
    If(IfStmt),
    Block(BlockStmt),
    Expr(ExprStmt),

    Error,
}

impl Statement {
    pub fn is_error(&self) -> bool {
        match self {
            Statement::Error => true,
            _ => false,
        }
    }
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
    pub fn new(
        variable: &Rc<RefCell<Variable>>,
        program: &Program,
        location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        {
            let variable = variable.borrow();
            let variable_type = variable.type_();
            if *variable_type != *program.int().borrow() {
                let error = CompilationError::read_target_not_int(&variable_type.name(), location);
                return Err(error);
            }
        }

        Ok(Statement::Read(ReadStmt {
            variable: Rc::clone(variable),
        }))
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
pub struct IfStmt {
    cond: Box<Expression>,
    then: Box<Statement>,
    else_: Option<Box<Statement>>,
}

impl IfStmt {
    pub fn new(
        cond: Expression,
        then: Statement,
        else_: Option<Statement>,
        program: &Program,
        cond_location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        check_condition_is_bool(&cond, program, cond_location).map(|_| {
            Statement::If(IfStmt {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: else_.map(Box::new),
            })
        })
    }

    pub fn cond(&self) -> &Expression {
        &self.cond
    }

    pub fn then(&self) -> &Statement {
        &self.then
    }

    pub fn else_(&self) -> Option<&Statement> {
        self.else_.as_deref()
    }
}

#[derive(Debug)]
pub struct BlockStmt {
    statements: Vec<Statement>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Statement>) -> Statement {
        Statement::Block(BlockStmt { statements })
    }

    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    expression: Box<Expression>,
}

impl ExprStmt {
    pub fn new(expression: Expression) -> Statement {
        match expression {
            Expression::Error => Statement::Error,
            _ => Statement::Expr(ExprStmt {
                expression: Box::new(expression),
            }),
        }
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
    If(IfExpr),
    Block(BlockExpr),
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
                    BinaryOperator::LessInt => program.bool(),
                    BinaryOperator::GreaterInt => program.bool(),
                    BinaryOperator::LessEqInt => program.bool(),
                    BinaryOperator::GreaterEqInt => program.bool(),
                    BinaryOperator::EqInt => program.bool(),
                    BinaryOperator::NotEqInt => program.bool(),
                };
                Rc::clone(type_)
            }
            If(e) => e.then.type_(program),
            Block(e) => e.final_expr.type_(program),
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
    LessInt,
    GreaterInt,
    LessEqInt,
    GreaterEqInt,
    EqInt,
    NotEqInt,
}

#[derive(Debug)]
pub struct BinaryOpExpr {
    pub operator: BinaryOperator,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

impl BinaryOpExpr {
    pub fn new(
        operator: BinaryOperator,
        lhs: Expression,
        rhs: Expression,
        program: &Program,
        lhs_location: InputSpan,
        rhs_location: InputSpan,
    ) -> Result<Expression, CompilationError> {
        check_operand_is_int(&lhs, program, lhs_location)?;
        check_operand_is_int(&rhs, program, rhs_location)?;

        Ok(Expression::BinaryOp(BinaryOpExpr {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }))
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

#[derive(Debug)]
pub struct IfExpr {
    cond: Box<Expression>,
    then: Box<Expression>,
    else_: Box<Expression>,
}

impl IfExpr {
    pub fn new(
        cond: Expression,
        then: Expression,
        else_: Expression,
        program: &Program,
        cond_location: InputSpan,
    ) -> Result<Expression, CompilationError> {
        check_condition_is_bool(&cond, program, cond_location).map(|_| {
            Expression::If(IfExpr {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: Box::new(else_),
            })
        })
    }

    pub fn cond(&self) -> &Expression {
        &self.cond
    }

    pub fn then(&self) -> &Expression {
        &self.then
    }

    pub fn else_(&self) -> &Expression {
        &self.else_
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    statements: Vec<Statement>,
    final_expr: Box<Expression>,
}

impl BlockExpr {
    pub fn new(statements: Vec<Statement>, final_expr: Expression) -> Expression {
        Expression::Block(BlockExpr {
            statements,
            final_expr: Box::new(final_expr),
        })
    }

    pub fn statements<'a>(&'a self) -> impl Iterator<Item = &Statement> + 'a {
        self.statements.iter()
    }

    pub fn final_expr(&self) -> &Expression {
        &self.final_expr
    }
}

/// Convenience function for checking condition type.
fn check_condition_is_bool(
    condition: &Expression,
    program: &Program,
    location: InputSpan,
) -> Result<(), CompilationError> {
    let cond_type = condition.type_(program);
    if cond_type != *program.bool() {
        let error = CompilationError::condition_is_not_bool(&cond_type.borrow().name(), location);
        Err(error)
    } else {
        Ok(())
    }
}

fn check_operand_is_int(
    operand: &Expression,
    program: &Program,
    location: InputSpan,
) -> Result<(), CompilationError> {
    let operand_type = operand.type_(program);
    if operand_type != *program.int() {
        let error = CompilationError::operand_is_not_int(&operand_type.borrow().name(), location);
        Err(error)
    } else {
        Ok(())
    }
}
