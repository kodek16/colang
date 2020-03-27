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

/// Public trait for all symbols in the program that have a unique ID.
pub trait Symbol {
    fn id(&self) -> SymbolId;
}

use private::SymbolImpl;

#[derive(Debug)]
pub struct Program {
    variables: Vec<Rc<RefCell<Variable>>>,
    functions: Vec<Rc<RefCell<Function>>>,
    types: Vec<Rc<RefCell<Type>>>,
    next_symbol_id: SymbolId,

    main_function: Option<Rc<RefCell<Function>>>,

    // Internal types can be accessed bypassing the scope mechanism.
    // Their canonical instances are referenced here.
    int_type: Rc<RefCell<Type>>,
    bool_type: Rc<RefCell<Type>>,
    void_type: Rc<RefCell<Type>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub fn new() -> Program {
        let void_type = Rc::new(RefCell::new(Type::Void));
        let int_type = Rc::new(RefCell::new(Type::Int));
        let bool_type = Rc::new(RefCell::new(Type::Bool));

        Program {
            int_type: Rc::clone(&int_type),
            bool_type: Rc::clone(&bool_type),
            void_type: Rc::clone(&void_type),

            variables: vec![],
            functions: vec![],
            types: vec![void_type, int_type, bool_type],
            next_symbol_id: 0,
            main_function: None,
        }
    }

    /// Adds a new variable to the symbol table.
    pub fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        variable.borrow_mut().set_id(self.next_symbol_id);
        self.next_symbol_id += 1;
        self.variables.push(variable);
    }

    /// Adds a new function to the symbol table.
    pub fn add_function(&mut self, function: Rc<RefCell<Function>>) {
        function.borrow_mut().set_id(self.next_symbol_id);
        self.next_symbol_id += 1;
        self.functions.push(function);
    }

    pub fn set_main_function(&mut self, main_function: Rc<RefCell<Function>>) {
        self.main_function = Some(main_function)
    }

    pub fn main_function(&self) -> impl Deref<Target = Function> + '_ {
        self.main_function
            .as_ref()
            .expect("`main` function has not been specified")
            .borrow()
    }

    /// The canonical `int` type reference.
    pub fn int(&self) -> &Rc<RefCell<Type>> {
        &self.int_type
    }

    /// The canonical `bool` type reference.
    pub fn bool(&self) -> &Rc<RefCell<Type>> {
        &self.bool_type
    }

    /// The canonical `void` type reference.
    pub fn void(&self) -> &Rc<RefCell<Type>> {
        &self.void_type
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub definition_site: Option<InputSpan>,
    return_type: Rc<RefCell<Type>>,
    body: Option<Expression>,
    id: Option<SymbolId>,
}

impl Function {
    /// Initialize a new, empty function.
    pub fn new(
        name: String,
        return_type: Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
    ) -> Function {
        Function {
            name,
            definition_site,
            return_type,
            body: None,
            id: None,
        }
    }

    pub fn fill_body(&mut self, body: Expression) {
        self.body = Some(body)
    }

    pub fn _return_type(&self) -> impl Deref<Target = Type> + '_ {
        self.return_type.borrow()
    }

    pub fn body(&self) -> &Expression {
        &self.body.as_ref().expect("function body was not filled")
    }
}

impl SymbolImpl for Function {
    fn id_option(&self) -> &Option<SymbolId> {
        &self.id
    }
    fn id_option_mut(&mut self) -> &mut Option<SymbolId> {
        &mut self.id
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

    pub fn type_(&self) -> impl Deref<Target = Type> + '_ {
        self.type_.borrow()
    }
}

impl SymbolImpl for Variable {
    fn id_option(&self) -> &Option<SymbolId> {
        &self.id
    }
    fn id_option_mut(&mut self) -> &mut Option<SymbolId> {
        &mut self.id
    }
}

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
    While(WhileStmt),
    Assign(AssignStmt),
    Expr(ExprStmt),
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
pub struct WhileStmt {
    cond: Box<Expression>,
    body: Box<Expression>,
}

impl WhileStmt {
    pub fn new(
        cond: Expression,
        body: Expression,
        program: &Program,
        cond_location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        check_condition_is_bool(&cond, program, cond_location)?;
        // TODO emit a warning if body is not of type void.

        Ok(Statement::While(WhileStmt {
            cond: Box::new(cond),
            body: Box::new(body),
        }))
    }

    pub fn cond(&self) -> &Expression {
        &self.cond
    }

    pub fn body(&self) -> &Expression {
        &self.body
    }
}

#[derive(Debug)]
pub struct AssignStmt {
    target: Rc<RefCell<Variable>>,
    value: Box<Expression>,
}

impl AssignStmt {
    pub fn new(
        target: &Rc<RefCell<Variable>>,
        value: Expression,
        program: &Program,
        location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        let variable = target.borrow();
        let variable_type = variable.type_();
        let value_type = value.type_(program);

        if *variable_type != *value_type.borrow() {
            let error = CompilationError::assignment_type_mismatch(
                &variable_type.name(),
                &value_type.borrow().name(),
                location,
            );
            return Err(error);
        }

        Ok(Statement::Assign(AssignStmt {
            target: Rc::clone(target),
            value: Box::new(value),
        }))
    }

    pub fn target(&self) -> impl Deref<Target = Variable> + '_ {
        (*self.target).borrow()
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    expression: Box<Expression>,
}

impl ExprStmt {
    pub fn new(expression: Expression) -> Statement {
        Statement::Expr(ExprStmt {
            expression: Box::new(expression),
        })
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
            Block(e) => match e.final_expr() {
                Some(final_expr) => final_expr.type_(program),
                None => Rc::clone(program.void()),
            },
            Error => Type::error(),
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            Expression::Error => true,
            _ => false,
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

    /// This should only be used in the frontend.
    pub fn variable_owned(&self) -> Rc<RefCell<Variable>> {
        Rc::clone(&self.variable)
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
    else_: Option<Box<Expression>>,
}

impl IfExpr {
    pub fn new(
        cond: Expression,
        then: Expression,
        else_: Option<Expression>,
        program: &Program,
        cond_location: InputSpan,
    ) -> Result<Expression, CompilationError> {
        check_condition_is_bool(&cond, program, cond_location).map(|_| {
            // TODO check that if both branches are not void, their type is the same.
            Expression::If(IfExpr {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: else_.map(Box::new),
            })
        })
    }

    pub fn cond(&self) -> &Expression {
        &self.cond
    }

    pub fn then(&self) -> &Expression {
        &self.then
    }

    pub fn else_(&self) -> Option<&Expression> {
        self.else_.as_deref()
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    statements: Vec<Statement>,
    final_expr: Option<Box<Expression>>,
}

impl BlockExpr {
    pub fn new(statements: Vec<Statement>, final_expr: Option<Expression>) -> Expression {
        Expression::Block(BlockExpr {
            statements,
            final_expr: final_expr.map(Box::new),
        })
    }

    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }

    pub fn final_expr(&self) -> Option<&Expression> {
        self.final_expr.as_deref()
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

mod private {
    /// Private trait that provides the default behavior for initializing symbol IDs.
    pub trait SymbolImpl: super::Symbol {
        fn id_option(&self) -> &Option<super::SymbolId>;
        fn id_option_mut(&mut self) -> &mut Option<super::SymbolId>;

        fn set_id(&mut self, new_id: super::SymbolId) {
            *self.id_option_mut() = Some(new_id);
        }
    }

    impl<T: SymbolImpl> super::Symbol for T {
        fn id(&self) -> super::SymbolId {
            self.id_option()
                .expect("Attempted to access ID of unbound symbol.")
        }
    }
}
