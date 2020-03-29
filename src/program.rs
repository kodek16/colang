//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::typing::{Type, TypeRegistry};
use private::SymbolImpl;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

/// Every distinct named entity in the program receives a unique ID.
pub type SymbolId = u32;

/// Public trait for all symbols in the program that have a unique ID.
pub trait Symbol {
    fn id(&self) -> SymbolId;
}

#[derive(Debug)]
pub struct Program {
    variables: Vec<Rc<RefCell<Variable>>>,
    functions: Vec<Rc<RefCell<Function>>>,
    types: TypeRegistry,
    next_symbol_id: SymbolId,

    main_function: Option<Rc<RefCell<Function>>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub fn new() -> Program {
        Program {
            variables: vec![],
            functions: vec![],
            types: TypeRegistry::new(),
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

    /// Mark a function as the "main" function that the program should start
    /// executing from.
    /// Frontend is guaranteed to do this for a valid program.
    pub fn fill_main_function(&mut self, main_function: Rc<RefCell<Function>>) {
        self.main_function = Some(main_function)
    }

    pub fn types(&self) -> &TypeRegistry {
        &self.types
    }

    pub fn main_function(&self) -> impl Deref<Target = Function> + '_ {
        self.main_function
            .as_ref()
            .expect("`main` function has not been specified")
            .borrow()
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub definition_site: Option<InputSpan>,
    parameters: Vec<Rc<RefCell<Variable>>>,
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
            parameters: vec![],
            return_type,
            body: None,
            id: None,
        }
    }

    pub fn fill_parameters(&mut self, parameters: Vec<Rc<RefCell<Variable>>>) {
        self.parameters = parameters
    }

    // We need to accept `body_type` as a separate parameter, because computing
    // the type for recursive functions involves borrowing the function immutably,
    // so if we do this inside the method, where we have a mutable borrow, it would cause a panic.
    #[must_use]
    pub fn fill_body(
        &mut self,
        body: Expression,
        body_type: Rc<RefCell<Type>>,
    ) -> Result<(), CompilationError> {
        if body_type != self.return_type {
            let error = CompilationError::function_body_type_mismatch(
                &self.return_type.borrow().name(),
                &body_type.borrow().name(),
                self.definition_site
                    .expect("Internal function body type mismatch"),
            );
            return Err(error);
        }

        self.body = Some(body);
        Ok(())
    }

    pub fn parameters(&self) -> impl Iterator<Item = impl Deref<Target = Variable> + '_> {
        self.parameters.iter().map(|parameter| parameter.borrow())
    }

    pub fn _return_type(&self) -> impl Deref<Target = Type> + '_ {
        self.return_type.borrow()
    }

    pub fn body(&self) -> &Expression {
        &self.body.as_ref().expect("function body was not filled")
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
        type_: Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
        program: &Program,
    ) -> Result<Variable, CompilationError> {
        if type_ == *program.types.void() {
            let error = CompilationError::variable_of_type_void(
                definition_site.expect("Internal variable of type `void` defined."),
            );
            return Err(error);
        }

        Ok(Variable {
            name,
            type_,
            definition_site,
            id: None,
        })
    }

    pub fn type_(&self) -> &Rc<RefCell<Type>> {
        &self.type_
    }
}

#[derive(Debug)]
pub enum Statement {
    Alloc(AllocStmt),
    Dealloc(DeallocStmt),
    Read(ReadStmt),
    Write(WriteStmt),
    While(WhileStmt),
    Assign(AssignStmt),
    Expr(ExprStmt),
}

#[derive(Debug)]
pub struct AllocStmt {
    variable: Rc<RefCell<Variable>>,
    initializer: Option<Expression>,
}

impl AllocStmt {
    pub fn new(
        variable: &Rc<RefCell<Variable>>,
        initializer: Option<Expression>,
        program: &Program,
        location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        {
            let variable = variable.borrow();
            let name = &variable.name;
            let type_ = &variable.type_;

            if let Some(ref initializer) = initializer {
                let initializer_type = initializer.type_(program);
                if initializer_type != *type_ {
                    let error = CompilationError::variable_initializer_type_mismatch(
                        name,
                        type_.borrow().name(),
                        initializer_type.borrow().name(),
                        location,
                    );
                    return Err(error);
                }
            }
        }

        let statement = Statement::Alloc(AllocStmt {
            variable: Rc::clone(variable),
            initializer,
        });
        Ok(statement)
    }

    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        self.variable.borrow()
    }

    pub fn initializer(&self) -> Option<&Expression> {
        self.initializer.as_ref()
    }
}

#[derive(Debug)]
pub struct DeallocStmt {
    variable: Rc<RefCell<Variable>>,
}

impl DeallocStmt {
    pub fn new(variable: Rc<RefCell<Variable>>) -> Statement {
        Statement::Dealloc(DeallocStmt { variable })
    }

    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        self.variable.borrow()
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
            if variable_type != program.types.int() {
                let error =
                    CompilationError::read_target_not_int(variable_type.borrow().name(), location);
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
    pub fn new(
        expression: Expression,
        program: &Program,
        expr_location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        let expression_type = expression.type_(program);
        if expression_type != *program.types.int() {
            let error = CompilationError::write_value_not_int(
                &expression_type.borrow().name(),
                expr_location,
            );
            return Err(error);
        }

        Ok(Statement::Write(WriteStmt { expression }))
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    cond: Box<Expression>,
    body: Box<Statement>,
}

impl WhileStmt {
    pub fn new(
        cond: Expression,
        body: Statement,
        program: &Program,
        cond_location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        check_condition_is_bool(&cond, program, cond_location)?;

        Ok(Statement::While(WhileStmt {
            cond: Box::new(cond),
            body: Box::new(body),
        }))
    }

    pub fn cond(&self) -> &Expression {
        &self.cond
    }

    pub fn body(&self) -> &Statement {
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

        if *variable_type != value_type {
            let error = CompilationError::assignment_type_mismatch(
                variable_type.borrow().name(),
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
    Literal(LiteralExpr),
    BinaryOp(BinaryOpExpr),
    Call(CallExpr),
    If(IfExpr),
    Block(BlockExpr),

    /// A no-op expression of type `void`.
    Empty,

    Error,
}

trait ExpressionKind {
    fn type_(&self, program: &Program) -> Rc<RefCell<Type>>;
}

impl Expression {
    pub fn type_(&self, program: &Program) -> Rc<RefCell<Type>> {
        match self {
            Expression::Variable(e) => e.type_(program),
            Expression::Literal(e) => e.type_(program),
            Expression::BinaryOp(e) => e.type_(program),
            Expression::Call(e) => e.type_(program),
            Expression::If(e) => e.type_(program),
            Expression::Block(e) => e.type_(program),
            Expression::Empty => Rc::clone(program.types.void()),
            Expression::Error => Type::error(),
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
        self.variable.borrow()
    }

    /// This should only be used in the frontend.
    pub fn variable_owned(&self) -> Rc<RefCell<Variable>> {
        Rc::clone(&self.variable)
    }
}

impl ExpressionKind for VariableExpr {
    fn type_(&self, _: &Program) -> Rc<RefCell<Type>> {
        Rc::clone(&self.variable.borrow().type_)
    }
}

#[derive(Debug)]
pub enum LiteralExpr {
    Int(i32),
    Bool(bool),
}

impl LiteralExpr {
    pub fn int(value: i32) -> Expression {
        Expression::Literal(LiteralExpr::Int(value))
    }

    pub fn bool(value: bool) -> Expression {
        Expression::Literal(LiteralExpr::Bool(value))
    }
}

impl ExpressionKind for LiteralExpr {
    fn type_(&self, program: &Program) -> Rc<RefCell<Type>> {
        Rc::clone(match self {
            LiteralExpr::Int(_) => program.types.int(),
            LiteralExpr::Bool(_) => program.types.bool(),
        })
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

impl ExpressionKind for BinaryOpExpr {
    fn type_(&self, program: &Program) -> Rc<RefCell<Type>> {
        let type_ = match self.operator {
            BinaryOperator::AddInt => program.types.int(),
            BinaryOperator::SubInt => program.types.int(),
            BinaryOperator::MulInt => program.types.int(),
            BinaryOperator::LessInt => program.types.bool(),
            BinaryOperator::GreaterInt => program.types.bool(),
            BinaryOperator::LessEqInt => program.types.bool(),
            BinaryOperator::GreaterEqInt => program.types.bool(),
            BinaryOperator::EqInt => program.types.bool(),
            BinaryOperator::NotEqInt => program.types.bool(),
        };
        Rc::clone(type_)
    }
}

#[derive(Debug)]
pub struct CallExpr {
    function: Rc<RefCell<Function>>,
    arguments: Vec<Expression>,
}

impl CallExpr {
    pub fn new(
        function: Rc<RefCell<Function>>,
        arguments: Vec<Expression>,
        location: InputSpan,
        program: &Program,
    ) -> Result<Expression, CompilationError> {
        {
            let function = function.borrow();
            let function_name = &function.name;
            let num_parameters = function.parameters.len();
            let num_arguments = arguments.len();
            if num_parameters != num_arguments {
                let error = CompilationError::call_wrong_number_of_arguments(
                    function_name,
                    num_parameters,
                    num_arguments,
                    location,
                );
                return Err(error);
            }

            for (argument, parameter) in arguments.iter().zip(function.parameters.iter()) {
                let parameter = parameter.borrow();
                let parameter_name = &parameter.name;
                let argument_type = argument.type_(program);
                let parameter_type = &parameter.type_;

                if argument_type != *parameter_type {
                    let error = CompilationError::call_argument_type_mismatch(
                        parameter_name,
                        parameter_type.borrow().name(),
                        argument_type.borrow().name(),
                        location,
                    );
                    return Err(error);
                }
            }
        }

        Ok(Expression::Call(CallExpr {
            function,
            arguments,
        }))
    }

    pub fn function(&self) -> impl Deref<Target = Function> + '_ {
        self.function.borrow()
    }

    pub fn arguments(&self) -> impl Iterator<Item = &Expression> {
        self.arguments.iter()
    }
}

impl ExpressionKind for CallExpr {
    fn type_(&self, _: &Program) -> Rc<RefCell<Type>> {
        Rc::clone(&self.function.borrow().return_type)
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
        else_: Option<Expression>,
        program: &Program,
        cond_location: InputSpan,
        then_location: InputSpan,
    ) -> Result<Expression, CompilationError> {
        check_condition_is_bool(&cond, program, cond_location)?;

        let then_type = then.type_(program);

        if else_.is_none() && then_type != *program.types.void() {
            let error = CompilationError::if_expression_missing_else(
                &then_type.borrow().name(),
                then_location,
            );
            return Err(error);
        }

        let else_ = else_.unwrap_or(Expression::Empty);
        let else_type = else_.type_(program);

        if then_type != else_type {
            let error = CompilationError::if_expression_branch_type_mismatch(
                &then_type.borrow().name(),
                &else_type.borrow().name(),
                cond_location,
            );
            return Err(error);
        }

        Ok(Expression::If(IfExpr {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_),
        }))
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

impl ExpressionKind for IfExpr {
    fn type_(&self, program: &Program) -> Rc<RefCell<Type>> {
        self.then.type_(program)
    }
}

#[derive(Debug)]
pub struct BlockExpr {
    statements: Vec<Statement>,
    final_expr: Box<Expression>,
}

impl BlockExpr {
    fn new(statements: Vec<Statement>, final_expr: Option<Expression>) -> Expression {
        let final_expr = match final_expr {
            Some(final_expr) => Box::new(final_expr),
            None => Box::new(Expression::Empty),
        };

        Expression::Block(BlockExpr {
            statements,
            final_expr,
        })
    }

    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }

    pub fn final_expr(&self) -> &Expression {
        &self.final_expr
    }
}

impl ExpressionKind for BlockExpr {
    fn type_(&self, program: &Program) -> Rc<RefCell<Type>> {
        self.final_expr.type_(program)
    }
}

/// Incremental interface for building block statements and expressions.
pub struct BlockBuilder {
    statements: Vec<Statement>,
}

impl BlockBuilder {
    pub fn new() -> BlockBuilder {
        BlockBuilder { statements: vec![] }
    }

    pub fn append_statement(&mut self, statement: Statement) {
        self.statements.push(statement)
    }

    pub fn into_expr(self, final_expr: Option<Expression>) -> Expression {
        let mut statements = self.statements;

        // We need to emit a `Dealloc` for every `Alloc` in this block, in reverse order.
        let mut deallocations: Vec<Statement> = statements
            .iter()
            .flat_map(|statement| match statement {
                Statement::Alloc(AllocStmt { variable, .. }) => {
                    Some(DeallocStmt::new(Rc::clone(variable)))
                }
                _ => None,
            })
            .collect();
        deallocations.reverse();
        statements.append(&mut deallocations);

        BlockExpr::new(statements, final_expr)
    }
}

/// Convenience function for checking condition type.
fn check_condition_is_bool(
    condition: &Expression,
    program: &Program,
    location: InputSpan,
) -> Result<(), CompilationError> {
    let cond_type = condition.type_(program);
    if cond_type != *program.types.bool() {
        let error = CompilationError::condition_is_not_bool(cond_type.borrow().name(), location);
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
    if operand_type != *program.types.int() {
        let error = CompilationError::operand_is_not_int(operand_type.borrow().name(), location);
        Err(error)
    } else {
        Ok(())
    }
}

mod private {
    use super::{Function, Symbol, SymbolId, Variable};

    /// Private trait that provides the default behavior for initializing symbol IDs.
    pub trait SymbolImpl: Symbol {
        fn id_option(&self) -> &Option<SymbolId>;
        fn id_option_mut(&mut self) -> &mut Option<SymbolId>;

        fn set_id(&mut self, new_id: SymbolId) {
            *self.id_option_mut() = Some(new_id);
        }
    }

    impl<T: SymbolImpl> Symbol for T {
        fn id(&self) -> SymbolId {
            self.id_option()
                .expect("Attempted to access ID of unbound symbol.")
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

    impl SymbolImpl for Function {
        fn id_option(&self) -> &Option<SymbolId> {
            &self.id
        }
        fn id_option_mut(&mut self) -> &mut Option<SymbolId> {
            &mut self.id
        }
    }
}
