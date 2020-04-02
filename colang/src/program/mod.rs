//! Intermediate representation for CO programs.
//! This is the interface between the front-end and various
//! backends.

mod checks;
mod display;
pub mod expressions;
pub mod internal;

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
    user_functions: Vec<Rc<RefCell<Function>>>,
    next_symbol_id: SymbolId,

    types: TypeRegistry,
    internal_functions: HashMap<InternalFunctionTag, Rc<RefCell<Function>>>,

    main_function: Option<Rc<RefCell<Function>>>,
}

impl Program {
    /// Creates a new program, populated with some internal symbols.
    pub fn new() -> Program {
        Program {
            variables: vec![],
            user_functions: vec![],
            next_symbol_id: 0,
            types: TypeRegistry::new(),
            internal_functions: HashMap::new(),
            main_function: None,
        }
    }

    /// Adds a new variable to the symbol table.
    pub fn add_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        variable.borrow_mut().set_id(self.next_symbol_id);
        self.next_symbol_id += 1;
        self.variables.push(variable);
    }

    /// Adds a new function to the program. If the function is user-defined,
    /// it is assigned a symbol id.
    pub fn add_function(&mut self, function: Rc<RefCell<Function>>) {
        let mut function_mut = function.borrow_mut();
        match *function_mut {
            Function::UserDefined(ref mut function_mut) => {
                function_mut.set_id(self.next_symbol_id);
                self.next_symbol_id += 1;
                self.user_functions.push(Rc::clone(&function));
            }
            Function::Internal(ref function_ref) => {
                self.internal_functions
                    .insert(function_ref.tag, Rc::clone(&function));
            }
        }
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

    pub fn types_mut(&mut self) -> &mut TypeRegistry {
        &mut self.types
    }

    pub fn internal_function(&self, tag: InternalFunctionTag) -> &Rc<RefCell<Function>> {
        &self.internal_functions[&tag]
    }

    pub fn main_function(&self) -> impl Deref<Target = Function> + '_ {
        self.main_function
            .as_ref()
            .expect("`main` function has not been specified")
            .borrow()
    }
}

#[derive(Debug)]
pub enum Function {
    UserDefined(UserDefinedFunction),
    Internal(InternalFunction),
}

pub trait Parameter {
    fn name(&self) -> &str;
    fn type_(&self) -> &Rc<RefCell<Type>>;
}

impl Function {
    pub fn name(&self) -> &str {
        match self {
            Function::UserDefined(function) => &function.name,
            Function::Internal(function) => &function.name,
        }
    }

    pub fn definition_site(&self) -> Option<InputSpan> {
        match self {
            Function::UserDefined(function) => Some(function.definition_site),
            Function::Internal(_) => None,
        }
    }

    // To implement this method normally, https://github.com/rust-lang/rust/issues/27732
    // needs to be stable: it is required if we want to coerce Ref<Variable> to Ref<dyn Parameter>.
    // In its absence, the best thing we can do is clone the parameter data manually to break
    // away from RefCell.
    pub fn parameters(&self) -> Box<dyn ExactSizeIterator<Item = impl Parameter> + '_> {
        struct SizedParameter {
            name: String,
            type_: Rc<RefCell<Type>>,
        }
        impl Parameter for SizedParameter {
            fn name(&self) -> &str {
                &self.name
            }

            fn type_(&self) -> &Rc<RefCell<Type>> {
                &self.type_
            }
        }

        match self {
            Function::UserDefined(function) => {
                Box::new(function.parameters.iter().map(|parameter| {
                    let parameter = parameter.borrow();
                    SizedParameter {
                        name: parameter.name().to_string(),
                        type_: Rc::clone(parameter.type_()),
                    }
                }))
            }
            Function::Internal(function) => {
                Box::new(function.parameters.iter().map(|parameter| SizedParameter {
                    name: parameter.name.clone(),
                    type_: Rc::clone(&parameter.type_),
                }))
            }
        }
    }

    pub fn return_type(&self) -> &Rc<RefCell<Type>> {
        match self {
            Function::UserDefined(ref function) => &function.return_type,
            Function::Internal(ref function) => &function.return_type,
        }
    }

    pub fn as_user_defined(&self) -> &UserDefinedFunction {
        match self {
            Function::UserDefined(ref function) => function,
            Function::Internal(ref function) => panic!(
                "Attempt to treat internal function `{}` as user-defined",
                function.name
            ),
        }
    }

    pub fn as_user_defined_mut(&mut self) -> &mut UserDefinedFunction {
        match self {
            Function::UserDefined(ref mut function) => function,
            Function::Internal(ref function) => panic!(
                "Attempt to treat internal function `{}` as user-defined",
                function.name
            ),
        }
    }
}

impl Parameter for Variable {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &Rc<RefCell<Type>> {
        Variable::type_(&self)
    }
}

#[derive(Debug)]
pub struct UserDefinedFunction {
    pub name: String,
    pub definition_site: InputSpan,
    parameters: Vec<Rc<RefCell<Variable>>>,
    return_type: Rc<RefCell<Type>>,
    body: Option<Expression>,
    id: Option<SymbolId>,
}

impl UserDefinedFunction {
    /// Initialize a new, empty function. Parameters and body are filled later.
    pub fn new(
        name: String,
        return_type: Rc<RefCell<Type>>,
        definition_site: InputSpan,
    ) -> Function {
        let function = UserDefinedFunction {
            name,
            definition_site,
            parameters: vec![],
            return_type,
            body: None,
            id: None,
        };

        Function::UserDefined(function)
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
                self.definition_site,
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
pub struct InternalFunction {
    pub name: String,
    pub tag: InternalFunctionTag,
    parameters: Vec<InternalParameter>,
    return_type: Rc<RefCell<Type>>,
}

#[derive(Debug)]
pub struct InternalParameter {
    pub name: String,
    pub type_: Rc<RefCell<Type>>,
}

impl InternalFunction {
    pub fn new(
        name: String,
        tag: InternalFunctionTag,
        parameters: Vec<InternalParameter>,
        return_type: Rc<RefCell<Type>>,
    ) -> Function {
        let function = InternalFunction {
            name,
            tag,
            parameters,
            return_type,
        };

        Function::Internal(function)
    }
}

impl Parameter for InternalParameter {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &Rc<RefCell<Type>> {
        &self.type_
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
    Return(ReturnStmt),
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
        location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        {
            let variable = variable.borrow();
            let name = &variable.name;
            let type_ = &variable.type_;

            if let Some(ref initializer) = initializer {
                let initializer_type = &initializer.type_;
                if *initializer_type != *type_ {
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
    target: Box<Expression>,
}

impl ReadStmt {
    pub fn new(target: Expression, types: &TypeRegistry) -> Result<Statement, CompilationError> {
        if target.value_category != ValueCategory::Lvalue {
            let error = CompilationError::read_target_not_lvalue(
                target.span.expect("Attempt to read generated rvalue."),
            );
            return Err(error);
        }

        let target_type = &target.type_;
        if *target_type != *types.int() {
            let error = CompilationError::read_target_not_int(
                target_type.borrow().name(),
                target
                    .span
                    .expect("Attempt to read generated non-int value."),
            );
            return Err(error);
        }

        Ok(Statement::Read(ReadStmt {
            target: Box::new(target),
        }))
    }

    pub fn target(&self) -> &Expression {
        &self.target
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
        let expression_type = &expression.type_;
        if *expression_type != *program.types.int() {
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
        types: &TypeRegistry,
    ) -> Result<Statement, CompilationError> {
        checks::check_condition_is_bool(&cond, types)?;

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
    target: Box<Expression>,
    value: Box<Expression>,
}

impl AssignStmt {
    pub fn new(
        target: Expression,
        value: Expression,
        location: InputSpan,
    ) -> Result<Statement, CompilationError> {
        if target.value_category != ValueCategory::Lvalue {
            let error = CompilationError::assignment_target_not_lvalue(
                target
                    .span
                    .expect("Generated rvalue expression used as assignment target"),
            );
            return Err(error);
        }

        let target_type = &target.type_;
        let value_type = &value.type_;

        if *target_type != *value_type {
            let error = CompilationError::assignment_type_mismatch(
                target_type.borrow().name(),
                value_type.borrow().name(),
                location,
            );
            return Err(error);
        }

        Ok(Statement::Assign(AssignStmt {
            target: Box::new(target),
            value: Box::new(value),
        }))
    }

    pub fn target(&self) -> &Expression {
        &self.target
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

/// This instruction represents memorization of a value that would later
/// become the value of the surrounding context. This context might be a
/// function (and then this is the return value), but more commonly
/// this is the final value of an expression block.
#[derive(Debug)]
pub struct ReturnStmt {
    expression: Box<Expression>,
}

impl ReturnStmt {
    pub fn new(expression: Expression) -> Statement {
        Statement::Return(ReturnStmt {
            expression: Box::new(expression),
        })
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
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
pub struct Expression {
    pub kind: ExpressionKind,
    pub type_: Rc<RefCell<Type>>,
    pub value_category: ValueCategory,
    pub span: Option<InputSpan>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ValueCategory {
    Lvalue,
    Rvalue,
}

use crate::program::internal::InternalFunctionTag;
pub use expressions::array_from_copy::ArrayFromCopyExpr;
pub use expressions::array_from_elements::ArrayFromElementsExpr;
pub use expressions::block::{BlockBuilder, BlockExpr};
pub use expressions::call::CallExpr;
pub use expressions::if_::IfExpr;
pub use expressions::index::IndexExpr;
pub use expressions::literal::LiteralExpr;
pub use expressions::variable::VariableExpr;
use std::collections::HashMap;

#[derive(Debug)]
pub enum ExpressionKind {
    Variable(VariableExpr),
    Literal(LiteralExpr),
    ArrayFromElements(ArrayFromElementsExpr),
    ArrayFromCopy(ArrayFromCopyExpr),
    Index(IndexExpr),
    Call(CallExpr),
    If(IfExpr),
    Block(BlockExpr),

    /// A no-op expression of type `void`.
    Empty,

    Error,
}

impl Expression {
    pub fn empty(types: &TypeRegistry) -> Expression {
        Expression {
            kind: ExpressionKind::Empty,
            type_: Rc::clone(types.void()),
            value_category: ValueCategory::Rvalue,
            span: None,
        }
    }

    pub fn error(span: InputSpan) -> Expression {
        Expression {
            kind: ExpressionKind::Error,
            type_: Type::error(),
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub fn is_error(&self) -> bool {
        match self.kind {
            ExpressionKind::Error => true,
            _ => false,
        }
    }
}

mod private {
    use super::{Symbol, SymbolId, Variable};
    use crate::program::UserDefinedFunction;

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

    impl SymbolImpl for UserDefinedFunction {
        fn id_option(&self) -> &Option<SymbolId> {
            &self.id
        }
        fn id_option_mut(&mut self) -> &mut Option<SymbolId> {
            &mut self.id
        }
    }
}
