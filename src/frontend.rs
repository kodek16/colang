//! Compiler frontend transforms the source code into an intermediate representation
//! defined in `program`. All static checks are performed during this phase.

use std::cell::RefCell;
use std::rc::Rc;

use crate::ast;
use crate::errors::CompilationError;
use crate::grammar;
use crate::program;
use crate::program::Variable;
use crate::scope::Scope;
use crate::typing::Type;

pub fn run(source_code: &str) -> Result<program::Program, Vec<CompilationError>> {
    let program_ast =
        parse(&source_code).map_err(|err| vec![CompilationError::syntax_error(err)])?;

    let program = compile(program_ast)?;
    Ok(program)
}

/// Parses the source code and returns an AST root.
fn parse(source_code: &str) -> Result<ast::Program, ast::ParseError> {
    grammar::ProgramParser::new().parse(source_code)
}

/// Context that gets passed along to various compiler routines.
struct CompilerContext {
    program: program::Program,
    scope: Scope,
    errors: Vec<CompilationError>,
}

impl CompilerContext {
    /// Creates the initial root context.
    pub fn new() -> CompilerContext {
        let program = program::Program::new();
        let scope = Scope::new(&program);
        CompilerContext {
            program,
            scope,
            errors: vec![],
        }
    }
}

/// Compiles a CO program.
fn compile(program_ast: ast::Program) -> Result<program::Program, Vec<CompilationError>> {
    let mut context = CompilerContext::new();

    let mut builder = BlockBuilder::new();
    for statement in program_ast.statements {
        compile_statement(statement, &mut builder, &mut context);
    }

    context.program.add_statement(builder.into_stmt());

    if context.errors.is_empty() {
        Ok(context.program)
    } else {
        Err(context.errors)
    }
}

/// Represents an object that expects statements to be written into it.
trait StatementSink {
    fn emit(&mut self, statement: program::Statement);
}

impl StatementSink for program::Program {
    fn emit(&mut self, statement: program::Statement) {
        self.add_statement(statement)
    }
}

impl StatementSink for BlockBuilder {
    fn emit(&mut self, statement: program::Statement) {
        self.append_statement(statement)
    }
}

impl StatementSink for Option<program::Statement> {
    fn emit(&mut self, statement: program::Statement) {
        *self = Some(statement);
    }
}

/// Compiles a single statement in the syntax sense. If compilation succeeds,
/// resulting target program statements are emitted to `sink`. If any errors
/// occur, they are added to `context`.
fn compile_statement(
    statement: ast::Statement,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    match statement {
        ast::Statement::VarDecl(s) => compile_var_decl_stmt(s, sink, context),
        ast::Statement::Read(s) => compile_read_stmt(s, sink, context),
        ast::Statement::Write(s) => compile_write_stmt(s, sink, context),
        ast::Statement::If(s) => compile_if_stmt(s, sink, context),
        ast::Statement::While(s) => compile_while_stmt(s, sink, context),
        ast::Statement::Block(s) => compile_block_stmt(s, sink, context),
        ast::Statement::Expr(s) => compile_expr_stmt(s, sink, context),
    }
}

fn compile_var_decl_stmt(
    statement: ast::VarDeclStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    for declaration in statement.entries {
        compile_var_decl_entry(declaration, sink, context);
    }
}

fn compile_var_decl_entry(
    declaration: ast::VarDeclEntry,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let name = &declaration.variable_name;

    // Compile the variable type and the initializer expression
    // even if the variable produces an error.
    let type_ = declaration
        .variable_type
        .map(|type_| compile_type_expr(type_, context));
    let initializer = declaration
        .initializer
        .map(|initializer| compile_expression(initializer, context));

    // Possibly infer type from the initializer expression.
    let type_ = match type_ {
        Some(t) => t,
        None => match initializer {
            Some(ref expr) => expr.type_(&context.program),
            None => {
                let error = CompilationError::variable_type_omitted(&name, declaration.span);
                context.errors.push(error);
                Type::error()
            }
        },
    };

    let variable = Variable::new(name.to_string(), &type_, Some(declaration.span));
    let variable = Rc::new(RefCell::new(variable));

    context.program.add_variable(Rc::clone(&variable));
    if let Err(error) = context.scope.add_variable(&variable) {
        context.errors.push(error);
        return;
    }

    let statement = program::VarDeclStmt::new(&variable, initializer);
    sink.emit(statement);
}

fn compile_read_stmt(
    statement: ast::ReadStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    for entry in statement.entries {
        compile_read_entry(entry, sink, context);
    }
}

fn compile_read_entry(
    entry: ast::ReadEntry,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let name = &entry.variable_name;
    let variable = context.scope.lookup_variable(&name, entry.span);
    let result =
        variable.and_then(|var| program::ReadStmt::new(&var, &context.program, entry.span));
    match result {
        Ok(statement) => sink.emit(statement),
        Err(error) => context.errors.push(error),
    }
}

fn compile_write_stmt(
    statement: ast::WriteStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, context);
    if let program::Expression::Error = expression {
        return;
    }

    let statement = program::WriteStmt::new(expression);
    sink.emit(statement);
}

fn compile_if_stmt(
    statement: ast::IfStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let cond_span = statement.cond.span();
    let cond = compile_expression(*statement.cond, context);

    let mut then: Option<program::Statement> = None;
    let mut else_: Option<program::Statement> = None;

    compile_statement(*statement.then, &mut then, context);
    statement
        .else_
        .map(|stmt| compile_statement(*stmt, &mut else_, context));

    if cond.is_error() {
        return;
    }

    if let Some(then) = then {
        let result = program::IfStmt::new(cond, then, else_, &context.program, cond_span);
        match result {
            Ok(statement) => sink.emit(statement),
            Err(error) => context.errors.push(error),
        }
    }
}

fn compile_while_stmt(
    statement: ast::WhileStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let cond_span = statement.cond.span();
    let cond = compile_expression(*statement.cond, context);

    let mut body: Option<program::Statement> = None;
    compile_statement(*statement.body, &mut body, context);

    if cond.is_error() {
        return;
    }

    if let Some(body) = body {
        let result = program::WhileStmt::new(cond, body, &context.program, cond_span);
        match result {
            Ok(statement) => sink.emit(statement),
            Err(error) => context.errors.push(error),
        }
    }
}

fn compile_block_stmt(
    block: ast::BlockStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    context.scope.push();
    let mut builder = BlockBuilder::new();
    for statement in block.statements {
        compile_statement(statement, &mut builder, context);
    }
    let result = builder.into_stmt();
    context.scope.pop();
    sink.emit(result);
}

fn compile_expr_stmt(
    statement: ast::ExprStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, context);
    if expression.is_error() {
        return;
    }

    let result = program::ExprStmt::new(expression);
    sink.emit(result);
}

fn compile_expression(
    expression: ast::Expression,
    context: &mut CompilerContext,
) -> program::Expression {
    match expression {
        ast::Expression::Variable(e) => compile_variable_expr(e, context),
        ast::Expression::IntLiteral(e) => compile_int_literal_expr(e, context),
        ast::Expression::BinaryOp(e) => compile_binary_op_expr(e, context),
        ast::Expression::If(e) => compile_if_expr(e, context),
        ast::Expression::Block(e) => compile_block_expr(e, context),
    }
}

fn compile_variable_expr(
    expression: ast::VariableExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let name = expression.name;

    let variable = context.scope.lookup_variable(&name, expression.span);
    match variable {
        Ok(variable) if variable.borrow().type_().is_error() => program::Expression::Error,
        Ok(variable) => program::VariableExpr::new(variable),
        Err(error) => {
            context.errors.push(error);
            program::Expression::Error
        }
    }
}

fn compile_int_literal_expr(
    expression: ast::IntLiteralExpr,
    _context: &CompilerContext,
) -> program::Expression {
    program::IntLiteralExpr::new(expression.value)
}

fn compile_binary_op_expr(
    expression: ast::BinaryOperatorExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let operator = match expression.operator {
        ast::BinaryOperator::Add => program::BinaryOperator::AddInt,
        ast::BinaryOperator::Sub => program::BinaryOperator::SubInt,
        ast::BinaryOperator::Mul => program::BinaryOperator::MulInt,
        ast::BinaryOperator::Less => program::BinaryOperator::LessInt,
        ast::BinaryOperator::Greater => program::BinaryOperator::GreaterInt,
        ast::BinaryOperator::LessEq => program::BinaryOperator::LessEqInt,
        ast::BinaryOperator::GreaterEq => program::BinaryOperator::GreaterEqInt,
        ast::BinaryOperator::Eq => program::BinaryOperator::EqInt,
        ast::BinaryOperator::NotEq => program::BinaryOperator::NotEqInt,
    };
    let lhs_location = expression.lhs.span();
    let rhs_location = expression.rhs.span();
    let lhs = compile_expression(*expression.lhs, context);
    let rhs = compile_expression(*expression.rhs, context);

    if lhs.is_error() || rhs.is_error() {
        return program::Expression::Error;
    }

    let result = program::BinaryOpExpr::new(
        operator,
        lhs,
        rhs,
        &context.program,
        lhs_location,
        rhs_location,
    );
    match result {
        Ok(expr) => expr,
        Err(error) => {
            context.errors.push(error);
            program::Expression::Error
        }
    }
}

fn compile_if_expr(expression: ast::IfExpr, context: &mut CompilerContext) -> program::Expression {
    let cond_span = expression.cond.span();
    let cond = compile_expression(*expression.cond, context);
    let then = compile_expression(*expression.then, context);
    let else_ = compile_expression(*expression.else_, context);

    let result = program::IfExpr::new(cond, then, else_, &context.program, cond_span);
    match result {
        Ok(expr) => expr,
        Err(error) => {
            context.errors.push(error);
            program::Expression::Error
        }
    }
}

fn compile_block_expr(
    expression: ast::BlockExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    context.scope.push();
    let mut builder = BlockBuilder::new();
    for statement in expression.statements {
        compile_statement(statement, &mut builder, context);
    }
    let final_expr = compile_expression(*expression.final_expr, context);
    let result = builder.into_expr(final_expr);
    context.scope.pop();
    result
}

fn compile_type_expr(type_expr: ast::TypeExpr, context: &mut CompilerContext) -> Rc<RefCell<Type>> {
    let name = &type_expr.name;
    let type_ = context.scope.lookup_type(name, type_expr.span);

    match type_ {
        Ok(type_) => Rc::clone(type_),
        Err(error) => {
            context.errors.push(error);
            Type::error()
        }
    }
}

/// Incremental interface for building block statements and expressions.
struct BlockBuilder {
    statements: Vec<program::Statement>,
}

impl BlockBuilder {
    fn new() -> BlockBuilder {
        BlockBuilder { statements: vec![] }
    }

    fn append_statement(&mut self, statement: program::Statement) {
        self.statements.push(statement)
    }

    fn into_stmt(self) -> program::Statement {
        program::BlockStmt::new(self.statements)
    }

    fn into_expr(self, final_expr: program::Expression) -> program::Expression {
        program::BlockExpr::new(self.statements, final_expr)
    }
}
