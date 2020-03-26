use lalrpop_util::lalrpop_mod;

mod ast;
mod backends;
mod errors;
mod program;
mod scope;
mod typing;
lalrpop_mod!(pub grammar);

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::cell::RefCell;
use std::env::Args;
use std::fs;
use std::rc::Rc;

use crate::backends::debug::DebugBackend;
use crate::typing::Type;
use backends::interpreter::InterpreterBackend;
use backends::Backend;
use errors::CompilationError;
use program::Program;
use program::Variable;
use scope::Scope;
use std::error::Error;

pub struct Config {
    source_path: String,
    backend: Box<dyn Backend>,
}

impl Config {
    /// Parse command line arguments.
    pub fn new(args: Args) -> Result<Config, &'static str> {
        // TODO use a command line parser library.
        let mut args: Vec<String> = args.skip(1).collect();
        let backend: Box<dyn Backend> = if args.iter().any(|a| a == "--debug") {
            Box::new(DebugBackend)
        } else {
            Box::new(InterpreterBackend)
        };

        // Drop flags.
        args.retain(|f| !f.starts_with("--"));

        if args.len() >= 1 {
            let source_path = args[0].to_owned();
            Ok(Config {
                source_path,
                backend,
            })
        } else {
            return Err("Missing path to source file");
        }
    }
}

/// Executes the program.
pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source_code = fs::read_to_string(&config.source_path)?;

    let program = match run_frontend(&source_code) {
        Ok(program) => program,
        Err(errors) => {
            report_compilation_errors(&config.source_path, &source_code, &errors)?;
            return Err("compilation did not succeed".to_string().into());
        }
    };

    config.backend.run(program)?;
    Ok(())
}

fn run_frontend(source_code: &str) -> Result<Program, Vec<CompilationError>> {
    let program_ast =
        parse(&source_code).map_err(|err| vec![CompilationError::syntax_error(err)])?;

    let program = compile(program_ast)?;
    Ok(program)
}

fn report_compilation_errors(
    file_name: &str,
    source_code: &str,
    errors: &Vec<CompilationError>,
) -> std::io::Result<()> {
    let file = SimpleFile::new(file_name, source_code);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for error in errors {
        codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &error.to_codespan())?;
    }

    Ok(())
}

/// Parses the source code and returns an AST root.
fn parse(source_code: &str) -> Result<ast::Program, ast::ParseError> {
    grammar::ProgramParser::new().parse(source_code)
}

/// Context that gets passed along to various compiler routines.
struct CompilerContext {
    program: Program,
    scope: Scope,
    errors: Vec<CompilationError>,
}

impl CompilerContext {
    /// Creates the initial root context.
    pub fn new() -> CompilerContext {
        let program = Program::new();
        let scope = Scope::new(&program);
        CompilerContext {
            program,
            scope,
            errors: vec![],
        }
    }
}

/// Compiles a CO program.
fn compile(program_ast: ast::Program) -> Result<Program, Vec<CompilationError>> {
    let mut context = CompilerContext::new();

    for statement in program_ast.statements {
        let statement = compile_statement(statement, &mut context);
        context.program.add_statement(statement)
    }

    if context.errors.is_empty() {
        Ok(context.program)
    } else {
        Err(context.errors)
    }
}

/// Compiles a single statement. If compilation succeeds, returns the resulting
/// statement as Some. If any errors occur, they are added to `context`, and
/// the function result is None.
fn compile_statement(
    statement: ast::Statement,
    context: &mut CompilerContext,
) -> program::Statement {
    match statement {
        ast::Statement::VarDecl(s) => compile_var_decl_stmt(s, context),
        ast::Statement::Read(s) => compile_read_stmt(s, context),
        ast::Statement::Write(s) => compile_write_stmt(s, context),
        ast::Statement::If(s) => compile_if_stmt(s, context),
        ast::Statement::Block(s) => compile_block_stmt(s, context),
        ast::Statement::Expr(s) => compile_expr_stmt(s, context),
    }
}

fn compile_var_decl_stmt(
    statement: ast::VarDeclStmt,
    context: &mut CompilerContext,
) -> program::Statement {
    let name = &statement.variable_name;

    // Compile the variable type and the initializer expression
    // even if the variable produces an error.
    let type_ = statement
        .variable_type
        .map(|type_| compile_type_expr(type_, context));
    let initializer = statement
        .initializer
        .map(|initializer| compile_expression(initializer, context));

    // Possibly infer type from the initializer expression.
    let type_ = match type_ {
        Some(t) => t,
        None => match initializer {
            Some(ref expr) => expr.type_(&context.program),
            None => {
                let error = CompilationError::variable_type_omitted(&name, statement.span);
                context.errors.push(error);
                Type::error()
            }
        },
    };

    let variable = Variable::new(name.to_string(), &type_, Some(statement.span));
    let variable = Rc::new(RefCell::new(variable));

    context.program.add_variable(Rc::clone(&variable));
    if let Err(error) = context.scope.add_variable(&variable) {
        context.errors.push(error);
        return program::Statement::Error;
    }

    program::VarDeclStmt::new(&variable, initializer)
}

fn compile_read_stmt(
    statement: ast::ReadStmt,
    context: &mut CompilerContext,
) -> program::Statement {
    let name = &statement.variable_name;

    // TODO here and in var_decl: span should be limited to variable name only.
    let variable = context.scope.lookup_variable(&name, statement.span);
    match variable {
        Ok(variable) => program::ReadStmt::new(&variable),
        Err(error) => {
            context.errors.push(error);
            program::Statement::Error
        }
    }
}

fn compile_write_stmt(
    statement: ast::WriteStmt,
    context: &mut CompilerContext,
) -> program::Statement {
    let expression = compile_expression(statement.expression, context);
    if let program::Expression::Error = expression {
        program::Statement::Error
    } else {
        program::WriteStmt::new(expression)
    }
}

fn compile_if_stmt(statement: ast::IfStmt, context: &mut CompilerContext) -> program::Statement {
    let cond_span = statement.cond.span();
    let cond = compile_expression(*statement.cond, context);
    let then = compile_statement(*statement.then, context);
    let else_ = statement
        .else_
        .map(|stmt| compile_statement(*stmt, context));

    let result = program::IfStmt::new(cond, then, else_, &context.program, cond_span);
    match result {
        Ok(statement) => statement,
        Err(error) => {
            context.errors.push(error);
            program::Statement::Error
        }
    }
}

fn compile_block_stmt(block: ast::BlockStmt, context: &mut CompilerContext) -> program::Statement {
    // TODO block nested scopes
    let statements = compile_statement_vec(block.statements, context);
    program::BlockStmt::new(statements)
}

fn compile_expr_stmt(
    statement: ast::ExprStmt,
    context: &mut CompilerContext,
) -> program::Statement {
    let expression = compile_expression(statement.expression, context);
    program::ExprStmt::new(expression)
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
    let statements = compile_statement_vec(expression.statements, context);
    let final_expr = compile_expression(*expression.final_expr, context);
    program::BlockExpr::new(statements, final_expr)
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

fn compile_statement_vec(
    statements: Vec<ast::Statement>,
    context: &mut CompilerContext,
) -> Vec<program::Statement> {
    statements
        .into_iter()
        .map(|stmt| compile_statement(stmt, context))
        .filter(|stmt| !stmt.is_error())
        .collect()
}
