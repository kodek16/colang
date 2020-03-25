use lalrpop_util::lalrpop_mod;

mod ast;
mod backends;
mod errors;
mod program;
lalrpop_mod!(pub grammar);

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::cell::RefCell;
use std::collections::HashMap;
use std::env::Args;
use std::fs;
use std::rc::Rc;

use backends::interpreter::InterpreterBackend;
use backends::Backend;
use errors::CompilationError;
use program::Program;
use program::Variable;
use std::error::Error;

pub struct Config {
    source_path: String,
    backend: Box<dyn Backend>,
}

impl Config {
    /// Parse command line arguments.
    pub fn new(mut args: Args) -> Result<Config, &'static str> {
        args.next();

        if let Some(source_path) = args.next() {
            Ok(Config {
                source_path,
                backend: Box::new(InterpreterBackend),
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

struct Scope {
    variables: HashMap<String, Rc<RefCell<Variable>>>,
}

impl Scope {
    /// Creates a new, empty scope.
    fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
        }
    }

    fn add(&mut self, variable: Rc<RefCell<Variable>>) {
        let name = variable.borrow().name.to_string();
        self.variables.insert(name, variable);
    }

    fn lookup(&self, name: &str) -> Option<&Rc<RefCell<Variable>>> {
        self.variables.get(name)
    }
}

/// Context that gets passed along to various compiler routines.
struct CompilerContext {
    program: Program,
    scope: Scope,
    errors: Vec<CompilationError>,
}

impl CompilerContext {
    /// Creates an empty, initial context.
    pub fn new() -> CompilerContext {
        CompilerContext {
            program: Program::new(),
            scope: Scope::new(),
            errors: vec![],
        }
    }
}

/// Compiles a CO program.
fn compile(program_ast: ast::Program) -> Result<Program, Vec<CompilationError>> {
    let mut context = CompilerContext::new();

    for statement in program_ast.statements {
        match statement {
            ast::Statement::VarDecl(s) => compile_var_decl(s, &mut context),
            ast::Statement::Read(s) => compile_read(s, &mut context),
            ast::Statement::Write(s) => compile_write(s, &mut context),
        }
    }

    if context.errors.is_empty() {
        Ok(context.program)
    } else {
        Err(context.errors)
    }
}

fn compile_var_decl(statement: ast::VarDeclStmt, context: &mut CompilerContext) {
    let name = &statement.variable_name;

    // Compile the initializer expression even if the variable produces an error.
    let initializer = statement
        .initializer
        .map(|initializer| compile_expression(initializer, context));

    if let Some(_) = context.scope.lookup(name) {
        let error = CompilationError::variable_already_exists(name, statement.span);
        context.errors.push(error);
        return;
    }

    let variable = Variable::new(name.to_string());
    let variable = Rc::new(RefCell::new(variable));

    context.program.add_variable(Rc::clone(&variable));
    context.scope.add(Rc::clone(&variable));

    let statement = program::VarDeclStmt::new(&variable, initializer);
    context.program.add_statement(statement);
}

fn compile_read(statement: ast::ReadStmt, context: &mut CompilerContext) {
    let name = &statement.variable_name;

    if let Some(variable) = context.scope.lookup(&name) {
        let statement = program::ReadStmt::new(&variable);
        context.program.add_statement(statement);
    } else {
        // TODO here and in var_decl: span should be limited to variable name only.
        let error = CompilationError::variable_not_found(name, statement.span);
        context.errors.push(error);
    }
}

fn compile_write(statement: ast::WriteStmt, context: &mut CompilerContext) {
    let expression = compile_expression(statement.expression, context);
    let statement = program::WriteStmt::new(expression);
    context.program.add_statement(statement);
}

fn compile_expression(
    expression: ast::Expression,
    context: &mut CompilerContext,
) -> program::Expression {
    match expression {
        ast::Expression::Variable(e) => compile_variable_expr(e, context),
        ast::Expression::IntLiteral(e) => compile_int_literal_expr(e, context),
        ast::Expression::Add(e) => compile_add_expr(e, context),
    }
}

fn compile_variable_expr(
    expression: ast::VariableExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let name = expression.name;
    if let Some(variable) = context.scope.lookup(&name) {
        program::VariableExpr::new(variable)
    } else {
        let error = CompilationError::variable_not_found(&name, expression.span);
        context.errors.push(error);
        program::Expression::Error
    }
}

fn compile_int_literal_expr(
    expression: ast::IntLiteralExpr,
    _context: &CompilerContext,
) -> program::Expression {
    program::IntLiteralExpr::new(expression.value)
}

fn compile_add_expr(
    expression: ast::AddExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let lhs = compile_expression(*expression.lhs, context);
    let rhs = compile_expression(*expression.rhs, context);
    program::AddExpr::new(lhs, rhs)
}
