//! Compiler frontend transforms the source code into an intermediate representation
//! defined in `program`. All static checks are performed during this phase.

use lalrpop_util::lalrpop_mod;

mod analyzer;
mod ast;
mod scope;

pub mod backends;
pub mod errors;
pub mod program;
pub mod source;
pub mod stdlib;
lalrpop_mod!(pub grammar);

use crate::analyzer::utils::global_visitor::GlobalVisitor;
use crate::errors::CompilationError;
use crate::program::transforms::valid::ValidityChecker;
use crate::program::{Function, Type, TypeTemplate, Variable};
use crate::scope::Scope;
use crate::source::{InputSpan, InputSpanFile};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn run(source_code: &str) -> Result<program::Program, Vec<CompilationError>> {
    let std_ast = parse(stdlib::STD_SOURCE, InputSpanFile::Std)
        .map_err(|err| vec![CompilationError::syntax_error(err, InputSpanFile::Std)])?;

    let program_ast = parse(&source_code, InputSpanFile::UserProgram).map_err(|err| {
        vec![CompilationError::syntax_error(
            err,
            InputSpanFile::UserProgram,
        )]
    })?;

    let mut program = compile(vec![std_ast, program_ast]).map_err(|(_, errors)| errors)?;

    let checker = ValidityChecker::new(&mut program);
    let errors = checker.check();
    if !errors.is_empty() {
        eprintln!("Compiler error: produced program is in an invalid state.");
        for error in errors {
            eprintln!("{}", error);
        }
        panic!("Internal error occurred");
    }

    Ok(program)
}

pub fn run_debug(source_code: &str) -> Result<(), ()> {
    let std_ast = parse(stdlib::STD_SOURCE, InputSpanFile::Std).expect("Syntax error in stdlib");
    let program_ast =
        parse(&source_code, InputSpanFile::UserProgram).expect("Syntax error in source file");

    let result = compile(vec![std_ast, program_ast]);
    let is_ok = result.is_ok();

    let program = match result {
        Ok(program) => program,
        Err((program, _)) => program,
    };
    println!("{}", program);

    if is_ok {
        Ok(())
    } else {
        Err(())
    }
}

/// Parses the source code and returns an AST root.
fn parse(source_code: &str, file: InputSpanFile) -> Result<ast::Program, ast::ParseError> {
    grammar::ProgramParser::new().parse(file, source_code)
}

/// Context that gets passed along to various compiler routines.
pub struct CompilerContext {
    program: program::Program,
    scope: Scope,

    // TODO we should have a separate BodyCompilerContext type that has the fields only relevant
    // in function bodies.
    /// Function currently being analyzed.
    function: Option<Rc<RefCell<Function>>>,

    /// In methods, this is the variable bound to `self`.
    self_: Option<Rc<RefCell<Variable>>>,

    // Keep track of semantic objects created from syntactic definitions so that later passes
    // can easily access them.
    defined_types: HashMap<InputSpan, Rc<RefCell<Type>>>,
    defined_type_templates: HashMap<InputSpan, Rc<RefCell<TypeTemplate>>>,
    defined_functions: HashMap<InputSpan, Rc<RefCell<Function>>>,
    defined_fields: HashMap<InputSpan, Rc<RefCell<Variable>>>,
    defined_methods: HashMap<InputSpan, Rc<RefCell<Function>>>,

    errors: Vec<CompilationError>,
}

impl CompilerContext {
    /// Creates the initial root context.
    pub fn new() -> CompilerContext {
        let mut program = program::Program::new();
        let mut scope = Scope::new();

        for type_ in program.types().basic_types() {
            scope.add_type(Rc::clone(type_)).unwrap();
        }

        program::internal::populate_internal_symbols(&mut program, &mut scope);
        CompilerContext {
            program,
            scope,
            function: None,
            self_: None,
            defined_types: HashMap::new(),
            defined_type_templates: HashMap::new(),
            defined_functions: HashMap::new(),
            defined_fields: HashMap::new(),
            defined_methods: HashMap::new(),
            errors: vec![],
        }
    }
}

/// Compiles a CO program.
fn compile(
    mut sources: Vec<ast::Program>,
) -> Result<program::Program, (program::Program, Vec<CompilationError>)> {
    let mut context = CompilerContext::new();

    // 1st pass: initialize all defined types (and base types of type templates).
    analyzer::incomplete_types::IncompleteTypesAnalyzerPass::new()
        .run(sources.iter_mut().collect(), &mut context);

    // 2nd pass: collect all global information.
    analyzer::global_structure::GlobalStructureAnalyzerPass::new()
        .run(sources.iter_mut().collect(), &mut context);

    // 3rd pass: complete all types referenced globally.
    analyzer::complete_types::CompleteTypesAnalyzerPass::new()
        .run(sources.iter_mut().collect(), &mut context);

    // 4th pass: compile all defined function bodies.
    analyzer::bodies::BodiesAnalyzerPass::new().run(sources.iter_mut().collect(), &mut context);

    // 5th pass: instantiate all template method bodies for methods called by other functions.
    analyzer::function_instantiations::FunctionInstantiationsAnalyzerPass::new()
        .run(sources.iter_mut().collect(), &mut context);

    let main_function = context
        .scope
        .lookup_function("main", InputSpan::top_of_file());
    if let Ok(main_function) = main_function {
        context.program.fill_main_function(Rc::clone(main_function));
    } else {
        let error = CompilationError::main_function_not_found();
        context.errors.push(error);
    }

    if context.errors.is_empty() {
        Ok(context.program)
    } else {
        Err((context.program, context.errors))
    }
}
