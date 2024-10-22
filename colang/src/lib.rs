//! This crate contains the CO compiler _frontend_: the part of the compiler which
//! visitors the source code into an intermediate representation defined in `program` and
//! subsequently consumed by some compiler _backend_.
//!
//! All static checks are performed during this phase. The generated IR is guaranteed to be
//! valid: any errors it might contain indicate compiler bugs, and not user errors.

use lalrpop_util::lalrpop_mod;

mod analyzer;
mod ast;
mod context;
mod escapes;
mod scope;
mod utils;
lalrpop_mod!(grammar);

pub mod backends;
pub mod errors;
pub mod program;
pub mod source;
pub mod stdlib;

use crate::analyzer::GlobalVisitor;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::program::visitors::valid::ValidityChecker;
use crate::scope::FunctionEntity;
use crate::source::InputSpanFile;
use std::rc::Rc;

/// Compiles a CO program, performs static checks, and returns the intermediate representation.
///
/// Representation returned from this function is typically then passed to a backend: either
/// an interpreter backend that runs the program, or a translator backend that translates it into
/// a target language.
pub fn compile(source_code: &str) -> Result<program::Program, Vec<CompilationError>> {
    let std_ast = parse(stdlib::STD_SOURCE, InputSpanFile::Std)
        .map_err(|err| vec![errors::syntax_error(err, InputSpanFile::Std)])?;

    let program_ast = parse(&source_code, InputSpanFile::UserProgram)
        .map_err(|err| vec![errors::syntax_error(err, InputSpanFile::UserProgram)])?;

    let mut program = analyze(vec![std_ast, program_ast]).map_err(|(_, errors)| errors)?;

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

/// Compiles a CO program and prints a textual representation of the generated IR.
///
/// This function prints the program even if it is invalid because of some encountered errors.
/// To see these errors, `compile` should be used.
pub fn debug(source_code: &str) -> Result<(), ()> {
    let std_ast = parse(stdlib::STD_SOURCE, InputSpanFile::Std).expect("Syntax error in stdlib");
    let program_ast =
        parse(&source_code, InputSpanFile::UserProgram).expect("Syntax error in source file");

    let result = analyze(vec![std_ast, program_ast]);
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

/// Parses the source code of a file and returns an AST root node.
fn parse(source_code: &str, file: InputSpanFile) -> Result<ast::Program, ast::ParseError> {
    grammar::ProgramParser::new().parse(file, source_code)
}

/// Constructs an intermediate representation of a program and performs all static error checking.
fn analyze(
    mut sources: Vec<ast::Program>,
) -> Result<program::Program, (program::Program, Vec<CompilationError>)> {
    let mut context = CompilerContext::new();

    // Initialize all defined types (and base types of type templates).
    analyzer::basic_types::BasicTypesAnalyzerPass.run(sources.iter_mut().collect(), &mut context);

    // Link all types to the traits they claim to implement.
    analyzer::traits::basic::BasicTraitsAnalyzerPass
        .run(sources.iter_mut().collect(), &mut context);

    // Collect all global information.
    analyzer::global_structure::GlobalStructureAnalyzerPass
        .run(sources.iter_mut().collect(), &mut context);

    // Check and wire trait implementations.
    analyzer::traits::wiring::TraitWiringAnalyzerPass
        .run(sources.iter_mut().collect(), &mut context);

    // Complete all types referenced globally.
    analyzer::complete_types::CompleteTypesAnalyzerPass
        .run(sources.iter_mut().collect(), &mut context);

    // Compile all defined function bodies.
    analyzer::bodies::BodiesAnalyzerPass.run(sources.iter_mut().collect(), &mut context);

    // Instantiate all template method bodies for methods called by other functions.
    analyzer::function_instantiations::FunctionInstantiationsAnalyzerPass
        .run(sources.iter_mut().collect(), &mut context);

    // Remove all template base types and their methods from the program.
    remove_technical_entities(&mut context.program);

    // Sort types topologically according to fields links.
    sort_types(&mut context);

    let main_function = context.scope.lookup::<FunctionEntity>("main");
    if let Ok(main_function) = main_function {
        context.program.main_function = Some(main_function);
    } else {
        let error = errors::main_function_not_found();
        context.errors.push(error);
    }

    if context.errors.is_empty() {
        Ok(context.program)
    } else {
        Err((context.program, context.errors))
    }
}

/// Removes all technical types and functions from the program.
///
/// This allows to hide some compile-time abstractions (templates, traits, etc.) from the backends
/// completely.
///
/// This is safe to do only when all required template instances are created.
fn remove_technical_entities(program: &mut program::Program) {
    // TODO(#24): once all functions are aware of whether they are technical, this can be omitted
    // altogether, and the backend can easily filter out technical entities where they need to.
    let mut functions_to_remove = Vec::new();
    let mut types_to_remove = Vec::new();

    for type_ in program.types().all_types() {
        if type_.borrow().is_technical() {
            types_to_remove.push(Rc::clone(type_));
            for method in &type_.borrow().methods {
                functions_to_remove.push(Rc::clone(&method));
            }
        }
    }

    for function in functions_to_remove {
        program.remove_function(&function.borrow());
    }

    for type_ in types_to_remove {
        program.types_mut().remove_type(&type_.borrow());
    }
}

/// Performs topological sorting for all types and reports any found type cycles as errors.
fn sort_types(context: &mut CompilerContext) {
    let result = context.program.types().all_types_sorted();
    match result {
        Ok(types) => context.program.sorted_types = Some(types),
        Err(cycle) => {
            let error = errors::type_cycle_through_fields(cycle);
            context.errors.push(error);
        }
    }
}
