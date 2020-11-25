use crate::ast;
pub use crate::parser::common::SyntaxError;
use crate::parser::common::{ParseResult, ParsedNode, Parser};
use crate::parser::input::Input;
use crate::source::InputSpanFile;

mod comma_list;
mod common;
mod expressions;
mod function_def;
mod ident;
mod input;
mod normal_param_def;
mod one_of;
mod param_def;
mod param_list;
mod prelude;
mod program;
mod repeat;
mod scalar_type_expr;
mod seq;
mod statements;
mod stmt_or_expr;
mod terminals;
mod tokens;
mod type_expr;

/// Parses an entire source file, constructing an `ast::Program`.
///
/// If any errors are encountered, a best-effort recovered version of the program is returned
/// alongside the errors.
pub fn parse(
    source_code: &str,
    file: InputSpanFile,
) -> Result<ast::Program, (ast::Program, Vec<SyntaxError>)> {
    let input = Input::new(source_code, file);

    let ParseResult(program, _) = program::Program::parse(input);
    match program {
        ParsedNode::Ok(program) => Ok(program),
        ParsedNode::Recovered(program, errors) => Err((program, errors)),
        ParsedNode::Missing(error) => Err((ast::Program::empty(), vec![error])),
    }
}
