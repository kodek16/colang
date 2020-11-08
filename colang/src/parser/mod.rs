use crate::ast;
pub use crate::parser::common::SyntaxError;
use crate::parser::common::{ParseResult, ParsedNode, Parser};
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use crate::source::InputSpanFile;

mod base;
mod block;
mod chars;
mod comma_list;
mod common;
mod context;
mod equals;
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
mod type_expr;
mod words;

/// Parses an entire source file, constructing an `ast::Program`.
pub fn parse(source_code: &str, file: InputSpanFile) -> Result<ast::Program, Vec<SyntaxError>> {
    let ctx = ParsingContext { file };
    let input = Input::new(source_code);

    let ParseResult(program, _) = program::Program::parse(input, &ctx);
    match program {
        ParsedNode::Ok(program) => Ok(program),
        ParsedNode::Recovered(_, errors) => Err(errors),
        ParsedNode::Missing(error) => Err(vec![error]),
    }
}
