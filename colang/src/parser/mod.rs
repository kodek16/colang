mod base;
mod block;
mod common;
mod context;
mod function_def;
mod ident;
mod input;
mod prelude;
mod program;
mod repeat;
mod seq;

use crate::ast;
use crate::parser::common::{ParseResult, ParsedNode, Parser};
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use crate::source::InputSpanFile;

pub use crate::parser::common::SyntaxError;

/// Parses an entire source file, constructing an `ast::Program`.
pub fn parse(source_code: &str, file: InputSpanFile) -> Result<ast::Program, Vec<SyntaxError>> {
    let ctx = ParsingContext { file };
    let input = Input::new(source_code);

    let ParseResult(program, input) = program::Program::parse(input, &ctx);
    assert!(input.is_fully_consumed());
    match program {
        ParsedNode::Ok(program) => Ok(program),
        ParsedNode::Recovered(_, errors) => Err(errors),
        ParsedNode::Missing(error) => Err(vec![error]),
    }
}
