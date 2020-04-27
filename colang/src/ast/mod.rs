//! Abstract syntax tree representation. This is the interface between the parser (LALRPOP)
//! and the rest of the compiler front-end.

use crate::source::InputSpan;

mod expressions;
mod globals;
mod statements;
mod type_expressions;

pub type ParseError<'a> =
    lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'static str>;

#[derive(Debug)]
pub struct Identifier {
    pub text: String,

    pub span: InputSpan,
}

pub use expressions::*;
pub use globals::*;
pub use statements::*;
pub use type_expressions::*;
