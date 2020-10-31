//! Statement/expression parser.
//!
//! In CO syntax, the boundary between statements and expressions is a bit vague (consider for
//! example a function call: without knowing if the function is void or not, we cannot say if
//! the call is a statement or an expression). What we do is delay the disambiguation between
//! the two until the later analysis phase.

use crate::ast;
use crate::parser::expressions::int_literal::IntLiteralExpr;
use crate::parser::prelude::*;

pub struct StmtOrExpr;

impl Parser for StmtOrExpr {
    type N = ast::StmtOrExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        IntLiteralExpr::parse(input, ctx).map(ast::StmtOrExpr::ExprLike)
    }
}
