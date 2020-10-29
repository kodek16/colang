//! Type expression parser.

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::scalar_type_expr::ScalarTypeExpr;

pub struct TypeExpr;

impl Parser for TypeExpr {
    type N = ast::TypeExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        ScalarTypeExpr::parse(input, ctx)
    }
}