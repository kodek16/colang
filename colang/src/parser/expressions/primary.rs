//! Primary expression parser.

use crate::ast;
use crate::parser::expressions::block::BlockExpr;
use crate::parser::expressions::bool_literal::BoolLiteralExpr;
use crate::parser::expressions::int_literal::IntLiteralExpr;
use crate::parser::prelude::*;

pub struct PrimaryExpr;

impl Parser for PrimaryExpr {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <OneOf3<BlockExpr, BoolLiteralExpr, IntLiteralExpr>>::parse(input, ctx)
    }
}
