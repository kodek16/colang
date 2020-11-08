//! Primary expression parser.

use crate::ast;
use crate::parser::expressions::block::BlockExpr;
use crate::parser::expressions::bool_literal::BoolLiteralExpr;
use crate::parser::expressions::int_literal::IntLiteralExpr;
use crate::parser::expressions::variable::VariableExpr;
use crate::parser::prelude::*;

pub struct PrimaryExpr;

impl Parser for PrimaryExpr {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <OneOf4<BlockExpr, BoolLiteralExpr, IntLiteralExpr, VariableExpr>>::parse(input, ctx)
    }
}
