//! Variable reference expression parser.

use crate::ast;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;

pub struct VariableExpr;

impl Parser for VariableExpr {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        Identifier::parse(input, ctx).map(|name| {
            ast::ExpressionLike::Variable(ast::VariableExpr {
                span: name.span,
                name,
            })
        })
    }
}
