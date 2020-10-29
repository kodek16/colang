//! "Scalar" (simply named) type expression parser.

use crate::ast;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;

pub struct ScalarTypeExpr;

impl Parser for ScalarTypeExpr {
    type N = ast::TypeExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        Identifier::parse(input, ctx).map(|name| {
            ast::TypeExpr::Scalar(ast::ScalarTypeExpr {
                span: name.span,
                name,
            })
        })
    }
}
