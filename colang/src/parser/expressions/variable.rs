//! Variable reference expression parser.

use crate::ast;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;

pub struct VariableExpr;

impl Parser for VariableExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        Identifier::parse(input).map(|name| {
            ast::ExpressionLike::Variable(ast::VariableExpr {
                span: name.span,
                name,
            })
        })
    }
}
