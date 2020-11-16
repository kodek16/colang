//! "Scalar" (simply named) type expression parser.

use crate::ast;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;

pub struct ScalarTypeExpr;

impl Parser for ScalarTypeExpr {
    type N = ast::TypeExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        Identifier::parse(input).map(|name| {
            ast::TypeExpr::Scalar(ast::ScalarTypeExpr {
                span: name.span,
                name,
            })
        })
    }
}
