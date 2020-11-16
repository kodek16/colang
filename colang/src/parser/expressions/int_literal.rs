//! Integer literal parser.

use crate::ast;
use crate::parser::common::parse_from_next_primary_token;
use crate::parser::prelude::*;
use crate::parser::tokens::primary::PrimaryTokenPayload;

pub struct IntLiteralExpr;

impl Parser for IntLiteralExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        parse_from_next_primary_token(input, |token| match token.payload {
            PrimaryTokenPayload::Int(value) => {
                Some(ast::ExpressionLike::IntLiteral(ast::IntLiteralExpr {
                    value,
                    span: token.span,
                }))
            }
            _ => None,
        })
    }
}
