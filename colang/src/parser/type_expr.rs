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

pub struct TypeExprOrSynthesize;

impl SynthesizeIfMissing for TypeExprOrSynthesize {
    type P = TypeExpr;

    fn synthesize(location: InputSpan) -> ast::TypeExpr {
        ast::TypeExpr::Scalar(ast::ScalarTypeExpr {
            name: ast::Identifier {
                text: "<missing>".to_string(),
                span: location,
            },
            span: location,
        })
    }
}
