//! Parser for "normal" (not `self`) parameter definitions.

use crate::ast;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;
use crate::parser::type_expr::TypeExpr;

pub struct NormalParameterDef;

impl Parser for NormalParameterDef {
    type N = ast::Parameter;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq3<AbortIfMissing<Identifier>, ColonItem, TypeItem>>::parse(input, ctx).map(
            |(name, _, type_)| {
                ast::Parameter::Normal(ast::NormalParameter {
                    span: name.span + type_.span(),
                    name,
                    type_,
                })
            },
        )
    }
}

struct ColonItem;

impl SynthesizeIfMissing for ColonItem {
    type P = chars::Colon;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

struct TypeItem;

impl SynthesizeIfMissing for TypeItem {
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
