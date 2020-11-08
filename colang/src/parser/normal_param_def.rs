//! Parser for "normal" (not `self`) parameter definitions.

use crate::ast;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;
use crate::parser::type_expr::TypeExprOrSynthesize;

pub struct NormalParameterDef;

impl Parser for NormalParameterDef {
    type N = ast::Parameter;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq3<AbortIfMissing<Identifier>, ColonItem, TypeExprOrSynthesize>>::parse(input, ctx).map(
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
    type P = CharsParser<Colon>;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}
