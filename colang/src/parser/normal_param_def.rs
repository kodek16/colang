//! Parser for "normal" (not `self`) parameter definitions.

use crate::ast;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;
use crate::parser::type_expr::TypeExprOrSynthesize;

pub struct NormalParameterDef;

impl Parser for NormalParameterDef {
    type N = ast::Parameter;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq3<AbortIfMissing<Identifier>, ColonOrSynthesize, TypeExprOrSynthesize>>::parse(input)
            .map(|(name, _, type_)| {
                ast::Parameter::Normal(ast::NormalParameter {
                    span: name.span + type_.span(),
                    name,
                    type_,
                })
            })
    }
}
