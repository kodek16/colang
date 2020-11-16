//! Parser for identifiers.

use crate::ast;
use crate::parser::common::parse_from_next_primary_token;
use crate::parser::prelude::*;
use crate::parser::tokens::primary::PrimaryTokenPayload;

pub struct Identifier;

impl Parser for Identifier {
    type N = ast::Identifier;

    fn parse(input: Input) -> ParseResult<Self::N> {
        parse_from_next_primary_token(input, |token| match token.payload {
            PrimaryTokenPayload::Ident(text) => Some(ast::Identifier {
                text,
                span: token.span,
            }),
            _ => None,
        })
    }
}
