//! String literal parser.

use crate::ast;
use crate::parser::common::{parse_from_next_string_token, SkipIgnoredByPrimaryTokenizer};
use crate::parser::prelude::*;
use crate::parser::seq::PanicIfMissing;
use crate::parser::tokens::string::StringTokenPayload;

pub struct StringLiteralExpr;

impl Parser for StringLiteralExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq4<
            PanicIfMissing<SkipIgnoredByPrimaryTokenizer>,
            AbortIfMissing<DoubleQuote>,
            // TODO: think if we can/should recover.
            PanicIfMissing<RepeatZeroOrMore<StringCharacter, DontRecover>>,
            DoubleQuoteOrSynthesize,
        >>::parse(input)
        .map(|(_, open, chars, end)| {
            let span = open + end;
            ast::ExpressionLike::StringLiteral(ast::StringLiteralExpr {
                value: chars.into_iter().collect(),
                span,
            })
        })
    }
}

struct StringCharacter;

impl Parser for StringCharacter {
    type N = char;

    fn parse(input: Input) -> ParseResult<Self::N> {
        parse_from_next_string_token(input, |token| match token.payload {
            StringTokenPayload::SingleQuote => Some('\''),
            StringTokenPayload::Character(c) => Some(c),
            _ => None,
        })
    }
}
