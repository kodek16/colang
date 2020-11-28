//! Char literal parser.

use crate::ast;
use crate::parser::common::{parse_from_next_string_token, SkipIgnoredByPrimaryTokenizer};
use crate::parser::prelude::*;
use crate::parser::seq::PanicIfMissing;
use crate::parser::tokens::string::StringTokenPayload;

pub struct CharLiteralExpr;

impl Parser for CharLiteralExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq4<
            PanicIfMissing<SkipIgnoredByPrimaryTokenizer>,
            AbortIfMissing<SingleQuote>,
            // TODO: think if we can/should recover.
            PanicIfMissing<RepeatZeroOrMore<CharCharacter, DontRecover>>,
            SingleQuoteOrSynthesize,
        >>::parse(input)
        .map(|(_, open, chars, end)| {
            ast::ExpressionLike::CharLiteral(ast::CharLiteralExpr {
                value: chars.into_iter().collect(),
                span: open + end,
            })
        })
    }
}

/// Parses a valid "character" inside a char literal.
///
/// Char literals normally only have one character, but this check is done later in the analyzer.
struct CharCharacter;

impl Parser for CharCharacter {
    type N = char;

    fn parse(input: Input) -> ParseResult<Self::N> {
        parse_from_next_string_token(input, |token| match token.payload {
            StringTokenPayload::DoubleQuote => Some('"'),
            StringTokenPayload::Character(c) => Some(c),
            _ => None,
        })
    }
}
