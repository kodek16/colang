//! Terminal node parsers for nodes that are trivially converted from empty tokens.

use crate::parser::common::{
    parse_from_next_primary_token, parse_from_next_string_token, ParseResult, Parser,
};
use crate::parser::input::Input;
use crate::parser::seq::SynthesizeIfMissing;
use crate::parser::tokens::primary::PrimaryTokenPayload;
use crate::parser::tokens::string::StringTokenPayload;
use crate::source::InputSpan;

// Primary token-backed terminals:
pub struct Colon;
pub struct Semicolon;
pub struct Comma;
pub struct Plus;
pub struct Minus;
pub struct Asterisk;
pub struct Slash;
pub struct SingleEqual;
pub struct DoubleEqual;
pub struct NotEqual;
pub struct LessEqual;
pub struct GreaterEqual;
pub struct Less;
pub struct Greater;
pub struct LeftParen;
pub struct RightParen;
pub struct LeftBrace;
pub struct RightBrace;

pub struct KwElse;
pub struct KwFalse;
pub struct KwFun;
pub struct KwIf;
pub struct KwTrue;
pub struct KwVar;
pub struct KwWrite;
pub struct KwWriteLn;

// String token-backed terminals:
pub struct SingleQuote;
pub struct DoubleQuote;

macro_rules! impl_parser_from_primary_token {
    ($name: ident) => {
        impl Parser for $name {
            type N = InputSpan;

            fn parse(input: Input) -> ParseResult<InputSpan> {
                parse_from_next_primary_token(input, |token| match token.payload {
                    PrimaryTokenPayload::$name => Some(token.span),
                    _ => None,
                })
            }
        }
    };
}

macro_rules! impl_parser_from_string_token {
    ($name: ident) => {
        impl Parser for $name {
            type N = InputSpan;

            fn parse(input: Input) -> ParseResult<InputSpan> {
                parse_from_next_string_token(input, |token| match token.payload {
                    StringTokenPayload::$name => Some(token.span),
                    _ => None,
                })
            }
        }
    };
}

impl_parser_from_primary_token!(Colon);
impl_parser_from_primary_token!(Semicolon);
impl_parser_from_primary_token!(Comma);
impl_parser_from_primary_token!(Plus);
impl_parser_from_primary_token!(Minus);
impl_parser_from_primary_token!(Asterisk);
impl_parser_from_primary_token!(Slash);
impl_parser_from_primary_token!(SingleEqual);
impl_parser_from_primary_token!(DoubleEqual);
impl_parser_from_primary_token!(NotEqual);
impl_parser_from_primary_token!(LessEqual);
impl_parser_from_primary_token!(GreaterEqual);
impl_parser_from_primary_token!(Less);
impl_parser_from_primary_token!(Greater);
impl_parser_from_primary_token!(LeftParen);
impl_parser_from_primary_token!(RightParen);
impl_parser_from_primary_token!(LeftBrace);
impl_parser_from_primary_token!(RightBrace);

impl_parser_from_primary_token!(KwElse);
impl_parser_from_primary_token!(KwFalse);
impl_parser_from_primary_token!(KwFun);
impl_parser_from_primary_token!(KwIf);
impl_parser_from_primary_token!(KwTrue);
impl_parser_from_primary_token!(KwVar);
impl_parser_from_primary_token!(KwWrite);
impl_parser_from_primary_token!(KwWriteLn);

impl_parser_from_string_token!(SingleQuote);
impl_parser_from_string_token!(DoubleQuote);

pub struct ColonOrSynthesize;
pub struct LeftParenOrSynthesize;
pub struct RightParenOrSynthesize;
pub struct RightBraceOrSynthesize;

pub struct SingleQuoteOrSynthesize;
pub struct DoubleQuoteOrSynthesize;

impl SynthesizeIfMissing for ColonOrSynthesize {
    type P = Colon;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for LeftParenOrSynthesize {
    type P = LeftParen;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for RightParenOrSynthesize {
    type P = RightParen;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for RightBraceOrSynthesize {
    type P = RightBrace;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for SingleQuoteOrSynthesize {
    type P = SingleQuote;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for DoubleQuoteOrSynthesize {
    type P = DoubleQuote;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}
