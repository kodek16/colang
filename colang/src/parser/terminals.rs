//! Terminal node parsers for nodes that are trivially converted from empty tokens.

use crate::parser::common::{parse_from_next_primary_token, ParseResult, Parser};
use crate::parser::input::Input;
use crate::parser::seq::SynthesizeIfMissing;
use crate::parser::tokens::primary::PrimaryTokenPayload;
use crate::source::InputSpan;

pub struct Colon;
pub struct Semicolon;
pub struct Comma;
pub struct Plus;
pub struct Minus;
pub struct Asterisk;
pub struct Slash;
pub struct SingleEquals;
pub struct DoubleEquals;
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

macro_rules! impl_parser_from_token {
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

impl_parser_from_token!(Colon);
impl_parser_from_token!(Semicolon);
impl_parser_from_token!(Comma);
impl_parser_from_token!(Plus);
impl_parser_from_token!(Minus);
impl_parser_from_token!(Asterisk);
impl_parser_from_token!(Slash);
impl_parser_from_token!(SingleEquals);
impl_parser_from_token!(DoubleEquals);
impl_parser_from_token!(LessEqual);
impl_parser_from_token!(GreaterEqual);
impl_parser_from_token!(Less);
impl_parser_from_token!(Greater);
impl_parser_from_token!(LeftParen);
impl_parser_from_token!(RightParen);
impl_parser_from_token!(LeftBrace);
impl_parser_from_token!(RightBrace);

impl_parser_from_token!(KwElse);
impl_parser_from_token!(KwFalse);
impl_parser_from_token!(KwFun);
impl_parser_from_token!(KwIf);
impl_parser_from_token!(KwTrue);
impl_parser_from_token!(KwVar);

pub struct ColonOrSynthesize;
pub struct LeftParenOrSynthesize;
pub struct RightParenOrSynthesize;
pub struct RightBraceOrSynthesize;

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
