//! Parser combinators for nodes with multiple "alternative" productions.

use crate::parser::common::{ParseResult, Parser};
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use std::marker::PhantomData;

pub struct OneOf2<P1: Parser, P2: Parser<N = P1::N>> {
    phantom: PhantomData<(P1, P2)>,
}

impl<P1: Parser, P2: Parser<N = P1::N>> Parser for OneOf2<P1, P2> {
    type N = P1::N;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        P1::parse(input, ctx).or(|| P2::parse(input, ctx))
    }
}

pub struct OneOf3<P1: Parser, P2: Parser<N = P1::N>, P3: Parser<N = P1::N>> {
    phantom: PhantomData<(P1, P2, P3)>,
}

impl<P1: Parser, P2: Parser<N = P1::N>, P3: Parser<N = P1::N>> Parser for OneOf3<P1, P2, P3> {
    type N = P1::N;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <OneOf2<P1, OneOf2<P2, P3>>>::parse(input, ctx)
    }
}
