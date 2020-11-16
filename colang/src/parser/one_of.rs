//! Parser combinators for nodes with multiple "alternative" productions.

use crate::parser::common::{ParseResult, ParsedNode, Parser};
use crate::parser::input::Input;
use std::marker::PhantomData;

pub struct OneOf2<P1: Parser, P2: Parser<N = P1::N>> {
    phantom: PhantomData<(P1, P2)>,
}

impl<P1: Parser, P2: Parser<N = P1::N>> Parser for OneOf2<P1, P2> {
    type N = P1::N;

    fn parse(input: Input) -> ParseResult<Self::N> {
        let ParseResult(node, input) = P1::parse(input);
        match node {
            ParsedNode::Ok(_) => ParseResult(node, input),
            ParsedNode::Recovered(_, _) => ParseResult(node, input),
            ParsedNode::Missing(_) => P2::parse(input),
        }
    }
}

pub struct OneOf3<P1: Parser, P2: Parser<N = P1::N>, P3: Parser<N = P1::N>> {
    phantom: PhantomData<(P1, P2, P3)>,
}

impl<P1: Parser, P2: Parser<N = P1::N>, P3: Parser<N = P1::N>> Parser for OneOf3<P1, P2, P3> {
    type N = P1::N;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <OneOf2<P1, OneOf2<P2, P3>>>::parse(input)
    }
}

pub struct OneOf4<P1: Parser, P2: Parser<N = P1::N>, P3: Parser<N = P1::N>, P4: Parser<N = P1::N>> {
    phantom: PhantomData<(P1, P2, P3, P4)>,
}

impl<P1: Parser, P2: Parser<N = P1::N>, P3: Parser<N = P1::N>, P4: Parser<N = P1::N>> Parser
    for OneOf4<P1, P2, P3, P4>
{
    type N = P1::N;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <OneOf2<P1, OneOf3<P2, P3, P4>>>::parse(input)
    }
}

pub struct OneOf5<
    P1: Parser,
    P2: Parser<N = P1::N>,
    P3: Parser<N = P1::N>,
    P4: Parser<N = P1::N>,
    P5: Parser<N = P1::N>,
> {
    phantom: PhantomData<(P1, P2, P3, P4, P5)>,
}

impl<
        P1: Parser,
        P2: Parser<N = P1::N>,
        P3: Parser<N = P1::N>,
        P4: Parser<N = P1::N>,
        P5: Parser<N = P1::N>,
    > Parser for OneOf5<P1, P2, P3, P4, P5>
{
    type N = P1::N;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <OneOf2<P1, OneOf4<P2, P3, P4, P5>>>::parse(input)
    }
}
