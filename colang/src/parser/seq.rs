//! Sequencing parser combinator.

use crate::parser::prelude::*;
use std::marker::PhantomData;

/// Signifies what the sequence parser should do if an item can't be constructed from input.
pub enum OnMissingStrategyOutput<N> {
    Abort,
    Synthesize(N),
}

/// Decides what the sequence parser should do if an item can't be constructed from input.
pub trait OnMissingStrategy {
    type N;
    fn on_missing(location: InputSpan) -> OnMissingStrategyOutput<Self::N>;
}

/// Implementation of `OnMissingStrategy` that aborts the parse.
///
/// External users should use `AbortIfMissing` wrapper instead of using this type directly.
pub struct AbortIfMissingStrategy<N> {
    phantom: PhantomData<N>,
}

impl<N> OnMissingStrategy for AbortIfMissingStrategy<N> {
    type N = N;

    fn on_missing(_: InputSpan) -> OnMissingStrategyOutput<Self::N> {
        OnMissingStrategyOutput::Abort
    }
}

pub trait Item {
    type P: Parser;
    type O: OnMissingStrategy<N = <Self::P as Parser>::N>;
}

/// Wraps a `Parser` into an `Item` with an aborting `OnMissingStrategy`.
pub struct AbortIfMissing<P: Parser> {
    phantom: PhantomData<P>,
}

impl<P: Parser> Item for AbortIfMissing<P> {
    type P = P;
    type O = AbortIfMissingStrategy<P::N>;
}

/// Defines an `Item` on top of a parser with a recovering `OnMissingStrategy`.
///
/// Unlike `AbortIfMissing` this is a trait that has to be implemented by a concrete type, which
/// cannot be done inline.
pub trait SynthesizeIfMissing {
    type P: Parser;

    fn synthesize(location: InputSpan) -> <Self::P as Parser>::N;
}

pub struct SynthesizeIfMissingStrategy<S: SynthesizeIfMissing> {
    phantom: PhantomData<S>,
}

impl<S: SynthesizeIfMissing> OnMissingStrategy for SynthesizeIfMissingStrategy<S> {
    type N = <S::P as Parser>::N;
    fn on_missing(location: InputSpan) -> OnMissingStrategyOutput<Self::N> {
        OnMissingStrategyOutput::Synthesize(S::synthesize(location))
    }
}

impl<S: SynthesizeIfMissing> Item for S {
    type P = S::P;
    type O = SynthesizeIfMissingStrategy<S>;
}

pub struct PanicIfMissingStrategy<N> {
    phantom: PhantomData<N>,
}

impl<N> OnMissingStrategy for PanicIfMissingStrategy<N> {
    type N = N;

    fn on_missing(_: InputSpan) -> OnMissingStrategyOutput<Self::N> {
        unreachable!()
    }
}

/// Wraps a `Parser` into an `Item` that maps `Missing` case into `Option::None`.
pub struct Optional<P: Parser> {
    phantom: PhantomData<P>,
}

pub struct OptionalParser<P: Parser> {
    phantom: PhantomData<P>,
}

impl<P: Parser> Parser for OptionalParser<P> {
    type N = Option<P::N>;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        let ParseResult(node, input) = <WithIgnored<P>>::parse(input, ctx);
        let node = match node {
            ParsedNode::Ok(node) => ParsedNode::Ok(Some(node)),
            ParsedNode::Recovered(node, errors) => ParsedNode::Recovered(Some(node), errors),
            ParsedNode::Missing(_) => ParsedNode::Ok(None),
        };
        ParseResult(node, input)
    }
}

impl<P: Parser> Item for Optional<P> {
    type P = OptionalParser<P>;
    type O = PanicIfMissingStrategy<<Self::P as Parser>::N>;
}

/// Sequencing parser combinator constructor for 2-element sequences.
pub struct Seq2<I1: Item, I2: Item> {
    phantom: PhantomData<(I1, I2)>,
}

impl<I1: Item, I2: Item> Parser for Seq2<I1, I2> {
    type N = (<I1::P as Parser>::N, <I2::P as Parser>::N);

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        let initial_input = input;
        let mut errors = Vec::new();
        let ParseResult(first, input) = <WithIgnored<I1::P>>::parse(input, ctx);
        let first = match first {
            ParsedNode::Ok(first) => first,
            ParsedNode::Recovered(first, mut es) => {
                errors.append(&mut es);
                first
            }
            ParsedNode::Missing(error) => match I1::O::on_missing(input.span_of_first(ctx)) {
                OnMissingStrategyOutput::Synthesize(first) => {
                    errors.push(error);
                    first
                }
                OnMissingStrategyOutput::Abort => {
                    return ParseResult(ParsedNode::Missing(error), initial_input)
                }
            },
        };

        let ParseResult(second, input) = <WithIgnored<I2::P>>::parse(input, ctx);
        let second = match second {
            ParsedNode::Ok(second) => second,
            ParsedNode::Recovered(second, mut es) => {
                errors.append(&mut es);
                second
            }
            ParsedNode::Missing(error) => match I2::O::on_missing(input.span_of_first(ctx)) {
                OnMissingStrategyOutput::Synthesize(second) => {
                    errors.push(error);
                    second
                }
                OnMissingStrategyOutput::Abort => {
                    return ParseResult(ParsedNode::Missing(error), initial_input)
                }
            },
        };

        ParseResult(ParsedNode::new((first, second), errors), input)
    }
}

// Unfortunately we don't have variadic generics, so we have to do this.

/// Sequencing parser combinator constructor for 3-element sequences.
pub struct Seq3<I1: Item, I2: Item, I3: Item> {
    phantom: PhantomData<(I1, I2, I3)>,
}

impl<I1: Item, I2: Item, I3: Item> Parser for Seq3<I1, I2, I3> {
    type N = (
        <I1::P as Parser>::N,
        <I2::P as Parser>::N,
        <I3::P as Parser>::N,
    );

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq2<I1, AbortIfMissing<Seq2<I2, I3>>>>::parse(input, ctx)
            .map(|(i1, (i2, i3))| (i1, i2, i3))
    }
}

/// Sequencing parser combinator constructor for 4-element sequences.
pub struct Seq4<I1: Item, I2: Item, I3: Item, I4: Item> {
    phantom: PhantomData<(I1, I2, I3, I4)>,
}

impl<I1: Item, I2: Item, I3: Item, I4: Item> Parser for Seq4<I1, I2, I3, I4> {
    type N = (
        <I1::P as Parser>::N,
        <I2::P as Parser>::N,
        <I3::P as Parser>::N,
        <I4::P as Parser>::N,
    );

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq2<I1, AbortIfMissing<Seq3<I2, I3, I4>>>>::parse(input, ctx)
            .map(|(i1, (i2, i3, i4))| (i1, i2, i3, i4))
    }
}

/// Sequencing parser combinator constructor for 5-element sequences.
pub struct Seq5<I1: Item, I2: Item, I3: Item, I4: Item, I5: Item> {
    phantom: PhantomData<(I1, I2, I3, I4, I5)>,
}

impl<I1: Item, I2: Item, I3: Item, I4: Item, I5: Item> Parser for Seq5<I1, I2, I3, I4, I5> {
    type N = (
        <I1::P as Parser>::N,
        <I2::P as Parser>::N,
        <I3::P as Parser>::N,
        <I4::P as Parser>::N,
        <I5::P as Parser>::N,
    );

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq2<I1, AbortIfMissing<Seq4<I2, I3, I4, I5>>>>::parse(input, ctx)
            .map(|(i1, (i2, i3, i4, i5))| (i1, i2, i3, i4, i5))
    }
}

/// Sequencing parser combinator constructor for 6-element sequences.
pub struct Seq6<I1: Item, I2: Item, I3: Item, I4: Item, I5: Item, I6: Item> {
    phantom: PhantomData<(I1, I2, I3, I4, I5, I6)>,
}

impl<I1: Item, I2: Item, I3: Item, I4: Item, I5: Item, I6: Item> Parser
    for Seq6<I1, I2, I3, I4, I5, I6>
{
    type N = (
        <I1::P as Parser>::N,
        <I2::P as Parser>::N,
        <I3::P as Parser>::N,
        <I4::P as Parser>::N,
        <I5::P as Parser>::N,
        <I6::P as Parser>::N,
    );

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq2<I1, AbortIfMissing<Seq5<I2, I3, I4, I5, I6>>>>::parse(input, ctx)
            .map(|(i1, (i2, i3, i4, i5, i6))| (i1, i2, i3, i4, i5, i6))
    }
}
