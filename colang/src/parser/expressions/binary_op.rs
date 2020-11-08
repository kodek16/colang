//! Binary operator expressions parser.

use crate::ast;
use crate::ast::BinaryOperator;
use crate::parser::expressions::primary::PrimaryExpr;
use crate::parser::prelude::*;
use std::marker::PhantomData;

/// Parses any expression that participates in precedence resolution mechanism.
///
/// "Primary" expressions without any binary operators also belong to this parser.
pub struct BinaryOperatorExpr;

impl Parser for BinaryOperatorExpr {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        Expr2::parse(input, ctx)
    }
}

struct InfixLeftPrecedenceTier<
    Lower: Parser<N = ast::ExpressionLike>,
    Ops: Parser<N = ast::BinaryOperator>,
> {
    phantom: PhantomData<(Lower, Ops)>,
}

impl<Lower: Parser<N = ast::ExpressionLike>, Ops: Parser<N = ast::BinaryOperator>> Parser
    for InfixLeftPrecedenceTier<Lower, Ops>
{
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq2<
            AbortIfMissing<Lower>,
            AbortIfMissing<
                RepeatZeroOrMore<Seq2<AbortIfMissing<Ops>, OrSynthesizeExpr<Lower>>, DontRecover>,
            >,
        >>::parse(input, ctx)
        .map(|(head, tail)| {
            tail.into_iter().fold(head, |lhs, (operator, rhs)| {
                ast::ExpressionLike::BinaryOp(ast::BinaryOperatorExpr {
                    span: lhs.span() + rhs.span(),
                    lhs: Box::new(lhs),
                    operator,
                    rhs: Box::new(rhs),
                })
            })
        })
    }
}

struct OrSynthesizeExpr<P: Parser<N = ast::ExpressionLike>> {
    phantom: PhantomData<P>,
}

impl<P: Parser<N = ast::ExpressionLike>> SynthesizeIfMissing for OrSynthesizeExpr<P> {
    type P = P;

    fn synthesize(location: InputSpan) -> ast::ExpressionLike {
        ast::ExpressionLike::Null(ast::NullExpr { span: location })
    }
}

pub trait OpParser {
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, ast::BinaryOperator>;
}

impl<P: OpParser> Parser for P {
    type N = ast::BinaryOperator;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        P::parse(input, ctx)
    }
}

// Tier 0: "primary" expressions.

type Expr0 = PrimaryExpr;

// Tier 1: multiplication and friends.

struct Ops1;

impl OpParser for Ops1 {
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, BinaryOperator> {
        <OneOf2<Mul, Div>>::parse(input, ctx)
    }
}

struct Mul;
struct Div;

impl OpParser for Mul {
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, BinaryOperator> {
        Asterisk::parse(input, ctx).map(|_| ast::BinaryOperator::Mul)
    }
}

impl OpParser for Div {
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, BinaryOperator> {
        Slash::parse(input, ctx).map(|_| ast::BinaryOperator::Div)
    }
}

type Expr1 = InfixLeftPrecedenceTier<Expr0, Ops1>;

// Tier 2: addition and friends.

struct Ops2;

impl OpParser for Ops2 {
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, BinaryOperator> {
        <OneOf2<Add, Sub>>::parse(input, ctx)
    }
}

struct Add;
struct Sub;

impl OpParser for Add {
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, BinaryOperator> {
        Plus::parse(input, ctx).map(|_| ast::BinaryOperator::Add)
    }
}

impl OpParser for Sub {
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, BinaryOperator> {
        Minus::parse(input, ctx).map(|_| ast::BinaryOperator::Sub)
    }
}

type Expr2 = InfixLeftPrecedenceTier<Expr1, Ops2>;
