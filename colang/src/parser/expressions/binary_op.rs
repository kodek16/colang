//! Binary operator expressions parser.
//!
//! "Binary operator expressions" do not necessarily have to contain a binary operator: an
//! expression that _may_ be used in a binary operator context is itself a
//! "binary operator expression". Put another way, a binary operator expression is a tree of
//! primary expressions joined by binary operators.
//!
//! Only a handful of expression kinds are not binary operator expressions (for the sake of
//! syntax clarity), but they can be converted to binary operator expressions by being wrapped
//! in parentheses.

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

    fn parse(input: Input) -> ParseResult<Self::N> {
        Expr2::parse(input)
    }
}

/// A "precedence tier" definition for operators of the same precedence with left associativity.
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

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq2<
            AbortIfMissing<Lower>,
            AbortIfMissing<
                RepeatZeroOrMore<Seq2<AbortIfMissing<Ops>, OrSynthesizeExpr<Lower>>, DontRecover>,
            >,
        >>::parse(input)
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

/// A convenience substitute trait for `Parser` that assumes `N` to be `ast::BinaryOperator`.
pub trait OpParser {
    fn parse(input: Input) -> ParseResult<ast::BinaryOperator>;
}

impl<P: OpParser> Parser for P {
    type N = ast::BinaryOperator;

    fn parse(input: Input) -> ParseResult<Self::N> {
        P::parse(input)
    }
}

// Tier 0: "primary" expressions.

type Expr0 = PrimaryExpr;

// Tier 1: multiplication and friends.

struct Ops1;

impl OpParser for Ops1 {
    fn parse(input: Input) -> ParseResult<BinaryOperator> {
        <OneOf2<Mul, Div>>::parse(input)
    }
}

struct Mul;
struct Div;

impl OpParser for Mul {
    fn parse(input: Input) -> ParseResult<BinaryOperator> {
        Asterisk::parse(input).map(|_| ast::BinaryOperator::Mul)
    }
}

impl OpParser for Div {
    fn parse(input: Input) -> ParseResult<BinaryOperator> {
        Slash::parse(input).map(|_| ast::BinaryOperator::Div)
    }
}

type Expr1 = InfixLeftPrecedenceTier<Expr0, Ops1>;

// Tier 2: addition and friends.

struct Ops2;

impl OpParser for Ops2 {
    fn parse(input: Input) -> ParseResult<BinaryOperator> {
        <OneOf2<Add, Sub>>::parse(input)
    }
}

struct Add;
struct Sub;

impl OpParser for Add {
    fn parse(input: Input) -> ParseResult<BinaryOperator> {
        Plus::parse(input).map(|_| ast::BinaryOperator::Add)
    }
}

impl OpParser for Sub {
    fn parse(input: Input) -> ParseResult<BinaryOperator> {
        Minus::parse(input).map(|_| ast::BinaryOperator::Sub)
    }
}

type Expr2 = InfixLeftPrecedenceTier<Expr1, Ops2>;
