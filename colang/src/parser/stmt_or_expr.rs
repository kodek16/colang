//! Statement/expression parser.
//!
//! In CO syntax, the boundary between statements and expressions is a bit vague (consider for
//! example a function call: without knowing if the function is void or not, we cannot say if
//! the call is a statement or an expression). What we do is delay the disambiguation between
//! the two until the later analysis phase.

use crate::ast;
use crate::parser::expressions::int_literal::IntLiteralExpr;
use crate::parser::prelude::*;

pub struct StmtOrExpr;

impl Parser for StmtOrExpr {
    type N = ast::StmtOrExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        IntLiteralExpr::parse(input, ctx).map(ast::StmtOrExpr::ExprLike)
    }
}

/// Parses a statement or expression, and emits an error if the result is definitely a statement.
pub struct ExprLike;

impl Parser for ExprLike {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        StmtOrExpr::parse(input, ctx).bind(|stmt_or_expr| match stmt_or_expr {
            ast::StmtOrExpr::ExprLike(e) => ParsedNode::Ok(e),
            s @ _ => {
                let error = SyntaxError::StatementInExprContext(s.span());
                ParsedNode::Recovered(synthetic_null_expr(s.span()), vec![error])
            }
        })
    }
}

pub struct StmtOrExprOrSynthesize;

impl SynthesizeIfMissing for StmtOrExprOrSynthesize {
    type P = StmtOrExpr;

    fn synthesize(location: InputSpan) -> ast::StmtOrExpr {
        ast::StmtOrExpr::ExprLike(synthetic_null_expr(location))
    }
}

pub struct ExprLikeOrSynthesize;

impl SynthesizeIfMissing for ExprLikeOrSynthesize {
    type P = ExprLike;

    fn synthesize(location: InputSpan) -> ast::ExpressionLike {
        synthetic_null_expr(location)
    }
}

fn synthetic_null_expr(span: InputSpan) -> ast::ExpressionLike {
    ast::ExpressionLike::Null(ast::NullExpr { span })
}
