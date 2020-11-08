//! `if` expression/statement parser.

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::ExprLikeOrSynthesize;

pub struct IfExpr;

impl Parser for IfExpr {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq5<
            AbortIfMissing<KwIf>,
            LeftParenOrSynthesize,
            ExprLikeOrSynthesize,
            RightParenOrSynthesize,
            ExprLikeOrSynthesize,
        >>::parse(input, ctx)
        .map(|(kw_if, _, cond, _, then)| {
            let span = kw_if + then.span();
            ast::ExpressionLike::If(ast::IfExpr {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: None,
                span,
            })
        })
    }
}
