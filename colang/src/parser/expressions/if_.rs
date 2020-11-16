//! `if` expression/statement parser.

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::ExprLikeOrSynthesize;

pub struct IfExpr;

impl Parser for IfExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq6<
            AbortIfMissing<KwIf>,
            LeftParenOrSynthesize,
            ExprLikeOrSynthesize,
            RightParenOrSynthesize,
            ExprLikeOrSynthesize,
            Optional<Seq2<AbortIfMissing<KwElse>, ExprLikeOrSynthesize>>,
        >>::parse(input)
        .map(|(kw_if, _, cond, _, then, else_)| {
            let span = kw_if + then.span() + else_.as_ref().map(|(_, e)| e.span());
            ast::ExpressionLike::If(ast::IfExpr {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: else_.map(|(_, e)| Box::new(e)),
                span,
            })
        })
    }
}
