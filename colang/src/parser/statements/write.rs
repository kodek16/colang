//! Parser for `write`/`writeln` statements.

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::ExprLikeOrSynthesize;
use crate::parser::terminals;

pub struct WriteStmt;

impl Parser for WriteStmt {
    type N = ast::StmtOrExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq2<AbortIfMissing<OneOf2<KwWrite, KwWriteLn>>, ExprLikeOrSynthesize>>::parse(input).map(
            |((newline, span), expression)| {
                ast::StmtOrExpr::Write(ast::WriteStmt {
                    expression,
                    newline,
                    span,
                })
            },
        )
    }
}

struct KwWrite;

impl Parser for KwWrite {
    // `true` means newline is set. For `KwWrite` this is `false`.
    type N = (bool, InputSpan);

    fn parse(input: Input) -> ParseResult<Self::N> {
        terminals::KwWrite::parse(input).map(|span| (false, span))
    }
}

struct KwWriteLn;

impl Parser for KwWriteLn {
    // `true` means newline is set. For `KwWriteLn` this is `true`.
    type N = (bool, InputSpan);

    fn parse(input: Input) -> ParseResult<Self::N> {
        terminals::KwWriteLn::parse(input).map(|span| (true, span))
    }
}
