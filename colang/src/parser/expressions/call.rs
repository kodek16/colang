//! Function call expression parser.

use crate::ast;
use crate::parser::comma_list::CommaSeparated;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;
use crate::parser::seq::PanicIfMissing;
use crate::parser::stmt_or_expr::ExprLike;

pub struct CallExpr;

impl Parser for CallExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq4<
            AbortIfMissing<Identifier>,
            AbortIfMissing<LeftParen>,
            PanicIfMissing<CommaSeparated<ExprLike>>,
            RightParenOrSynthesize,
        >>::parse(input)
        .map(|(function, _, args, closing)| {
            ast::ExpressionLike::Call(ast::CallExpr {
                span: function.span + closing,
                function_name: function,
                arguments: args,
            })
        })
    }
}
