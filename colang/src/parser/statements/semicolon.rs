//! Semicolon parser.

use crate::ast;
use crate::parser::prelude::*;

pub struct SemicolonStmt;

impl Parser for SemicolonStmt {
    type N = ast::StmtOrExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        Semicolon::parse(input).map(|span| ast::StmtOrExpr::Semicolon(ast::SemicolonStmt { span }))
    }
}
