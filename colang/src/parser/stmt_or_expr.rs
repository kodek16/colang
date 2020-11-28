//! Statement/expression parser.
//!
//! In CO syntax, the boundary between statements and expressions is a bit vague (consider for
//! example a function call: without knowing if the function is void or not, we cannot say if
//! the call is a statement or an expression). What we do is delay the disambiguation between
//! the two until the later analysis phase.

use crate::ast;
use crate::parser::expressions::binary_op::BinaryOperatorExpr;
use crate::parser::expressions::block::BlockExpr;
use crate::parser::expressions::if_::IfExpr;
use crate::parser::prelude::*;
use crate::parser::statements::assign::AssignStmt;
use crate::parser::statements::semicolon::SemicolonStmt;
use crate::parser::statements::var_decl::VarDeclStmt;
use crate::parser::statements::while_::WhileStmt;
use crate::parser::statements::write::WriteStmt;
use std::marker::PhantomData;

pub struct StmtOrExpr;

impl Parser for StmtOrExpr {
    type N = ast::StmtOrExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <OneOf6<
            VarDeclStmt,
            WhileStmt,
            WriteStmt,
            SemicolonStmt,
            AssignStmt,
            WrapExprLike<ExprLike>,
        >>::parse(input)
    }
}

/// Parses only expression-like nodes.
pub struct ExprLike;

impl Parser for ExprLike {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <OneOf3<IfExpr, BlockExpr, BinaryOperatorExpr>>::parse(input)
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

struct WrapExprLike<P: Parser<N = ast::ExpressionLike>> {
    phantom: PhantomData<P>,
}

impl<P: Parser<N = ast::ExpressionLike>> Parser for WrapExprLike<P> {
    type N = ast::StmtOrExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        P::parse(input).map(ast::StmtOrExpr::ExprLike)
    }
}