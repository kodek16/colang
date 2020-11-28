//! "Primary" expression parser.
//!
//! A primary expression is an expression that can be used in a binary operator context but is
//! itself not rooted with a binary operator node.

use crate::ast;
use crate::parser::expressions::bool_literal::BoolLiteralExpr;
use crate::parser::expressions::call::CallExpr;
use crate::parser::expressions::char_literal::CharLiteralExpr;
use crate::parser::expressions::int_literal::IntLiteralExpr;
use crate::parser::expressions::parens::ParensExpr;
use crate::parser::expressions::string_literal::StringLiteralExpr;
use crate::parser::expressions::variable::VariableExpr;
use crate::parser::prelude::*;

pub struct PrimaryExpr;

impl Parser for PrimaryExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <OneOf7<
            ParensExpr,
            BoolLiteralExpr,
            IntLiteralExpr,
            CharLiteralExpr,
            StringLiteralExpr,
            CallExpr,
            VariableExpr,
        >>::parse(input)
    }
}
