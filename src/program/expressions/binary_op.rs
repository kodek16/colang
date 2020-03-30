use crate::ast::InputSpan;
use crate::program::checks;
use crate::program::{Expression, ExpressionKind, ValueCategory};
use crate::typing::TypeRegistry;

use crate::errors::CompilationError;
use std::rc::Rc;

#[derive(Debug)]
pub struct BinaryOpExpr {
    pub operator: BinaryOperator,
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

impl BinaryOpExpr {
    pub fn new(
        operator: BinaryOperator,
        lhs: Expression,
        rhs: Expression,
        types: &TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        checks::check_operand_is_int(&lhs, types)?;
        checks::check_operand_is_int(&rhs, types)?;

        let type_ = Rc::clone(match operator {
            BinaryOperator::AddInt => types.int(),
            BinaryOperator::SubInt => types.int(),
            BinaryOperator::MulInt => types.int(),
            BinaryOperator::LessInt => types.bool(),
            BinaryOperator::GreaterInt => types.bool(),
            BinaryOperator::LessEqInt => types.bool(),
            BinaryOperator::GreaterEqInt => types.bool(),
            BinaryOperator::EqInt => types.bool(),
            BinaryOperator::NotEqInt => types.bool(),
        });

        let kind = ExpressionKind::BinaryOp(BinaryOpExpr {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });

        Ok(Expression {
            kind,
            type_,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        })
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    AddInt,
    SubInt,
    MulInt,
    LessInt,
    GreaterInt,
    LessEqInt,
    GreaterEqInt,
    EqInt,
    NotEqInt,
}
