use crate::ast::InputSpan;
use crate::program::{checks, Expression, ExpressionKind, ValueCategory};
use crate::typing::TypeRegistry;

use crate::errors::CompilationError;

use std::rc::Rc;

#[derive(Debug)]
pub struct IfExpr {
    cond: Box<Expression>,
    then: Box<Expression>,
    else_: Box<Expression>,
}

impl IfExpr {
    pub fn new(
        cond: Expression,
        then: Expression,
        else_: Option<Expression>,
        types: &TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        checks::check_condition_is_bool(&cond, types)?;

        let then_type = &then.type_;

        if else_.is_none() && *then_type != *types.void() {
            let error = CompilationError::if_expression_missing_else(
                &then_type.borrow().name(),
                then.span
                    .expect("Generated `then` block expression in a single-branch `if`"),
            );
            return Err(error);
        }

        let else_ = else_.unwrap_or_else(|| Expression::empty(types));
        let else_type = &else_.type_;

        if then_type != else_type {
            let error = CompilationError::if_expression_branch_type_mismatch(
                &then_type.borrow().name(),
                &else_type.borrow().name(),
                span,
            );
            return Err(error);
        }

        let type_ = Rc::clone(then_type);

        let value_category = match (then.value_category, else_.value_category) {
            (ValueCategory::Lvalue, ValueCategory::Lvalue) => ValueCategory::Lvalue,
            _ => ValueCategory::Rvalue,
        };

        let kind = ExpressionKind::If(IfExpr {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_),
        });

        Ok(Expression {
            kind,
            type_,
            value_category,
            span: Some(span),
        })
    }

    pub fn cond(&self) -> &Expression {
        &self.cond
    }

    pub fn then(&self) -> &Expression {
        &self.then
    }

    pub fn else_(&self) -> &Expression {
        &self.else_
    }
}
