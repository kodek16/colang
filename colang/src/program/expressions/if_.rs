use crate::ast::InputSpan;
use crate::program::{checks, Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};

use crate::errors::CompilationError;

use crate::program::expressions::ExpressionKindImpl;
use std::cell::RefCell;
use std::rc::Rc;

pub struct IfExpr {
    pub cond: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Box<Expression>,
    pub span: Option<InputSpan>,
}

impl IfExpr {
    pub fn new(
        cond: Expression,
        then: Expression,
        else_: Option<Expression>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        checks::check_condition_is_bool(&cond, types)?;

        let then_type = &then.type_;

        if else_.is_none() && *then_type != *types.void() {
            let error = CompilationError::if_expression_missing_else(
                &then_type.borrow().name(),
                then.span()
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

        let kind = ExpressionKind::If(IfExpr {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_),
            span: Some(span),
        });

        Ok(Expression::new(kind, types))
    }
}

impl ExpressionKindImpl for IfExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(self.then.type_())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        match (self.then.value_category(), self.else_.value_category()) {
            (ValueCategory::Lvalue, ValueCategory::Lvalue) => ValueCategory::Lvalue,
            _ => ValueCategory::Rvalue,
        }
    }

    fn span(&self) -> Option<InputSpan> {
        self.span
    }
}
