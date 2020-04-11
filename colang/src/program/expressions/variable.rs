use crate::ast::InputSpan;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory, Variable};

use std::cell::RefCell;
use std::rc::Rc;

pub struct VariableExpr {
    pub variable: Rc<RefCell<Variable>>,
}

impl VariableExpr {
    pub fn new(
        variable: &Rc<RefCell<Variable>>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let kind = ExpressionKind::Variable(VariableExpr {
            variable: Rc::clone(variable),
        });
        Expression::new(kind, Some(span), types)
    }
}

impl ExpressionKindImpl for VariableExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.variable.borrow().type_)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Lvalue
    }
}
