use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind, ValueCategory, Variable};

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub struct VariableExpr {
    variable: Rc<RefCell<Variable>>,
}

impl VariableExpr {
    pub fn new(variable: &Rc<RefCell<Variable>>, span: InputSpan) -> Expression {
        let kind = ExpressionKind::Variable(VariableExpr {
            variable: Rc::clone(variable),
        });
        let type_ = Rc::clone(&variable.borrow().type_);
        Expression {
            kind,
            type_,
            value_category: ValueCategory::Lvalue,
            span: Some(span),
        }
    }

    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        self.variable.borrow()
    }
}
