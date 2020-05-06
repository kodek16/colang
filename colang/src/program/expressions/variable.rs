use crate::program::expressions::ExpressionKind;
use crate::program::{Type, TypeRegistry, ValueCategory, Variable};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that accesses the value of a variable currently in scope.
///
/// This expression is always an lvalue.
pub struct VariableExpr {
    /// The variable to be accessed.
    ///
    /// Must be in scope at the point where expression is defined.
    pub variable: Rc<RefCell<Variable>>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for VariableExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.variable.borrow().type_)
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Lvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
