use crate::program::expressions::ExpressionKind;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that executes one of its branches depending on the value of the condition.
///
/// The expression value is taken from the value of the executed branch expression.
pub struct IfExpr {
    /// The condition expression that determines which branch is executed.
    ///
    /// Must have type `bool`.
    pub cond: Box<Expression>,

    /// The branch that gets executed when `condition` evaluates to `true`.
    pub then: Box<Expression>,

    /// The branch that gets executed when `condition` evaluated to `false`.
    ///
    /// Must have the same type as `then`.
    pub else_: Box<Expression>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for IfExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(self.then.type_())
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
