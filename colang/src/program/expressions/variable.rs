use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement, Type, TypeRegistry, ValueCategory, Variable};
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

impl<'a> LocalCodeNode<'a> for VariableExpr {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::iter::Empty<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::empty()
    }
}
