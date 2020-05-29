use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that compares two pointers checking if they point to the same value.
pub struct IsExpr {
    /// The expression evaluating to the first of the pointers being compared.
    ///
    /// Must have a pointer type.
    pub lhs: Box<Expression>,

    /// The expression evaluating to the second of the pointers being compared.
    ///
    /// Must have the same type as `lhs`.
    pub rhs: Box<Expression>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for IsExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(types.bool())
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for IsExpr {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::vec::IntoIter<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        vec![&mut *self.lhs, &mut *self.rhs].into_iter()
    }
}
