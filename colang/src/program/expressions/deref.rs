use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that accesses the value pointer to by some pointer.
///
/// This expression is always an lvalue.
pub struct DerefExpr {
    /// The expression evaluating to the pointer to the target value.
    ///
    /// Must have a pointer type.
    pub pointer: Box<Expression>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for DerefExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        if let Some(target_type) = self.pointer.type_().borrow().pointer_target_type() {
            target_type
        } else {
            panic!(
                "DerefExpr is in an invalid state: pointer expression type is `{}`",
                self.pointer.type_().borrow().name
            )
        }
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Lvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for DerefExpr {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.pointer)
    }
}
