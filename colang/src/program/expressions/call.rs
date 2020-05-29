use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Function, Statement, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that executes a function with certain arguments.
///
/// The return value of the function becomes the value of the expression. If the function returns
/// `void`, the expression is `void`, and so only allowed in void contexts.
pub struct CallExpr {
    /// The function to be executed.
    pub function: Rc<RefCell<Function>>,

    /// The expressions that evaluate to arguments that are passed to `function`.
    ///
    /// The number of arguments must match the number of function parameters, and every argument
    /// must have the same type as its corresponding parameter.
    pub arguments: Vec<Expression>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for CallExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.function.borrow().return_type)
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for CallExpr {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::slice::IterMut<'a, Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        self.arguments.iter_mut()
    }
}
