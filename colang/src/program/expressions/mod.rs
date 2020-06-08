use crate::program::dual;
use crate::program::{Type, TypeRegistry, ValueCategory};
use crate::source::{InputSpan, SourceOrigin};
use enum_dispatch::enum_dispatch;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub mod address;
pub mod array_from_copy;
pub mod array_from_elements;
pub mod boolean_op;
pub mod call;
pub mod deref;
pub mod empty;
pub mod error;
pub mod field_access;
pub mod if_;
pub mod is;
pub mod literal;
pub mod new;
pub mod null;
pub mod variable;

/// A fragment of imperative CO code that evaluates to some value.
///
/// Expressions comprise most of the code in function bodies. Some of them can contain statements
/// (`BlockExpr`, `IfExpr`), which in turn can contain expressions.
///
/// Expressions have a statically determined type and "value category". Value category determines
/// if an expression is assignable, to put it simply.
///
/// Expressions may have type `void`, indicating that they do not in fact produce a useful value,
/// but this is allowed only in a select few "void contexts". If not documented otherwise, it
/// should be assumed that an expression context does not allow `void` expressions.
///
/// The implementation of `Expression` uses caching for type calculation: if an expression is
/// modified through a mutable reference, the modifying code must call `recalculate` after it's
/// done. The caching pattern requires two types: `ExpressionImpl` which contains the actual
/// expression data, and `Expression` which is a wrapper that handles caching. `Expression` can
/// be deref-coerced to `ExpressionImpl`, so `ExpressionImpl` should not be explicitly handled
/// unless there is an actual need (e.g. switching on expression kind).
pub struct Expression {
    // The actual payload.
    impl_: ExpressionImpl,

    // The cached properties.
    type_: Rc<RefCell<Type>>,
    value_category: ValueCategory,

    // The modification flag which is set on mutable borrow and cleaned on `recalculate` call.
    dirty: bool,
}

/// The backing type for `Expression`.
///
/// This type is necessary because of the caching pattern that `Expression` uses. In most cases,
/// `Expression` should be used, as it can be deref-coerced to `ExpressionImpl`.
#[enum_dispatch]
pub enum ExpressionImpl {
    Address(address::AddressExpr),
    ArrayFromCopy(array_from_copy::ArrayFromCopyExpr),
    ArrayFromElements(array_from_elements::ArrayFromElementsExpr),
    Block(dual::block::Block),
    BooleanOp(boolean_op::BooleanOpExpr),
    Call(call::CallExpr),
    Deref(deref::DerefExpr),
    Empty(empty::EmptyExpr),
    Err(error::ErrorExpr),
    FieldAccess(field_access::FieldAccessExpr),
    If(if_::IfExpr),
    Is(is::IsExpr),
    Literal(literal::LiteralExpr),
    New(new::NewExpr),
    Null(null::NullExpr),
    Variable(variable::VariableExpr),
}

/// The common behavior for all kinds of expressions.
#[enum_dispatch(ExpressionImpl)]
pub trait ExpressionKind {
    /// Calculates the expression type.
    ///
    /// This method should NOT be used when possible: use `Expression::type(&Self)` instead which
    /// provides caching.
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>>;

    /// Calculates the expression value category.
    ///
    /// This method should NOT be used when possible: use `Expression::value_category(&Self)`
    /// instead which provides caching.
    fn value_category(&self) -> ValueCategory;

    /// The location of source code that produced this expression.
    fn location(&self) -> SourceOrigin;
}

impl Deref for Expression {
    type Target = ExpressionImpl;

    fn deref(&self) -> &Self::Target {
        self.panic_if_dirty();
        &self.impl_
    }
}

impl DerefMut for Expression {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.dirty = true;
        &mut self.impl_
    }
}

impl Expression {
    /// Creates a new expression.
    ///
    /// Any of the expression kind types can be used as the first parameter. The type and value
    /// category are calculated and cached.
    pub fn new(impl_: impl Into<ExpressionImpl>, types: &mut TypeRegistry) -> Expression {
        let impl_ = impl_.into();
        let type_ = impl_.type_(types);
        let value_category = impl_.value_category();

        Expression {
            impl_,
            type_,
            value_category,
            dirty: false,
        }
    }

    /// Creates a new error expression.
    ///
    /// A helper constructor which conveniently does require access to the type registry.
    pub fn error(span: InputSpan) -> Expression {
        Expression {
            impl_: ExpressionImpl::Err(error::ErrorExpr {
                location: SourceOrigin::Plain(span),
            }),
            type_: Type::error(),
            value_category: ValueCategory::Rvalue,
            dirty: false,
        }
    }

    /// Accesses the expression type.
    ///
    /// The cached type is NOT lazily updated: if it was modified before and `recalculate` was not
    /// called, this causes an error.
    pub fn type_(&self) -> &Rc<RefCell<Type>> {
        self.panic_if_dirty();
        &self.type_
    }

    /// Accesses the expression value category.
    ///
    /// The cached value category is NOT lazily updated: if it was modified before and `recalculate`
    /// was not called, this causes an error.
    pub fn value_category(&self) -> ValueCategory {
        self.panic_if_dirty();
        self.value_category
    }

    /// Recalculates the cached properties: type and value category.
    ///
    /// This method must be called after all mutable borrows as soon as the intended modifications
    /// are done.
    pub fn recalculate(&mut self, types: &mut TypeRegistry) {
        self.type_ = self.impl_.type_(types);
        self.value_category = self.impl_.value_category();
        self.dirty = false;
    }

    fn panic_if_dirty(&self) {
        if self.dirty {
            panic!("Expression type was not recalculated after update.")
        }
    }
}

impl ExpressionImpl {
    pub fn is_empty(&self) -> bool {
        match &self {
            ExpressionImpl::Empty(_) => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match &self {
            ExpressionImpl::Err(_) => true,
            _ => false,
        }
    }
}
