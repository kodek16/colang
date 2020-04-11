use crate::ast::InputSpan;
use crate::program::{Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub mod address;
pub mod array_from_copy;
pub mod array_from_elements;
pub mod block;
pub mod call;
pub mod deref;
pub mod field_access;
pub mod if_;
pub mod literal;
pub mod new;
pub mod variable;

pub struct Expression {
    pub span: Option<InputSpan>,

    kind: ExpressionKind,
    type_: Rc<RefCell<Type>>,
    value_category: ValueCategory,

    dirty: bool,
}

impl Expression {
    pub fn new(
        kind: ExpressionKind,
        span: Option<InputSpan>,
        types: &mut TypeRegistry,
    ) -> Expression {
        let type_ = kind.calculate_type(types);
        let value_category = kind.calculate_value_category();

        Expression {
            span,
            kind,
            type_,
            value_category,
            dirty: false,
        }
    }

    pub fn empty(types: &TypeRegistry) -> Expression {
        Expression {
            span: None,
            kind: ExpressionKind::Empty,
            type_: Rc::clone(&types.void()),
            value_category: ValueCategory::Rvalue,
            dirty: false,
        }
    }

    pub fn error(span: InputSpan) -> Expression {
        Expression {
            span: Some(span),
            kind: ExpressionKind::Error,
            type_: Type::error(),
            value_category: ValueCategory::Rvalue,
            dirty: false,
        }
    }

    pub fn kind(&self) -> &ExpressionKind {
        &self.kind
    }

    /// Provides mutator access to underlying expression.
    /// After using this method, `recalculate` must be called to
    /// update the types.
    pub fn kind_mut(&mut self) -> &mut ExpressionKind {
        self.dirty = true;
        &mut self.kind
    }

    pub fn recalculate(&mut self, types: &mut TypeRegistry) {
        self.type_ = self.kind.calculate_type(types);
        self.value_category = self.kind.calculate_value_category();
        self.dirty = false;
    }

    pub fn is_error(&self) -> bool {
        match self.kind {
            ExpressionKind::Error => true,
            _ => false,
        }
    }

    pub fn type_(&self) -> &Rc<RefCell<Type>> {
        self.panic_if_dirty();
        &self.type_
    }

    pub fn value_category(&self) -> ValueCategory {
        self.panic_if_dirty();
        self.value_category
    }

    fn panic_if_dirty(&self) {
        if self.dirty {
            panic!("Expression type was not recalculated after update.")
        }
    }
}

pub enum ExpressionKind {
    Variable(variable::VariableExpr),
    Literal(literal::LiteralExpr),
    Address(address::AddressExpr),
    Deref(deref::DerefExpr),
    New(new::NewExpr),
    ArrayFromElements(array_from_elements::ArrayFromElementsExpr),
    ArrayFromCopy(array_from_copy::ArrayFromCopyExpr),
    FieldAccess(field_access::FieldAccessExpr),
    Call(call::CallExpr),
    If(if_::IfExpr),
    Block(block::BlockExpr),

    /// A no-op expression of type `void`.
    Empty,

    Error,
}

impl ExpressionKind {
    pub fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        use ExpressionKind::*;
        match self {
            Variable(expr) => expr.calculate_type(types),
            Literal(expr) => expr.calculate_type(types),
            Address(expr) => expr.calculate_type(types),
            Deref(expr) => expr.calculate_type(types),
            New(expr) => expr.calculate_type(types),
            ArrayFromElements(expr) => expr.calculate_type(types),
            ArrayFromCopy(expr) => expr.calculate_type(types),
            FieldAccess(expr) => expr.calculate_type(types),
            Call(expr) => expr.calculate_type(types),
            If(expr) => expr.calculate_type(types),
            Block(expr) => expr.calculate_type(types),

            Empty => Rc::clone(&types.void()),
            Error => Type::error(),
        }
    }

    pub fn calculate_value_category(&self) -> ValueCategory {
        use ExpressionKind::*;
        match self {
            Variable(expr) => expr.calculate_value_category(),
            Literal(expr) => expr.calculate_value_category(),
            Address(expr) => expr.calculate_value_category(),
            Deref(expr) => expr.calculate_value_category(),
            New(expr) => expr.calculate_value_category(),
            ArrayFromElements(expr) => expr.calculate_value_category(),
            ArrayFromCopy(expr) => expr.calculate_value_category(),
            FieldAccess(expr) => expr.calculate_value_category(),
            Call(expr) => expr.calculate_value_category(),
            If(expr) => expr.calculate_value_category(),
            Block(expr) => expr.calculate_value_category(),

            Empty => ValueCategory::Rvalue,
            Error => ValueCategory::Rvalue,
        }
    }
}

trait ExpressionKindImpl {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>>;
    fn calculate_value_category(&self) -> ValueCategory;
}
