use crate::ast::InputSpan;
use crate::program::{SourceOrigin, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub mod address;
pub mod array_from_copy;
pub mod array_from_elements;
pub mod block;
pub mod boolean_op;
pub mod call;
pub mod deref;
pub mod field_access;
pub mod if_;
pub mod is;
pub mod literal;
pub mod new;
pub mod null;
pub mod variable;

pub struct Expression {
    kind: ExpressionKind,
    type_: Rc<RefCell<Type>>,
    value_category: ValueCategory,

    dirty: bool,
}

impl Expression {
    pub fn new(kind: ExpressionKind, types: &mut TypeRegistry) -> Expression {
        let type_ = kind.calculate_type(types);
        let value_category = kind.calculate_value_category();

        Expression {
            kind,
            type_,
            value_category,
            dirty: false,
        }
    }

    pub fn empty(location: SourceOrigin, types: &TypeRegistry) -> Expression {
        Expression {
            kind: ExpressionKind::Empty(location),
            type_: Rc::clone(&types.void()),
            value_category: ValueCategory::Rvalue,
            dirty: false,
        }
    }

    pub fn error(span: InputSpan) -> Expression {
        Expression {
            kind: ExpressionKind::Error(SourceOrigin::Plain(span)),
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

    pub fn is_empty(&self) -> bool {
        match self.kind {
            ExpressionKind::Empty(_) => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self.kind {
            ExpressionKind::Error(_) => true,
            _ => false,
        }
    }

    pub fn location(&self) -> SourceOrigin {
        self.kind.location()
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
    Is(is::IsExpr),
    Null(null::NullExpr),
    ArrayFromElements(array_from_elements::ArrayFromElementsExpr),
    ArrayFromCopy(array_from_copy::ArrayFromCopyExpr),
    FieldAccess(field_access::FieldAccessExpr),
    Call(call::CallExpr),
    BooleanOp(boolean_op::BooleanOpExpr),
    If(if_::IfExpr),
    Block(block::BlockExpr),

    Empty(SourceOrigin),
    Error(SourceOrigin),
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
            Is(expr) => expr.calculate_type(types),
            Null(expr) => expr.calculate_type(types),
            ArrayFromElements(expr) => expr.calculate_type(types),
            ArrayFromCopy(expr) => expr.calculate_type(types),
            FieldAccess(expr) => expr.calculate_type(types),
            Call(expr) => expr.calculate_type(types),
            BooleanOp(expr) => expr.calculate_type(types),
            If(expr) => expr.calculate_type(types),
            Block(expr) => expr.calculate_type(types),

            Empty(_) => Rc::clone(types.void()),
            Error(_) => Type::error(),
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
            Is(expr) => expr.calculate_value_category(),
            Null(expr) => expr.calculate_value_category(),
            ArrayFromElements(expr) => expr.calculate_value_category(),
            ArrayFromCopy(expr) => expr.calculate_value_category(),
            FieldAccess(expr) => expr.calculate_value_category(),
            Call(expr) => expr.calculate_value_category(),
            BooleanOp(expr) => expr.calculate_value_category(),
            If(expr) => expr.calculate_value_category(),
            Block(expr) => expr.calculate_value_category(),

            Empty(_) => ValueCategory::Rvalue,
            Error(_) => ValueCategory::Rvalue,
        }
    }

    pub fn location(&self) -> SourceOrigin {
        use ExpressionKind::*;
        match self {
            Variable(expr) => expr.location(),
            Literal(expr) => expr.location(),
            Address(expr) => expr.location(),
            Deref(expr) => expr.location(),
            New(expr) => expr.location(),
            Is(expr) => expr.location(),
            Null(expr) => expr.location(),
            ArrayFromElements(expr) => expr.location(),
            ArrayFromCopy(expr) => expr.location(),
            FieldAccess(expr) => expr.location(),
            Call(expr) => expr.location(),
            BooleanOp(expr) => expr.location(),
            If(expr) => expr.location(),
            Block(expr) => expr.location(),

            Empty(location) => *location,
            Error(location) => *location,
        }
    }
}

trait ExpressionKindImpl {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>>;
    fn calculate_value_category(&self) -> ValueCategory;
    fn location(&self) -> SourceOrigin;
}
