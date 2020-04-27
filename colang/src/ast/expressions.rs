//! Expressions are code that produces a value. Most of the CO source code is comprised of
//! expressions.

use crate::ast::statements::Statement;
use crate::ast::type_expressions::TypeExpr;
use crate::ast::Identifier;
use crate::source::InputSpan;
use enum_dispatch::enum_dispatch;
use std::fmt::{self, Display, Formatter};

#[enum_dispatch]
#[derive(Debug)]
pub enum Expression {
    Variable(VariableExpr),
    IntLiteral(IntLiteralExpr),
    BoolLiteral(BoolLiteralExpr),
    CharLiteral(CharLiteralExpr),
    StringLiteral(StringLiteralExpr),
    Null(NullExpr),
    Self_(SelfExpr),
    UnaryOp(UnaryOperatorExpr),
    BinaryOp(BinaryOperatorExpr),
    Address(AddressExpr),
    Deref(DerefExpr),
    New(NewExpr),
    Is(IsExpr),
    ArrayFromElements(ArrayFromElementsExpr),
    ArrayFromCopy(ArrayFromCopyExpr),
    Index(IndexExpr),
    Call(CallExpr),
    FieldAccess(FieldAccessExpr),
    MethodCall(MethodCallExpr),
    If(IfExpr),
    Block(BlockExpr),
}

// We don't expose the generated methods of `Expression` to the outside, keeping usage of
// `enum_dispatch` as opaque as possible.
#[enum_dispatch(Expression)]
trait ExpressionKind {
    fn span_(&self) -> InputSpan;
    fn span_mut_(&mut self) -> &mut InputSpan;
}

// It would be nice to provide a #[derive(...)] macro, but this would require creating (and
// potentially publishing) a separate crate just for the sake of this macro, which we don't
// really want to do.
macro_rules! impl_expr_kind {
    ($type_name:ty) => {
        impl ExpressionKind for $type_name {
            fn span_(&self) -> InputSpan {
                self.span
            }

            fn span_mut_(&mut self) -> &mut InputSpan {
                &mut self.span
            }
        }
    };
}

impl Expression {
    pub fn span(&self) -> InputSpan {
        self.span_()
    }

    pub fn map_span(mut self, f: impl FnOnce(InputSpan) -> InputSpan) -> Expression {
        *self.span_mut_() = f(self.span_());
        self
    }
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Identifier,

    pub span: InputSpan,
}
impl_expr_kind!(VariableExpr);

#[derive(Debug)]
pub struct IntLiteralExpr {
    pub value: i32,

    pub span: InputSpan,
}
impl_expr_kind!(IntLiteralExpr);

#[derive(Debug)]
pub struct BoolLiteralExpr {
    pub value: bool,

    pub span: InputSpan,
}
impl_expr_kind!(BoolLiteralExpr);

#[derive(Debug)]
pub struct CharLiteralExpr {
    pub value: String,

    pub span: InputSpan,
}
impl_expr_kind!(CharLiteralExpr);

#[derive(Debug)]
pub struct StringLiteralExpr {
    pub value: String,

    pub span: InputSpan,
}
impl_expr_kind!(StringLiteralExpr);

#[derive(Debug)]
pub struct NullExpr {
    pub span: InputSpan,
}
impl_expr_kind!(NullExpr);

#[derive(Debug)]
pub struct SelfExpr {
    pub span: InputSpan,
}
impl_expr_kind!(SelfExpr);

#[derive(Debug)]
pub struct UnaryOperatorExpr {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(UnaryOperatorExpr);

#[derive(Debug)]
pub struct BinaryOperatorExpr {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(BinaryOperatorExpr);

#[derive(Debug)]
pub struct AddressExpr {
    pub target: Box<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(AddressExpr);

#[derive(Debug)]
pub struct DerefExpr {
    pub pointer: Box<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(DerefExpr);

#[derive(Debug)]
pub struct NewExpr {
    pub target_type: TypeExpr,

    pub span: InputSpan,
}
impl_expr_kind!(NewExpr);

#[derive(Debug)]
pub struct IsExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(IsExpr);

#[derive(Debug)]
pub struct ArrayFromElementsExpr {
    pub elements: Vec<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(ArrayFromElementsExpr);

#[derive(Debug)]
pub struct ArrayFromCopyExpr {
    pub element: Box<Expression>,
    pub size: Box<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(ArrayFromCopyExpr);

#[derive(Debug)]
pub struct IndexExpr {
    pub collection: Box<Expression>,
    pub index: Box<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(IndexExpr);

#[derive(Debug)]
pub struct FieldAccessExpr {
    pub receiver: Box<Expression>,
    pub field: Identifier,

    pub span: InputSpan,
}
impl_expr_kind!(FieldAccessExpr);

#[derive(Debug)]
pub struct CallExpr {
    pub function_name: Identifier,
    pub arguments: Vec<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(CallExpr);

#[derive(Debug)]
pub struct MethodCallExpr {
    pub receiver: Box<Expression>,
    pub method: Identifier,
    pub arguments: Vec<Expression>,

    pub span: InputSpan,
}
impl_expr_kind!(MethodCallExpr);

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Option<Box<Expression>>,

    pub span: InputSpan,
}
impl_expr_kind!(IfExpr);

#[derive(Debug)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
    pub final_expr: Option<Box<Expression>>,

    pub span: InputSpan,
}
impl_expr_kind!(BlockExpr);

#[derive(Copy, Clone, Debug)]
pub enum UnaryOperator {
    LogicalNot,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            UnaryOperator::LogicalNot => "!",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Eq,
    NotEq,
    LogicalAnd,
    LogicalOr,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BinaryOperator::*;
        let symbol = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Less => "<",
            Greater => ">",
            LessEq => "<=",
            GreaterEq => ">=",
            Eq => "==",
            NotEq => "!=",
            LogicalAnd => "&&",
            LogicalOr => "||",
        };
        write!(f, "{}", symbol)
    }
}
