//! Definitions of CO statement and expression nodes.
//!
//! In CO, expressions (code that produces a value on execution) cannot be distinguished from
//! statements (code that does not produce a value) at the parser level. Their disambiguation is
//! made later, during the analysis phase.
//!
//! To represent these ambiguous objects, two types are provided in `colang::ast`.

use crate::ast::expressions::ExpressionLike;
use crate::ast::type_expressions::TypeExpr;
use crate::ast::Identifier;
use crate::source::InputSpan;

#[derive(Debug)]
pub enum StmtOrExpr {
    // Definitely statements:
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
    While(WhileStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Semicolon(SemicolonStmt),

    // Either a statement or an expression:
    ExprLike(ExpressionLike),
}

impl StmtOrExpr {
    pub fn span(&self) -> InputSpan {
        match self {
            StmtOrExpr::VarDecl(VarDeclStmt { span, .. }) => *span,
            StmtOrExpr::Read(ReadStmt { span, .. }) => *span,
            StmtOrExpr::Write(WriteStmt { span, .. }) => *span,
            StmtOrExpr::While(WhileStmt { span, .. }) => *span,
            StmtOrExpr::Assign(AssignStmt { span, .. }) => *span,
            StmtOrExpr::Return(ReturnStmt { span, .. }) => *span,
            StmtOrExpr::Semicolon(SemicolonStmt { span }) => *span,
            StmtOrExpr::ExprLike(e) => e.span(),
        }
    }
}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub entries: Vec<VarDeclEntry>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct VarDeclEntry {
    pub variable_name: Identifier,
    pub variable_type: Option<TypeExpr>,
    pub initializer: Option<ExpressionLike>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReadStmt {
    pub entries: Vec<ReadEntry>,
    pub whole_line: bool,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReadEntry {
    pub target: ExpressionLike,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WriteStmt {
    pub expression: ExpressionLike,
    pub newline: bool,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Box<ExpressionLike>,
    pub body: Box<ExpressionLike>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub lhs: Box<ExpressionLike>,
    pub rhs: Box<ExpressionLike>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expression: Option<ExpressionLike>,

    pub span: InputSpan,
}

/// A no-op statement produced from a semicolon.
///
/// `SemicolonStmt` is a curious case. In CO, semicolons are somewhat similar to Rust in that
/// they force expressions to be statements, dropping their value. Unlike Rust, from a syntactic
/// perspective semicolons are not _required_ for this: even without semicolons expression values
/// will be dropped where context requires this. Semicolons in CO behave exactly as an empty
/// statement, which is mostly useful for syntactic purposes (delimitation), and not semantic.
#[derive(Debug)]
pub struct SemicolonStmt {
    pub span: InputSpan,
}
