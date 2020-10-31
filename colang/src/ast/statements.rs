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

    // Either a statement or an expression:
    ExprLike(ExpressionLike),
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
