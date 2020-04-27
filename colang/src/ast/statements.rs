//! Statements are code that does not produce a value. They must be enclosed in a block (which
//! itself is an expression, see `BlockExpr`).

use crate::ast::expressions::Expression;
use crate::ast::type_expressions::TypeExpr;
use crate::ast::Identifier;
use crate::source::InputSpan;

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
    While(WhileStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
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
    pub initializer: Option<Expression>,

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
    pub target: Expression,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WriteStmt {
    pub expression: Expression,
    pub newline: bool,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Box<Expression>,
    pub body: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expression: Option<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expression: Expression,

    pub span: InputSpan,
}
