use crate::program::{dual, Expression};
use crate::source::{InputSpan, SourceOrigin};
use enum_dispatch::enum_dispatch;

pub mod assign;
pub mod eval;
pub mod if_;
pub mod read;
pub mod return_;
pub mod semicolon;
pub mod while_;
pub mod write;

/// A fragment of imperative CO code that does not evaluate to a value.
#[enum_dispatch]
pub enum Statement {
    Assign(assign::AssignStmt),
    Block(dual::block::Block),
    Call(dual::call::Call),
    Eval(eval::EvalStmt),
    If(if_::IfStmt),
    Read(read::ReadStmt),
    Return(return_::ReturnStmt),
    Semicolon(semicolon::SemicolonStmt),
    While(while_::WhileStmt),
    Write(write::WriteStmt),
}

/// Common behavior for all kinds of statements.
#[enum_dispatch(Statement)]
pub trait StatementKind {
    fn location(&self) -> SourceOrigin;
}

impl Statement {
    /// Creates a statement standing for a code with already reported errors.
    pub fn error(span: InputSpan) -> Statement {
        Statement::Eval(eval::EvalStmt {
            expression: Expression::error(span),
        })
    }
}
