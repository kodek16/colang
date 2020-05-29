use crate::source::SourceOrigin;
use enum_dispatch::enum_dispatch;

pub mod assign;
pub mod eval;
pub mod read;
pub mod return_;
pub mod while_;
pub mod write;

/// A fragment of imperative CO code that does not evaluate to a value.
#[enum_dispatch]
pub enum Statement {
    Read(read::ReadStmt),
    Write(write::WriteStmt),
    While(while_::WhileStmt),
    Assign(assign::AssignStmt),
    Eval(eval::EvalStmt),
    Return(return_::ReturnStmt),
}

/// Common behavior for all kinds of statements.
#[enum_dispatch(Statement)]
pub trait StatementKind {
    fn location(&self) -> SourceOrigin;
}
