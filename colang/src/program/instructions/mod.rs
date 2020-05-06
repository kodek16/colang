use crate::source::SourceOrigin;
use enum_dispatch::enum_dispatch;

pub mod assign;
pub mod eval;
pub mod read;
pub mod return_;
pub mod while_;
pub mod write;

/// A fragment of imperative CO code that does not evaluate to a value.
///
/// "Instructions" in CO IR closely correspond to statements in CO syntax. The different name
/// is mostly because of historical reasons.
#[enum_dispatch]
pub enum Instruction {
    Read(read::ReadInstruction),
    Write(write::WriteInstruction),
    While(while_::WhileInstruction),
    Assign(assign::AssignInstruction),
    Eval(eval::EvalInstruction),
    Return(return_::ReturnInstruction),
}

/// Common behavior for all kinds of instructions.
#[enum_dispatch(Instruction)]
pub trait InstructionKind {
    fn location(&self) -> SourceOrigin;
}
