use crate::source::SourceOrigin;

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
pub enum Instruction {
    Read(read::ReadInstruction),
    Write(write::WriteInstruction),
    While(while_::WhileInstruction),
    Assign(assign::AssignInstruction),
    Eval(eval::EvalInstruction),
    Return(return_::ReturnInstruction),
}

impl Instruction {
    /// The location in source code which produced this instruction.
    pub fn location(&self) -> SourceOrigin {
        match self {
            Instruction::Read(instruction) => instruction.location,
            Instruction::Write(instruction) => instruction.location,
            Instruction::While(instruction) => instruction.location,
            Instruction::Assign(instruction) => instruction.location,
            Instruction::Eval(instruction) => instruction.expression.location(),
            Instruction::Return(instruction) => instruction.location,
        }
    }
}
