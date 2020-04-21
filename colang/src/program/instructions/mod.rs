use crate::source::SourceOrigin;

pub mod assign;
pub mod eval;
pub mod while_;
pub mod return_;
pub mod write;

pub enum Instruction {
    Write(write::WriteInstruction),
    While(while_::WhileInstruction),
    Assign(assign::AssignInstruction),
    Eval(eval::EvalInstruction),
    Return(return_::ReturnInstruction),
}

impl Instruction {
    pub fn location(&self) -> SourceOrigin {
        match self {
            Instruction::Write(instruction) => instruction.location,
            Instruction::While(instruction) => instruction.location,
            Instruction::Assign(instruction) => instruction.location,
            Instruction::Eval(instruction) => instruction.expression.location(),
            Instruction::Return(instruction) => instruction.location,
        }
    }
}
