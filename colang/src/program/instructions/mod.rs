pub mod assign;
pub mod eval;
pub mod while_;
pub mod write;

pub enum Instruction {
    Write(write::WriteInstruction),
    While(while_::WhileInstruction),
    Assign(assign::AssignInstruction),
    Eval(eval::EvalInstruction),
}
