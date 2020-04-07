pub mod alloc;
pub mod assign;
pub mod dealloc;
pub mod eval;
pub mod return_;
pub mod while_;
pub mod write;

#[derive(Debug)]
pub enum Instruction {
    Alloc(alloc::AllocInstruction),
    Dealloc(dealloc::DeallocInstruction),
    Write(write::WriteInstruction),
    While(while_::WhileInstruction),
    Assign(assign::AssignInstruction),
    Return(return_::ReturnInstruction),
    Eval(eval::EvalInstruction),
}
