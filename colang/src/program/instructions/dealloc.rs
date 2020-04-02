use crate::program::instructions::Instruction;
use crate::program::Variable;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub struct DeallocInstruction {
    pub(crate) variable: Rc<RefCell<Variable>>,
}

impl DeallocInstruction {
    pub fn new(variable: Rc<RefCell<Variable>>) -> Instruction {
        Instruction::Dealloc(DeallocInstruction { variable })
    }

    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        self.variable.borrow()
    }
}
