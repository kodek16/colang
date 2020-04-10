use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{Expression, Variable};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub struct AllocInstruction {
    pub(crate) variable: Rc<RefCell<Variable>>,
    initializer: Option<Expression>,
}

impl AllocInstruction {
    pub fn new(
        variable: &Rc<RefCell<Variable>>,
        initializer: Option<Expression>,
        location: InputSpan,
    ) -> Result<Instruction, CompilationError> {
        {
            let variable = variable.borrow();
            let name = &variable.name;
            let type_ = &variable.type_;

            if let Some(ref initializer) = initializer {
                let initializer_type = &initializer.type_;
                if *initializer_type != *type_ {
                    let error = CompilationError::variable_initializer_type_mismatch(
                        name,
                        type_.borrow().name(),
                        initializer_type.borrow().name(),
                        location,
                    );
                    return Err(error);
                }
            }
        }

        let statement = Instruction::Alloc(AllocInstruction {
            variable: Rc::clone(variable),
            initializer,
        });
        Ok(statement)
    }

    pub fn variable(&self) -> impl Deref<Target = Variable> + '_ {
        self.variable.borrow()
    }

    pub fn initializer(&self) -> Option<&Expression> {
        self.initializer.as_ref()
    }
}
