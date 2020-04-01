use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind, Function, ValueCategory};

use crate::errors::CompilationError;
use crate::program::Parameter;

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub struct CallExpr {
    function: Rc<RefCell<Function>>,
    arguments: Vec<Expression>,
}

impl CallExpr {
    pub fn new(
        function: Rc<RefCell<Function>>,
        arguments: Vec<Expression>,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        {
            let function = function.borrow();
            let function_name = function.name();
            let parameters = function.parameters();

            if parameters.len() != arguments.len() {
                let error = CompilationError::call_wrong_number_of_arguments(
                    function_name,
                    parameters.len(),
                    arguments.len(),
                    span,
                );
                return Err(error);
            }

            for (argument, parameter) in arguments.iter().zip(parameters) {
                let argument_type = &argument.type_;
                let parameter_name = parameter.name();
                let parameter_type = parameter.type_();

                if *argument_type != *parameter_type {
                    let error = CompilationError::call_argument_type_mismatch(
                        parameter_name,
                        parameter_type.borrow().name(),
                        argument_type.borrow().name(),
                        span,
                    );
                    return Err(error);
                }
            }
        }

        let type_ = Rc::clone(&function.borrow().return_type());
        let kind = ExpressionKind::Call(CallExpr {
            function,
            arguments,
        });

        Ok(Expression {
            kind,
            type_,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        })
    }

    pub fn function(&self) -> impl Deref<Target = Function> + '_ {
        self.function.borrow()
    }

    pub fn arguments(&self) -> impl Iterator<Item = &Expression> {
        self.arguments.iter()
    }
}