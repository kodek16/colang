use crate::ast::InputSpan;
use crate::program::{
    AddressExpr, Expression, ExpressionKind, Function, InternalFunctionTag, Program, TypeId,
    ValueCategory,
};

use crate::errors::CompilationError;
use crate::program::Parameter;

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

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

    pub fn new_read(
        target: Expression,
        program: &mut Program,
    ) -> Result<Expression, CompilationError> {
        let target_span = target.span.expect("Attempt to read to generated target");
        if target.value_category != ValueCategory::Lvalue {
            let error = CompilationError::read_target_not_lvalue(
                target.span.expect("Attempt to read generated rvalue."),
            );
            return Err(error);
        }

        let target_type = &target.type_;

        let function = Rc::clone(match target_type.borrow().type_id().clone() {
            TypeId::Int => program.internal_function(InternalFunctionTag::ReadInt),
            TypeId::String => program.internal_function(InternalFunctionTag::ReadWord),
            _ => {
                let error = CompilationError::read_unsupported_type(
                    target_type.borrow().name(),
                    target
                        .span
                        .expect("Attempt to read generated value of unsupported type"),
                );
                return Err(error);
            }
        });

        let argument = AddressExpr::new_synthetic(target, program.types_mut(), target_span);

        Ok(Expression {
            kind: ExpressionKind::Call(CallExpr {
                function,
                arguments: vec![argument],
            }),
            type_: Rc::clone(program.types().void()),
            value_category: ValueCategory::Rvalue,
            span: Some(target_span),
        })
    }

    pub fn function(&self) -> impl Deref<Target = Function> + '_ {
        self.function.borrow()
    }

    pub fn arguments(&self) -> impl Iterator<Item = &Expression> {
        self.arguments.iter()
    }
}
