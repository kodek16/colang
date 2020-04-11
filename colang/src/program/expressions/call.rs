use crate::ast::InputSpan;
use crate::program::{
    AddressExpr, Expression, ExpressionKind, Function, InternalFunctionTag, Program, Type, TypeId,
    TypeRegistry, ValueCategory,
};

use crate::errors::CompilationError;

use crate::program::expressions::ExpressionKindImpl;
use std::cell::RefCell;
use std::rc::Rc;

pub struct CallExpr {
    pub function: Rc<RefCell<Function>>,
    pub arguments: Vec<Expression>,
}

impl CallExpr {
    pub fn new(
        function: Rc<RefCell<Function>>,
        arguments: Vec<Expression>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        {
            let function = function.borrow();
            let function_name = &function.name;
            let parameters = function.parameters.iter();

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
                let parameter_name = &parameter.borrow().name;
                let parameter_type = &parameter.borrow().type_;

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

        let kind = ExpressionKind::Call(CallExpr {
            function,
            arguments,
        });

        Ok(Expression::new(kind, Some(span), types))
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

        let kind = ExpressionKind::Call(CallExpr {
            function,
            arguments: vec![argument],
        });
        Ok(Expression::new(
            kind,
            Some(target_span),
            program.types_mut(),
        ))
    }
}

impl ExpressionKindImpl for CallExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.function.borrow().return_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }
}
