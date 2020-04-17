use super::compile_expression;
use crate::analyzer::bodies::call_and_remember;
use crate::errors::CompilationError;
use crate::program::{InternalFunctionTag, TypeId};
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_binary_op_expr(
    expression: ast::BinaryOperatorExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let lhs = compile_expression(*expression.lhs, None, context);
    let rhs = compile_expression(*expression.rhs, None, context);

    if lhs.is_error() || rhs.is_error() {
        return program::Expression::error(expression.span);
    }

    let tag = match expression.operator {
        ast::BinaryOperator::Add => match lhs.type_().borrow().type_id {
            TypeId::Int => Some(InternalFunctionTag::AddInt),
            TypeId::String => Some(InternalFunctionTag::StringAdd),
            _ => None,
        },
        ast::BinaryOperator::Sub => Some(InternalFunctionTag::SubInt),
        ast::BinaryOperator::Mul => Some(InternalFunctionTag::MulInt),
        ast::BinaryOperator::Less => Some(InternalFunctionTag::LessInt),
        ast::BinaryOperator::Greater => Some(InternalFunctionTag::GreaterInt),
        ast::BinaryOperator::LessEq => Some(InternalFunctionTag::LessEqInt),
        ast::BinaryOperator::GreaterEq => Some(InternalFunctionTag::GreaterEqInt),
        ast::BinaryOperator::Eq => match lhs.type_().borrow().type_id {
            TypeId::Int => Some(InternalFunctionTag::EqInt),
            TypeId::String => Some(InternalFunctionTag::StringEq),
            _ => None,
        },
        ast::BinaryOperator::NotEq => match lhs.type_().borrow().type_id {
            TypeId::Int => Some(InternalFunctionTag::NotEqInt),
            TypeId::String => Some(InternalFunctionTag::StringNotEq),
            _ => None,
        },
    };

    let tag = match tag {
        Some(tag) => tag,
        None => {
            let error = CompilationError::binary_operator_unsupported_types(
                &expression.operator.to_string(),
                &lhs.type_().borrow().name,
                &rhs.type_().borrow().name,
                expression.span,
            );
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let function = Rc::clone(context.program.internal_function(tag));

    let result = call_and_remember(function, vec![lhs, rhs], expression.span, context);
    match result {
        Ok(expr) => expr,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
