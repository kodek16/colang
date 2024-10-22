use super::compile_expression;
use crate::analyzer::bodies::check_argument_types;
use crate::context::CompilerContext;
use crate::errors;
use crate::program::{InternalFunctionTag, TypeId};
use crate::source::{InputSpan, SourceOrigin};
use crate::{ast, program};
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

    match expression.operator {
        ast::BinaryOperator::LogicalAnd | ast::BinaryOperator::LogicalOr => {
            compile_logical_binary_op_expr(expression.operator, lhs, rhs, expression.span, context)
        }
        _ => compile_method_backed_binary_op_expr(
            expression.operator,
            lhs,
            rhs,
            expression.span,
            context,
        ),
    }
}

fn compile_method_backed_binary_op_expr(
    operator: ast::BinaryOperator,
    lhs: program::Expression,
    rhs: program::Expression,
    span: InputSpan,
    context: &mut CompilerContext,
) -> program::Expression {
    let tag = match operator {
        ast::BinaryOperator::Add => match lhs.type_().borrow().type_id {
            TypeId::Int => Some(InternalFunctionTag::AddInt),
            TypeId::String => Some(InternalFunctionTag::StringAdd),
            _ => None,
        },
        ast::BinaryOperator::Sub => Some(InternalFunctionTag::SubInt),
        ast::BinaryOperator::Mul => Some(InternalFunctionTag::MulInt),
        ast::BinaryOperator::Div => Some(InternalFunctionTag::DivInt),
        ast::BinaryOperator::Mod => Some(InternalFunctionTag::ModInt),
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
        ast::BinaryOperator::LogicalAnd | ast::BinaryOperator::LogicalOr => {
            panic!("Logical `and` and `or` operators must be handled separately")
        }
    };

    let tag = match tag {
        Some(tag) => tag,
        None => {
            let error = errors::binary_operator_unsupported_types(
                &operator.to_string(),
                &lhs,
                &rhs,
                SourceOrigin::Plain(span),
            );
            context.errors.push(error);
            return program::Expression::error(span);
        }
    };

    let function = Rc::clone(context.program.internal_function(tag));
    let arguments = vec![lhs, rhs];

    if check_argument_types(&function, &arguments, span, context).is_err() {
        return program::Expression::error(span);
    }

    program::Expression::new(
        program::Call {
            function,
            arguments,
            location: SourceOrigin::Plain(span),
        },
        context.program.types_mut(),
    )
}

fn compile_logical_binary_op_expr(
    operator: ast::BinaryOperator,
    lhs: program::Expression,
    rhs: program::Expression,
    span: InputSpan,
    context: &mut CompilerContext,
) -> program::Expression {
    for operand in &[&lhs, &rhs] {
        if !operand.type_().borrow().is_bool() {
            let error = errors::logical_operator_operand_wrong_type(&operator.to_string(), operand);
            context.errors.push(error);
        }
    }

    let op = match operator {
        ast::BinaryOperator::LogicalAnd => program::BooleanOp::And(Box::new(lhs), Box::new(rhs)),
        ast::BinaryOperator::LogicalOr => program::BooleanOp::Or(Box::new(lhs), Box::new(rhs)),
        _ => panic!("`{}` is not a logical operator", operator.to_string()),
    };

    program::Expression::new(
        program::BooleanOpExpr {
            op,
            location: SourceOrigin::Plain(span),
        },
        context.program.types_mut(),
    )
}
