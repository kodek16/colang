mod address;
mod array_from_copy;
mod array_from_elements;
mod binary_op;
mod block;
mod bool_literal;
mod call;
mod char_literal;
mod deref;
mod field_access;
mod if_;
mod index;
mod int_literal;
mod is;
mod method_call;
mod new;
mod null;
mod self_;
mod string_literal;
mod unary_op;
mod variable;

use crate::analyzer::bodies::dual::DualNode;
use crate::ast;
use crate::context::CompilerContext;
use crate::errors;
use crate::program;
use crate::program::{ExpressionKind, Type};
use std::cell::RefCell;
use std::rc::Rc;

/// Compiles a syntax-expression without requiring it to be a semantic expression.
///
/// There is a limit to how well can the parser distinguish expressions from statements,
/// so some "syntax-expressions" (e.g. no-value function calls) actually correspond to
/// statements in the program IR.
///
/// This function returns a `DualNode` object which can hold either an expression or a statement.
/// The sister `compile_expression` function forces the syntax-expression to be a true expression,
/// and reports an error otherwise.
pub fn compile_expression_or_statement(
    expression: ast::ExpressionLike,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> DualNode {
    let node = match expression {
        ast::ExpressionLike::Block(e) => block::compile_block(e, type_hint, context),
        ast::ExpressionLike::Call(e) => call::compile_call(e, context),
        ast::ExpressionLike::If(e) => if_::compile_if(e, type_hint, context),
        ast::ExpressionLike::MethodCall(e) => method_call::compile_method_call(e, context),

        other => DualNode::Expression(match other {
            ast::ExpressionLike::Variable(e) => variable::compile_variable_expr(e, context),
            ast::ExpressionLike::IntLiteral(e) => int_literal::compile_int_literal_expr(e, context),
            ast::ExpressionLike::BoolLiteral(e) => {
                bool_literal::compile_bool_literal_expr(e, context)
            }
            ast::ExpressionLike::CharLiteral(e) => {
                char_literal::compile_char_literal_expr(e, context)
            }
            ast::ExpressionLike::StringLiteral(e) => {
                string_literal::compile_string_literal_expr(e, context)
            }
            ast::ExpressionLike::Null(e) => null::compile_null_expr(e, type_hint, context),
            ast::ExpressionLike::Self_(e) => self_::compile_self_expr(e, context),
            ast::ExpressionLike::UnaryOp(e) => unary_op::compile_unary_op_expression(e, context),
            ast::ExpressionLike::BinaryOp(e) => binary_op::compile_binary_op_expr(e, context),
            ast::ExpressionLike::Address(e) => address::compile_address_expr(e, type_hint, context),
            ast::ExpressionLike::Deref(e) => deref::compile_deref_expr(e, type_hint, context),
            ast::ExpressionLike::New(e) => new::compile_new_expr(e, context),
            ast::ExpressionLike::Is(e) => is::compile_is_expr(e, context),
            ast::ExpressionLike::ArrayFromElements(e) => {
                array_from_elements::compile_array_from_elements_expr(e, type_hint, context)
            }
            ast::ExpressionLike::ArrayFromCopy(e) => {
                array_from_copy::compile_array_from_copy_expr(e, context)
            }
            ast::ExpressionLike::Index(e) => index::compile_index_expr(e, context),
            ast::ExpressionLike::FieldAccess(e) => {
                field_access::compile_field_access_expr(e, context)
            }

            ast::ExpressionLike::Block(_) => unreachable!(),
            ast::ExpressionLike::Call(_) => unreachable!(),
            ast::ExpressionLike::If(_) => unreachable!(),
            ast::ExpressionLike::MethodCall(_) => unreachable!(),
        }),
    };

    // All expressions must have fully complete types.
    if let DualNode::Expression(ref expression) = node {
        let result = Type::ensure_is_fully_complete(
            Rc::clone(expression.type_()),
            context.program.types_mut(),
        );
        if let Err(type_chain) = result {
            let error = errors::type_infinite_dependency_chain(
                &expression.type_().borrow(),
                type_chain,
                expression.location(),
            );
            context.errors.push(error);
        }
    }

    node
}

/// Compiles a syntax-expression requiring it to be a semantic expression.
///
/// There is a limit to how well can the parser distinguish expressions from statements,
/// so some "syntax-expressions" (e.g. no-value function calls) actually correspond to
/// statements in the program IR.
///
/// This function forcefully treats the result as a true expression, and reports an error
/// if it is not one.
pub fn compile_expression(
    expression: ast::ExpressionLike,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let location = expression.span();
    let node = compile_expression_or_statement(expression, type_hint, context);
    match node {
        DualNode::Expression(expression) => expression,
        DualNode::Statement(statement) => {
            let error = errors::statement_used_as_expression(&statement);
            context.errors.push(error);
            program::Expression::error(location)
        }
    }
}
