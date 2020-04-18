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
mod method_call;
mod new;
mod self_;
mod string_literal;
mod unary_op;
mod variable;

use crate::errors::CompilationError;
use crate::program::Type;
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_expression(
    expression: ast::Expression,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let expression = match expression {
        ast::Expression::Variable(e) => variable::compile_variable_expr(e, context),
        ast::Expression::IntLiteral(e) => int_literal::compile_int_literal_expr(e, context),
        ast::Expression::BoolLiteral(e) => bool_literal::compile_bool_literal_expr(e, context),
        ast::Expression::CharLiteral(e) => char_literal::compile_char_literal_expr(e, context),
        ast::Expression::StringLiteral(e) => {
            string_literal::compile_string_literal_expr(e, context)
        }
        ast::Expression::Self_(e) => self_::compile_self_expr(e, context),
        ast::Expression::UnaryOp(e) => unary_op::compile_unary_op_expression(e, context),
        ast::Expression::BinaryOp(e) => binary_op::compile_binary_op_expr(e, context),
        ast::Expression::Address(e) => address::compile_address_expr(e, type_hint, context),
        ast::Expression::Deref(e) => deref::compile_deref_expr(e, type_hint, context),
        ast::Expression::New(e) => new::compile_new_expr(e, context),
        ast::Expression::ArrayFromElements(e) => {
            array_from_elements::compile_array_from_elements_expr(e, type_hint, context)
        }
        ast::Expression::ArrayFromCopy(e) => {
            array_from_copy::compile_array_from_copy_expr(e, context)
        }
        ast::Expression::Index(e) => index::compile_index_expr(e, context),
        ast::Expression::Call(e) => call::compile_call_expr(e, context),
        ast::Expression::FieldAccess(e) => field_access::compile_field_access_expr(e, context),
        ast::Expression::MethodCall(e) => method_call::compile_method_call_expr(e, context),
        ast::Expression::If(e) => if_::compile_if_expr(e, context),
        ast::Expression::Block(e) => block::compile_block_expr(e, type_hint, context),
    };

    // All expressions must have fully complete types.
    let result =
        Type::ensure_is_fully_complete(Rc::clone(expression.type_()), context.program.types_mut());
    if let Err(type_chain) = result {
        let error = CompilationError::type_infinite_dependency_chain(
            &expression.type_().borrow(),
            type_chain,
            expression
                .span()
                .expect("Synthetic expression type could not be fully completed"),
        );
        context.errors.push(error);
    }

    expression
}
