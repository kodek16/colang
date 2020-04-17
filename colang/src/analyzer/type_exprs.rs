//! Utilities for analysing type expressions. Used in multiple passes.

use crate::errors::CompilationError;
use crate::program::Type;
use crate::{ast, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_type_expr(
    type_expr: &ast::TypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    match type_expr {
        ast::TypeExpr::Scalar(type_expr) => compile_scalar_type_expr(type_expr, context),
        ast::TypeExpr::Array(type_expr) => compile_array_type_expr(type_expr, context),
        ast::TypeExpr::Pointer(type_expr) => compile_pointer_type_expr(type_expr, context),
        ast::TypeExpr::TemplateInstance(type_expr) => {
            compile_template_instance_type_expr(type_expr, context)
        }
    }
}

pub fn compile_type_expr_and_ensure_complete(
    type_expr: &ast::TypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let type_ = compile_type_expr(&type_expr, context);
    let result = Type::ensure_is_fully_complete(Rc::clone(&type_), context.program.types_mut());
    if let Err(type_chain) = result {
        let error = CompilationError::type_infinite_dependency_chain(
            &type_.borrow(),
            type_chain,
            type_expr.span(),
        );
        context.errors.push(error);
    }
    type_
}

fn compile_scalar_type_expr(
    type_expr: &ast::ScalarTypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let name = &type_expr.name;
    let type_ = context.scope.lookup_type(&name.text, type_expr.span);

    match type_ {
        Ok(type_) => Rc::clone(type_),
        Err(error) => {
            context.errors.push(error);
            Type::error()
        }
    }
}

fn compile_array_type_expr(
    type_expr: &ast::ArrayTypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let element = compile_type_expr(&type_expr.element, context);
    let result = context.program.types_mut().array_of(&element);
    result
}

fn compile_pointer_type_expr(
    type_expr: &ast::PointerTypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let target = compile_type_expr(&type_expr.target, context);
    let result = context.program.types_mut().pointer_to(&target);
    result
}

fn compile_template_instance_type_expr(
    type_expr: &ast::TemplateInstanceTypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let type_arguments: Vec<_> = type_expr
        .type_arguments
        .iter()
        .map(|type_arg| compile_type_expr(type_arg, context))
        .collect();

    if type_arguments
        .iter()
        .any(|type_arg| type_arg.borrow().is_error())
    {
        return Type::error();
    }

    let template = context
        .scope
        .lookup_type_template(&type_expr.template.text, type_expr.template.span);
    let template = match template {
        Ok(template) => template,
        Err(error) => {
            context.errors.push(error);
            return Type::error();
        }
    };

    let type_ = template.borrow().instantiate(
        type_arguments.iter().collect(),
        context.program.types_mut(),
        Some(type_expr.span),
    );
    match type_ {
        Ok(type_) => type_,
        Err(error) => {
            context.errors.push(error);
            return Type::error();
        }
    }
}
