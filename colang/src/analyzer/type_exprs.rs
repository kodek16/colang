//! Utilities for analysing type expressions. Used in multiple passes.

use crate::context::CompilerContext;
use crate::program::{Type, TypeRef, TypeTemplate};
use crate::scope::{TypeEntity, TypeTemplateEntity};
use crate::source::SourceOrigin;
use crate::{ast, errors};
use std::rc::Rc;

pub fn compile_type_expr(type_expr: &ast::TypeExpr, context: &mut CompilerContext) -> TypeRef {
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
) -> TypeRef {
    let type_ = compile_type_expr(&type_expr, context);
    let result = Type::ensure_is_fully_complete(Rc::clone(&type_), context.program.types_mut());
    if let Err(type_chain) = result {
        let error = errors::type_infinite_dependency_chain(
            &type_.borrow(),
            type_chain,
            SourceOrigin::Plain(type_expr.span()),
        );
        context.errors.push(error);
    }
    type_
}

fn compile_scalar_type_expr(
    type_expr: &ast::ScalarTypeExpr,
    context: &mut CompilerContext,
) -> TypeRef {
    let name = &type_expr.name;
    let type_ = context.scope.lookup::<TypeEntity>(&name.text);

    let type_ = match type_ {
        Ok(type_) => type_,
        Err(error) => {
            let error = error.into_direct_lookup_error(SourceOrigin::Plain(type_expr.span));
            context.errors.push(error);
            Type::error()
        }
    };

    TypeRef::new(type_, Some(SourceOrigin::Plain(type_expr.span)))
}

fn compile_array_type_expr(
    type_expr: &ast::ArrayTypeExpr,
    context: &mut CompilerContext,
) -> TypeRef {
    let element = compile_type_expr(&type_expr.element, context);
    let result = context.program.types_mut().array_of(&element);
    TypeRef::new(result, Some(SourceOrigin::Plain(type_expr.span)))
}

fn compile_pointer_type_expr(
    type_expr: &ast::PointerTypeExpr,
    context: &mut CompilerContext,
) -> TypeRef {
    let target = compile_type_expr(&type_expr.target, context);
    let result = context.program.types_mut().pointer_to(&target);
    TypeRef::new(result, Some(SourceOrigin::Plain(type_expr.span)))
}

fn compile_template_instance_type_expr(
    type_expr: &ast::TemplateInstanceTypeExpr,
    context: &mut CompilerContext,
) -> TypeRef {
    let location = SourceOrigin::Plain(type_expr.span);
    let type_arguments: Vec<_> = type_expr
        .type_arguments
        .iter()
        .map(|type_arg| compile_type_expr(type_arg, context))
        .collect();

    if type_arguments
        .iter()
        .any(|type_arg| type_arg.borrow().is_error())
    {
        return TypeRef::new(Type::error(), Some(location));
    }

    let template = context
        .scope
        .lookup::<TypeTemplateEntity>(&type_expr.template.text);

    let template = match template {
        Ok(template) => template,
        Err(error) => {
            let error =
                error.into_direct_lookup_error(SourceOrigin::Plain(type_expr.template.span));
            context.errors.push(error);
            return TypeRef::new(Type::error(), Some(location));
        }
    };

    let type_ = TypeTemplate::instantiate(
        template,
        type_arguments,
        context.program.types_mut(),
        location,
    );
    match type_ {
        Ok(type_) => TypeRef::new(type_, Some(location)),
        Err(mut errors) => {
            context.errors.append(&mut errors);
            TypeRef::new(Type::error(), Some(location))
        }
    }
}
