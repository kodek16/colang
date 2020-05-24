//! Utilities for analysing type expressions in trait contexts.

use crate::ast;
use crate::context::CompilerContext;
use crate::errors;
use crate::program::Trait;
use crate::scope::TraitEntity;
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_trait_expr(
    trait_expr: &ast::TypeExpr,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Trait>>> {
    match trait_expr {
        ast::TypeExpr::Scalar(trait_expr) => compile_scalar_trait_expr(trait_expr, context),
        _ => {
            let error = errors::type_used_as_trait(trait_expr);
            context.errors.push(error);
            None
        }
    }
}

fn compile_scalar_trait_expr(
    trait_expr: &ast::ScalarTypeExpr,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Trait>>> {
    let result = context.scope.lookup::<TraitEntity>(&trait_expr.name.text);

    match result {
        Ok(trait_) => Some(trait_),
        Err(error) => {
            let error = error.into_direct_lookup_error(SourceOrigin::Plain(trait_expr.span));
            context.errors.push(error);
            None
        }
    }
}
