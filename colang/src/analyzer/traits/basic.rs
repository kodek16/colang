//! This pass links types to the traits they claim to implement.
//!
//! This is one of the earlier passes, no method information is assumed to be known at this point,
//! trait checking and wiring is not performed.

use crate::analyzer::visitor::TypeMemberContext;
use crate::analyzer::{trait_exprs, GlobalVisitor};
use crate::ast::{self, FieldDef, FunctionDef, TypeDef};
use crate::context::CompilerContext;
use crate::program::{Type, TypeTemplate};
use std::cell::RefCell;
use std::rc::Rc;

pub struct BasicTraitsAnalyzerPass;

impl GlobalVisitor for BasicTraitsAnalyzerPass {
    fn revisit_non_template_struct_def(
        &mut self,
        struct_def: &mut TypeDef,
        type_: Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        analyze_and_add_implemented_traits(&type_, &struct_def.implemented_traits, context);
    }

    fn revisit_template_struct_def(
        &mut self,
        struct_def: &mut TypeDef,
        template: Rc<RefCell<TypeTemplate>>,
        context: &mut CompilerContext,
    ) {
        let template = template.borrow();
        let base_type = template.base_type();

        analyze_and_add_implemented_traits(base_type, &struct_def.implemented_traits, context);

        for (type_parameter, type_parameter_def) in (&template.type_parameters)
            .iter()
            .zip(struct_def.type_parameters.iter())
        {
            analyze_and_add_implemented_traits(
                type_parameter,
                &type_parameter_def.trait_bounds,
                context,
            );
        }
    }

    fn analyze_field_def(
        &mut self,
        _: &mut FieldDef,
        _: &TypeMemberContext,
        _: &mut CompilerContext,
    ) {
        // skip
    }

    fn analyze_method_def(
        &mut self,
        _: &mut FunctionDef,
        _: &TypeMemberContext,
        _: &mut CompilerContext,
    ) {
        // skip
    }

    fn analyze_function_def(&mut self, _: &mut FunctionDef, _: &mut CompilerContext) {
        // skip
    }
}

fn analyze_and_add_implemented_traits(
    type_: &Rc<RefCell<Type>>,
    traits: &Vec<ast::TypeExpr>,
    context: &mut CompilerContext,
) {
    for trait_expr in traits {
        if let Some(trait_) = trait_exprs::compile_trait_expr(trait_expr, context) {
            type_.borrow_mut().implemented_traits.push(trait_);
        }
    }
}
