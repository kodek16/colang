//! The very first analysis pass: creating incomplete types (skipping fields and methods)
//! for all type definitions in the program. Type templates and incomplete base types also get
//! created.

use crate::analyzer::visitor::GlobalVisitor;
use crate::ast;
use crate::ast::FunctionDef;
use crate::program::{ProtoTypeParameter, Type, TypeTemplate};
use crate::CompilerContext;
use std::rc::Rc;

pub struct IncompleteTypesAnalyzerPass;

impl GlobalVisitor for IncompleteTypesAnalyzerPass {
    fn analyze_non_template_struct_def(
        &mut self,
        struct_def: &mut ast::StructDef,
        context: &mut CompilerContext,
    ) {
        let name = &struct_def.name.text;
        let type_ = Type::new_struct(
            name.to_string(),
            struct_def.signature_span,
            &mut context.program,
        );
        context
            .globals
            .register_struct(&struct_def, Rc::clone(&type_));

        let result = context.scope.add_type(Rc::clone(&type_));
        if let Err(error) = result {
            context.errors.push(error);
        }
    }

    fn analyze_template_struct_def(
        &mut self,
        struct_def: &mut ast::StructDef,
        context: &mut CompilerContext,
    ) {
        let type_parameters: Vec<_> = struct_def
            .type_parameters
            .iter()
            .map(|type_param| ProtoTypeParameter {
                name: type_param.text.clone(),
                definition_site: Some(type_param.span),
            })
            .collect();
        let template = TypeTemplate::new_struct_template(
            struct_def.name.text.clone(),
            type_parameters,
            struct_def.signature_span,
            &mut context.program,
        );
        context
            .globals
            .register_struct_template(&struct_def, Rc::clone(&template));

        let result = context.scope.add_type_template(Rc::clone(&template));
        if let Err(error) = result {
            context.errors.push(error);
        }
    }

    fn analyze_function_def(&mut self, _: &mut FunctionDef, _: &mut CompilerContext) {
        // skip functions
    }
}
