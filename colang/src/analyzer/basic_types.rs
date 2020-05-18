//! The very first analysis pass: initializing types for all type definitions in the program.
//! Type templates and incomplete base types also get initialized. No type members are analyzed
//! yet.

use crate::analyzer::visitor::GlobalVisitor;
use crate::ast;
use crate::ast::FunctionDef;
use crate::program::{ProtoTypeParameter, Trait, Type, TypeTemplate};
use crate::scope::{TraitEntity, TypeEntity, TypeTemplateEntity};
use crate::source::SourceOrigin;
use crate::CompilerContext;
use std::rc::Rc;

pub struct BasicTypesAnalyzerPass;

impl GlobalVisitor for BasicTypesAnalyzerPass {
    fn analyze_non_template_struct_def(
        &mut self,
        struct_def: &mut ast::TypeDef,
        context: &mut CompilerContext,
    ) {
        let type_ = Type::new_struct(
            struct_def.name.text.clone(),
            SourceOrigin::Plain(struct_def.signature_span),
            &mut context.program,
        );
        context
            .globals
            .register_struct(&struct_def, Rc::clone(&type_));

        let result = context.scope.add(TypeEntity(Rc::clone(&type_)));
        if let Err(error) = result {
            context.errors.push(error);
        }
    }

    fn analyze_template_struct_def(
        &mut self,
        struct_def: &mut ast::TypeDef,
        context: &mut CompilerContext,
    ) {
        let type_parameters: Vec<_> = struct_def
            .type_parameters
            .iter()
            .map(|type_param| ProtoTypeParameter {
                name: type_param.text.clone(),
                definition_site: Some(SourceOrigin::Plain(type_param.span)),
            })
            .collect();
        let template = TypeTemplate::new_struct_template(
            struct_def.name.text.clone(),
            type_parameters,
            SourceOrigin::Plain(struct_def.signature_span),
            &mut context.program,
        );
        context
            .globals
            .register_struct_template(&struct_def, Rc::clone(&template));

        let result = context.scope.add(TypeTemplateEntity(Rc::clone(&template)));
        if let Err(error) = result {
            context.errors.push(error);
        }
    }

    fn analyze_trait_def(&mut self, trait_def: &mut ast::TypeDef, context: &mut CompilerContext) {
        let trait_ = Trait::new(
            trait_def.name.text.clone(),
            SourceOrigin::Plain(trait_def.signature_span),
            &mut context.program,
        );
        context
            .globals
            .register_trait(trait_def, Rc::clone(&trait_));

        let result = context.scope.add(TraitEntity(Rc::clone(&trait_)));
        if let Err(error) = result {
            context.errors.push(error);
        }
    }

    fn analyze_function_def(&mut self, _: &mut FunctionDef, _: &mut CompilerContext) {
        // skip functions
    }
}
