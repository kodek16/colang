//! Framework for defining analyzer passes that visit all defined global symbols.

use crate::ast;
use crate::program::{Field, Function, Trait, Type, TypeTemplate};
use crate::scope::TypeEntity;
use crate::CompilerContext;
use std::cell::RefCell;
use std::rc::Rc;

/// An analyzer pass that traverses the program AST starting from the root node.
///
/// The traversal routines handle scope population boilerplate and determining target type
/// for type member definitions.
///
/// The implementers should typically only redefine the `revisit_*` methods they care about:
/// if a pass is only concerned with field definitions for example, `revisit_field_def` is
/// the only method that needs to be defined.
///
/// Some of the earlier passes which construct the program IR skeleton may need to override
/// the `analyze_*` methods.
pub trait GlobalVisitor {
    /// Executes the analyzer pass.
    fn run(&mut self, program: Vec<&mut ast::Program>, context: &mut CompilerContext) {
        for unit in program {
            for struct_def in &mut unit.structs {
                self.analyze_struct_def(struct_def, context);
            }

            for trait_def in &mut unit.traits {
                self.analyze_trait_def(trait_def, context);
            }

            for function_def in &mut unit.functions {
                self.analyze_function_def(function_def, context);
            }
        }
    }

    fn analyze_struct_def(&mut self, struct_def: &mut ast::TypeDef, context: &mut CompilerContext) {
        if struct_def.type_parameters.is_empty() {
            self.analyze_non_template_struct_def(struct_def, context);
        } else {
            self.analyze_template_struct_def(struct_def, context);
        }
    }

    fn analyze_non_template_struct_def(
        &mut self,
        struct_def: &mut ast::TypeDef,
        context: &mut CompilerContext,
    ) {
        let type_ = Rc::clone(context.globals.struct_(struct_def));
        let type_context = TypeMemberContext::Type(Rc::clone(&type_));

        for field_def in &mut struct_def.fields {
            self.analyze_field_def(field_def, &type_context, context);
        }

        for method_def in &mut struct_def.methods {
            self.analyze_method_def(method_def, &type_context, context);
        }

        self.revisit_non_template_struct_def(struct_def, type_, context);
    }

    fn revisit_non_template_struct_def(
        &mut self,
        _: &mut ast::TypeDef,
        _: Rc<RefCell<Type>>,
        _: &mut CompilerContext,
    ) {
    }

    fn analyze_template_struct_def(
        &mut self,
        struct_def: &mut ast::TypeDef,
        context: &mut CompilerContext,
    ) {
        let template = Rc::clone(context.globals.struct_template(struct_def));

        // Type parameter scope.
        context.scope.push();

        for type_parameter in template.borrow().type_parameters.iter() {
            let result = context.scope.add(TypeEntity(Rc::clone(&type_parameter)));
            if let Err(error) = result {
                context.errors.push(error);
            }
        }

        let type_context = TypeMemberContext::TemplateBaseType {
            template: Rc::clone(&template),
            type_: Rc::clone(template.borrow().base_type()),
        };

        for field_def in &mut struct_def.fields {
            self.analyze_field_def(field_def, &type_context, context);
        }

        for method_def in &mut struct_def.methods {
            self.analyze_method_def(method_def, &type_context, context);
        }

        // Type parameter scope.
        context.scope.pop();

        self.revisit_template_struct_def(struct_def, template, context);
    }

    fn revisit_template_struct_def(
        &mut self,
        _: &mut ast::TypeDef,
        _: Rc<RefCell<TypeTemplate>>,
        _: &mut CompilerContext,
    ) {
    }

    fn analyze_trait_def(&mut self, trait_def: &mut ast::TypeDef, context: &mut CompilerContext) {
        let trait_ = Rc::clone(context.globals.trait_(trait_def));
        {
            let type_context = TypeMemberContext::TraitSelfType {
                trait_: Rc::clone(&trait_),
                type_: Rc::clone(&trait_.borrow().self_type),
            };

            for field_def in &mut trait_def.fields {
                self.analyze_field_def(field_def, &type_context, context);
            }

            for method_def in &mut trait_def.methods {
                self.analyze_method_def(method_def, &type_context, context);
            }
        }

        self.revisit_trait_def(trait_def, trait_, context);
    }

    fn revisit_trait_def(
        &mut self,
        _: &mut ast::TypeDef,
        _: Rc<RefCell<Trait>>,
        _: &mut CompilerContext,
    ) {
    }

    fn analyze_field_def(
        &mut self,
        field_def: &mut ast::FieldDef,
        type_context: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        let field = Rc::clone(context.globals.field(field_def));
        self.revisit_field_def(field_def, field, type_context, context);
    }

    fn revisit_field_def(
        &mut self,
        _: &mut ast::FieldDef,
        _: Rc<RefCell<Field>>,
        _: &TypeMemberContext,
        _: &mut CompilerContext,
    ) {
    }

    fn analyze_method_def(
        &mut self,
        method_def: &mut ast::FunctionDef,
        type_context: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        let method = Rc::clone(context.globals.method(method_def));
        self.revisit_method_def(method_def, method, type_context, context);
    }

    fn revisit_method_def(
        &mut self,
        _: &mut ast::FunctionDef,
        _: Rc<RefCell<Function>>,
        _: &TypeMemberContext,
        _: &mut CompilerContext,
    ) {
    }

    fn analyze_function_def(
        &mut self,
        function_def: &mut ast::FunctionDef,
        context: &mut CompilerContext,
    ) {
        let function = Rc::clone(context.globals.function(function_def));
        self.revisit_function_def(function_def, function, context);
    }

    fn revisit_function_def(
        &mut self,
        _: &mut ast::FunctionDef,
        _: Rc<RefCell<Function>>,
        _: &mut CompilerContext,
    ) {
    }
}

/// Enclosing context for type member analysis.
pub enum TypeMemberContext {
    Type(Rc<RefCell<Type>>),
    TemplateBaseType {
        template: Rc<RefCell<TypeTemplate>>,
        type_: Rc<RefCell<Type>>,
    },
    TraitSelfType {
        trait_: Rc<RefCell<Trait>>,
        type_: Rc<RefCell<Type>>,
    },
}

impl TypeMemberContext {
    pub fn type_(&self) -> &Rc<RefCell<Type>> {
        match self {
            TypeMemberContext::Type(type_) => type_,
            TypeMemberContext::TemplateBaseType { type_, .. } => type_,
            TypeMemberContext::TraitSelfType { type_, .. } => type_,
        }
    }
}
