//! Framework for defining analyzer passes that visit all defined global symbols.

use crate::ast;
use crate::program::{Function, Type, TypeTemplate, Variable};
use crate::scope::TypeEntity;
use crate::CompilerContext;
use std::cell::RefCell;
use std::rc::Rc;

/// An analyzer pass that traverses the program AST starting from the root node.
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
            for mut struct_def in &mut unit.structs {
                self.analyze_struct_def(&mut struct_def, context);
            }

            for mut function_def in &mut unit.functions {
                self.analyze_function_def(&mut function_def, context);
            }
        }
    }

    fn analyze_struct_def(
        &mut self,
        struct_def: &mut ast::StructDef,
        context: &mut CompilerContext,
    ) {
        if struct_def.type_parameters.is_empty() {
            self.analyze_non_template_struct_def(struct_def, context);
        } else {
            self.analyze_template_struct_def(struct_def, context);
        }
    }

    fn analyze_non_template_struct_def(
        &mut self,
        struct_def: &mut ast::StructDef,
        context: &mut CompilerContext,
    ) {
        let type_ = Rc::clone(context.globals.struct_(struct_def));

        for mut field_def in &mut struct_def.fields {
            self.analyze_field_def(&mut field_def, &type_, context);
        }

        for mut method_def in &mut struct_def.methods {
            self.analyze_method_def(&mut method_def, &type_, context);
        }

        self.revisit_non_template_struct_def(struct_def, type_, context);
    }

    fn revisit_non_template_struct_def(
        &mut self,
        _struct_def: &mut ast::StructDef,
        _type_: Rc<RefCell<Type>>,
        _context: &mut CompilerContext,
    ) {
    }

    fn analyze_template_struct_def(
        &mut self,
        struct_def: &mut ast::StructDef,
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

        let base_type = Rc::clone(template.borrow().base_type());

        for mut field_def in &mut struct_def.fields {
            self.analyze_field_def(&mut field_def, &base_type, context);
        }

        for mut method_def in &mut struct_def.methods {
            self.analyze_method_def(&mut method_def, &base_type, context);
        }

        // Type parameter scope.
        context.scope.pop();

        self.revisit_template_struct_def(struct_def, template, base_type, context);
    }

    fn revisit_template_struct_def(
        &mut self,
        _struct_def: &mut ast::StructDef,
        _template: Rc<RefCell<TypeTemplate>>,
        _base_type: Rc<RefCell<Type>>,
        _context: &mut CompilerContext,
    ) {
    }

    fn analyze_field_def(
        &mut self,
        field_def: &mut ast::FieldDef,
        current_type: &Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        let field = Rc::clone(context.globals.field(field_def));
        self.revisit_field_def(field_def, current_type, field, context);
    }

    fn revisit_field_def(
        &mut self,
        _field_def: &mut ast::FieldDef,
        _current_type: &Rc<RefCell<Type>>,
        _field: Rc<RefCell<Variable>>,
        _context: &mut CompilerContext,
    ) {
    }

    fn analyze_method_def(
        &mut self,
        method_def: &mut ast::FunctionDef,
        current_type: &Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        let method = Rc::clone(context.globals.method(method_def));
        self.revisit_method_def(method_def, current_type, method, context);
    }

    fn revisit_method_def(
        &mut self,
        _method_def: &mut ast::FunctionDef,
        _current_type: &Rc<RefCell<Type>>,
        _method: Rc<RefCell<Function>>,
        _context: &mut CompilerContext,
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
        _function_def: &mut ast::FunctionDef,
        _function: Rc<RefCell<Function>>,
        _context: &mut CompilerContext,
    ) {
    }
}
