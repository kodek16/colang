//! Global compiler context definition.

use crate::ast;
use crate::errors::CompilationError;
use crate::program::{self, Function, Type, TypeTemplate, Variable};
use crate::scope::Scope;
use crate::source::InputSpan;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Global compilation context that gets passed along to various compiler routines.
pub struct CompilerContext {
    pub program: program::Program,
    pub scope: Scope,

    pub globals: GlobalEntities,

    // TODO we should have a separate BodyCompilerContext type that has the fields only relevant
    // in function bodies.
    /// Function currently being analyzed.
    pub function: Option<Rc<RefCell<Function>>>,

    /// In methods, this is the variable bound to `self`.
    pub self_: Option<Rc<RefCell<Variable>>>,

    pub errors: Vec<CompilationError>,
}

impl CompilerContext {
    /// Creates the initial root context.
    pub fn new() -> CompilerContext {
        let mut program = program::Program::new();
        let mut scope = Scope::new();

        for type_ in program.types().basic_types() {
            scope.add_type(Rc::clone(type_)).unwrap();
        }

        program::internal::populate_internal_symbols(&mut program, &mut scope);
        CompilerContext {
            program,
            scope,
            globals: GlobalEntities::new(),
            function: None,
            self_: None,
            errors: vec![],
        }
    }
}

/// A registry mapping syntactic global entities to their semantic counterparts.
pub struct GlobalEntities {
    structs: HashMap<InputSpan, Rc<RefCell<Type>>>,
    struct_templates: HashMap<InputSpan, Rc<RefCell<TypeTemplate>>>,
    functions: HashMap<InputSpan, Rc<RefCell<Function>>>,
    fields: HashMap<InputSpan, Rc<RefCell<Variable>>>,
    methods: HashMap<InputSpan, Rc<RefCell<Function>>>,
}

impl GlobalEntities {
    pub fn new() -> GlobalEntities {
        GlobalEntities {
            structs: HashMap::new(),
            struct_templates: HashMap::new(),
            functions: HashMap::new(),
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn register_struct(&mut self, struct_def: &ast::StructDef, struct_: Rc<RefCell<Type>>) {
        self.structs.insert(struct_def.signature_span, struct_);
    }

    pub fn register_struct_template(
        &mut self,
        struct_def: &ast::StructDef,
        struct_template: Rc<RefCell<TypeTemplate>>,
    ) {
        self.struct_templates
            .insert(struct_def.signature_span, struct_template);
    }

    pub fn register_function(
        &mut self,
        function_def: &ast::FunctionDef,
        function: Rc<RefCell<Function>>,
    ) {
        self.functions.insert(function_def.signature_span, function);
    }

    pub fn register_field(&mut self, field_def: &ast::FieldDef, field: Rc<RefCell<Variable>>) {
        self.fields.insert(field_def.span, field);
    }

    pub fn register_method(
        &mut self,
        method_def: &ast::FunctionDef,
        method: Rc<RefCell<Function>>,
    ) {
        self.methods.insert(method_def.signature_span, method);
    }

    pub fn struct_(&self, struct_def: &ast::StructDef) -> &Rc<RefCell<Type>> {
        self.structs
            .get(&struct_def.signature_span)
            .expect(&format!(
                "Struct `{}` was not analyzed in previous passes",
                struct_def.name.text
            ))
    }

    pub fn struct_template(&self, struct_def: &ast::StructDef) -> &Rc<RefCell<TypeTemplate>> {
        self.struct_templates
            .get(&struct_def.signature_span)
            .expect(&format!(
                "Struct template `{}` was not analyzed in previous passes",
                struct_def.name.text
            ))
    }

    pub fn function(&self, function_def: &ast::FunctionDef) -> &Rc<RefCell<Function>> {
        self.functions
            .get(&function_def.signature_span)
            .expect(&format!(
                "Function `{}` was not analyzed in previous passes",
                function_def.name.text
            ))
    }

    pub fn field(&self, field_def: &ast::FieldDef) -> &Rc<RefCell<Variable>> {
        self.fields.get(&field_def.span).expect(&format!(
            "Field `{}` was not analyzed in previous passes",
            field_def.name.text
        ))
    }

    pub fn method(&self, method_def: &ast::FunctionDef) -> &Rc<RefCell<Function>> {
        self.methods
            .get(&method_def.signature_span)
            .expect(&format!(
                "Method `{}` was not analyzed in previous passes",
                method_def.name.text
            ))
    }
}
