//! Global compiler context definition.

use crate::ast;
use crate::errors::CompilationError;
use crate::program::{self, Field, Function, Trait, Type, TypeTemplate, Variable};
use crate::scope::{FreeScope, TypeEntity};
use crate::source::InputSpan;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Global compilation context that gets passed along to various compiler routines.
pub struct CompilerContext {
    /// Program intermediate representation that is being constructed.
    pub program: program::Program,

    /// Current free scope. Plain name lookups (not bound to a receiver) will use this scope.
    pub scope: FreeScope,

    /// Errors in the user program encountered so far.
    pub errors: Vec<CompilationError>,

    /// Mapping from syntactic to semantic global entities.
    pub globals: GlobalEntities,

    /// Current local (function body) context.
    local: Option<LocalCompilerContext>,
}

impl CompilerContext {
    /// Creates the initial root context.
    pub fn new() -> CompilerContext {
        let mut program = program::Program::new();
        let mut scope = FreeScope::new();

        for type_ in program.types().visible_basic_types() {
            scope.add(TypeEntity(Rc::clone(type_))).unwrap();
        }

        program::internal::populate_internal_symbols(&mut program, &mut scope);
        CompilerContext {
            program,
            scope,
            errors: vec![],
            globals: GlobalEntities::new(),
            local: None,
        }
    }

    /// Initializes a new `LocalCompilerContext` for an analyzed function.
    pub fn push_local(
        &mut self,
        function: Rc<RefCell<Function>>,
        self_: Option<Rc<RefCell<Variable>>>,
    ) {
        self.local = Some(LocalCompilerContext { function, self_ });
    }

    /// Cleans up the active `LocalCompilerContext`.
    pub fn pop_local(&mut self) {
        self.local = None;
    }

    pub fn local(&mut self) -> &mut LocalCompilerContext {
        self.local
            .as_mut()
            .expect("Local context cannot be accessed outside of function bodies")
    }
}

/// A registry mapping syntactic global entities to their semantic counterparts.
pub struct GlobalEntities {
    structs: HashMap<InputSpan, Rc<RefCell<Type>>>,
    struct_templates: HashMap<InputSpan, Rc<RefCell<TypeTemplate>>>,
    traits: HashMap<InputSpan, Rc<RefCell<Trait>>>,
    functions: HashMap<InputSpan, Rc<RefCell<Function>>>,
    fields: HashMap<InputSpan, Rc<RefCell<Field>>>,
    methods: HashMap<InputSpan, Rc<RefCell<Function>>>,
}

impl GlobalEntities {
    pub fn new() -> GlobalEntities {
        GlobalEntities {
            structs: HashMap::new(),
            struct_templates: HashMap::new(),
            traits: HashMap::new(),
            functions: HashMap::new(),
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn register_struct(&mut self, struct_def: &ast::TypeDef, struct_: Rc<RefCell<Type>>) {
        self.structs.insert(struct_def.signature_span, struct_);
    }

    pub fn register_struct_template(
        &mut self,
        struct_def: &ast::TypeDef,
        struct_template: Rc<RefCell<TypeTemplate>>,
    ) {
        self.struct_templates
            .insert(struct_def.signature_span, struct_template);
    }

    pub fn register_trait(&mut self, trait_def: &ast::TypeDef, trait_: Rc<RefCell<Trait>>) {
        self.traits.insert(trait_def.signature_span, trait_);
    }

    pub fn register_function(
        &mut self,
        function_def: &ast::FunctionDef,
        function: Rc<RefCell<Function>>,
    ) {
        self.functions.insert(function_def.signature_span, function);
    }

    pub fn register_field(&mut self, field_def: &ast::FieldDef, field: Rc<RefCell<Field>>) {
        self.fields.insert(field_def.span, field);
    }

    pub fn register_method(
        &mut self,
        method_def: &ast::FunctionDef,
        method: Rc<RefCell<Function>>,
    ) {
        self.methods.insert(method_def.signature_span, method);
    }

    pub fn struct_(&self, struct_def: &ast::TypeDef) -> &Rc<RefCell<Type>> {
        self.structs
            .get(&struct_def.signature_span)
            .expect(&format!(
                "Struct `{}` was not analyzed in previous passes",
                struct_def.name.text
            ))
    }

    pub fn struct_template(&self, struct_def: &ast::TypeDef) -> &Rc<RefCell<TypeTemplate>> {
        self.struct_templates
            .get(&struct_def.signature_span)
            .expect(&format!(
                "Struct template `{}` was not analyzed in previous passes",
                struct_def.name.text
            ))
    }

    pub fn trait_(&self, trait_def: &ast::TypeDef) -> &Rc<RefCell<Trait>> {
        self.traits.get(&trait_def.signature_span).expect(&format!(
            "Trait `{}` was not analyzed in previous passes",
            trait_def.name.text
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

    pub fn field(&self, field_def: &ast::FieldDef) -> &Rc<RefCell<Field>> {
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

/// Extension of `CompilerContext` for local (function bodies) contexts.
pub struct LocalCompilerContext {
    /// Function currently being analyzed.
    pub function: Rc<RefCell<Function>>,

    /// In methods, this is the variable bound to `self`.
    pub self_: Option<Rc<RefCell<Variable>>>,
}
