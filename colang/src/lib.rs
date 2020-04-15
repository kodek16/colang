//! Compiler frontend transforms the source code into an intermediate representation
//! defined in `program`. All static checks are performed during this phase.

use lalrpop_util::lalrpop_mod;

mod analyzer;
mod ast;
mod scope;

pub mod backends;
pub mod errors;
pub mod program;
pub mod stdlib;
lalrpop_mod!(pub grammar);

use std::cell::RefCell;
use std::iter;
use std::rc::Rc;

use crate::analyzer::type_exprs;
use crate::analyzer::utils::global_visitor::GlobalVisitor;
use crate::ast::{InputSpan, InputSpanFile};
use crate::errors::CompilationError;
use crate::program::transforms::valid::ValidityChecker;
use crate::program::{
    BlockBuilder, Function, InternalFunctionTag, Type, TypeId, TypeTemplate, ValueCategory,
    Variable,
};
use crate::scope::Scope;
use std::collections::HashMap;

pub fn run(source_code: &str) -> Result<program::Program, Vec<CompilationError>> {
    let std_ast = parse(stdlib::STD_SOURCE, InputSpanFile::Std)
        .map_err(|err| vec![CompilationError::syntax_error(err, InputSpanFile::Std)])?;

    let program_ast = parse(&source_code, InputSpanFile::UserProgram).map_err(|err| {
        vec![CompilationError::syntax_error(
            err,
            InputSpanFile::UserProgram,
        )]
    })?;

    let mut program = compile(vec![std_ast, program_ast])?;

    let checker = ValidityChecker::new(&mut program);
    let errors = checker.check();
    if !errors.is_empty() {
        eprintln!("Compiler error: produced program is in an invalid state.");
        for error in errors {
            eprintln!("{}", error);
        }
        panic!("Internal error occurred");
    }

    Ok(program)
}

/// Parses the source code and returns an AST root.
fn parse(source_code: &str, file: InputSpanFile) -> Result<ast::Program, ast::ParseError> {
    grammar::ProgramParser::new().parse(file, source_code)
}

/// Context that gets passed along to various compiler routines.
pub struct CompilerContext {
    program: program::Program,
    scope: Scope,

    /// In methods, this is the variable bound to `self`.
    self_: Option<Rc<RefCell<Variable>>>,

    // Keep track of semantic objects created from syntactic definitions so that later passes
    // can easily access them.
    defined_types: HashMap<InputSpan, Rc<RefCell<Type>>>,
    defined_type_templates: HashMap<InputSpan, Rc<RefCell<TypeTemplate>>>,
    defined_functions: HashMap<InputSpan, Rc<RefCell<Function>>>,
    defined_fields: HashMap<InputSpan, Rc<RefCell<Variable>>>,
    defined_methods: HashMap<InputSpan, Rc<RefCell<Function>>>,

    errors: Vec<CompilationError>,
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
            self_: None,
            defined_types: HashMap::new(),
            defined_type_templates: HashMap::new(),
            defined_functions: HashMap::new(),
            defined_fields: HashMap::new(),
            defined_methods: HashMap::new(),
            errors: vec![],
        }
    }
}

/// Compiles a CO program.
fn compile(sources: Vec<ast::Program>) -> Result<program::Program, Vec<CompilationError>> {
    let mut context = CompilerContext::new();

    // 1st pass: initialize all defined types (and base types of type templates).
    analyzer::incomplete_types::IncompleteTypesAnalyzerPass::new()
        .run(sources.iter().collect(), &mut context);

    // 2nd pass: collect all global information.
    analyzer::global_structure::GlobalStructureAnalyzerPass::new()
        .run(sources.iter().collect(), &mut context);

    // 3rd pass: complete all types referenced globally.
    analyzer::complete_types::CompleteTypesAnalyzerPass::new()
        .run(sources.iter().collect(), &mut context);

    // Last pass (legacy).
    for source in sources {
        for struct_def in source.structs {
            compile_struct_def(struct_def, &mut context);
        }

        for function_def in source.functions {
            compile_function_def(function_def, &mut context);
        }
    }

    let main_function = context
        .scope
        .lookup_function("main", InputSpan::top_of_file());
    if let Ok(main_function) = main_function {
        context.program.fill_main_function(Rc::clone(main_function));
    } else {
        let error = CompilationError::main_function_not_found();
        context.errors.push(error);
    }

    if context.errors.is_empty() {
        Ok(context.program)
    } else {
        Err(context.errors)
    }
}

fn compile_struct_def(struct_def: ast::StructDef, context: &mut CompilerContext) {
    if !struct_def.type_parameters.is_empty() {
        compile_struct_template_def(struct_def, context);
        return;
    }

    let type_ = Rc::clone(
        context
            .defined_types
            .get(&struct_def.signature_span)
            .expect(&format!(
                "Missing type `{}` from previous passes",
                struct_def.name.text
            )),
    );

    for method_def in struct_def.methods {
        compile_method_def(method_def, &type_, context);
    }
}

fn compile_struct_template_def(struct_def: ast::StructDef, context: &mut CompilerContext) {
    let template = context
        .defined_type_templates
        .get(&struct_def.signature_span)
        .expect(&format!(
            "Missing type template `{}` from previous passes",
            struct_def.name.text
        ));

    // Type parameter scope.
    context.scope.push();

    for type_parameter in template.borrow().type_parameters() {
        // Any scope errors have already been reported in a previous phase.
        let _ = context.scope.add_type(Rc::clone(&type_parameter));
    }

    let base_type = Rc::clone(template.borrow().base_type());

    for method_def in struct_def.methods {
        compile_method_def(method_def, &base_type, context);
    }

    // Type parameter scope.
    context.scope.pop();
}

fn compile_method_def(
    method_def: ast::FunctionDef,
    current_type: &Rc<RefCell<Type>>,
    context: &mut CompilerContext,
) {
    let method = Rc::clone(
        context
            .defined_methods
            .get(&method_def.signature_span)
            .expect(&format!(
                "Missing method `{}` of type `{}` from previous phase",
                method_def.name.text,
                current_type.borrow().name()
            )),
    );

    // Parameters have their own scope.
    context.scope.push();
    context.self_ = Some(Rc::clone(&method.borrow().parameters.get(0).expect(
        &format!(
            "Attempt to parse method `{}` of type `{}` which is in an error state: no `self` parameter",
            method_def.name.text,
            current_type.borrow().name()
        ),
    )));
    for parameter in &method.borrow().parameters[1..] {
        // Ignore errors, they should be already reported in the previous phase.
        let _ = context.scope.add_variable(Rc::clone(&parameter));
    }

    let body = compile_block_expr(
        method_def.body,
        Some(Rc::clone(&method.borrow().return_type)),
        context,
    );

    context.self_ = None;
    context.scope.pop();

    let body_type = Rc::clone(body.type_());
    let result = method.borrow_mut().fill_body(body, body_type);
    if let Err(error) = result {
        context.errors.push(error)
    };
}

/// Compiles the given function, adding it to the program symbol table. If any
/// errors occur, they are added to `context`.
fn compile_function_def(function_def: ast::FunctionDef, context: &mut CompilerContext) {
    let function = Rc::clone(
        context
            .defined_functions
            .get(&function_def.signature_span)
            .expect(&format!(
                "Missing function `{}` from previous phase",
                function_def.name.text
            )),
    );

    // Parameters have their own scope.
    context.scope.push();

    for parameter in &function.borrow().parameters {
        // Ignore errors, they should be already reported in the previous phase.
        let _ = context.scope.add_variable(Rc::clone(&parameter));
    }

    let body = compile_block_expr(
        function_def.body,
        Some(Rc::clone(&function.borrow().return_type)),
        context,
    );

    context.scope.pop();

    let body_type = Rc::clone(body.type_());
    if let Err(error) = function.borrow_mut().fill_body(body, body_type) {
        context.errors.push(error)
    };
}

/// Compiles a single statement in the syntax sense. If compilation succeeds,
/// resulting target program statements are emitted to `sink`. If any errors
/// occur, they are added to `context`.
fn compile_statement(
    statement: ast::Statement,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    match statement {
        ast::Statement::VarDecl(s) => compile_var_decl_stmt(s, current_block, context),
        ast::Statement::Read(s) => compile_read_stmt(s, current_block, context),
        ast::Statement::Write(s) => compile_write_stmt(s, current_block, context),
        ast::Statement::While(s) => compile_while_stmt(s, current_block, context),
        ast::Statement::Assign(s) => compile_assign_stmt(s, current_block, context),
        ast::Statement::Expr(s) => compile_expr_stmt(s, current_block, context),
    }
}

fn compile_var_decl_stmt(
    statement: ast::VarDeclStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    for declaration in statement.entries {
        compile_var_decl_entry(declaration, current_block, context);
    }
}

fn compile_var_decl_entry(
    declaration: ast::VarDeclEntry,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let name = declaration.variable_name;

    // Compile the variable type and the initializer expression
    // even if the variable produces an error.
    let type_ = declaration
        .variable_type
        .map(|type_| type_exprs::compile_type_expr(&type_, context));
    let initializer = declaration
        .initializer
        .map(|initializer| compile_expression(initializer, type_.clone(), context));

    // Possibly infer type from the initializer expression.
    let type_ = match type_ {
        Some(t) => t,
        None => match initializer {
            Some(ref expr) => Rc::clone(expr.type_()),
            None => {
                let error = CompilationError::variable_type_omitted(&name.text, declaration.span);
                context.errors.push(error);
                Type::error()
            }
        },
    };

    let variable = match Variable::new_variable(
        name.text,
        type_,
        Some(declaration.span),
        &mut context.program,
    ) {
        Ok(variable) => Rc::new(RefCell::new(variable)),
        Err(error) => {
            context.errors.push(error);
            return;
        }
    };

    if let Err(error) = context.scope.add_variable(Rc::clone(&variable)) {
        context.errors.push(error);
    };

    current_block.add_local_variable(Rc::clone(&variable));

    if let Some(initializer) = initializer {
        let initialization = program::AssignInstruction::new(
            program::VariableExpr::new(&variable, context.program.types_mut(), name.span),
            initializer,
            declaration.span,
        );

        match initialization {
            Ok(initialization) => current_block.append_instruction(initialization),
            Err(error) => context.errors.push(error),
        }
    }
}

fn compile_read_stmt(
    statement: ast::ReadStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    for entry in statement.entries {
        compile_read_entry(entry, current_block, context);
    }
}

fn compile_read_entry(
    entry: ast::ReadEntry,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let target = compile_expression(entry.target, None, context);
    if target.is_error() {
        return;
    }

    let result = program::CallExpr::new_read(target, &mut context.program);
    match result {
        Ok(expression) => {
            current_block.append_instruction(program::EvalInstruction::new(expression))
        }
        Err(error) => context.errors.push(error),
    }
}

fn compile_write_stmt(
    statement: ast::WriteStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let result = program::WriteInstruction::new(expression, &mut context.program);
    match result {
        Ok(instruction) => {
            current_block.append_instruction(instruction);

            if statement.newline {
                let newline =
                    program::LiteralExpr::string("\n", context.program.types_mut(), statement.span)
                        .expect("Couldn't construct '\\n' string literal");
                let instruction = program::WriteInstruction::new(newline, &mut context.program)
                    .expect("Couldn't construct `write` instruction for synthetic newline");
                current_block.append_instruction(instruction)
            }
        }
        Err(error) => context.errors.push(error),
    }
}

fn compile_while_stmt(
    statement: ast::WhileStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let body_span = statement.body.span;
    let cond = compile_expression(
        *statement.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );
    let body = compile_block_expr(*statement.body, None, context);

    let body_type = body.type_();
    if *body_type != *context.program.types().void() {
        let error = CompilationError::while_body_not_void(&body_type.borrow().name(), body_span);
        context.errors.push(error)
    }
    let body = program::EvalInstruction::new(body);

    if cond.is_error() {
        return;
    }

    let result = program::WhileInstruction::new(cond, body, context.program.types());
    match result {
        Ok(statement) => current_block.append_instruction(statement),
        Err(error) => context.errors.push(error),
    }
}

fn compile_assign_stmt(
    statement: ast::AssignStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let lhs = compile_expression(*statement.lhs, None, context);
    let rhs = compile_expression(*statement.rhs, Some(Rc::clone(lhs.type_())), context);
    if lhs.is_error() || rhs.is_error() {
        return;
    }

    let result = program::AssignInstruction::new(lhs, rhs, statement.span);
    match result {
        Ok(statement) => current_block.append_instruction(statement),
        Err(error) => context.errors.push(error),
    }
}

fn compile_expr_stmt(
    statement: ast::ExprStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let result = program::EvalInstruction::new(expression);
    current_block.append_instruction(result);
}

fn compile_expression(
    expression: ast::Expression,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    match expression {
        ast::Expression::Variable(e) => compile_variable_expr(e, context),
        ast::Expression::IntLiteral(e) => compile_int_literal_expr(e, context),
        ast::Expression::BoolLiteral(e) => compile_bool_literal_expr(e, context),
        ast::Expression::CharLiteral(e) => compile_char_literal_expr(e, context),
        ast::Expression::StringLiteral(e) => compile_string_literal_expr(e, context),
        ast::Expression::Self_(e) => compile_self_expr(e, context),
        ast::Expression::BinaryOp(e) => compile_binary_op_expr(e, context),
        ast::Expression::Address(e) => compile_address_expr(e, type_hint, context),
        ast::Expression::Deref(e) => compile_deref_expr(e, type_hint, context),
        ast::Expression::New(e) => compile_new_expr(e, context),
        ast::Expression::ArrayFromElements(e) => {
            compile_array_from_elements_expr(e, type_hint, context)
        }
        ast::Expression::ArrayFromCopy(e) => compile_array_from_copy_expr(e, context),
        ast::Expression::Index(e) => compile_index_expr(e, context),
        ast::Expression::Call(e) => compile_call_expr(e, context),
        ast::Expression::FieldAccess(e) => compile_field_access_expr(e, context),
        ast::Expression::MethodCall(e) => compile_method_call_expr(e, context),
        ast::Expression::If(e) => compile_if_expr(e, context),
        ast::Expression::Block(e) => compile_block_expr(e, type_hint, context),
    }
}

fn compile_variable_expr(
    expression: ast::VariableExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let name = expression.name;

    let variable = context.scope.lookup_variable(&name.text, expression.span);
    match variable {
        Ok(variable) if variable.borrow().type_.borrow().is_error() => {
            program::Expression::error(expression.span)
        }
        Ok(variable) => {
            program::VariableExpr::new(variable, context.program.types_mut(), expression.span)
        }
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_int_literal_expr(
    expression: ast::IntLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    program::LiteralExpr::int(
        expression.value,
        context.program.types_mut(),
        expression.span,
    )
}

fn compile_bool_literal_expr(
    expression: ast::BoolLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    program::LiteralExpr::bool(
        expression.value,
        context.program.types_mut(),
        expression.span,
    )
}

fn compile_char_literal_expr(
    expression: ast::CharLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let result = program::LiteralExpr::char(
        &expression.value,
        context.program.types_mut(),
        expression.span,
    );
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_string_literal_expr(
    expression: ast::StringLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let result = program::LiteralExpr::string(
        &expression.value,
        context.program.types_mut(),
        expression.span,
    );
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_self_expr(
    expression: ast::SelfExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    match context.self_ {
        Some(ref variable) => {
            program::VariableExpr::new(variable, context.program.types_mut(), expression.span)
        }
        None => {
            let error = CompilationError::self_in_function_body(expression.span);
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_binary_op_expr(
    expression: ast::BinaryOperatorExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let lhs = compile_expression(*expression.lhs, None, context);
    let rhs = compile_expression(*expression.rhs, None, context);

    if lhs.is_error() || rhs.is_error() {
        return program::Expression::error(expression.span);
    }

    let tag = match expression.operator {
        ast::BinaryOperator::Add => match lhs.type_().borrow().type_id {
            TypeId::Int => Some(InternalFunctionTag::AddInt),
            TypeId::String => Some(InternalFunctionTag::StringAdd),
            _ => None,
        },
        ast::BinaryOperator::Sub => Some(InternalFunctionTag::SubInt),
        ast::BinaryOperator::Mul => Some(InternalFunctionTag::MulInt),
        ast::BinaryOperator::Less => Some(InternalFunctionTag::LessInt),
        ast::BinaryOperator::Greater => Some(InternalFunctionTag::GreaterInt),
        ast::BinaryOperator::LessEq => Some(InternalFunctionTag::LessEqInt),
        ast::BinaryOperator::GreaterEq => Some(InternalFunctionTag::GreaterEqInt),
        ast::BinaryOperator::Eq => match lhs.type_().borrow().type_id {
            TypeId::Int => Some(InternalFunctionTag::EqInt),
            TypeId::String => Some(InternalFunctionTag::StringEq),
            _ => None,
        },
        ast::BinaryOperator::NotEq => match lhs.type_().borrow().type_id {
            TypeId::Int => Some(InternalFunctionTag::NotEqInt),
            TypeId::String => Some(InternalFunctionTag::StringNotEq),
            _ => None,
        },
    };

    let tag = match tag {
        Some(tag) => tag,
        None => {
            let error = CompilationError::binary_operator_unsupported_types(
                &expression.operator.to_string(),
                lhs.type_().borrow().name(),
                rhs.type_().borrow().name(),
                expression.span,
            );
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let function = Rc::clone(context.program.internal_function(tag));

    let result = program::CallExpr::new(
        function,
        vec![lhs, rhs],
        context.program.types_mut(),
        expression.span,
    );

    match result {
        Ok(expr) => expr,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_address_expr(
    expression: ast::AddressExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let hint =
        type_hint.and_then(|hint| hint.borrow().pointer_target_type(context.program.types()));

    let target = compile_expression(*expression.target, hint, context);

    let result = program::AddressExpr::new(target, context.program.types_mut(), expression.span);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_deref_expr(
    expression: ast::DerefExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let hint = type_hint.map(|hint| context.program.types_mut().pointer_to(&hint));

    let pointer = compile_expression(*expression.pointer, hint, context);

    let result =
        program::DerefExpr::new(pointer, context.program.types_mut(), Some(expression.span));
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_new_expr(
    expression: ast::NewExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let target_type = type_exprs::compile_type_expr(&expression.target_type, context);
    if target_type.borrow().is_error() {
        return program::Expression::error(expression.span);
    }

    let result = program::NewExpr::new(target_type, context.program.types_mut(), expression.span);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_array_from_elements_expr(
    expression: ast::ArrayFromElementsExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let elements: Vec<_> = expression
        .elements
        .into_iter()
        .map(|element| compile_expression(element, None, context))
        .collect();

    let result = program::ArrayFromElementsExpr::new(
        elements,
        context.program.types_mut(),
        type_hint,
        expression.span,
    );
    match result {
        Ok(expression) => expression,
        Err(mut errors) => {
            context.errors.append(&mut errors);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_array_from_copy_expr(
    expression: ast::ArrayFromCopyExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let element = compile_expression(*expression.element, None, context);
    let size = compile_expression(
        *expression.size,
        Some(Rc::clone(context.program.types().int())),
        context,
    );
    if element.is_error() || size.is_error() {
        return program::Expression::error(expression.span);
    }

    let result = program::ArrayFromCopyExpr::new(
        element,
        size,
        context.program.types_mut(),
        expression.span,
    );
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_index_expr(
    expression: ast::IndexExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let collection = compile_expression(*expression.collection, None, context);
    let collection_type = Rc::clone(collection.type_());
    let index = compile_expression(*expression.index, None, context);

    if collection.is_error() || index.is_error() {
        return program::Expression::error(expression.span);
    }

    let method = lookup_method(collection.type_(), "index", expression.span);
    let method = match method {
        Ok(method) => method,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let pointer = program::CallExpr::new(
        method,
        vec![collection, index],
        context.program.types_mut(),
        expression.span,
    );
    let pointer = match pointer {
        Ok(pointer) => pointer,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };
    let pointer_type = Rc::clone(pointer.type_());

    let result =
        program::DerefExpr::new(pointer, context.program.types_mut(), Some(expression.span));
    match result {
        Ok(result) => result,
        Err(_) => {
            let error = CompilationError::index_method_returns_not_pointer(
                collection_type.borrow().name(),
                pointer_type.borrow().name(),
                expression.span,
            );
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_call_expr(
    expression: ast::CallExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let function_name = expression.function_name.text;
    let function_name_span = expression.function_name.span;

    let function = context
        .scope
        .lookup_function(&function_name, function_name_span)
        .map(Rc::clone);

    let function: Rc<RefCell<Function>> = match function {
        Ok(function) => function,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let arguments = compile_arguments(
        expression.arguments.into_iter(),
        function.borrow().parameters.iter(),
        context,
    );

    let result = program::CallExpr::new(
        function,
        arguments,
        context.program.types_mut(),
        expression.span,
    );
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_field_access_expr(
    expression: ast::FieldAccessExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let receiver = compile_expression(*expression.receiver, None, context);
    if receiver.is_error() {
        return program::Expression::error(expression.span);
    }

    // Automatically dereference pointers.
    let receiver = maybe_deref(receiver, context);

    let receiver_type = receiver.type_();

    let field = receiver_type
        .borrow()
        .lookup_field(&expression.field.text, expression.field.span)
        .map(Rc::clone);

    let field = match field {
        Ok(field) => field,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    program::FieldAccessExpr::new(
        receiver,
        Rc::clone(&field),
        context.program.types_mut(),
        expression.span,
    )
}

fn compile_method_call_expr(
    expression: ast::MethodCallExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let receiver_span = expression.receiver.span();
    let receiver = compile_expression(*expression.receiver, None, context);
    if receiver.is_error() {
        return program::Expression::error(expression.span);
    }

    // Automatically dereference pointers.
    let receiver = maybe_deref(receiver, context);

    let method = lookup_method(
        receiver.type_(),
        &expression.method.text,
        expression.method.span,
    );
    let method = match method {
        Ok(method) => method,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.method.span);
        }
    };

    let self_parameter = Rc::clone(
        method
            .borrow()
            .parameters
            .get(0)
            .expect("Method does not have a self parameter"),
    );
    let self_type = Rc::clone(&self_parameter.borrow().type_);

    let self_argument = if self_type == *receiver.type_() {
        // self-by-value
        receiver
    } else if self_type == context.program.types_mut().pointer_to(receiver.type_()) {
        // self-by-pointer
        if receiver.value_category() == ValueCategory::Lvalue {
            // TODO handle synthetic span in a special way for errors.
            program::AddressExpr::new_synthetic(
                receiver,
                context.program.types_mut(),
                receiver_span,
            )
        } else {
            let error =
                CompilationError::self_must_be_lvalue(&expression.method.text, receiver_span);
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    } else {
        panic!("Unexpected method `self` type");
    };

    let arguments = {
        let mut other_arguments = compile_arguments(
            expression.arguments.into_iter(),
            method.borrow().parameters.iter().skip(1),
            context,
        );
        let mut arguments = vec![self_argument];
        arguments.append(&mut other_arguments);
        arguments
    };

    let result = program::CallExpr::new(
        method,
        arguments,
        context.program.types_mut(),
        expression.span,
    );
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn lookup_method(
    receiver_type: &Rc<RefCell<Type>>,
    method_name: &str,
    span: InputSpan,
) -> Result<Rc<RefCell<Function>>, CompilationError> {
    receiver_type
        .borrow()
        .lookup_method(method_name, span)
        .map(Rc::clone)
}

fn compile_arguments<'a>(
    arguments: impl Iterator<Item = ast::Expression>,
    parameters: impl Iterator<Item = &'a Rc<RefCell<Variable>>>,
    context: &mut CompilerContext,
) -> Vec<program::Expression> {
    let hints = parameters
        .map(|parameter| Some(Rc::clone(&parameter.borrow().type_)))
        .chain(iter::repeat(None));
    arguments
        .zip(hints)
        .map(|(argument, hint)| compile_expression(argument, hint, context))
        .collect()
}

fn compile_if_expr(if_: ast::IfExpr, context: &mut CompilerContext) -> program::Expression {
    let span = if_.span;
    let cond = compile_expression(
        *if_.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );
    let then = compile_expression(*if_.then, None, context);
    let else_ = if_
        .else_
        .map(|else_| compile_expression(*else_, Some(Rc::clone(then.type_())), context));

    let result = program::IfExpr::new(cond, then, else_, context.program.types_mut(), span);

    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(if_.span)
        }
    }
}

fn compile_block_expr(
    block: ast::BlockExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    context.scope.push();
    let mut builder = BlockBuilder::new();
    for statement in block.statements {
        compile_statement(statement, &mut builder, context);
    }

    let final_expr = block
        .final_expr
        .map(|expr| compile_expression(*expr, type_hint, context));
    let result = builder.into_expr(final_expr, context.program.types_mut(), block.span);

    context.scope.pop();
    result
}

/// Automatic pointer dereferencing: in some contexts where it's obvious that pointers
/// have to be dereferenced the user can omit the dereference operator.
fn maybe_deref(
    expression: program::Expression,
    context: &mut CompilerContext,
) -> program::Expression {
    let span = expression.span;
    if expression.type_().borrow().is_pointer() {
        program::DerefExpr::new(expression, context.program.types_mut(), span).unwrap()
    } else {
        expression
    }
}
