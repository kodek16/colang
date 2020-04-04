//! Compiler frontend transforms the source code into an intermediate representation
//! defined in `program`. All static checks are performed during this phase.

use lalrpop_util::lalrpop_mod;

mod ast;
mod scope;

pub mod backends;
pub mod errors;
pub mod program;
lalrpop_mod!(pub grammar);

use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
use std::rc::Rc;

use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{
    BlockBuilder, Function, InternalFunctionTag, Parameter, Type, TypeId, UserDefinedFunction,
    ValueCategory, Variable,
};
use crate::scope::Scope;

pub fn run(source_code: &str) -> Result<program::Program, Vec<CompilationError>> {
    let program_ast =
        parse(&source_code).map_err(|err| vec![CompilationError::syntax_error(err)])?;

    let program = compile(program_ast)?;
    Ok(program)
}

/// Parses the source code and returns an AST root.
fn parse(source_code: &str) -> Result<ast::Program, ast::ParseError> {
    grammar::ProgramParser::new().parse(source_code)
}

/// Context that gets passed along to various compiler routines.
struct CompilerContext {
    program: program::Program,
    scope: Scope,
    type_scopes: HashMap<TypeId, Scope>,
    errors: Vec<CompilationError>,
}

impl CompilerContext {
    /// Creates the initial root context.
    pub fn new() -> CompilerContext {
        let mut program = program::Program::new();
        let mut scope = Scope::new();
        let mut type_scopes = HashMap::new();

        for type_ in program.types().primitive_types() {
            scope.add_type(Rc::clone(type_)).unwrap();
        }

        program::internal::populate_internal_symbols(&mut program, &mut scope, &mut type_scopes);
        CompilerContext {
            program,
            scope,
            type_scopes,
            errors: vec![],
        }
    }
}

/// Compiles a CO program.
fn compile(program_ast: ast::Program) -> Result<program::Program, Vec<CompilationError>> {
    let mut context = CompilerContext::new();

    for struct_def in program_ast.structs {
        compile_struct_def(struct_def, &mut context);
    }

    for function_def in program_ast.functions {
        compile_function_def(function_def, &mut context);
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

/// Represents an object that expects statements to be written into it.
trait StatementSink {
    fn emit(&mut self, statement: program::Instruction);
}

impl StatementSink for BlockBuilder {
    fn emit(&mut self, statement: program::Instruction) {
        self.append_instruction(statement)
    }
}

fn compile_struct_def(struct_def: ast::StructDef, context: &mut CompilerContext) {
    let name = &struct_def.name.text;
    let type_ = Type::new_struct(name.to_string(), &mut context.program);
    let type_id = type_.borrow().type_id().clone();

    if let Err(error) = context.scope.add_type(type_) {
        context.errors.push(error);
    }

    for field_def in struct_def.fields {
        compile_field_def(field_def, type_id.clone(), context);
    }
}

fn compile_field_def(
    field_def: ast::FieldDef,
    current_type_id: TypeId,
    context: &mut CompilerContext,
) {
    let type_ = compile_type_expr(field_def.type_, context);

    let field = Variable::new(
        field_def.name.text,
        Rc::clone(&type_),
        Some(field_def.span),
        &mut context.program,
    );
    let field = match field {
        Ok(field) => field,
        Err(error) => {
            context.errors.push(error);
            return;
        }
    };
    let field = Rc::new(RefCell::new(field));

    context
        .program
        .types()
        .lookup(&current_type_id)
        .borrow_mut()
        .add_field(Rc::clone(&field));

    let result = context
        .type_scopes
        .entry(current_type_id)
        .or_insert_with(Scope::new)
        .add_variable(field);
    if let Err(error) = result {
        context.errors.push(error);
    }
}

/// Compiles the given function, adding it to the program symbol table. If any
/// errors occur, they are added to `context`.
fn compile_function_def(function_def: ast::FunctionDef, context: &mut CompilerContext) {
    let name = function_def.name;
    let return_type = match function_def.return_type {
        Some(return_type) => compile_type_expr(return_type, context),
        None => Rc::clone(context.program.types().void()),
    };

    let function = UserDefinedFunction::new(
        name.text,
        Rc::clone(&return_type),
        function_def.signature_span,
        context.program.symbol_ids_mut(),
    );
    let function = Rc::new(RefCell::new(function));
    context.program.add_function(Rc::clone(&function));
    if let Err(error) = context.scope.add_function(Rc::clone(&function)) {
        context.errors.push(error);
    }

    // Parameters have their own scope.
    context.scope.push();
    let parameters: Vec<Rc<RefCell<Variable>>> = function_def
        .parameters
        .into_iter()
        .flat_map(|parameter| compile_parameter(parameter, context))
        .collect();
    function
        .borrow_mut()
        .as_user_defined_mut()
        .fill_parameters(parameters);

    let body = compile_block_expr(function_def.body, Some(Rc::clone(&return_type)), context);

    context.scope.pop();

    let body_type = Rc::clone(&body.type_);
    if let Err(error) = function
        .borrow_mut()
        .as_user_defined_mut()
        .fill_body(body, body_type)
    {
        context.errors.push(error)
    };
}

fn compile_parameter(
    parameter: ast::Parameter,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Variable>>> {
    let name = parameter.name.text;
    let type_ = compile_type_expr(parameter.type_, context);

    create_variable(name, type_, Some(parameter.span), context)
}

/// Compiles a single statement in the syntax sense. If compilation succeeds,
/// resulting target program statements are emitted to `sink`. If any errors
/// occur, they are added to `context`.
fn compile_statement(
    statement: ast::Statement,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    match statement {
        ast::Statement::VarDecl(s) => compile_var_decl_stmt(s, sink, context),
        ast::Statement::Read(s) => compile_read_stmt(s, sink, context),
        ast::Statement::Write(s) => compile_write_stmt(s, sink, context),
        ast::Statement::While(s) => compile_while_stmt(s, sink, context),
        ast::Statement::Assign(s) => compile_assign_stmt(s, sink, context),
        ast::Statement::Expr(s) => compile_expr_stmt(s, sink, context),
    }
}

fn compile_var_decl_stmt(
    statement: ast::VarDeclStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    for declaration in statement.entries {
        compile_var_decl_entry(declaration, sink, context);
    }
}

fn compile_var_decl_entry(
    declaration: ast::VarDeclEntry,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let name = declaration.variable_name;

    // Compile the variable type and the initializer expression
    // even if the variable produces an error.
    let type_ = declaration
        .variable_type
        .map(|type_| compile_type_expr(type_, context));
    let initializer = declaration
        .initializer
        .map(|initializer| compile_expression(initializer, type_.clone(), context));

    // Possibly infer type from the initializer expression.
    let type_ = match type_ {
        Some(t) => t,
        None => match initializer {
            Some(ref expr) => Rc::clone(&expr.type_),
            None => {
                let error = CompilationError::variable_type_omitted(&name.text, declaration.span);
                context.errors.push(error);
                Type::error()
            }
        },
    };

    let variable = create_variable(name.text, type_, Some(declaration.span), context);
    if let Some(variable) = variable {
        let result = program::AllocInstruction::new(&variable, initializer, declaration.span);
        match result {
            Ok(statement) => sink.emit(statement),
            Err(error) => context.errors.push(error),
        }
    }
}

/// Creates a variable, registers it with the program's symbol table, and adds it to the current
/// scope.
fn create_variable(
    name: String,
    type_: Rc<RefCell<Type>>,
    definition_site: Option<InputSpan>,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Variable>>> {
    let result = Variable::new(name, type_, definition_site, &mut context.program);
    let variable = match result {
        Ok(variable) => Rc::new(RefCell::new(variable)),
        Err(error) => {
            context.errors.push(error);
            return None;
        }
    };

    context.program.add_variable(Rc::clone(&variable));
    if let Err(error) = context.scope.add_variable(Rc::clone(&variable)) {
        context.errors.push(error);
    };
    Some(variable)
}

fn compile_read_stmt(
    statement: ast::ReadStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    for entry in statement.entries {
        compile_read_entry(entry, sink, context);
    }
}

fn compile_read_entry(
    entry: ast::ReadEntry,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let target = compile_expression(entry.target, None, context);
    if target.is_error() {
        return;
    }

    let result = program::ReadInstruction::new(target, context.program.types());
    match result {
        Ok(statement) => sink.emit(statement),
        Err(error) => context.errors.push(error),
    }
}

fn compile_write_stmt(
    statement: ast::WriteStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let expr_span = statement.expression.span();
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let result = program::WriteInstruction::new(expression, context.program.types(), expr_span);
    match result {
        Ok(statement) => sink.emit(statement),
        Err(error) => context.errors.push(error),
    }
}

fn compile_while_stmt(
    statement: ast::WhileStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let body_span = statement.body.span;
    let cond = compile_expression(
        *statement.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );
    let body = compile_block_expr(*statement.body, None, context);

    let body_type = &body.type_;
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
        Ok(statement) => sink.emit(statement),
        Err(error) => context.errors.push(error),
    }
}

fn compile_assign_stmt(
    statement: ast::AssignStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let lhs = compile_expression(*statement.lhs, None, context);
    let rhs = compile_expression(*statement.rhs, Some(Rc::clone(&lhs.type_)), context);
    if lhs.is_error() || rhs.is_error() {
        return;
    }

    let result = program::AssignInstruction::new(lhs, rhs, statement.span);
    match result {
        Ok(statement) => sink.emit(statement),
        Err(error) => context.errors.push(error),
    }
}

fn compile_expr_stmt(
    statement: ast::ExprStmt,
    sink: &mut impl StatementSink,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let result = program::EvalInstruction::new(expression);
    sink.emit(result);
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
        ast::Expression::BinaryOp(e) => compile_binary_op_expr(e, context),
        ast::Expression::Address(e) => compile_address_expr(e, type_hint, context),
        ast::Expression::Deref(e) => compile_deref_expr(e, type_hint, context),
        ast::Expression::ArrayFromElements(e) => {
            compile_array_from_elements_expr(e, type_hint, context)
        }
        ast::Expression::ArrayFromCopy(e) => compile_array_from_copy_expr(e, context),
        ast::Expression::Index(e) => compile_index_expr(e, context),
        ast::Expression::Call(e) => compile_call_expr(e, context),
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
        Ok(variable) if variable.borrow().type_().is_error() => {
            program::Expression::error(expression.span)
        }
        Ok(variable) => program::VariableExpr::new(variable, expression.span),
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_int_literal_expr(
    expression: ast::IntLiteralExpr,
    context: &CompilerContext,
) -> program::Expression {
    program::LiteralExpr::int(expression.value, context.program.types(), expression.span)
}

fn compile_bool_literal_expr(
    expression: ast::BoolLiteralExpr,
    context: &CompilerContext,
) -> program::Expression {
    program::LiteralExpr::bool(expression.value, context.program.types(), expression.span)
}

fn compile_binary_op_expr(
    expression: ast::BinaryOperatorExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let tag = match expression.operator {
        ast::BinaryOperator::Add => InternalFunctionTag::AddInt,
        ast::BinaryOperator::Sub => InternalFunctionTag::SubInt,
        ast::BinaryOperator::Mul => InternalFunctionTag::MulInt,
        ast::BinaryOperator::Less => InternalFunctionTag::LessInt,
        ast::BinaryOperator::Greater => InternalFunctionTag::GreaterInt,
        ast::BinaryOperator::LessEq => InternalFunctionTag::LessEqInt,
        ast::BinaryOperator::GreaterEq => InternalFunctionTag::GreaterEqInt,
        ast::BinaryOperator::Eq => InternalFunctionTag::EqInt,
        ast::BinaryOperator::NotEq => InternalFunctionTag::NotEqInt,
    };
    let function = Rc::clone(context.program.internal_function(tag));

    let lhs = compile_expression(*expression.lhs, None, context);
    let rhs = compile_expression(*expression.rhs, None, context);

    if lhs.is_error() || rhs.is_error() {
        return program::Expression::error(expression.span);
    }

    let result = program::CallExpr::new(function, vec![lhs, rhs], expression.span);

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
    let hint = type_hint.map(|hint| context.program.types_mut().pointer_to(&hint.borrow()));

    let pointer = compile_expression(*expression.pointer, hint, context);

    let result = program::DerefExpr::new(pointer, context.program.types(), expression.span);
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
    let location = expression.span;
    let collection = compile_expression(*expression.collection, None, context);
    let index = compile_expression(*expression.index, None, context);

    if collection.is_error() || index.is_error() {
        return program::Expression::error(expression.span);
    }

    let result = program::IndexExpr::new(collection, index, context.program.types(), location);
    match result {
        Ok(expression) => expression,
        Err(error) => {
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
        function.borrow().parameters(),
        context,
    );

    let result = program::CallExpr::new(function, arguments, expression.span);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
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

    let method_name = &expression.method.text;
    let receiver_type_id = receiver.type_.borrow().type_id().clone();

    let method = {
        let receiver_type_scope = context
            .type_scopes
            .entry(receiver_type_id.clone())
            .or_insert_with(Scope::new);

        receiver_type_scope
            .lookup_function(method_name, expression.method.span)
            .map(Rc::clone)
    };

    let method: Rc<RefCell<Function>> = match method {
        Ok(method) => method,
        Err(error) => {
            let instantiated_method = receiver
                .type_
                .borrow()
                .uninstantiate(context.program.types())
                .and_then(|(template, type_parameters)| {
                    template.borrow().method_template(
                        type_parameters.iter().map(|x| x).collect(),
                        method_name,
                        &mut context.program,
                    )
                });

            if let Some(method) = instantiated_method {
                context
                    .type_scopes
                    .get_mut(&receiver_type_id)
                    .unwrap()
                    .add_function(Rc::clone(&method))
                    .expect("Name conflict on instantiating method template");
                method
            } else {
                context.errors.push(error);
                return program::Expression::error(expression.span);
            }
        }
    };

    let self_parameter = method
        .borrow()
        .parameters()
        .next()
        .expect("Method does not have a self parameter");

    let self_argument = if *self_parameter.type_() == receiver.type_ {
        // self-by-value
        receiver
    } else if *self_parameter.type_()
        == context
            .program
            .types_mut()
            .pointer_to(&receiver.type_.borrow())
    {
        // self-by-pointer
        if receiver.value_category == ValueCategory::Lvalue {
            // TODO handle generated span in a special way for errors.
            program::AddressExpr::new(receiver, context.program.types_mut(), receiver_span)
                .expect("Couldn't generate address operation for `self` in method call")
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
            method.borrow().parameters().skip(1),
            context,
        );
        let mut arguments = vec![self_argument];
        arguments.append(&mut other_arguments);
        arguments
    };

    let result = program::CallExpr::new(method, arguments, expression.span);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}

fn compile_arguments(
    arguments: impl Iterator<Item = ast::Expression>,
    parameters: impl Iterator<Item = impl Parameter>,
    context: &mut CompilerContext,
) -> Vec<program::Expression> {
    let hints = parameters
        .map(|parameter| Some(Rc::clone(parameter.type_())))
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
        .map(|else_| compile_expression(*else_, Some(Rc::clone(&then.type_)), context));

    let result = program::IfExpr::new(cond, then, else_, context.program.types(), span);

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
    let result = builder.into_expr(final_expr, context.program.types(), block.span);

    context.scope.pop();
    result
}

fn compile_type_expr(type_expr: ast::TypeExpr, context: &mut CompilerContext) -> Rc<RefCell<Type>> {
    match type_expr {
        ast::TypeExpr::Scalar(type_expr) => compile_scalar_type_expr(type_expr, context),
        ast::TypeExpr::Array(type_expr) => compile_array_type_expr(type_expr, context),
        ast::TypeExpr::Pointer(type_expr) => compile_pointer_type_expr(type_expr, context),
    }
}

fn compile_scalar_type_expr(
    type_expr: ast::ScalarTypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let name = &type_expr.name;
    let type_ = context.scope.lookup_type(&name.text, type_expr.span);

    match type_ {
        Ok(type_) => Rc::clone(type_),
        Err(error) => {
            context.errors.push(error);
            Type::error()
        }
    }
}

fn compile_array_type_expr(
    type_expr: ast::ArrayTypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let element = compile_type_expr(*type_expr.element, context);
    let result = context.program.types_mut().array_of(&element.borrow());
    result
}

fn compile_pointer_type_expr(
    type_expr: ast::PointerTypeExpr,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    let target = compile_type_expr(*type_expr.target, context);
    let result = context.program.types_mut().pointer_to(&target.borrow());
    result
}
