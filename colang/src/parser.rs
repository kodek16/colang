//! The new Pest-powered parser bindings for CO.

use crate::ast;
use crate::source::{InputSpan, InputSpanFile};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::cmp::{max, min};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct COParser;

macro_rules! unexpected_rule {
    ($pair: expr) => {
        panic!("Unexpected syntax rule: {:?}", $pair.as_rule());
    };
}

macro_rules! expect_rule {
    ($pair: expr, $rule: expr) => {
        assert_eq!($pair.as_rule(), $rule);
    };
}

pub fn parse(source_code: &str, file: InputSpanFile) -> ast::Program {
    eprintln!("Entering!");
    match COParser::parse(Rule::program, source_code) {
        Ok(mut program) => {
            eprintln!("Exited");
            let program = program.next().unwrap();
            let mut result = ast::Program::new();

            for definition in program.into_inner() {
                match definition.as_rule() {
                    Rule::function_def => {
                        let function_def = handle_function_def(definition, file);
                        result.functions.push(function_def);
                    }
                    Rule::struct_def => {
                        let struct_def = handle_struct_def(definition, file);
                        result.structs.push(struct_def);
                    }
                    Rule::trait_def => {
                        let trait_def = handle_trait_def(definition);
                        result.traits.push(trait_def);
                    }

                    Rule::EOI => {
                        break;
                    }

                    _ => unexpected_rule!(definition),
                };
            }

            result
        }
        Err(error) => {
            eprintln!("{}", error);
            unimplemented!()
        }
    }
}

macro_rules! maybe_subrule {
    ($iter: expr, $rule: pat) => {
        $iter.peek().and_then(|pair| match pair.as_rule() {
            $rule => $iter.next(),
            _ => None,
        })
    };
}

fn handle_function_def(function_def: Pair<'_, Rule>, file: InputSpanFile) -> ast::FunctionDef {
    expect_rule!(function_def, Rule::function_def);
    let mut function_def = function_def.into_inner();

    let keyword = function_def.next().unwrap();
    let name = function_def.next().unwrap();
    let param_list = function_def.next().unwrap();
    let return_type = maybe_subrule!(function_def, Rule::return_type);
    let body = maybe_subrule!(function_def, Rule::function_body);

    let signature_span = span_of(&[&keyword, &param_list])
        .and_maybe(&[&return_type])
        .build(file);

    ast::FunctionDef {
        name: handle_ident(name, file),
        parameters: handle_param_list(param_list, file),
        return_type: return_type.map(|rt| handle_type_expr(rt.into_inner().next().unwrap(), file)),
        body: body.map(|body| {
            ast::Expression::Block(handle_block(body.into_inner().next().unwrap(), file))
        }),
        signature_span,
    }
}

fn handle_param_list(param_list: Pair<'_, Rule>, file: InputSpanFile) -> Vec<ast::Parameter> {
    expect_rule!(param_list, Rule::param_list);
    param_list
        .into_inner()
        .map(|param| handle_parameter(param, file))
        .collect()
}

fn handle_parameter(parameter: Pair<'_, Rule>, file: InputSpanFile) -> ast::Parameter {
    expect_rule!(parameter, Rule::param_def);
    let parameter = parameter.into_inner().next().unwrap();
    let span = span_of_just(&parameter, file);

    match parameter.as_rule() {
        Rule::normal_param_def => {
            let mut parameter = parameter.into_inner();

            let name = parameter.next().unwrap();
            let type_ = parameter.next().unwrap();

            ast::Parameter::Normal(ast::NormalParameter {
                name: handle_ident(name, file),
                type_: handle_type_expr(type_, file),
                span,
            })
        }
        Rule::self_by_pointer_param_def => ast::Parameter::Self_(ast::SelfParameter {
            kind: ast::SelfParameterKind::ByPointer,
            span,
        }),
        Rule::self_by_value_param_def => ast::Parameter::Self_(ast::SelfParameter {
            kind: ast::SelfParameterKind::ByValue,
            span,
        }),
        _ => unexpected_rule!(parameter),
    }
}

fn handle_struct_def(struct_def: Pair<'_, Rule>, file: InputSpanFile) -> ast::TypeDef {
    expect_rule!(struct_def, Rule::struct_def);
    let mut struct_def = struct_def.into_inner();

    let keyword = struct_def.next().unwrap();
    let name = struct_def.next().unwrap();
    let type_param_list = maybe_subrule!(struct_def, Rule::type_param_list);
    let implemented_traits_list = maybe_subrule!(struct_def, Rule::implemented_traits_list);
    let body = struct_def.next().unwrap();

    let signature_span = span_of(&[&keyword, &name])
        .and_maybe(&[&type_param_list])
        .build(file);

    let (fields, methods) = handle_user_type_body(body, file);

    ast::TypeDef {
        name: handle_ident(name, file),
        type_parameters: type_param_list
            .map(|tpl| handle_type_param_list(tpl, file))
            .unwrap_or(vec![]),
        implemented_traits: implemented_traits_list
            .map(|itl| handle_implemented_traits_list(itl, file))
            .unwrap_or(vec![]),
        fields,
        methods,
        signature_span,
    }
}

fn handle_type_param_list(
    type_params: Pair<'_, Rule>,
    file: InputSpanFile,
) -> Vec<ast::TypeParameter> {
    expect_rule!(type_params, Rule::type_param_list);
    type_params
        .into_inner()
        .map(|param| handle_type_param_def(param, file))
        .collect()
}

fn handle_type_param_def(
    type_param_def: Pair<'_, Rule>,
    file: InputSpanFile,
) -> ast::TypeParameter {
    expect_rule!(type_param_def, Rule::type_param_def);
    let span = span_of_just(&type_param_def, file);
    let mut type_param_def = type_param_def.into_inner();

    let name = type_param_def.next().unwrap();
    let bounds = type_param_def.next();

    ast::TypeParameter {
        name: handle_ident(name, file),
        trait_bounds: bounds
            .map(|bounds| handle_type_param_bounds(bounds, file))
            .unwrap_or(vec![]),
        span,
    }
}

fn handle_type_param_bounds(bounds: Pair<'_, Rule>, file: InputSpanFile) -> Vec<ast::TypeExpr> {
    expect_rule!(bounds, Rule::type_param_bounds);
    bounds
        .into_inner()
        .map(|bound| handle_type_expr(bound, file))
        .collect()
}

fn handle_implemented_traits_list(
    traits: Pair<'_, Rule>,
    file: InputSpanFile,
) -> Vec<ast::TypeExpr> {
    expect_rule!(traits, Rule::implemented_traits_list);
    traits
        .into_inner()
        .map(|bound| handle_type_expr(bound, file))
        .collect()
}

fn handle_user_type_body(
    body: Pair<'_, Rule>,
    file: InputSpanFile,
) -> (Vec<ast::FieldDef>, Vec<ast::FunctionDef>) {
    expect_rule!(body, Rule::user_type_body);
    let body = body.into_inner().next().unwrap().into_inner().skip(1);

    let mut fields = Vec::new();
    let mut methods = Vec::new();

    for member in body {
        match member.as_rule() {
            Rule::method_def => {
                methods.push(handle_function_def(member, file));
            }
            Rule::field_def => {
                fields.push(handle_field_def(member, file));
            }
            _ => unexpected_rule!(member),
        }
    }

    (fields, methods)
}

fn handle_field_def(field_def: Pair<'_, Rule>, file: InputSpanFile) -> ast::FieldDef {
    expect_rule!(field_def, Rule::field_def);
    unimplemented!()
}

fn handle_trait_def(trait_def: Pair<'_, Rule>) -> ast::TypeDef {
    expect_rule!(trait_def, Rule::trait_def);
    unimplemented!()
}

fn handle_stmt(stmt: Pair<'_, Rule>, file: InputSpanFile) -> ast::Statement {
    expect_rule!(stmt, Rule::stmt);
    let stmt = stmt.into_inner().next().unwrap();

    match stmt.as_rule() {
        Rule::read_stmt => ast::Statement::Read(handle_read_stmt(stmt, false, file)),
        Rule::readln_stmt => ast::Statement::Read(handle_read_stmt(stmt, true, file)),
        Rule::return_stmt => ast::Statement::Return(handle_return_stmt(stmt, file)),
        Rule::var_decl_stmt => ast::Statement::VarDecl(handle_var_decl_stmt(stmt, file)),
        Rule::while_stmt => ast::Statement::While(handle_while_stmt(stmt, file)),
        Rule::write_stmt => ast::Statement::Write(handle_write_stmt(stmt, false, file)),
        Rule::writeln_stmt => ast::Statement::Write(handle_write_stmt(stmt, true, file)),
        Rule::assign_stmt => ast::Statement::Assign(handle_assign_stmt(stmt, file)),
        Rule::expr_stmt => ast::Statement::Expr(handle_expr_stmt(stmt, file)),

        _ => unexpected_rule!(stmt),
    }
}

fn handle_assign_stmt(stmt: Pair<'_, Rule>, file: InputSpanFile) -> ast::AssignStmt {
    expect_rule!(stmt, Rule::assign_stmt);
    let span = span_of_just(&stmt, file);
    let mut stmt = stmt.into_inner();

    let lhs = stmt.next().unwrap();
    let rhs = stmt.next().unwrap();

    ast::AssignStmt {
        lhs: Box::new(handle_expr(lhs, file)),
        rhs: Box::new(handle_expr(rhs, file)),
        span,
    }
}

fn handle_expr_stmt(stmt: Pair<'_, Rule>, file: InputSpanFile) -> ast::ExprStmt {
    expect_rule!(stmt, Rule::expr_stmt);
    let span = span_of_just(&stmt, file);
    let expression = stmt.into_inner().next().unwrap();

    ast::ExprStmt {
        expression: handle_expr(expression, file),
        span,
    }
}

// Can handle both `read` and `readln`.
fn handle_read_stmt(stmt: Pair<'_, Rule>, whole_line: bool, file: InputSpanFile) -> ast::ReadStmt {
    let span = span_of_just(&stmt, file);

    ast::ReadStmt {
        entries: stmt
            .into_inner()
            .map(|target| ast::ReadEntry {
                span: span_of_just(&target, file),
                target: handle_expr(target, file),
            })
            .collect(),

        whole_line,
        span,
    }
}

fn handle_return_stmt(stmt: Pair<'_, Rule>, file: InputSpanFile) -> ast::ReturnStmt {
    expect_rule!(stmt, Rule::return_stmt);
    let span = span_of_just(&stmt, file);
    let mut stmt = stmt.into_inner();

    let _ = stmt.next().unwrap();
    let expression = stmt.next();

    ast::ReturnStmt {
        expression: expression.map(|expr| handle_expr(expr, file)),
        span,
    }
}

fn handle_var_decl_stmt(stmt: Pair<'_, Rule>, file: InputSpanFile) -> ast::VarDeclStmt {
    expect_rule!(stmt, Rule::var_decl_stmt);
    let span = span_of_just(&stmt, file);

    ast::VarDeclStmt {
        entries: stmt
            .into_inner()
            .skip(1)
            .map(|entry| handle_var_decl_entry(entry, file))
            .collect(),
        span,
    }
}

fn handle_var_decl_entry(entry: Pair<'_, Rule>, file: InputSpanFile) -> ast::VarDeclEntry {
    expect_rule!(entry, Rule::var_decl_entry);
    let span = span_of_just(&entry, file);
    let mut entry = entry.into_inner();

    let variable_name = entry.next().unwrap();
    let variable_type = maybe_subrule!(entry, Rule::var_type);
    let initializer = maybe_subrule!(entry, Rule::var_initializer);

    ast::VarDeclEntry {
        variable_name: handle_ident(variable_name, file),
        variable_type: variable_type
            .map(|vt| handle_type_expr(vt.into_inner().next().unwrap(), file)),
        initializer: initializer.map(|init| handle_expr(init.into_inner().next().unwrap(), file)),
        span,
    }
}

fn handle_while_stmt(stmt: Pair<'_, Rule>, file: InputSpanFile) -> ast::WhileStmt {
    expect_rule!(stmt, Rule::while_stmt);
    let span = span_of_just(&stmt, file);
    let mut stmt = stmt.into_inner();

    let _ = stmt.next().unwrap();
    let cond = stmt.next().unwrap();
    let body = stmt.next().unwrap();

    ast::WhileStmt {
        cond: Box::new(handle_expr(cond, file)),
        body: Box::new(ast::Expression::Block(handle_block(body, file))),
        span,
    }
}

// Can handle both `read` and `readln`.
fn handle_write_stmt(stmt: Pair<'_, Rule>, newline: bool, file: InputSpanFile) -> ast::WriteStmt {
    let span = span_of_just(&stmt, file);
    let expression = stmt.into_inner().skip(1).next().unwrap();

    ast::WriteStmt {
        expression: handle_expr(expression, file),
        newline,
        span,
    }
}

// Can handle both `Rule::expr` and any of the concrete expr rules.
fn handle_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::Expression {
    match expr.as_rule() {
        Rule::expr => handle_expr(expr.into_inner().next().unwrap(), file),

        Rule::block => ast::Expression::Block(handle_block(expr, file)),

        Rule::if_expr => ast::Expression::If(handle_if_expr(expr, file)),

        Rule::new_expr => ast::Expression::New(handle_new_expr(expr, file)),

        Rule::disjunction_expr => ast::Expression::BinaryOp(handle_binary_op_expr(expr, file)),

        Rule::conjunction_expr => ast::Expression::BinaryOp(handle_binary_op_expr(expr, file)),

        Rule::equality_expr => ast::Expression::BinaryOp(handle_binary_op_expr(expr, file)),

        Rule::is_expr => ast::Expression::Is(handle_is_expr(expr, file)),

        Rule::comparison_expr => ast::Expression::BinaryOp(handle_binary_op_expr(expr, file)),

        Rule::addition_expr => ast::Expression::BinaryOp(handle_binary_op_expr(expr, file)),

        Rule::multiplication_expr => ast::Expression::BinaryOp(handle_binary_op_expr(expr, file)),

        Rule::address_expr => ast::Expression::Address(handle_address_expr(expr, file)),
        Rule::deref_expr => ast::Expression::Deref(handle_deref_expr(expr, file)),
        Rule::logical_not_expr => ast::Expression::UnaryOp(handle_logical_not_expr(expr, file)),

        Rule::method_call_expr => ast::Expression::MethodCall(handle_method_call_expr(expr, file)),
        Rule::field_access_expr => {
            ast::Expression::FieldAccess(handle_field_access_expr(expr, file))
        }
        Rule::index_expr => ast::Expression::Index(handle_index_expr(expr, file)),
        Rule::call_expr => ast::Expression::Call(handle_call_expr(expr, file)),
        Rule::primary_expr => handle_expr(expr, file),

        Rule::array_from_copy_expr => {
            ast::Expression::ArrayFromCopy(handle_array_from_copy_expr(expr, file))
        }
        Rule::array_from_elems_expr => {
            ast::Expression::ArrayFromElements(handle_array_from_elems_expr(expr, file))
        }
        Rule::char_literal_expr => {
            ast::Expression::CharLiteral(handle_char_literal_expr(expr, file))
        }
        Rule::string_literal_expr => {
            ast::Expression::StringLiteral(handle_string_literal_expr(expr, file))
        }
        Rule::null_expr => ast::Expression::Null(handle_null_expr(expr, file)),
        Rule::self_expr => ast::Expression::Self_(handle_self_expr(expr, file)),
        Rule::bool_literal_expr => {
            ast::Expression::BoolLiteral(handle_bool_literal_expr(expr, file))
        }
        Rule::int_literal_expr => ast::Expression::IntLiteral(handle_int_literal_expr(expr, file)),
        Rule::variable_expr => ast::Expression::Variable(handle_variable_expr(expr, file)),

        _ => unexpected_rule!(expr),
    }
}

fn handle_block(block: Pair<'_, Rule>, file: InputSpanFile) -> ast::BlockExpr {
    expect_rule!(block, Rule::block);
    let span = span_of_just(&block, file);

    // Split pairs into (normal..,  last).
    let mut block: Vec<_> = block.into_inner().collect();
    let statements: Vec<_> = block.drain(..block.len() - 1).collect();
    let final_expr = block.drain(..1).next();

    // Coerce all items except last into statements.
    let mut statements: Vec<_> = statements
        .into_iter()
        .map(|stmt| match stmt.as_rule() {
            Rule::stmt => handle_stmt(stmt, file),
            Rule::expr => {
                let expression = handle_expr(stmt, file);
                ast::Statement::Expr(ast::ExprStmt {
                    span: expression.span(),
                    expression,
                })
            }
            _ => unreachable!(),
        })
        .collect();

    let final_expr = final_expr
        .and_then(|final_expr| match final_expr.as_rule() {
            Rule::stmt => {
                statements.push(handle_stmt(final_expr, file));
                None
            }
            Rule::expr => Some(handle_expr(final_expr, file)),
            _ => unreachable!(),
        })
        .map(Box::new);

    ast::BlockExpr {
        statements,
        final_expr,
        span,
    }
}

fn handle_if_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::IfExpr {
    expect_rule!(expr, Rule::if_expr);
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let _ = expr.next().unwrap();
    let cond = expr.next().unwrap();
    let then = expr.next().unwrap();

    let else_ = if expr.peek().is_some() {
        let _ = expr.next().unwrap();
        Some(expr.next().unwrap())
    } else {
        None
    };

    ast::IfExpr {
        cond: Box::new(handle_expr(cond, file)),
        then: Box::new(handle_expr(then, file)),
        else_: else_.map(|else_| Box::new(handle_expr(else_, file))),
        span,
    }
}

fn handle_new_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::NewExpr {
    expect_rule!(expr, Rule::new_expr);
    let span = span_of_just(&expr, file);
    let target_type = expr.into_inner().skip(1).next().unwrap();

    ast::NewExpr {
        target_type: handle_type_expr(target_type, file),
        span,
    }
}

fn handle_is_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::IsExpr {
    expect_rule!(expr, Rule::is_expr);
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let lhs = expr.next().unwrap();
    let _ = expr.next().unwrap();
    let rhs = expr.next().unwrap();

    ast::IsExpr {
        lhs: Box::new(handle_expr(lhs, file)),
        rhs: Box::new(handle_expr(rhs, file)),
        span,
    }
}

fn handle_binary_op_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::BinaryOperatorExpr {
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let lhs = expr.next().unwrap();
    let op = expr.next().unwrap();
    let rhs = expr.next().unwrap();

    ast::BinaryOperatorExpr {
        operator: handle_binary_op(op),
        lhs: Box::new(handle_expr(lhs, file)),
        rhs: Box::new(handle_expr(rhs, file)),
        span,
    }
}

fn handle_binary_op(op: Pair<'_, Rule>) -> ast::BinaryOperator {
    match op.as_str() {
        "&&" => ast::BinaryOperator::LogicalAnd,
        "||" => ast::BinaryOperator::LogicalOr,

        "==" => ast::BinaryOperator::Eq,
        "!=" => ast::BinaryOperator::NotEq,

        "<=" => ast::BinaryOperator::LessEq,
        ">=" => ast::BinaryOperator::GreaterEq,
        "<" => ast::BinaryOperator::Less,
        ">" => ast::BinaryOperator::Greater,

        "+" => ast::BinaryOperator::Add,
        "-" => ast::BinaryOperator::Sub,

        "*" => ast::BinaryOperator::Mul,
        "/" => ast::BinaryOperator::Div,
        "%" => ast::BinaryOperator::Mod,

        _ => panic!("Unknown binary operator: `{}`", op.as_str()),
    }
}

fn handle_address_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::AddressExpr {
    expect_rule!(expr, Rule::address_expr);
    let span = span_of_just(&expr, file);

    let target = expr.into_inner().next().unwrap();

    ast::AddressExpr {
        target: Box::new(handle_expr(target, file)),
        span,
    }
}

fn handle_deref_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::DerefExpr {
    expect_rule!(expr, Rule::deref_expr);
    let span = span_of_just(&expr, file);

    let pointer = expr.into_inner().next().unwrap();

    ast::DerefExpr {
        pointer: Box::new(handle_expr(pointer, file)),
        span,
    }
}

fn handle_logical_not_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::UnaryOperatorExpr {
    expect_rule!(expr, Rule::logical_not_expr);
    let span = span_of_just(&expr, file);

    let operand = expr.into_inner().next().unwrap();

    ast::UnaryOperatorExpr {
        operator: ast::UnaryOperator::LogicalNot,
        operand: Box::new(handle_expr(operand, file)),
        span,
    }
}

fn handle_index_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::IndexExpr {
    expect_rule!(expr, Rule::index_expr);
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let collection = expr.next().unwrap();
    let index = expr.next().unwrap();

    ast::IndexExpr {
        collection: Box::new(handle_expr(collection, file)),
        index: Box::new(handle_expr(index, file)),
        span,
    }
}

fn handle_call_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::CallExpr {
    expect_rule!(expr, Rule::call_expr);
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let function = expr.next().unwrap();
    let args = expr.next();

    ast::CallExpr {
        function_name: handle_ident(function, file),
        arguments: args
            .map(|args| handle_call_arg_list(args, file))
            .unwrap_or(Vec::new()),
        span,
    }
}

fn handle_field_access_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::FieldAccessExpr {
    expect_rule!(expr, Rule::field_access_expr);
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let receiver = expr.next().unwrap();
    let field = expr.next().unwrap();

    ast::FieldAccessExpr {
        receiver: Box::new(handle_expr(receiver, file)),
        field: handle_ident(field, file),
        span,
    }
}

fn handle_method_call_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::MethodCallExpr {
    expect_rule!(expr, Rule::method_call_expr);
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let receiver = expr.next().unwrap();
    let method = expr.next().unwrap();
    let args = expr.next();

    ast::MethodCallExpr {
        receiver: Box::new(handle_expr(receiver, file)),
        method: handle_ident(method, file),
        arguments: args
            .map(|args| handle_call_arg_list(args, file))
            .unwrap_or(Vec::new()),
        span,
    }
}

fn handle_call_arg_list(args: Pair<'_, Rule>, file: InputSpanFile) -> Vec<ast::Expression> {
    expect_rule!(args, Rule::call_arg_list);
    args.into_inner()
        .map(|arg| handle_expr(arg, file))
        .collect()
}

fn handle_variable_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::VariableExpr {
    expect_rule!(expr, Rule::variable_expr);
    let span = span_of_just(&expr, file);
    let name = handle_ident(expr.into_inner().next().unwrap(), file);
    ast::VariableExpr { name, span }
}

fn handle_int_literal_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::IntLiteralExpr {
    expect_rule!(expr, Rule::int_literal_expr);
    let span = span_of_just(&expr, file);
    let value = expr.as_str().parse().unwrap();
    ast::IntLiteralExpr { value, span }
}

fn handle_bool_literal_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::BoolLiteralExpr {
    expect_rule!(expr, Rule::bool_literal_expr);
    let span = span_of_just(&expr, file);
    let literal = expr.into_inner().next().unwrap();
    let value = match literal.as_rule() {
        Rule::KW_TRUE => true,
        Rule::KW_FALSE => false,
        _ => unexpected_rule!(literal),
    };
    ast::BoolLiteralExpr { value, span }
}

fn handle_char_literal_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::CharLiteralExpr {
    expect_rule!(expr, Rule::char_literal_expr);
    let span = span_of_just(&expr, file);
    ast::CharLiteralExpr {
        value: expr.as_str()[1..expr.as_str().len() - 1].to_string(),
        span,
    }
}

fn handle_string_literal_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::StringLiteralExpr {
    expect_rule!(expr, Rule::string_literal_expr);
    let span = span_of_just(&expr, file);
    ast::StringLiteralExpr {
        value: expr.as_str()[1..expr.as_str().len() - 1].to_string(),
        span,
    }
}

fn handle_null_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::NullExpr {
    expect_rule!(expr, Rule::null_expr);
    let span = span_of_just(&expr, file);
    ast::NullExpr { span }
}

fn handle_self_expr(expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::SelfExpr {
    expect_rule!(expr, Rule::self_expr);
    let span = span_of_just(&expr, file);
    ast::SelfExpr { span }
}

fn handle_array_from_elems_expr(
    expr: Pair<'_, Rule>,
    file: InputSpanFile,
) -> ast::ArrayFromElementsExpr {
    expect_rule!(expr, Rule::array_from_elems_expr);
    let span = span_of_just(&expr, file);

    let elements = expr
        .into_inner()
        .map(|elem| handle_expr(elem, file))
        .collect();

    ast::ArrayFromElementsExpr { elements, span }
}

fn handle_array_from_copy_expr(
    expr: Pair<'_, Rule>,
    file: InputSpanFile,
) -> ast::ArrayFromCopyExpr {
    expect_rule!(expr, Rule::array_from_copy_expr);
    let span = span_of_just(&expr, file);
    let mut expr = expr.into_inner();

    let element = expr.next().unwrap();
    let size = expr.next().unwrap();

    ast::ArrayFromCopyExpr {
        element: Box::new(handle_expr(element, file)),
        size: Box::new(handle_expr(size, file)),
        span,
    }
}

// Type expressions:

fn handle_type_expr(type_expr: Pair<'_, Rule>, file: InputSpanFile) -> ast::TypeExpr {
    expect_rule!(type_expr, Rule::type_expr);
    let type_expr = type_expr.into_inner().next().unwrap();

    match type_expr.as_rule() {
        Rule::scalar_type_expr => ast::TypeExpr::Scalar(handle_scalar_type_expr(type_expr, file)),
        Rule::array_type_expr => ast::TypeExpr::Array(handle_array_type_expr(type_expr, file)),
        Rule::pointer_type_expr => {
            ast::TypeExpr::Pointer(handle_pointer_type_expr(type_expr, file))
        }
        Rule::tmpl_inst_type_expr => {
            ast::TypeExpr::TemplateInstance(handle_tmpl_inst_type_expr(type_expr, file))
        }
        _ => unexpected_rule!(type_expr),
    }
}

fn handle_scalar_type_expr(
    scalar_type_expr: Pair<'_, Rule>,
    file: InputSpanFile,
) -> ast::ScalarTypeExpr {
    expect_rule!(scalar_type_expr, Rule::scalar_type_expr);
    let name = scalar_type_expr.into_inner().next().unwrap();

    ast::ScalarTypeExpr {
        span: span_of_just(&name, file),
        name: handle_ident(name, file),
    }
}

fn handle_array_type_expr(
    array_type_expr: Pair<'_, Rule>,
    file: InputSpanFile,
) -> ast::ArrayTypeExpr {
    expect_rule!(array_type_expr, Rule::array_type_expr);
    let span = span_of_just(&array_type_expr, file);

    let element_type = array_type_expr.into_inner().next().unwrap();

    ast::ArrayTypeExpr {
        span,
        element: Box::new(handle_type_expr(element_type, file)),
    }
}

fn handle_pointer_type_expr(
    pointer_type_expr: Pair<'_, Rule>,
    file: InputSpanFile,
) -> ast::PointerTypeExpr {
    expect_rule!(pointer_type_expr, Rule::pointer_type_expr);
    let span = span_of_just(&pointer_type_expr, file);

    let target_type = pointer_type_expr.into_inner().next().unwrap();

    ast::PointerTypeExpr {
        span,
        target: Box::new(handle_type_expr(target_type, file)),
    }
}

fn handle_tmpl_inst_type_expr(
    tmpl_inst_type_expr: Pair<'_, Rule>,
    file: InputSpanFile,
) -> ast::TemplateInstanceTypeExpr {
    expect_rule!(tmpl_inst_type_expr, Rule::tmpl_inst_type_expr);
    let span = span_of_just(&tmpl_inst_type_expr, file);
    let mut tmpl_inst_type_expr = tmpl_inst_type_expr.into_inner();

    let template = tmpl_inst_type_expr.next().unwrap();
    let type_arguments = tmpl_inst_type_expr
        .map(|type_arg| handle_type_expr(type_arg, file))
        .collect();

    ast::TemplateInstanceTypeExpr {
        span,
        type_arguments,
        template: handle_ident(template, file),
    }
}

// Terminals:

fn handle_ident(ident: Pair<'_, Rule>, file: InputSpanFile) -> ast::Identifier {
    expect_rule!(ident, Rule::IDENT);
    ast::Identifier {
        text: ident.as_str().to_string(),
        span: span_of_just(&ident, file),
    }
}

// Span conversion utilities:

struct SpanBuilder {
    start: usize,
    end: usize,
}

impl SpanBuilder {
    fn from_pest(span: &pest::Span) -> SpanBuilder {
        SpanBuilder {
            start: span.start(),
            end: span.end(),
        }
    }

    fn extend_with(&mut self, span: &pest::Span) {
        self.start = min(self.start, span.start());
        self.end = max(self.end, span.end())
    }

    fn and_maybe(mut self, pairs: &[&Option<Pair<'_, Rule>>]) -> SpanBuilder {
        for pair in pairs {
            if let Some(pair) = pair {
                self.extend_with(&pair.as_span())
            }
        }
        self
    }

    fn build(self, file: InputSpanFile) -> InputSpan {
        InputSpan {
            file,
            start: self.start,
            end: self.end,
        }
    }
}

fn span_of_just(pair: &Pair<'_, Rule>, file: InputSpanFile) -> InputSpan {
    InputSpan {
        start: pair.as_span().start(),
        end: pair.as_span().end(),
        file,
    }
}

fn span_of(pairs: &[&Pair<'_, Rule>]) -> SpanBuilder {
    let mut builder = SpanBuilder::from_pest(&pairs[0].as_span());
    for pair in &pairs[1..] {
        builder.extend_with(&pair.as_span())
    }
    builder
}
