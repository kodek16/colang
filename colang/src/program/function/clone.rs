//! Cloning function bodies for instantiation.

use crate::program::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type VariableCloneFn<'a> = &'a dyn Fn(&Variable, &mut TypeRegistry) -> Variable;
type VariableMap = HashMap<VariableId, Rc<RefCell<Variable>>>;

struct CloneContext<'a> {
    variable_clone_fn: VariableCloneFn<'a>,
    variable_map: VariableMap,
    types: &'a mut TypeRegistry,
}

/// Creates a copy of a function body performing a transformation on local variables.
///
/// The created copy is "deep": it is supposed to represent a body of a new, different function
/// based on the source function. This can be used to instantiate function bodies from the
/// base function bodies.
///
/// `variable_clone_fn` defines how the local variables in the copied function body are created
/// from the original variables.
pub fn clone_function_body(
    source_body: Rc<RefCell<Statement>>,
    source_parameters: &Vec<Rc<RefCell<Variable>>>,
    target_parameters: &Vec<Rc<RefCell<Variable>>>,
    variable_clone_fn: VariableCloneFn,
    types: &mut TypeRegistry,
) -> Statement {
    assert_eq!(source_parameters.len(), target_parameters.len());

    let variable_map: VariableMap = source_parameters
        .iter()
        .map(|source| source.borrow().id.clone())
        .zip(target_parameters.iter().map(Rc::clone))
        .collect();

    let mut context = CloneContext {
        variable_clone_fn,
        variable_map,
        types,
    };

    clone_statement(&source_body.borrow(), &mut context)
}

fn clone_statement(statement: &Statement, context: &mut CloneContext) -> Statement {
    use Statement::*;
    match statement {
        Assign(statement) => Assign(clone_assign_stmt(statement, context)),
        Block(block) => Block(clone_block(block, context)),
        Call(call) => Call(clone_call(call, context)),
        Eval(statement) => Eval(clone_eval_stmt(statement, context)),
        If(statement) => If(clone_if_stmt(statement, context)),
        Read(statement) => Read(clone_read_stmt(statement, context)),
        Return(statement) => Return(clone_return_stmt(statement, context)),
        Semicolon(statement) => Semicolon(clone_semicolon_stmt(statement)),
        While(statement) => While(clone_while_stmt(statement, context)),
        Write(statement) => Write(clone_write_stmt(statement, context)),
    }
}

fn clone_assign_stmt(statement: &AssignStmt, context: &mut CloneContext) -> AssignStmt {
    AssignStmt {
        target: clone_expression(&statement.target, context),
        value: clone_expression(&statement.value, context),
        location: statement.location,
    }
}

fn clone_eval_stmt(statement: &EvalStmt, context: &mut CloneContext) -> EvalStmt {
    EvalStmt {
        expression: clone_expression(&statement.expression, context),
    }
}

fn clone_if_stmt(statement: &IfStmt, context: &mut CloneContext) -> IfStmt {
    IfStmt {
        cond: Box::new(clone_expression(&statement.cond, context)),
        then: Box::new(clone_statement(&statement.then, context)),
        else_: statement
            .else_
            .as_ref()
            .map(|else_| Box::new(clone_statement(else_, context))),
        location: statement.location,
    }
}

fn clone_read_stmt(statement: &ReadStmt, context: &mut CloneContext) -> ReadStmt {
    ReadStmt {
        target: clone_expression(&statement.target, context),
        whole_line: statement.whole_line,
        location: statement.location,
    }
}

fn clone_return_stmt(statement: &ReturnStmt, context: &mut CloneContext) -> ReturnStmt {
    ReturnStmt {
        expression: statement
            .expression
            .as_ref()
            .map(|expression| clone_expression(expression, context)),
        location: statement.location,
    }
}

fn clone_semicolon_stmt(statement: &SemicolonStmt) -> SemicolonStmt {
    SemicolonStmt {
        location: statement.location,
    }
}

fn clone_while_stmt(statement: &WhileStmt, context: &mut CloneContext) -> WhileStmt {
    WhileStmt {
        cond: clone_expression(&statement.cond, context),
        body: Box::new(clone_statement(&statement.body, context)),
        location: statement.location,
    }
}

fn clone_write_stmt(statement: &WriteStmt, context: &mut CloneContext) -> WriteStmt {
    WriteStmt {
        expression: clone_expression(&statement.expression, context),
        location: statement.location,
    }
}

fn clone_expression(expression: &Expression, context: &mut CloneContext) -> Expression {
    use ExpressionImpl::*;
    let expression = match **expression {
        Address(ref expression) => Address(clone_address_expr(expression, context)),
        ArrayFromCopy(ref expression) => {
            ArrayFromCopy(clone_array_from_copy_expr(expression, context))
        }
        ArrayFromElements(ref expression) => {
            ArrayFromElements(clone_array_from_elements_expr(expression, context))
        }
        Block(ref expression) => Block(clone_block(expression, context)),
        BooleanOp(ref expression) => BooleanOp(clone_boolean_op_expr(expression, context)),
        Call(ref expression) => Call(clone_call(expression, context)),
        Deref(ref expression) => Deref(clone_deref_expr(expression, context)),
        Err(ref expression) => Err(clone_error_expr(expression, context)),
        FieldAccess(ref expression) => FieldAccess(clone_field_access_expr(expression, context)),
        If(ref expression) => If(clone_if_expr(expression, context)),
        Is(ref expression) => Is(clone_is_expr(expression, context)),
        Literal(ref expression) => Literal(clone_literal_expr(expression, context)),
        New(ref expression) => New(clone_new_expr(expression, context)),
        Null(ref expression) => Null(clone_null_expr(expression, context)),
        Variable(ref expression) => Variable(clone_variable_expr(expression, context)),
    };

    Expression::new(expression, context.types)
}

fn clone_address_expr(expression: &AddressExpr, context: &mut CloneContext) -> AddressExpr {
    AddressExpr {
        target: Box::new(clone_expression(&expression.target, context)),
        location: expression.location,
    }
}

fn clone_array_from_copy_expr(
    expression: &ArrayFromCopyExpr,
    context: &mut CloneContext,
) -> ArrayFromCopyExpr {
    ArrayFromCopyExpr {
        element: Box::new(clone_expression(&expression.element, context)),
        size: Box::new(clone_expression(&expression.size, context)),
        location: expression.location,
    }
}

fn clone_array_from_elements_expr(
    expression: &ArrayFromElementsExpr,
    context: &mut CloneContext,
) -> ArrayFromElementsExpr {
    ArrayFromElementsExpr {
        elements: expression
            .elements
            .iter()
            .map(|element| clone_expression(element, context))
            .collect(),
        element_type: Rc::clone(&expression.element_type),
        location: expression.location,
    }
}

fn clone_boolean_op_expr(expression: &BooleanOpExpr, context: &mut CloneContext) -> BooleanOpExpr {
    let op = match &expression.op {
        BooleanOp::And(lhs, rhs) => BooleanOp::And(
            Box::new(clone_expression(lhs, context)),
            Box::new(clone_expression(rhs, context)),
        ),
        BooleanOp::Or(lhs, rhs) => BooleanOp::Or(
            Box::new(clone_expression(lhs, context)),
            Box::new(clone_expression(rhs, context)),
        ),
        BooleanOp::Not(operand) => BooleanOp::Not(Box::new(clone_expression(operand, context))),
    };

    BooleanOpExpr {
        op,
        location: expression.location,
    }
}

fn clone_deref_expr(expression: &DerefExpr, context: &mut CloneContext) -> DerefExpr {
    DerefExpr {
        pointer: Box::new(clone_expression(&expression.pointer, context)),
        location: expression.location,
    }
}

fn clone_error_expr(expression: &ErrorExpr, _: &mut CloneContext) -> ErrorExpr {
    ErrorExpr {
        location: expression.location,
    }
}

fn clone_field_access_expr(
    expression: &FieldAccessExpr,
    context: &mut CloneContext,
) -> FieldAccessExpr {
    FieldAccessExpr {
        receiver: Box::new(clone_expression(&expression.receiver, context)),
        field: Rc::clone(&expression.field),
        location: expression.location,
    }
}

fn clone_if_expr(expression: &IfExpr, context: &mut CloneContext) -> IfExpr {
    IfExpr {
        cond: Box::new(clone_expression(&expression.cond, context)),
        then: Box::new(clone_expression(&expression.then, context)),
        else_: Box::new(clone_expression(&expression.else_, context)),
        location: expression.location,
    }
}

fn clone_literal_expr(expression: &LiteralExpr, _: &mut CloneContext) -> LiteralExpr {
    LiteralExpr {
        value: expression.value.clone(),
        location: expression.location,
    }
}

fn clone_is_expr(expression: &IsExpr, context: &mut CloneContext) -> IsExpr {
    IsExpr {
        lhs: Box::new(clone_expression(&expression.lhs, context)),
        rhs: Box::new(clone_expression(&expression.rhs, context)),
        location: expression.location,
    }
}

fn clone_new_expr(expression: &NewExpr, _: &mut CloneContext) -> NewExpr {
    NewExpr {
        target_type: Rc::clone(&expression.target_type),
        location: expression.location,
    }
}

fn clone_null_expr(expression: &NullExpr, _: &mut CloneContext) -> NullExpr {
    NullExpr {
        target_type: Rc::clone(&expression.target_type),
        location: expression.location,
    }
}

fn clone_variable_expr(expression: &VariableExpr, context: &mut CloneContext) -> VariableExpr {
    VariableExpr {
        variable: Rc::clone(
            context
                .variable_map
                .get(&expression.variable.borrow().id)
                .expect("Attempt to clone access to non-cloned variable"),
        ),
        location: expression.location,
    }
}

fn clone_block(block: &Block, context: &mut CloneContext) -> Block {
    let local_variables = block
        .local_variables
        .iter()
        .map(|variable| {
            let cloned_variable = Rc::new(RefCell::new((*context.variable_clone_fn)(
                &variable.borrow(),
                context.types,
            )));
            context
                .variable_map
                .insert(variable.borrow().id.clone(), Rc::clone(&cloned_variable));
            cloned_variable
        })
        .collect();

    let statements = block
        .statements
        .iter()
        .map(|statement| clone_statement(statement, context))
        .collect();

    let value = block
        .value
        .as_ref()
        .map(|value| Box::new(clone_expression(&value, context)));

    Block {
        local_variables,
        statements,
        value,
        location: block.location,
    }
}

fn clone_call(call: &Call, context: &mut CloneContext) -> Call {
    Call {
        function: Rc::clone(&call.function),
        arguments: call
            .arguments
            .iter()
            .map(|argument| clone_expression(argument, context))
            .collect(),
        location: call.location,
    }
}
