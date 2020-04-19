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

pub fn clone_function_body(
    source_body: Rc<RefCell<Expression>>,
    source_parameters: &Vec<Rc<RefCell<Variable>>>,
    target_parameters: &Vec<Rc<RefCell<Variable>>>,
    variable_clone_fn: VariableCloneFn,
    types: &mut TypeRegistry,
) -> Expression {
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

    clone_expression(&source_body.borrow(), &mut context)
}

fn clone_instruction(instruction: &Instruction, context: &mut CloneContext) -> Instruction {
    use Instruction::*;
    match instruction {
        Assign(instruction) => Assign(clone_assign_instruction(instruction, context)),
        Eval(instruction) => Eval(clone_eval_instruction(instruction, context)),
        While(instruction) => While(clone_while_instruction(instruction, context)),
        Write(instruction) => Write(clone_write_instruction(instruction, context)),
    }
}

fn clone_assign_instruction(
    instruction: &AssignInstruction,
    context: &mut CloneContext,
) -> AssignInstruction {
    AssignInstruction {
        target: Box::new(clone_expression(&instruction.target, context)),
        value: Box::new(clone_expression(&instruction.value, context)),
    }
}

fn clone_eval_instruction(
    instruction: &EvalInstruction,
    context: &mut CloneContext,
) -> EvalInstruction {
    EvalInstruction {
        expression: Box::new(clone_expression(&instruction.expression, context)),
    }
}

fn clone_while_instruction(
    instruction: &WhileInstruction,
    context: &mut CloneContext,
) -> WhileInstruction {
    WhileInstruction {
        cond: Box::new(clone_expression(&instruction.cond, context)),
        body: Box::new(clone_instruction(&instruction.body, context)),
    }
}

fn clone_write_instruction(
    instruction: &WriteInstruction,
    context: &mut CloneContext,
) -> WriteInstruction {
    WriteInstruction {
        expression: clone_expression(&instruction.expression, context),
    }
}

fn clone_expression(expression: &Expression, context: &mut CloneContext) -> Expression {
    use ExpressionKind::*;
    let kind = match expression.kind() {
        Address(expression) => Address(clone_address_expr(expression, context)),
        ArrayFromCopy(expression) => ArrayFromCopy(clone_array_from_copy_expr(expression, context)),
        ArrayFromElements(expression) => {
            ArrayFromElements(clone_array_from_elements_expr(expression, context))
        }
        Block(expression) => Block(clone_block_expr(expression, context)),
        BooleanOp(expression) => BooleanOp(clone_boolean_op_expr(expression, context)),
        Call(expression) => Call(clone_call_expr(expression, context)),
        Deref(expression) => Deref(clone_deref_expr(expression, context)),
        FieldAccess(expression) => FieldAccess(clone_field_access_expr(expression, context)),
        If(expression) => If(clone_if_expr(expression, context)),
        Is(expression) => Is(clone_is_expr(expression, context)),
        Literal(expression) => Literal(clone_literal_expr(expression, context)),
        New(expression) => New(clone_new_expr(expression, context)),
        Null(expression) => Null(clone_null_expr(expression, context)),
        Variable(expression) => Variable(clone_variable_expr(expression, context)),
        Empty => Empty,
        Error(span) => Error(*span),
    };

    Expression::new(kind, context.types)
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

fn clone_block_expr(block: &BlockExpr, context: &mut CloneContext) -> BlockExpr {
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

    let instructions = block
        .instructions
        .iter()
        .map(|instruction| clone_instruction(instruction, context))
        .collect();

    let value = Box::new(clone_expression(&block.value, context));

    BlockExpr {
        local_variables,
        instructions,
        value,
        location: block.location,
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

fn clone_call_expr(expression: &CallExpr, context: &mut CloneContext) -> CallExpr {
    CallExpr {
        function: Rc::clone(&expression.function),
        arguments: expression
            .arguments
            .iter()
            .map(|argument| clone_expression(argument, context))
            .collect(),
        location: expression.location,
    }
}

fn clone_deref_expr(expression: &DerefExpr, context: &mut CloneContext) -> DerefExpr {
    DerefExpr {
        pointer: Box::new(clone_expression(&expression.pointer, context)),
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
    expression.clone()
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
