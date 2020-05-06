use crate::program::expressions::empty::EmptyExpr;
use crate::program::expressions::error::ErrorExpr;
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
        Read(instruction) => Read(clone_read_instruction(instruction, context)),
        Return(instruction) => Return(clone_return_instruction(instruction, context)),
        While(instruction) => While(clone_while_instruction(instruction, context)),
        Write(instruction) => Write(clone_write_instruction(instruction, context)),
    }
}

fn clone_assign_instruction(
    instruction: &AssignInstruction,
    context: &mut CloneContext,
) -> AssignInstruction {
    AssignInstruction {
        target: clone_expression(&instruction.target, context),
        value: clone_expression(&instruction.value, context),
        location: instruction.location,
    }
}

fn clone_eval_instruction(
    instruction: &EvalInstruction,
    context: &mut CloneContext,
) -> EvalInstruction {
    EvalInstruction {
        expression: clone_expression(&instruction.expression, context),
    }
}

fn clone_read_instruction(
    instruction: &ReadInstruction,
    context: &mut CloneContext,
) -> ReadInstruction {
    ReadInstruction {
        target: clone_expression(&instruction.target, context),
        whole_line: instruction.whole_line,
        location: instruction.location,
    }
}

fn clone_return_instruction(
    instruction: &ReturnInstruction,
    context: &mut CloneContext,
) -> ReturnInstruction {
    ReturnInstruction {
        expression: clone_expression(&instruction.expression, context),
        location: instruction.location,
    }
}

fn clone_while_instruction(
    instruction: &WhileInstruction,
    context: &mut CloneContext,
) -> WhileInstruction {
    WhileInstruction {
        cond: clone_expression(&instruction.cond, context),
        body: Box::new(clone_instruction(&instruction.body, context)),
        location: instruction.location,
    }
}

fn clone_write_instruction(
    instruction: &WriteInstruction,
    context: &mut CloneContext,
) -> WriteInstruction {
    WriteInstruction {
        expression: clone_expression(&instruction.expression, context),
        location: instruction.location,
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
        Block(ref expression) => Block(clone_block_expr(expression, context)),
        BooleanOp(ref expression) => BooleanOp(clone_boolean_op_expr(expression, context)),
        Call(ref expression) => Call(clone_call_expr(expression, context)),
        Deref(ref expression) => Deref(clone_deref_expr(expression, context)),
        Empty(ref expression) => Empty(clone_empty_expr(expression, context)),
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

fn clone_empty_expr(expression: &EmptyExpr, _: &mut CloneContext) -> EmptyExpr {
    EmptyExpr {
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
