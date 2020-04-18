//! An interface for different kinds of code processors and rewriters.

use crate::program::*;

/// A base trait for code processors and rewriters that provides a default
/// implementation for a no-op visitor.
pub trait CodeVisitor {
    fn types(&mut self) -> &mut TypeRegistry;

    fn visit_instruction(&mut self, instruction: &mut Instruction) {
        self.walk_instruction(instruction);
    }

    fn walk_instruction(&mut self, instruction: &mut Instruction) {
        match instruction {
            Instruction::Write(instruction) => self.visit_write(instruction),
            Instruction::While(instruction) => self.visit_while(instruction),
            Instruction::Assign(instruction) => self.visit_assign(instruction),
            Instruction::Eval(instruction) => self.visit_eval(instruction),
        }
    }

    fn visit_write(&mut self, instruction: &mut WriteInstruction) {
        self.walk_write(instruction);
    }

    fn walk_write(&mut self, instruction: &mut WriteInstruction) {
        self.visit_expression(&mut instruction.expression);
    }

    fn visit_while(&mut self, instruction: &mut WhileInstruction) {
        self.walk_while(instruction);
    }

    fn walk_while(&mut self, instruction: &mut WhileInstruction) {
        self.visit_expression(&mut instruction.cond);
        self.visit_instruction(&mut instruction.body);
    }

    fn visit_assign(&mut self, instruction: &mut AssignInstruction) {
        self.walk_assign(instruction);
    }

    fn walk_assign(&mut self, instruction: &mut AssignInstruction) {
        self.visit_expression(&mut instruction.target);
        self.visit_expression(&mut instruction.value);
    }

    fn visit_eval(&mut self, instruction: &mut EvalInstruction) {
        self.walk_eval(instruction);
    }

    fn walk_eval(&mut self, instruction: &mut EvalInstruction) {
        self.visit_expression(&mut instruction.expression);
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        self.walk_expression(expression);
    }

    fn walk_expression(&mut self, expression: &mut Expression) {
        use ExpressionKind::*;
        match expression.kind_mut() {
            Address(expression) => self.visit_address_expr(expression),
            ArrayFromCopy(expression) => self.visit_array_from_copy_expr(expression),
            ArrayFromElements(expression) => self.visit_array_from_elements_expr(expression),
            Block(expression) => self.visit_block_expr(expression),
            BooleanOp(expression) => self.visit_boolean_op_expr(expression),
            Call(expression) => self.visit_call_expr(expression),
            Deref(expression) => self.visit_deref_expr(expression),
            FieldAccess(expression) => self.visit_field_access_expr(expression),
            If(expression) => self.visit_if_expr(expression),
            Literal(expression) => self.visit_literal_expr(expression),
            New(expression) => self.visit_new_expr(expression),
            Variable(expression) => self.visit_variable_expr(expression),
            Empty => (),
            Error(_) => (),
        };
        expression.recalculate(self.types())
    }

    fn visit_address_expr(&mut self, expression: &mut AddressExpr) {
        self.walk_address_expr(expression);
    }

    fn walk_address_expr(&mut self, expression: &mut AddressExpr) {
        self.visit_expression(&mut expression.target);
    }

    fn visit_array_from_copy_expr(&mut self, expression: &mut ArrayFromCopyExpr) {
        self.walk_array_from_copy_expr(expression);
    }

    fn walk_array_from_copy_expr(&mut self, expression: &mut ArrayFromCopyExpr) {
        self.visit_expression(&mut expression.element);
        self.visit_expression(&mut expression.size);
    }

    fn visit_array_from_elements_expr(&mut self, expression: &mut ArrayFromElementsExpr) {
        self.walk_array_from_elements_expr(expression);
    }

    fn walk_array_from_elements_expr(&mut self, expression: &mut ArrayFromElementsExpr) {
        for element in expression.elements.iter_mut() {
            self.visit_expression(element);
        }
    }

    fn visit_block_expr(&mut self, block: &mut BlockExpr) {
        self.walk_block_expr(block);
    }

    fn walk_block_expr(&mut self, block: &mut BlockExpr) {
        for variable in block.local_variables.iter_mut() {
            self.visit_local_variable(&mut variable.borrow_mut())
        }

        for instruction in block.instructions.iter_mut() {
            self.visit_instruction(instruction);
        }

        self.visit_expression(&mut block.value);
    }

    fn visit_boolean_op_expr(&mut self, expression: &mut BooleanOpExpr) {
        self.walk_boolean_op_expr(expression);
    }

    fn walk_boolean_op_expr(&mut self, expression: &mut BooleanOpExpr) {
        match &mut expression.op {
            BooleanOp::And(ref mut lhs, ref mut rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
            }
            BooleanOp::Or(ref mut lhs, ref mut rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
            }
            BooleanOp::Not(ref mut operand) => {
                self.visit_expression(operand);
            }
        }
    }

    fn visit_call_expr(&mut self, expression: &mut CallExpr) {
        self.walk_call_expr(expression);
    }

    fn walk_call_expr(&mut self, expression: &mut CallExpr) {
        for argument in expression.arguments.iter_mut() {
            self.visit_expression(argument);
        }
    }

    fn visit_deref_expr(&mut self, expression: &mut DerefExpr) {
        self.walk_deref_expr(expression);
    }

    fn walk_deref_expr(&mut self, expression: &mut DerefExpr) {
        self.visit_expression(&mut expression.pointer);
    }

    fn visit_field_access_expr(&mut self, expression: &mut FieldAccessExpr) {
        self.walk_field_access_expr(expression);
    }

    fn walk_field_access_expr(&mut self, expression: &mut FieldAccessExpr) {
        self.visit_expression(&mut expression.receiver);
    }

    fn visit_if_expr(&mut self, expression: &mut IfExpr) {
        self.walk_if_expr(expression);
    }

    fn walk_if_expr(&mut self, expression: &mut IfExpr) {
        self.visit_expression(&mut expression.cond);
        self.visit_expression(&mut expression.then);
        self.visit_expression(&mut expression.else_);
    }

    fn visit_literal_expr(&mut self, expression: &mut LiteralExpr) {
        self.walk_literal_expr(expression);
    }

    fn walk_literal_expr(&mut self, _expression: &mut LiteralExpr) {}

    fn visit_new_expr(&mut self, expression: &mut NewExpr) {
        self.walk_new_expr(expression);
    }

    fn walk_new_expr(&mut self, _expression: &mut NewExpr) {}

    fn visit_variable_expr(&mut self, expression: &mut VariableExpr) {
        self.walk_variable_expr(expression);
    }

    fn walk_variable_expr(&mut self, _expression: &mut VariableExpr) {}

    fn visit_local_variable(&mut self, _variable: &mut Variable) {}
}
