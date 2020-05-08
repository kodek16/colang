use crate::program::visitors::LocalCodeNode;
use crate::program::*;

/// A framework trait for function body code processors and rewriters.
///
/// Unlike `GlobalVisitor`, this trait is concerned with "local", i.e. function body code.
///
/// Users of the trait should generally override the `visit_*` methods they care about, always
/// remembering to delegate to `walk` for traversing the subtree.
pub trait LocalVisitor {
    fn types(&mut self) -> &mut TypeRegistry;

    fn visit_instruction(&mut self, instruction: &mut Instruction) {
        self.walk_instruction(instruction);
    }

    fn walk_instruction(&mut self, instruction: &mut Instruction) {
        match instruction {
            Instruction::Read(instruction) => self.visit_read(instruction),
            Instruction::Write(instruction) => self.visit_write(instruction),
            Instruction::While(instruction) => self.visit_while(instruction),
            Instruction::Assign(instruction) => self.visit_assign(instruction),
            Instruction::Eval(instruction) => self.visit_eval(instruction),
            Instruction::Return(instruction) => self.visit_return(instruction),
        }
    }

    fn visit_read(&mut self, instruction: &mut ReadInstruction) {
        self.walk(instruction);
    }

    fn visit_write(&mut self, instruction: &mut WriteInstruction) {
        self.walk(instruction);
    }

    fn visit_while(&mut self, instruction: &mut WhileInstruction) {
        self.walk(instruction);
    }

    fn visit_assign(&mut self, instruction: &mut AssignInstruction) {
        self.walk(instruction);
    }

    fn visit_eval(&mut self, instruction: &mut EvalInstruction) {
        self.walk(instruction);
    }

    fn visit_return(&mut self, instruction: &mut ReturnInstruction) {
        self.walk(instruction);
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        self.walk_expression(expression);
    }

    fn walk_expression(&mut self, expression: &mut Expression) {
        use ExpressionImpl::*;
        match **expression {
            Address(ref mut expression) => self.visit_address_expr(expression),
            ArrayFromCopy(ref mut expression) => self.visit_array_from_copy_expr(expression),
            ArrayFromElements(ref mut expression) => {
                self.visit_array_from_elements_expr(expression)
            }
            Block(ref mut expression) => self.visit_block_expr(expression),
            BooleanOp(ref mut expression) => self.visit_boolean_op_expr(expression),
            Call(ref mut expression) => self.visit_call_expr(expression),
            Deref(ref mut expression) => self.visit_deref_expr(expression),
            FieldAccess(ref mut expression) => self.visit_field_access_expr(expression),
            If(ref mut expression) => self.visit_if_expr(expression),
            Is(ref mut expression) => self.visit_is_expr(expression),
            Literal(ref mut expression) => self.visit_literal_expr(expression),
            New(ref mut expression) => self.visit_new_expr(expression),
            Null(ref mut expression) => self.visit_null_expr(expression),
            Variable(ref mut expression) => self.visit_variable_expr(expression),
            Empty(_) => (),
            Err(_) => (),
        };
        expression.recalculate(self.types())
    }

    fn visit_address_expr(&mut self, expression: &mut AddressExpr) {
        self.walk(expression);
    }

    fn visit_array_from_copy_expr(&mut self, expression: &mut ArrayFromCopyExpr) {
        self.walk(expression);
    }

    fn visit_array_from_elements_expr(&mut self, expression: &mut ArrayFromElementsExpr) {
        self.walk(expression);
    }

    fn visit_block_expr(&mut self, block: &mut BlockExpr) {
        self.walk(block);
    }

    fn visit_boolean_op_expr(&mut self, expression: &mut BooleanOpExpr) {
        self.walk(expression);
    }

    fn visit_call_expr(&mut self, expression: &mut CallExpr) {
        self.walk(expression);
    }

    fn visit_deref_expr(&mut self, expression: &mut DerefExpr) {
        self.walk(expression);
    }

    fn visit_field_access_expr(&mut self, expression: &mut FieldAccessExpr) {
        self.walk(expression);
    }

    fn visit_if_expr(&mut self, expression: &mut IfExpr) {
        self.walk(expression);
    }

    fn visit_literal_expr(&mut self, expression: &mut LiteralExpr) {
        self.walk(expression);
    }

    fn visit_is_expr(&mut self, expression: &mut IsExpr) {
        self.walk(expression);
    }

    fn visit_new_expr(&mut self, expression: &mut NewExpr) {
        self.walk(expression);
    }

    fn visit_null_expr(&mut self, expression: &mut NullExpr) {
        self.walk(expression);
    }

    fn visit_variable_expr(&mut self, expression: &mut VariableExpr) {
        self.walk(expression);
    }

    fn walk<T>(&mut self, node: &mut T)
    where
        for<'a> T: LocalCodeNode<'a>,
    {
        for instruction in node.child_instructions() {
            self.visit_instruction(instruction);
        }

        for expression in node.child_expressions() {
            self.visit_expression(expression);
        }
    }
}
