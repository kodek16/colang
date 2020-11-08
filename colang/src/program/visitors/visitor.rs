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

    fn visit_statement(&mut self, statement: &mut Statement) {
        self.walk_statement(statement);
    }

    fn walk_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Assign(statement) => self.visit_assign(statement),
            Statement::Block(block) => self.visit_block(block),
            Statement::Call(call) => self.visit_call(call),
            Statement::Eval(statement) => self.visit_eval(statement),
            Statement::If(statement) => self.visit_if(statement),
            Statement::Read(statement) => self.visit_read(statement),
            Statement::Return(statement) => self.visit_return(statement),
            Statement::Semicolon(statement) => self.visit_semicolon(statement),
            Statement::While(statement) => self.visit_while(statement),
            Statement::Write(statement) => self.visit_write(statement),
        }
    }
    fn visit_assign(&mut self, statement: &mut AssignStmt) {
        self.walk(statement);
    }

    fn visit_eval(&mut self, statement: &mut EvalStmt) {
        self.walk(statement);
    }

    fn visit_if(&mut self, statement: &mut IfStmt) {
        self.walk(statement);
    }

    fn visit_read(&mut self, statement: &mut ReadStmt) {
        self.walk(statement);
    }

    fn visit_return(&mut self, statement: &mut ReturnStmt) {
        self.walk(statement);
    }

    fn visit_semicolon(&mut self, statement: &mut SemicolonStmt) {
        self.walk(statement);
    }

    fn visit_while(&mut self, statement: &mut WhileStmt) {
        self.walk(statement);
    }

    fn visit_write(&mut self, statement: &mut WriteStmt) {
        self.walk(statement);
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
            Block(ref mut expression) => self.visit_block(expression),
            BooleanOp(ref mut expression) => self.visit_boolean_op_expr(expression),
            Call(ref mut expression) => self.visit_call(expression),
            Deref(ref mut expression) => self.visit_deref_expr(expression),
            FieldAccess(ref mut expression) => self.visit_field_access_expr(expression),
            If(ref mut expression) => self.visit_if_expr(expression),
            Is(ref mut expression) => self.visit_is_expr(expression),
            Literal(ref mut expression) => self.visit_literal_expr(expression),
            New(ref mut expression) => self.visit_new_expr(expression),
            Null(ref mut expression) => self.visit_null_expr(expression),
            Variable(ref mut expression) => self.visit_variable_expr(expression),
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

    fn visit_boolean_op_expr(&mut self, expression: &mut BooleanOpExpr) {
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

    fn visit_block(&mut self, block: &mut Block) {
        self.walk(block);
    }

    fn visit_call(&mut self, call: &mut Call) {
        self.walk(call);
    }

    fn walk<T>(&mut self, node: &mut T)
    where
        for<'a> T: LocalCodeNode<'a>,
    {
        for statement in node.child_statements() {
            self.visit_statement(statement);
        }

        for expression in node.child_expressions() {
            self.visit_expression(expression);
        }
    }
}
