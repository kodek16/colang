//! Last-resort validity checking.

use crate::program::visitors::visitor::LocalVisitor;
use crate::program::*;
use std::cell::RefCell;
use std::rc::Rc;

/// Program processor that handles last-resort validity checking.
///
/// Erroneous programs should not be produced by the analyzer in the first place, but this processor
/// exists as an additional level of protection that discovers potential errors in the compiler
/// before the code reaches the backend.
///
/// It does not perform error recovery (which is the responsibility of the analyzer),
/// instead it simply checks if all assumptions hold and reports an error otherwise.
pub struct ValidityChecker<'a> {
    program: &'a mut Program,
    errors: Vec<String>,
}

impl<'a> ValidityChecker<'a> {
    pub fn new(program: &mut Program) -> ValidityChecker {
        ValidityChecker {
            program,
            errors: Vec::new(),
        }
    }

    pub fn check(mut self) -> Vec<String> {
        let all_functions: Vec<Rc<RefCell<Function>>> =
            self.program.all_user_functions().map(Rc::clone).collect();

        for function in all_functions {
            let function = function.borrow();
            let body = function.body();
            let mut body = body.borrow_mut();
            self.visit_statement(&mut body);
        }
        self.errors
    }
}

impl<'a> LocalVisitor for ValidityChecker<'a> {
    fn types(&mut self) -> &mut TypeRegistry {
        self.program.types_mut()
    }

    fn visit_write(&mut self, statement: &mut WriteStmt) {
        self.walk(statement);
        if !statement.expression.type_().borrow().is_string() {
            self.errors.push(format!(
                "`write` statement expression has type `{}`",
                statement.expression.type_().borrow().name
            ))
        }
    }

    fn visit_while(&mut self, statement: &mut WhileStmt) {
        self.walk(statement);
        if !statement.cond.type_().borrow().is_bool() {
            self.errors.push(format!(
                "`while` condition has type `{}`",
                statement.cond.type_().borrow().name
            ))
        }
    }

    fn visit_assign(&mut self, statement: &mut AssignStmt) {
        self.walk(statement);
        if statement.target.type_() != statement.value.type_() {
            self.errors.push(format!(
                "`assign` statement target has type `{}`, value has different type `{}`",
                statement.target.type_().borrow().name,
                statement.value.type_().borrow().name
            ))
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        self.walk_expression(expression);
        let expression_type = expression.type_().borrow();
        if expression_type.is_technical() {
            self.errors.push(format!(
                "expression has type `{}` which has no values",
                expression_type.name
            ))
        }
    }

    fn visit_address_expr(&mut self, expression: &mut AddressExpr) {
        self.walk(expression);
        if expression.target.value_category() != ValueCategory::Lvalue {
            self.errors
                .push("attempt to take address of rvalue".to_string());
        }
    }

    fn visit_array_from_copy_expr(&mut self, expression: &mut ArrayFromCopyExpr) {
        self.walk(expression);
        if !expression.size.type_().borrow().is_int() {
            self.errors.push(format!(
                "array-from-copy expr has size of type `{}`",
                expression.size.type_().borrow().name
            ))
        }
    }

    fn visit_array_from_elements_expr(&mut self, expression: &mut ArrayFromElementsExpr) {
        self.walk(expression);
        let element_type = expression.element_type.borrow();
        for element in expression.elements.iter() {
            if *element.type_().borrow() != *element_type {
                self.errors.push(format!(
                    "one of array elements has type `{}`, not expected `{}`",
                    element.type_().borrow().name,
                    element_type.name
                ))
            }
        }
    }

    fn visit_call_expr(&mut self, expression: &mut CallExpr) {
        self.walk(expression);
        let parameters = &expression.function.borrow().parameters;
        if parameters.len() != expression.arguments.len() {
            self.errors.push(format!(
                "call expression expected {} parameters, got {}",
                parameters.len(),
                expression.arguments.len()
            ))
        }
    }

    fn visit_deref_expr(&mut self, expression: &mut DerefExpr) {
        self.walk(expression);
        if !expression.pointer.type_().borrow().is_pointer() {
            self.errors.push(format!(
                "deref expr for target of non-pointer type `{}`",
                expression.pointer.type_().borrow().name
            ))
        }
    }

    fn visit_field_access_expr(&mut self, expression: &mut FieldAccessExpr) {
        self.walk(expression);
        let receiver_type = expression.receiver.type_().borrow();
        if !receiver_type.fields.iter().any(|f| *f == expression.field) {
            self.errors.push(format!(
                "field access expr for type `{}` and wrong field `{}`",
                receiver_type.name,
                expression.field.borrow().name
            ))
        }
    }

    fn visit_if_expr(&mut self, expression: &mut IfExpr) {
        self.walk(expression);
        if !expression.cond.type_().borrow().is_bool() {
            self.errors.push(format!(
                "`if` condition has type `{}`",
                expression.cond.type_().borrow().name
            ))
        }

        if expression.then.type_() != expression.else_.type_() {
            self.errors.push(format!(
                "`if` branches have differing types `{}` and `{}`",
                expression.then.type_().borrow().name,
                expression.else_.type_().borrow().name
            ))
        }
    }

    fn visit_new_expr(&mut self, expression: &mut NewExpr) {
        self.walk(expression);
        if expression.target_type.borrow().is_void() {
            self.errors
                .push(format!("`new` expression for type `void`"));
        }
    }

    // TODO(#9) also check VariableExpr, so that it only accesses local variables that are in scope.
    // TODO(#9) also check ReturnStmt, so that expression type is the same as function type.
    // TODO(#9) also check type & function references so only registered entities are referenced.
}
