//! Last-resort validity checking is handled by this module. Erroneous programs should
//! be not produced by the analyzer in the first place, so this module is not concerned with
//! error recovery (which is the responsibility of the analyzer), but with simply checking
//! if all assumptions hold.

use crate::program::visitors::visitor::LocalVisitor;
use crate::program::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct ValidityChecker<'a> {
    program: &'a mut Program,
    errors: Vec<String>,
}

impl<'a> ValidityChecker<'a> {
    pub fn new(program: &mut Program) -> ValidityChecker {
        ValidityChecker {
            program,
            errors: vec![],
        }
    }

    pub fn check(mut self) -> Vec<String> {
        let all_functions: Vec<Rc<RefCell<Function>>> =
            self.program.all_user_functions().map(Rc::clone).collect();

        for function in all_functions {
            let function = function.borrow();
            let body = function.body();
            let mut body = body.borrow_mut();
            self.visit_expression(&mut body);
        }
        self.errors
    }
}

impl<'a> LocalVisitor for ValidityChecker<'a> {
    fn types(&mut self) -> &mut TypeRegistry {
        self.program.types_mut()
    }

    fn visit_write(&mut self, instruction: &mut WriteInstruction) {
        self.walk(instruction);
        if !instruction.expression.type_().borrow().is_string() {
            self.errors.push(format!(
                "`write` instruction expression has type `{}`",
                instruction.expression.type_().borrow().name
            ))
        }
    }

    fn visit_while(&mut self, instruction: &mut WhileInstruction) {
        self.walk(instruction);
        if !instruction.cond.type_().borrow().is_bool() {
            self.errors.push(format!(
                "`while` condition has type `{}`",
                instruction.cond.type_().borrow().name
            ))
        }
    }

    fn visit_assign(&mut self, instruction: &mut AssignInstruction) {
        self.walk(instruction);
        if instruction.target.type_() != instruction.value.type_() {
            self.errors.push(format!(
                "`assign` instruction target has type `{}`, value has different type `{}`",
                instruction.target.type_().borrow().name,
                instruction.value.type_().borrow().name
            ))
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        self.walk_expression(expression);

        let expression_type = expression.type_().borrow();
        if expression_type.depends_on_type_parameter_placeholders() {
            self.errors.push(format!(
                "expression has type `{}` which depends on unfilled type parameter placeholders",
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
        if expression.target_type.borrow().is_void() {
            self.errors
                .push(format!("`new` expression for type `void`"));
        }
    }

    // TODO also check VariableExpr, so that it only accesses local variables that are in scope.
    // TODO also check ReturnInstruction, so that expression type is the same as function type.
    // TODO also check type and function references so that only registered entities are referenced.
}
