use crate::ast::InputSpan;
use crate::program::{
    Expression, ExpressionKind, Instruction, Type, TypeRegistry, ValueCategory, Variable,
};

use crate::program::expressions::ExpressionKindImpl;
use std::cell::RefCell;
use std::rc::Rc;

pub struct BlockExpr {
    pub local_variables: Vec<Rc<RefCell<Variable>>>,
    pub instructions: Vec<Instruction>,
    pub value: Box<Expression>,
}

impl ExpressionKindImpl for BlockExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.value.type_())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }
}

/// Incremental interface for building block expressions.
pub struct BlockBuilder {
    local_variables: Vec<Rc<RefCell<Variable>>>,
    instructions: Vec<Instruction>,
}

impl BlockBuilder {
    pub fn new() -> BlockBuilder {
        BlockBuilder {
            local_variables: vec![],
            instructions: vec![],
        }
    }

    pub fn add_local_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        self.local_variables.push(variable);
    }

    pub fn append_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    pub fn into_expr(
        self,
        final_expr: Option<Expression>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let value = Box::new(final_expr.unwrap_or_else(|| Expression::empty(types)));

        let kind = ExpressionKind::Block(BlockExpr {
            local_variables: self.local_variables,
            instructions: self.instructions,
            value,
        });
        Expression::new(kind, Some(span), types)
    }
}
