use crate::program::expressions::empty::EmptyExpr;
use crate::program::expressions::{Expression, ExpressionKind};
use crate::program::instructions::Instruction;
use crate::program::{Type, TypeRegistry, ValueCategory, Variable};
use crate::source::{InputSpan, SourceOrigin};
use std::cell::RefCell;
use std::rc::Rc;

pub struct BlockExpr {
    pub local_variables: Vec<Rc<RefCell<Variable>>>,
    pub instructions: Vec<Instruction>,
    pub value: Box<Expression>,
    pub location: SourceOrigin,
}

impl ExpressionKind for BlockExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.value.type_())
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
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
        let value = Box::new(final_expr.unwrap_or_else(|| {
            Expression::new(
                EmptyExpr {
                    location: SourceOrigin::MissingBlockValue(span),
                },
                types,
            )
        }));

        Expression::new(
            BlockExpr {
                local_variables: self.local_variables,
                instructions: self.instructions,
                value,
                location: SourceOrigin::Plain(span),
            },
            types,
        )
    }
}
