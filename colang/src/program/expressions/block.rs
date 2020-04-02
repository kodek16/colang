use crate::ast::InputSpan;
use crate::program::{
    AllocInstruction, DeallocInstruction, Expression, ExpressionKind, Instruction,
    ReturnInstruction, Type, TypeRegistry, ValueCategory,
};

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct BlockExpr {
    instructions: Vec<Instruction>,
}

impl BlockExpr {
    fn new(
        instructions: Vec<Instruction>,
        value_type: Rc<RefCell<Type>>,
        span: InputSpan,
    ) -> Expression {
        Expression {
            kind: ExpressionKind::Block(BlockExpr { instructions }),
            type_: value_type,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub fn instructions(&self) -> impl Iterator<Item = &Instruction> {
        self.instructions.iter()
    }
}

/// Incremental interface for building block expressions.
pub struct BlockBuilder {
    instructions: Vec<Instruction>,
}

impl BlockBuilder {
    pub fn new() -> BlockBuilder {
        BlockBuilder {
            instructions: vec![],
        }
    }

    pub fn append_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction)
    }

    pub fn into_expr(
        self,
        final_expr: Option<Expression>,
        types: &TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let mut instructions = self.instructions;

        let type_ = if let Some(final_expr) = final_expr {
            let type_ = Rc::clone(&final_expr.type_);
            instructions.push(ReturnInstruction::new(final_expr));
            type_
        } else {
            Rc::clone(types.void())
        };

        // We need to emit a `Dealloc` for every `Alloc` in this block, in reverse order.
        let mut deallocations: Vec<Instruction> = instructions
            .iter()
            .flat_map(|statement| match statement {
                Instruction::Alloc(AllocInstruction { variable, .. }) => {
                    Some(DeallocInstruction::new(Rc::clone(variable)))
                }
                _ => None,
            })
            .collect();
        deallocations.reverse();
        instructions.append(&mut deallocations);

        BlockExpr::new(instructions, type_, span)
    }
}
