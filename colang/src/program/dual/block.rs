use crate::program::expressions::{Expression, ExpressionKind};
use crate::program::statements::Statement;
use crate::program::visitors::LocalCodeNode;
use crate::program::{StatementKind, Type, TypeRegistry, ValueCategory, Variable};
use crate::source::{InputSpan, SourceOrigin};
use std::cell::RefCell;
use std::rc::Rc;

/// A local code node that contains a sequence of statements potentially followed by an expression.
///
/// If the final expression in a block is present, the block itself is an expression, and its value
/// is taken from the value of the final expression. If it is absent, the block is a statement.
///
/// Blocks may have local variables defined in them that get created when the block execution
/// starts, and destroyed when it ends. Block local variables are still available throughout the
/// runtime of the final expression.
pub struct Block {
    /// Local variables defined in the block.
    pub local_variables: Vec<Rc<RefCell<Variable>>>,

    /// Statements that are executed when running the block.
    pub statements: Vec<Statement>,

    /// The final expression that produces the value for the whole block.
    pub value: Option<Box<Expression>>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for Block {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        match &self.value {
            Some(value) => Rc::clone(value.type_()),
            None => {
                panic!("Attempt to treat a block statement as an expression.");
            }
        }
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl StatementKind for Block {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

/// Incremental interface for building blocks.
pub struct BlockBuilder {
    local_variables: Vec<Rc<RefCell<Variable>>>,
    statements: Vec<Statement>,
}

impl BlockBuilder {
    pub fn new() -> BlockBuilder {
        BlockBuilder {
            local_variables: vec![],
            statements: vec![],
        }
    }

    pub fn add_local_variable(&mut self, variable: Rc<RefCell<Variable>>) {
        self.local_variables.push(variable);
    }

    pub fn append_statement(&mut self, statement: impl Into<Statement>) {
        self.statements.push(statement.into())
    }

    pub fn into_stmt(self, span: InputSpan) -> Statement {
        Statement::Block(Block {
            local_variables: self.local_variables,
            statements: self.statements,
            value: None,
            location: SourceOrigin::Plain(span),
        })
    }

    pub fn into_expr(
        self,
        final_expr: Expression,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        Expression::new(
            Block {
                local_variables: self.local_variables,
                statements: self.statements,
                value: Some(Box::new(final_expr)),
                location: SourceOrigin::Plain(span),
            },
            types,
        )
    }
}

impl<'a> LocalCodeNode<'a> for Block {
    type StmtIter = std::slice::IterMut<'a, Statement>;
    type ExprIter = std::vec::IntoIter<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        self.statements.iter_mut()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        match self.value {
            Some(ref mut value) => vec![&mut **value].into_iter(),
            None => vec![].into_iter(),
        }
    }
}
