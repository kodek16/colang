use super::compile_expression;
use crate::analyzer::bodies::check_condition_is_bool;
use crate::analyzer::bodies::dual::DualNode;
use crate::analyzer::bodies::expressions::compile_expression_or_statement;
use crate::context::CompilerContext;
use crate::errors;
use crate::program::Type;
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_if(
    if_: ast::IfExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> DualNode {
    let span = if_.span;
    let cond = compile_expression(
        *if_.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );

    let then = compile_expression_or_statement(*if_.then, type_hint, context);
    let type_hint = if let DualNode::Expression(ref then) = then {
        Some(Rc::clone(then.type_()))
    } else {
        None
    };
    let else_ = if_
        .else_
        .map(|else_| compile_expression_or_statement(*else_, type_hint, context));

    check_condition_is_bool(&cond, context);

    match (then, else_) {
        (DualNode::Expression(then), Some(DualNode::Expression(else_))) => {
            if then.type_() == else_.type_() {
                DualNode::Expression(program::Expression::new(
                    program::IfExpr {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        else_: Box::new(else_),
                        location: SourceOrigin::Plain(span),
                    },
                    context.program.types_mut(),
                ))
            } else {
                let error = errors::if_expression_branch_type_mismatch(
                    &then,
                    &else_,
                    SourceOrigin::Plain(span),
                );
                context.errors.push(error);
                DualNode::Expression(program::Expression::error(span))
            }
        }
        (then, else_) => {
            let then = then.into_statement();
            let else_ = else_.map(|else_| Box::new(else_.into_statement()));
            DualNode::Statement(program::Statement::If(program::IfStmt {
                cond: Box::new(cond),
                then: Box::new(then),
                else_,
                location: SourceOrigin::Plain(span),
            }))
        }
    }
}
