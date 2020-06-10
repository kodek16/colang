//! Debug printing for program IR through s-expressions.

use crate::program::*;
use crate::sexp_list;
use crate::utils::sexp::{Sexp, ToSexp};
use std::fmt;
use std::fmt::{Display, Formatter};

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let sexp = self.to_sexp();
        write!(f, "{}", sexp.pretty_print(80))
    }
}

impl ToSexp for Program {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_list!(
                Sexp::str("types"),
                Sexp::List(
                    self.types
                        .all_user_defined_types()
                        .map(|type_| type_.borrow().to_full_sexp())
                        .collect()
                ),
            ),
            sexp_list!(
                Sexp::str("functions"),
                Sexp::List(
                    self.all_user_functions()
                        .map(|function| function.borrow().to_full_sexp())
                        .collect()
                ),
            )
        )
    }
}

impl Type {
    fn to_full_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("type"),
            sexp_list!(Sexp::str("name"), Sexp::str(&self.name)),
            sexp_list!(
                Sexp::str("fields"),
                Sexp::List(
                    self.fields
                        .iter()
                        .map(|field| field.borrow().to_full_sexp())
                        .collect()
                ),
            ),
            sexp_list!(
                Sexp::str("methods"),
                Sexp::List(
                    self.methods
                        .iter()
                        .map(|method| method.borrow().to_full_sexp())
                        .collect()
                ),
            )
        )
    }
}

impl Function {
    fn to_full_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("function"),
            sexp_list!(Sexp::str("name"), Sexp::str(&self.name)),
            sexp_list!(Sexp::str("signature"), self.signature_sexp()),
            sexp_list!(Sexp::str("body"), self.body().borrow().to_sexp())
        )
    }

    fn signature_sexp(&self) -> Sexp {
        let mut list = vec![sexp_list!(
            Sexp::str("params"),
            Sexp::List(
                self.parameters
                    .iter()
                    .map(|param| param.borrow().to_full_sexp())
                    .collect()
            ),
        )];

        if let Some(ref return_type) = self.return_type {
            list.push(sexp_list!(
                Sexp::str("returns"),
                return_type.borrow().to_sexp()
            ));
        }

        Sexp::List(list)
    }
}

impl Variable {
    fn to_full_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("variable"),
            sexp_list!(Sexp::str("name"), Sexp::str(&self.name)),
            sexp_list!(Sexp::str("type"), self.type_.borrow().to_sexp()),
        )
    }
}

impl Field {
    fn to_full_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("field"),
            sexp_list!(Sexp::str("name"), Sexp::str(&self.name)),
            sexp_list!(Sexp::str("type"), self.type_.borrow().to_sexp()),
        )
    }
}

impl ToSexp for Variable {
    fn to_sexp(&self) -> Sexp {
        Sexp::str(&self.name)
    }
}

impl ToSexp for Function {
    fn to_sexp(&self) -> Sexp {
        Sexp::str(&self.name)
    }
}

impl ToSexp for Type {
    fn to_sexp(&self) -> Sexp {
        Sexp::str(&self.name)
    }
}

impl ToSexp for Field {
    fn to_sexp(&self) -> Sexp {
        Sexp::str(&self.name)
    }
}

impl ToSexp for Statement {
    fn to_sexp(&self) -> Sexp {
        match self {
            Statement::Assign(statement) => statement.to_sexp(),
            Statement::Block(block) => block.to_sexp(),
            Statement::Call(call) => call.to_sexp(),
            Statement::Eval(statement) => statement.to_sexp(),
            Statement::If(statement) => statement.to_sexp(),
            Statement::Read(statement) => statement.to_sexp(),
            Statement::Return(statement) => statement.to_sexp(),
            Statement::While(statement) => statement.to_sexp(),
            Statement::Write(statement) => statement.to_sexp(),
        }
    }
}

impl ToSexp for AssignStmt {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("assign"),
            self.target.to_sexp(),
            self.value.to_sexp(),
        )
    }
}

impl ToSexp for EvalStmt {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("eval"), self.expression.to_sexp())
    }
}

impl ToSexp for IfStmt {
    fn to_sexp(&self) -> Sexp {
        let mut list = vec![
            Sexp::str("if"),
            self.cond.to_sexp(),
            sexp_list!(Sexp::str("then"), self.then.to_sexp()),
        ];

        if let Some(ref else_) = self.else_ {
            list.push(sexp_list!(Sexp::str("else"), else_.to_sexp()))
        }

        Sexp::List(list)
    }
}

impl ToSexp for ReadStmt {
    fn to_sexp(&self) -> Sexp {
        let mut list = Vec::new();
        list.push(Sexp::str("read"));
        if self.whole_line {
            list.push(Sexp::str("whole-line"));
        }
        list.push(self.target.to_sexp());
        Sexp::List(list)
    }
}

impl ToSexp for ReturnStmt {
    fn to_sexp(&self) -> Sexp {
        let mut list = vec![Sexp::str("return")];
        if let Some(ref expression) = self.expression {
            list.push(expression.to_sexp());
        }

        Sexp::List(list)
    }
}

impl ToSexp for WhileStmt {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("while"), self.cond.to_sexp(), self.body.to_sexp())
    }
}

impl ToSexp for WriteStmt {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("write"), self.expression.to_sexp())
    }
}

impl ToSexp for Expression {
    fn to_sexp(&self) -> Sexp {
        use ExpressionImpl::*;
        match **self {
            Address(ref expr) => expr.to_sexp(),
            ArrayFromCopy(ref expr) => expr.to_sexp(),
            ArrayFromElements(ref expr) => expr.to_sexp(),
            Block(ref expr) => expr.to_sexp(),
            BooleanOp(ref expr) => expr.to_sexp(),
            Call(ref expr) => expr.to_sexp(),
            Deref(ref expr) => expr.to_sexp(),
            FieldAccess(ref expr) => expr.to_sexp(),
            If(ref expr) => expr.to_sexp(),
            Is(ref expr) => expr.to_sexp(),
            Literal(ref expr) => expr.to_sexp(),
            New(ref expr) => expr.to_sexp(),
            Null(ref expr) => expr.to_sexp(),
            Variable(ref expr) => expr.to_sexp(),
            Err(_) => Sexp::str("error"),
        }
    }
}

impl ToSexp for AddressExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("address"), self.target.to_sexp())
    }
}

impl ToSexp for ArrayFromCopyExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("array-from-copy"),
            self.element.to_sexp(),
            self.size.to_sexp()
        )
    }
}

impl ToSexp for ArrayFromElementsExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("array-from-elems"),
            Sexp::List(self.elements.iter().map(Expression::to_sexp).collect())
        )
    }
}

impl ToSexp for Block {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("block"),
            sexp_list!(
                Sexp::str("locals"),
                Sexp::List(
                    self.local_variables
                        .iter()
                        .map(|variable| variable.borrow().to_full_sexp())
                        .collect()
                )
            ),
            sexp_list!(
                Sexp::str("statements"),
                Sexp::List(self.statements.iter().map(Statement::to_sexp).collect())
            ),
            sexp_list!(
                Sexp::str("value"),
                Sexp::List(self.value.iter().map(|v| v.to_sexp()).collect()),
            )
        )
    }
}

impl ToSexp for BooleanOpExpr {
    fn to_sexp(&self) -> Sexp {
        match self.op {
            BooleanOp::And(ref lhs, ref rhs) => {
                sexp_list!(Sexp::str("and"), lhs.to_sexp(), rhs.to_sexp())
            }
            BooleanOp::Or(ref lhs, ref rhs) => {
                sexp_list!(Sexp::str("or"), lhs.to_sexp(), rhs.to_sexp())
            }
            BooleanOp::Not(ref operand) => sexp_list!(Sexp::str("not"), operand.to_sexp()),
        }
    }
}

impl ToSexp for Call {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("call"),
            self.function.borrow().to_sexp(),
            Sexp::List(self.arguments.iter().map(Expression::to_sexp).collect()),
        )
    }
}

impl ToSexp for DerefExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("deref"), self.pointer.to_sexp())
    }
}

impl ToSexp for FieldAccessExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("field-access"),
            self.receiver.to_sexp(),
            self.field.borrow().to_sexp(),
        )
    }
}

impl ToSexp for IfExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("if"),
            self.cond.to_sexp(),
            sexp_list!(Sexp::str("then"), self.then.to_sexp()),
            sexp_list!(Sexp::str("else"), self.else_.to_sexp()),
        )
    }
}

impl ToSexp for IsExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("is"), self.lhs.to_sexp(), self.rhs.to_sexp(),)
    }
}

impl ToSexp for LiteralExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            Sexp::str("literal"),
            match &self.value {
                LiteralValue::Int(value) => Sexp::int(*value),
                LiteralValue::Bool(value) => sexp_list!(
                    Sexp::str("bool"),
                    Sexp::str(if *value { "true" } else { "false" })
                ),
                LiteralValue::Char(value) =>
                    sexp_list!(Sexp::str("char"), Sexp::int(*value as i32)),
                LiteralValue::String(value) => sexp_list!(Sexp::str("string"), Sexp::str(value)),
            }
        )
    }
}

impl ToSexp for NewExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("new"), self.target_type.borrow().to_sexp())
    }
}

impl ToSexp for NullExpr {
    fn to_sexp(&self) -> Sexp {
        Sexp::str("null")
    }
}

impl ToSexp for VariableExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(Sexp::str("variable"), self.variable.borrow().to_sexp())
    }
}
