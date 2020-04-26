use crate::program::*;

use std::fmt;
use std::fmt::{Display, Formatter};

use sexp::{sexp_int, sexp_list, sexp_str, Atom, Sexp, ToSexp};

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
                sexp_str!("types"),
                Sexp::List(
                    self.types
                        .all_user_defined_types()
                        .map(|type_| type_.borrow().to_full_sexp())
                        .collect()
                ),
            ),
            sexp_list!(
                sexp_str!("functions"),
                Sexp::List(
                    self.user_functions
                        .iter()
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
            sexp_str!("type"),
            sexp_list!(sexp_str!("name"), sexp_str!(&self.name)),
            sexp_list!(
                sexp_str!("fields"),
                Sexp::List(
                    self.fields()
                        .map(|field| field.borrow().to_full_sexp())
                        .collect()
                ),
            ),
            sexp_list!(
                sexp_str!("methods"),
                Sexp::List(
                    self.methods()
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
            sexp_str!("function"),
            sexp_list!(sexp_str!("name"), sexp_str!(&self.name)),
            sexp_list!(sexp_str!("signature"), self.signature_sexp()),
            sexp_list!(sexp_str!("body"), self.body().borrow().to_sexp())
        )
    }

    fn signature_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_list!(
                sexp_str!("params"),
                Sexp::List(
                    self.parameters
                        .iter()
                        .map(|param| param.borrow().to_full_sexp())
                        .collect()
                ),
            ),
            sexp_list!(sexp_str!("returns"), self.return_type.borrow().to_sexp(),)
        )
    }
}

impl Variable {
    fn to_full_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("variable"),
            sexp_list!(sexp_str!("name"), sexp_str!(&self.name)),
            sexp_list!(sexp_str!("type"), self.type_.borrow().to_sexp()),
        )
    }
}

impl ToSexp for Variable {
    fn to_sexp(&self) -> Sexp {
        sexp_str!(&self.name)
    }
}

impl ToSexp for Function {
    fn to_sexp(&self) -> Sexp {
        sexp_str!(&self.name)
    }
}

impl ToSexp for Type {
    fn to_sexp(&self) -> Sexp {
        sexp_str!(&self.name)
    }
}

impl ToSexp for Instruction {
    fn to_sexp(&self) -> Sexp {
        match self {
            Instruction::Read(instruction) => instruction.to_sexp(),
            Instruction::Write(instruction) => instruction.to_sexp(),
            Instruction::While(instruction) => instruction.to_sexp(),
            Instruction::Assign(instruction) => instruction.to_sexp(),
            Instruction::Eval(instruction) => instruction.to_sexp(),
            Instruction::Return(instruction) => instruction.to_sexp(),
        }
    }
}

impl ToSexp for ReadInstruction {
    fn to_sexp(&self) -> Sexp {
        let mut list = Vec::new();
        list.push(sexp_str!("read"));
        if self.whole_line {
            list.push(sexp_str!("whole-line"));
        }
        list.push(self.target.to_sexp());
        Sexp::List(list)
    }
}

impl ToSexp for WriteInstruction {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("write"), self.expression.to_sexp())
    }
}

impl ToSexp for WhileInstruction {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("while"), self.cond.to_sexp(), self.body.to_sexp())
    }
}

impl ToSexp for AssignInstruction {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("assign"),
            self.target.to_sexp(),
            self.value.to_sexp(),
        )
    }
}

impl ToSexp for EvalInstruction {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("eval"), self.expression.to_sexp())
    }
}

impl ToSexp for ReturnInstruction {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("return"), self.expression.to_sexp())
    }
}

impl ToSexp for Expression {
    fn to_sexp(&self) -> Sexp {
        use ExpressionKind::*;
        match self.kind() {
            Address(expr) => expr.to_sexp(),
            ArrayFromCopy(expr) => expr.to_sexp(),
            ArrayFromElements(expr) => expr.to_sexp(),
            Block(expr) => expr.to_sexp(),
            BooleanOp(expr) => expr.to_sexp(),
            Call(expr) => expr.to_sexp(),
            Deref(expr) => expr.to_sexp(),
            FieldAccess(expr) => expr.to_sexp(),
            If(expr) => expr.to_sexp(),
            Is(expr) => expr.to_sexp(),
            Literal(expr) => expr.to_sexp(),
            New(expr) => expr.to_sexp(),
            Null(expr) => expr.to_sexp(),
            Variable(expr) => expr.to_sexp(),
            Empty(_) => sexp_str!("empty"),
            Error(_) => sexp_str!("error"),
        }
    }
}

impl ToSexp for AddressExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("address"), self.target.to_sexp())
    }
}

impl ToSexp for ArrayFromCopyExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("array-from-copy"),
            self.element.to_sexp(),
            self.size.to_sexp()
        )
    }
}

impl ToSexp for ArrayFromElementsExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("array-from-elems"),
            Sexp::List(self.elements.iter().map(Expression::to_sexp).collect())
        )
    }
}

impl ToSexp for BlockExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("block"),
            sexp_list!(
                sexp_str!("locals"),
                Sexp::List(
                    self.local_variables
                        .iter()
                        .map(|variable| variable.borrow().to_full_sexp())
                        .collect()
                )
            ),
            sexp_list!(
                sexp_str!("instructions"),
                Sexp::List(self.instructions.iter().map(Instruction::to_sexp).collect())
            ),
            sexp_list!(sexp_str!("value"), self.value.to_sexp())
        )
    }
}

impl ToSexp for BooleanOpExpr {
    fn to_sexp(&self) -> Sexp {
        match self.op {
            BooleanOp::And(ref lhs, ref rhs) => {
                sexp_list!(sexp_str!("and"), lhs.to_sexp(), rhs.to_sexp())
            }
            BooleanOp::Or(ref lhs, ref rhs) => {
                sexp_list!(sexp_str!("or"), lhs.to_sexp(), rhs.to_sexp())
            }
            BooleanOp::Not(ref operand) => sexp_list!(sexp_str!("not"), operand.to_sexp()),
        }
    }
}

impl ToSexp for CallExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("call"),
            self.function.borrow().to_sexp(),
            Sexp::List(self.arguments.iter().map(Expression::to_sexp).collect()),
        )
    }
}

impl ToSexp for DerefExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("deref"), self.pointer.to_sexp())
    }
}

impl ToSexp for FieldAccessExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("field-access"),
            self.receiver.to_sexp(),
            self.field.borrow().to_sexp(),
        )
    }
}

impl ToSexp for IfExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("if"),
            self.cond.to_sexp(),
            sexp_list!(sexp_str!("then"), self.then.to_sexp()),
            sexp_list!(sexp_str!("else"), self.else_.to_sexp()),
        )
    }
}

impl ToSexp for IsExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("is"), self.lhs.to_sexp(), self.rhs.to_sexp(),)
    }
}

impl ToSexp for LiteralExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(
            sexp_str!("literal"),
            match self {
                LiteralExpr::Int(value, _) => sexp_int!(*value),
                LiteralExpr::Bool(value, _) => sexp_list!(
                    sexp_str!("bool"),
                    sexp_str!(if *value { "true" } else { "false " })
                ),
                LiteralExpr::Char(value, _) =>
                    sexp_list!(sexp_str!("char"), sexp_int!(*value as i32)),
                LiteralExpr::String(value, _) => sexp_list!(sexp_str!("string"), sexp_str!(&value)),
            }
        )
    }
}

impl ToSexp for NewExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("new"), self.target_type.borrow().to_sexp())
    }
}

impl ToSexp for NullExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_str!("null")
    }
}

impl ToSexp for VariableExpr {
    fn to_sexp(&self) -> Sexp {
        sexp_list!(sexp_str!("variable"), self.variable.borrow().to_sexp())
    }
}
