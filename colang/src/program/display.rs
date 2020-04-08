use crate::program::*;

use std::fmt;
use std::fmt::{Display, Formatter};

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "types:\n")?;
        for type_ in self.types.all_types() {
            let type_ = type_.borrow();
            if type_.is_user_defined() {
                write!(f, "{}", indent(&type_.to_string()))?;
                for field in type_.fields() {
                    let field = field.borrow();
                    write!(
                        f,
                        "\n{}: {}",
                        indent(&indent(&field.to_string())),
                        field.type_.borrow().to_string()
                    )?;
                }
                write!(f, "\n")?;
            }
        }
        write!(f, "\n")?;

        for function in self.user_functions.iter() {
            let function = function.borrow();
            if let Function::UserDefined(ref function) = *function {
                let param_types: Vec<_> = function
                    .parameters()
                    .map(|param| param.type_().name().to_string())
                    .collect();
                let param_types = param_types.join(", ");
                let return_type = function.return_type().name().to_string();
                write!(f, "{}: {} -> {}:", function.name, param_types, return_type)?;
                write!(f, "\n{}", indent(&function.body().to_string()))?;
                write!(f, "\n\n")?;
            }
        }

        Ok(())
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<{}:{}>", self.name, self.id)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Function::UserDefined(function) => write!(f, "<{}:{}>", function.name, function.id),
            Function::Internal(function) => write!(f, "<{:?}:internal>", function.tag),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Alloc(statement) => statement.fmt(f),
            Instruction::Dealloc(statement) => statement.fmt(f),
            Instruction::Write(statement) => statement.fmt(f),
            Instruction::While(statement) => statement.fmt(f),
            Instruction::Assign(statement) => statement.fmt(f),
            Instruction::Return(statement) => statement.fmt(f),
            Instruction::Eval(statement) => statement.fmt(f),
        }
    }
}

impl Display for AllocInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "alloc {}", *self.variable())?;
        if let Some(ref initializer) = self.initializer() {
            let initializer = initializer.to_string();
            if initializer.contains('\n') {
                write!(f, ":\n{}", indent(&initializer))?;
            } else {
                write!(f, ", {}", initializer)?;
            }
        }
        Ok(())
    }
}

impl Display for DeallocInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "dealloc {}", *self.variable())
    }
}

impl Display for WriteInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression().to_string();
        if expression.contains('\n') {
            write!(f, "write:\n{}", indent(&expression))
        } else {
            write!(f, "write {}", expression)
        }
    }
}

impl Display for WhileInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "while {}:\n{}",
            self.cond(),
            indent(&self.body().to_string())
        )
    }
}

impl Display for AssignInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let target = self.target().to_string();
        let value = self.value().to_string();
        if target.contains('\n') || value.contains('\n') {
            write!(f, "assign:\n{}\n{}", indent(&target), indent(&value))
        } else {
            write!(f, "assign {}, {}", target, value)
        }
    }
}

impl Display for ReturnInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression().to_string();
        if expression.contains('\n') {
            write!(f, "return:\n{}", indent(&expression))
        } else {
            write!(f, "return {}", expression)
        }
    }
}

impl Display for EvalInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression().to_string();
        if expression.contains('\n') {
            write!(f, "eval:\n{}", indent(&expression))
        } else {
            write!(f, "eval {}", expression)
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ExpressionKind::*;
        match &self.kind {
            Address(expr) => expr.fmt(f),
            ArrayFromCopy(expr) => expr.fmt(f),
            ArrayFromElements(expr) => expr.fmt(f),
            Block(expr) => expr.fmt(f),
            Call(expr) => expr.fmt(f),
            Deref(expr) => expr.fmt(f),
            FieldAccess(expr) => expr.fmt(f),
            If(expr) => expr.fmt(f),
            Literal(expr) => expr.fmt(f),
            New(expr) => expr.fmt(f),
            Variable(expr) => expr.fmt(f),
            Empty => write!(f, "()"),
            Error => write!(f, "<error>"),
        }
    }
}

impl Display for AddressExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let target = self.target().to_string();

        if target.contains('\n') {
            write!(f, "address_of:\n{}", indent(&target))
        } else {
            write!(f, "address_of({})", target)
        }
    }
}

impl Display for ArrayFromCopyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let element = self.element().to_string();
        let size = self.size().to_string();

        if element.contains('\n') || size.contains('\n') {
            write!(f, "arr_from_copy:\n{}\n{}", indent(&element), indent(&size))
        } else {
            write!(f, "arr_from_copy({}, {})", element, size)
        }
    }
}

impl Display for ArrayFromElementsExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "arr_from_elems:")?;
        for element in self.elements() {
            write!(f, "\n{}", indent(&element.to_string()))?;
        }
        Ok(())
    }
}

impl Display for BlockExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "block:")?;
        for statement in self.instructions() {
            write!(f, "\n{}", indent(&statement.to_string()))?;
        }
        Ok(())
    }
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let function = self.function().to_string();
        let args: Vec<_> = self.arguments().map(|arg| arg.to_string()).collect();

        if args.iter().any(|arg| arg.contains('\n')) {
            write!(f, "call:\n{}", indent(&function))?;
            for arg in args {
                write!(f, "\n{}", indent(&arg))?;
            }
        } else {
            let args = args.join(", ");
            write!(f, "call {}({})", function, args)?;
        }
        Ok(())
    }
}

impl Display for DerefExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let pointer = self.pointer().to_string();

        if pointer.contains('\n') {
            write!(f, "deref:\n{}", indent(&pointer))
        } else {
            write!(f, "deref({})", pointer)
        }
    }
}

impl Display for FieldAccessExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let receiver = self.receiver().to_string();
        let field = self.field().to_string();

        if receiver.contains('\n') {
            write!(f, "field {}:\n{}", field, indent(&receiver))
        } else {
            write!(f, "field({}, {})", field, receiver)
        }
    }
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let cond = self.cond().to_string();
        let then = self.then().to_string();
        let else_ = self.else_().to_string();

        write!(f, "if {}:\n{}\n{}", cond, indent(&then), indent(&else_))
    }
}

impl Display for LiteralExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralExpr::Int(value) => write!(f, "{}", value),
            LiteralExpr::Bool(value) => write!(f, "{}", value),
            LiteralExpr::Char(value) => write!(f, "'{}'", *value as char),
            LiteralExpr::String(value) => write!(f, "\"{}\"", value),
        }
    }
}

impl Display for NewExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "new({})", self.target_type().borrow().to_string())
    }
}

impl Display for VariableExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let variable = self.variable().to_string();
        write!(f, "{}", variable)
    }
}

fn indent(code: &str) -> String {
    let lines: Vec<_> = code
        .lines()
        .map(|line| {
            if line.is_empty() {
                line.to_string()
            } else {
                format!("  {}", line)
            }
        })
        .collect();

    lines.join("\n")
}
