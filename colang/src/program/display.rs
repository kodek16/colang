use crate::program::*;

use std::fmt;
use std::fmt::{Display, Formatter};

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for function in self.user_functions.iter() {
            let function = function.borrow();
            if let Function::UserDefined(ref function) = *function {
                let param_types: Vec<_> = function
                    .parameters
                    .iter()
                    .map(|param| param.borrow().type_.borrow().name().to_string())
                    .collect();
                let param_types = param_types.join(", ");
                let return_type = function.return_type.borrow().name().to_string();
                write!(f, "{}: {} -> {}:", function.name, param_types, return_type)?;
                write!(
                    f,
                    "\n{}",
                    indent(&function.body.as_ref().unwrap().to_string())
                )?;
            }
        }

        Ok(())
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<{}:{}>", self.name, self.id.unwrap(),)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Function::UserDefined(function) => {
                write!(f, "<{}:{}>", function.name, function.id.unwrap())
            }
            Function::Internal(function) => write!(f, "<{:?}:internal>", function.tag),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Alloc(statement) => statement.fmt(f),
            Statement::Dealloc(statement) => statement.fmt(f),
            Statement::Read(statement) => statement.fmt(f),
            Statement::Write(statement) => statement.fmt(f),
            Statement::While(statement) => statement.fmt(f),
            Statement::Assign(statement) => statement.fmt(f),
            Statement::Return(statement) => statement.fmt(f),
            Statement::Expr(statement) => statement.fmt(f),
        }
    }
}

impl Display for AllocStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "alloc {}", self.variable.borrow())?;
        if let Some(ref initializer) = self.initializer {
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

impl Display for DeallocStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "dealloc {}", self.variable.borrow())
    }
}

impl Display for ReadStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let target = self.target.to_string();
        if target.contains('\n') {
            write!(f, "read:\n{}", indent(&target))
        } else {
            write!(f, "read {}", target)
        }
    }
}

impl Display for WriteStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression.to_string();
        if expression.contains('\n') {
            write!(f, "write:\n{}", indent(&expression))
        } else {
            write!(f, "write {}", expression)
        }
    }
}

impl Display for WhileStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "while {}:\n{}",
            self.cond,
            indent(&self.body.to_string())
        )
    }
}

impl Display for AssignStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let target = self.target.to_string();
        let value = self.value.to_string();
        if target.contains('\n') || value.contains('\n') {
            write!(f, "assign:\n{}\n{}", indent(&target), indent(&value))
        } else {
            write!(f, "assign {}, {}", target, value)
        }
    }
}

impl Display for ReturnStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression.to_string();
        if expression.contains('\n') {
            write!(f, "return:\n{}", indent(&expression))
        } else {
            write!(f, "return {}", expression)
        }
    }
}

impl Display for ExprStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression.to_string();
        if expression.contains('\n') {
            write!(f, "of_expr:\n{}", indent(&expression))
        } else {
            write!(f, "of_expr {}", expression)
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ExpressionKind::*;
        match &self.kind {
            ArrayFromCopy(expr) => expr.fmt(f),
            ArrayFromElements(expr) => expr.fmt(f),
            Block(expr) => expr.fmt(f),
            Call(expr) => expr.fmt(f),
            If(expr) => expr.fmt(f),
            Index(expr) => expr.fmt(f),
            Literal(expr) => expr.fmt(f),
            Variable(expr) => expr.fmt(f),
            Empty => write!(f, "()"),
            Error => write!(f, "<error>"),
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
        for statement in self.statements() {
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

impl Display for IfExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let cond = self.cond().to_string();
        let then = self.then().to_string();
        let else_ = self.else_().to_string();

        write!(f, "if {}:\n{}\n{}", cond, indent(&then), indent(&else_))
    }
}

impl Display for IndexExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let collection = self.collection().to_string();
        let index = self.index().to_string();

        if collection.contains('\n') || index.contains('\n') {
            write!(f, "index:\n{}\n{}", indent(&collection), indent(&index))
        } else {
            write!(f, "index {}, {}", collection, index)
        }
    }
}

impl Display for LiteralExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralExpr::Int(value) => write!(f, "{}", value),
            LiteralExpr::Bool(value) => write!(f, "{}", value),
        }
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
