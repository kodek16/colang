use crate::program::*;

use std::fmt;
use std::fmt::{Display, Formatter, Write};

const MAX_LINE_LENGTH: usize = 100;

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "types:\n")?;
        for type_ in self.types.all_types() {
            let type_ = type_.borrow();
            if type_.is_user_defined() {
                write!(f, "{} ({:?})", indent(&type_.to_string()), type_.type_id())?;
                for field in type_.fields() {
                    let field = field.borrow();
                    write!(
                        f,
                        "\n{}: {}",
                        indent(&indent(&field.to_string())),
                        field.type_.borrow().to_string()
                    )?;
                }

                for method in type_.methods() {
                    let method = write_function(&method.borrow())?;
                    write!(f, "\n{}", indent(&indent(&method)))?;
                }

                write!(f, "\n")?;
            }
        }
        write!(f, "\n")?;

        for function in self.user_functions.iter() {
            let function = write_function(&function.borrow())?;
            write!(f, "{}\n", function)?;
        }

        Ok(())
    }
}

fn write_function(function: &Function) -> Result<String, fmt::Error> {
    let mut result = String::new();

    let signature = write_function_signature(function);

    write!(
        &mut result,
        "{}:{:?}: {}:",
        function.name, function.id, signature
    )?;
    write!(&mut result, "\n{}", indent(&function.body().to_string()))?;
    write!(&mut result, "\n")?;
    Ok(result)
}

fn write_function_signature(function: &Function) -> String {
    let param_types: Vec<_> = function
        .parameters
        .iter()
        .map(|param| param.borrow().type_.borrow().name().to_string())
        .collect();
    let param_types = param_types.join(", ");
    let return_type = function.return_type.borrow().name().to_string();
    format!("({}) -> {}", param_types, return_type)
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.id {
            VariableId::LocalVariable(id) => write!(f, "<{}:{}>", self.name, id),
            VariableId::InstantiatedLocalVariable(id, _) => {
                write!(f, "<{}:{}:inst>", self.name, id)
            }
            VariableId::Field(id) => write!(f, "<{}:{}:field>", self.name, id),
            VariableId::InstantiatedField(id, _) => write!(f, "<{}:{}:field_inst>", self.name, id),
            VariableId::InternalParameter(_, _) => write!(f, "<{}:internal_param>", self.name),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.id {
            FunctionId::UserDefined(id) => write!(f, "<{}:{}>", self.name, id),
            FunctionId::Internal(_) => write!(
                f,
                "<{}:internal:{}>",
                self.name,
                write_function_signature(self)
            ),
            FunctionId::InstantiatedMethod(id, ref type_id) => {
                write!(f, "<{}:{}:inst:{:?}>", self.name, id, type_id.clone())
            }
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
            Instruction::Write(statement) => statement.fmt(f),
            Instruction::While(statement) => statement.fmt(f),
            Instruction::Assign(statement) => statement.fmt(f),
            Instruction::Eval(statement) => statement.fmt(f),
        }
    }
}

impl Display for WriteInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression.to_string();
        if should_wrap(&expression) {
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
            self.cond,
            indent(&self.body.to_string())
        )
    }
}

impl Display for AssignInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let target = self.target.to_string();
        let value = self.value.to_string();
        if should_wrap(&target) || should_wrap(&value) {
            write!(f, "assign:\n{}\n{}", indent(&target), indent(&value))
        } else {
            write!(f, "assign {}, {}", target, value)
        }
    }
}

impl Display for EvalInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expression = self.expression.to_string();
        if should_wrap(&expression) {
            write!(f, "eval:\n{}", indent(&expression))
        } else {
            write!(f, "eval {}", expression)
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ExpressionKind::*;
        match self.kind() {
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
        let target = self.target.to_string();

        if should_wrap(&target) {
            write!(f, "address_of:\n{}", indent(&target))
        } else {
            write!(f, "address_of({})", target)
        }
    }
}

impl Display for ArrayFromCopyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let element = self.element.to_string();
        let size = self.size.to_string();

        if should_wrap(&element) || should_wrap(&size) {
            write!(f, "arr_from_copy:\n{}\n{}", indent(&element), indent(&size))
        } else {
            write!(f, "arr_from_copy({}, {})", element, size)
        }
    }
}

impl Display for ArrayFromElementsExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "arr_from_elems:")?;
        for element in self.elements.iter() {
            write!(f, "\n{}", indent(&element.to_string()))?;
        }
        Ok(())
    }
}

impl Display for BlockExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "block:")?;
        for variable in self.local_variables.iter() {
            write!(
                f,
                "\n{}: {}",
                indent(&variable.borrow().to_string()),
                variable.borrow().type_.borrow().to_string()
            )?;
        }

        for statement in self.instructions.iter() {
            write!(f, "\n{}", indent(&statement.to_string()))?;
        }

        let value = self.value.to_string();
        if should_wrap(&value) {
            write!(f, "\n  value:\n  {}", indent(&value))
        } else {
            write!(f, "\n  value {}", value)
        }
    }
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let function = self.function.borrow().to_string();
        let args: Vec<_> = self.arguments.iter().map(|arg| arg.to_string()).collect();

        if args.iter().any(|arg| should_wrap(&arg)) {
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
        let pointer = self.pointer.to_string();

        if should_wrap(&pointer) {
            write!(f, "deref:\n{}", indent(&pointer))
        } else {
            write!(f, "deref({})", pointer)
        }
    }
}

impl Display for FieldAccessExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let receiver = self.receiver.to_string();
        let field = self.field.borrow().to_string();

        if should_wrap(&receiver) {
            write!(f, "field {}:\n{}", field, indent(&receiver))
        } else {
            write!(f, "field({}, {})", field, receiver)
        }
    }
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let cond = self.cond.to_string();
        let then = self.then.to_string();
        let else_ = self.else_.to_string();

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
        write!(f, "new({})", self.target_type.borrow().to_string())
    }
}

impl Display for VariableExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let variable = self.variable.borrow().to_string();
        write!(f, "{}", variable)
    }
}

fn should_wrap(line: &str) -> bool {
    line.contains('\n') || line.len() > MAX_LINE_LENGTH
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
