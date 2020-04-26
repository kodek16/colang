use crate::names::CNameRegistry;
use colang::program::*;
use std::fmt::{self, Write};

pub struct CCodePrinter {
    code: String,
    indent_level: usize,
}

type ExprWriteResult = Result<Option<String>, fmt::Error>;

impl CCodePrinter {
    pub fn new() -> CCodePrinter {
        CCodePrinter {
            code: String::new(),
            indent_level: 0,
        }
    }

    pub fn write_program(
        &mut self,
        names: &mut impl CNameRegistry,
        program: &Program,
    ) -> fmt::Result {
        write!(self, "{}\n", crate::prelude::PRELUDE_SOURCE)?;

        for type_ in program.types().all_user_defined_types() {
            self.write_type_forward_decl(names, &type_.borrow())?;
        }
        write!(self, "\n")?;

        for type_ in program.types().all_user_defined_types() {
            self.write_type_def(names, &type_.borrow())?;
            write!(self, "\n")?;
        }

        for function in program.all_user_functions() {
            self.write_function_forward_decl(names, &function.borrow())?;
        }
        write!(self, "\n")?;

        for function in program.all_user_functions() {
            self.write_function_def(names, &function.borrow())?;
            write!(self, "\n")?;
        }

        write!(
            self,
            "int main() {{\n  {}();\n}}\n",
            names.function_name(&program.main_function().borrow())
        )?;

        Ok(())
    }

    fn write_type_forward_decl(
        &mut self,
        names: &mut impl CNameRegistry,
        type_: &Type,
    ) -> fmt::Result {
        names.add_type(&type_.type_id);

        write!(
            self,
            "struct /* {} */ {};\n",
            type_.name,
            names.type_name(&type_.type_id)
        )
    }

    fn write_type_def(&mut self, names: &mut impl CNameRegistry, type_: &Type) -> fmt::Result {
        write!(
            self,
            "struct /* {} */ {} {{\n",
            type_.name,
            names.type_name(&type_.type_id)
        )?;

        self.indent();
        for field in type_.fields() {
            self.write_field_def(names, &field.borrow())?;
        }
        self.dedent();

        write!(self, "}};\n")
    }

    fn write_function_forward_decl(
        &mut self,
        names: &mut impl CNameRegistry,
        function: &Function,
    ) -> fmt::Result {
        names.add_function(function);

        self.write_type_name(names, &function.return_type.borrow().type_id)?;
        write!(self, " {}(", names.function_name(function))?;

        for (index, parameter) in function.parameters.iter().enumerate() {
            if index > 0 {
                write!(self, ", ")?;
            }
            self.write_type_name(names, &parameter.borrow().type_.borrow().type_id)?;
        }

        write!(self, "); /* {} */\n", function.name)
    }

    fn write_function_def(
        &mut self,
        names: &mut impl CNameRegistry,
        function: &Function,
    ) -> fmt::Result {
        self.write_type_name(names, &function.return_type.borrow().type_id)?;
        write!(self, " {}(", names.function_name(function))?;

        for (index, parameter) in function.parameters.iter().enumerate() {
            names.add_variable(&parameter.borrow());

            if index > 0 {
                write!(self, ", ")?;
            }
            self.write_type_name(names, &parameter.borrow().type_.borrow().type_id)?;
            write!(self, " {}", names.variable_name(&parameter.borrow()))?;
        }

        write!(self, ") {{\n")?;
        self.indent();

        let return_value = self.write_expression(names, &function.body().borrow())?;
        if let Some(return_value) = return_value {
            write!(self, "return {};\n", return_value)?;
        }

        self.dedent();
        write!(self, "}}\n")
    }

    fn write_field_def(&mut self, names: &mut impl CNameRegistry, field: &Variable) -> fmt::Result {
        names.add_variable(field);

        self.write_type_name(names, &field.type_.borrow().type_id)?;
        write!(
            self,
            " {}; /* {}: {} */\n",
            names.variable_name(field),
            field.name,
            field.type_.borrow().name,
        )
    }

    // Note: if `expression` is an lvalue, the returned `String` must be a C lvalue that can
    // be assigned in the current context.
    fn write_expression(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &Expression,
    ) -> ExprWriteResult {
        let type_ = expression.type_().borrow();
        match expression.kind() {
            ExpressionKind::Address(ref expr) => self.write_address_expr(names, expr),
            ExpressionKind::ArrayFromCopy(ref expr) => self.write_array_from_copy_expr(names, expr),
            ExpressionKind::ArrayFromElements(ref expr) => {
                self.write_array_from_elements_expr(names, expr)
            }
            ExpressionKind::Block(ref block) => self.write_block_expr(names, block, &type_),
            ExpressionKind::BooleanOp(ref expr) => self.write_boolean_op_expr(names, expr),
            ExpressionKind::Call(ref expr) => self.write_call_expr(names, expr),
            ExpressionKind::Deref(ref expr) => self.write_deref_expr(names, expr),
            ExpressionKind::FieldAccess(ref expr) => self.write_field_access_expr(names, expr),
            ExpressionKind::If(ref expr) => self.write_if_expr(names, expr, &type_),
            ExpressionKind::Is(ref expr) => self.write_is_expr(names, expr),
            ExpressionKind::Literal(ref expr) => self.write_literal_expr(names, expr),
            ExpressionKind::New(ref expr) => self.write_new_expr(names, expr),
            ExpressionKind::Null(ref expr) => self.write_null_expr(names, expr),
            ExpressionKind::Variable(ref expr) => self.write_variable_expr(names, expr),

            ExpressionKind::Empty(_) => Ok(None),
            ExpressionKind::Error(_) => panic!("Error expression encountered"),
        }
    }

    fn write_address_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &AddressExpr,
    ) -> ExprWriteResult {
        let target = self.write_expression(names, &expression.target)?.unwrap();
        Ok(Some(format!("&{}", target)))
    }

    fn write_array_from_copy_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &ArrayFromCopyExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        let element_type = expression.element.type_().borrow();

        let element = self.write_expression(names, &expression.element)?.unwrap();
        let size = self.write_expression(names, &expression.size)?.unwrap();

        write!(self, "vec<")?;
        self.write_type_name(names, &element_type.type_id)?;
        write!(self, ">* {} = new vec<", expression_name)?;
        self.write_type_name(names, &element_type.type_id)?;
        write!(self, ">({}, {});\n", size, element)?;

        Ok(Some(expression_name))
    }

    fn write_array_from_elements_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &ArrayFromElementsExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();

        let elements: Result<Vec<_>, _> = expression
            .elements
            .iter()
            .map(|element| self.write_expression(names, element))
            .collect();

        let elements: Vec<_> = elements?
            .into_iter()
            .map(|element| element.unwrap())
            .collect();

        let element_type = expression.element_type.borrow();

        write!(self, "vec<")?;
        self.write_type_name(names, &element_type.type_id)?;
        write!(self, ">* {} = new vec<", expression_name)?;
        self.write_type_name(names, &element_type.type_id)?;
        write!(self, "> {{ {} }};\n", elements.join(", "))?;

        Ok(Some(expression_name))
    }

    fn write_block_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        block: &BlockExpr,
        block_type: &Type,
    ) -> ExprWriteResult {
        let expression_name = self.write_expr_value_placeholder(names, block_type)?;

        write!(self, "{{\n")?;
        self.indent();

        for variable in &block.local_variables {
            let variable = variable.borrow();
            names.add_variable(&variable);

            self.write_type_name(names, &variable.type_.borrow().type_id)?;
            write!(self, " {};\n", names.variable_name(&variable))?;

            if variable.type_.borrow().is_array() || variable.type_.borrow().is_string() {
                write!(self, "init_vec(&{});\n", names.variable_name(&variable))?;
            } else {
                write!(self, "init0({});\n", names.variable_name(&variable))?;
            }
        }

        for instruction in &block.instructions {
            self.write_instruction(names, instruction)?;
        }

        let value = self.write_expression(names, &block.value)?;
        if let Some(ref target) = expression_name {
            write!(self, "{} = {};\n", target, value.unwrap())?;
        }

        self.dedent();
        write!(self, "}}\n")?;

        Ok(expression_name)
    }

    fn write_boolean_op_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &BooleanOpExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        match &expression.op {
            BooleanOp::And(lhs, rhs) => {
                let lhs = self.write_expression(names, &lhs)?.unwrap();
                write!(self, "char {} = {};\n", expression_name, lhs)?;
                write!(self, "if ({}) {{\n", expression_name)?;
                self.indent();

                let rhs = self.write_expression(names, &rhs)?.unwrap();
                write!(self, "{} &= {};\n", expression_name, rhs)?;

                self.dedent();
                write!(self, "}}\n")?;

                Ok(Some(expression_name))
            }
            BooleanOp::Or(lhs, rhs) => {
                let lhs = self.write_expression(names, &lhs)?.unwrap();
                write!(self, "char {} = {};\n", expression_name, lhs)?;
                write!(self, "if (!{}) {{\n", expression_name)?;
                self.indent();

                let rhs = self.write_expression(names, &rhs)?.unwrap();
                write!(self, "{} |= {};\n", expression_name, rhs)?;

                self.dedent();
                write!(self, "}}\n")?;

                Ok(Some(expression_name))
            }
            BooleanOp::Not(operand) => {
                let operand = self.write_expression(names, operand)?.unwrap();
                Ok(Some(format!("(!{})", operand)))
            }
        }
    }

    fn write_call_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &CallExpr,
    ) -> ExprWriteResult {
        let arguments: Result<Vec<Option<String>>, fmt::Error> = expression
            .arguments
            .iter()
            .map(|argument| self.write_expression(names, argument))
            .collect();
        let arguments: Vec<_> = arguments?
            .into_iter()
            .map(|argument| argument.unwrap())
            .collect();

        let function = expression.function.borrow();
        let return_type = function.return_type.borrow();

        if return_type.is_void() {
            self.write_function_name(names, &function)?;
            write!(self, "({});\n", arguments.join(", "))?;
            Ok(None)
        } else {
            let expression_name = names.expression_name();
            self.write_type_name(names, &return_type.type_id)?;
            write!(self, " {} = ", expression_name)?;
            self.write_function_name(names, &function)?;
            write!(self, "({});\n", arguments.join(", "))?;
            Ok(Some(expression_name))
        }
    }

    fn write_deref_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &DerefExpr,
    ) -> ExprWriteResult {
        let pointer = self.write_expression(names, &expression.pointer)?.unwrap();
        Ok(Some(format!("(*{})", pointer)))
    }

    fn write_field_access_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &FieldAccessExpr,
    ) -> ExprWriteResult {
        let receiver = self.write_expression(names, &expression.receiver)?.unwrap();
        let field = expression.field.borrow();
        Ok(Some(format!(
            "({}.{})",
            receiver,
            names.variable_name(&field)
        )))
    }

    fn write_if_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &IfExpr,
        expression_type: &Type,
    ) -> ExprWriteResult {
        let expression_name = self.write_expr_value_placeholder(names, expression_type)?;

        let cond_name = self.write_expression(names, &expression.cond)?.unwrap();
        write!(self, "if ({}) {{\n", cond_name)?;
        self.indent();

        let then = self.write_expression(names, &expression.then)?;
        if let Some(ref target) = expression_name {
            write!(self, "{} = {};\n", target, then.unwrap())?;
        }

        self.dedent();
        write!(self, "}} else {{\n")?;
        self.indent();

        let else_ = self.write_expression(names, &expression.else_)?;
        if let Some(ref target) = expression_name {
            write!(self, "{} = {};\n", target, else_.unwrap())?;
        }

        self.dedent();
        write!(self, "}}\n")?;

        Ok(expression_name)
    }

    fn write_is_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &IsExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        let lhs = self.write_expression(names, &expression.lhs)?.unwrap();
        let rhs = self.write_expression(names, &expression.rhs)?.unwrap();
        write!(self, "char {} = {} == {};\n", expression_name, lhs, rhs)?;
        Ok(Some(expression_name))
    }

    fn write_literal_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &LiteralExpr,
    ) -> ExprWriteResult {
        match expression {
            LiteralExpr::Int(x, _) => Ok(Some(format!("{}", x))),
            LiteralExpr::Char(c, _) => Ok(Some(format!("'\\x{:x}'", c))),
            LiteralExpr::Bool(b, _) => Ok(Some(String::from(if *b { "1" } else { "0" }))),
            LiteralExpr::String(s, _) => {
                let literal_name = names.expression_name();
                let expression_name = names.expression_name();
                write!(
                    self,
                    "static_str {} = {};\n",
                    literal_name,
                    create_c_literal(s)
                )?;
                write!(
                    self,
                    "str {} = new vec<char>({}, {} + {});\n",
                    expression_name,
                    literal_name,
                    literal_name,
                    s.len()
                )?;
                Ok(Some(expression_name))
            }
        }
    }

    fn write_new_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &NewExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        let target_type = expression.target_type.borrow();

        self.write_type_name(names, &target_type.type_id)?;
        write!(self, "* {} = (", expression_name)?;
        self.write_type_name(names, &target_type.type_id)?;
        write!(self, "*) calloc(1, sizeof(")?;
        self.write_type_name(names, &target_type.type_id)?;
        write!(self, "));\n")?;

        if target_type.is_array() || target_type.is_string() {
            // We could use malloc or `new` instead of calloc in this case, but the whole
            // "auto-constructor" mechanism needs a refactor which would probably also fix
            // this issue.
            write!(self, "init_vec({});\n", expression_name)?;
        }

        Ok(Some(expression_name))
    }

    fn write_null_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &NullExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        let target_type = expression.target_type.borrow();

        self.write_type_name(names, &target_type.type_id)?;
        write!(self, "* {} = ((", expression_name)?;
        self.write_type_name(names, &target_type.type_id)?;
        write!(self, "*) NULL);\n")?;

        Ok(Some(expression_name))
    }

    fn write_variable_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &VariableExpr,
    ) -> ExprWriteResult {
        let name = names.variable_name(&expression.variable.borrow());
        Ok(Some(name.to_string()))
    }

    fn write_instruction(
        &mut self,
        names: &mut impl CNameRegistry,
        instruction: &Instruction,
    ) -> fmt::Result {
        match instruction {
            Instruction::Assign(ref instruction) => self.write_assign(names, instruction),
            Instruction::Eval(ref instruction) => self.write_eval(names, instruction),
            Instruction::Read(ref instruction) => self.write_read(names, instruction),
            Instruction::Return(ref instruction) => self.write_return(names, instruction),
            Instruction::While(ref instruction) => self.write_while(names, instruction),
            Instruction::Write(ref instruction) => self.write_write(names, instruction),
        }
    }

    fn write_assign(
        &mut self,
        names: &mut impl CNameRegistry,
        instruction: &AssignInstruction,
    ) -> fmt::Result {
        let target = self.write_expression(names, &instruction.target)?.unwrap();
        let value = self.write_expression(names, &instruction.value)?.unwrap();
        write!(self, "{} = {};\n", target, value)
    }

    fn write_eval(
        &mut self,
        names: &mut impl CNameRegistry,
        instruction: &EvalInstruction,
    ) -> fmt::Result {
        let _ = self.write_expression(names, &instruction.expression)?;
        Ok(())
    }

    fn write_read(
        &mut self,
        names: &mut impl CNameRegistry,
        instruction: &ReadInstruction,
    ) -> fmt::Result {
        let target = self.write_expression(names, &instruction.target)?.unwrap();

        if instruction.whole_line {
            write!(self, "readln(&{});\n", target)
        } else {
            let target_type = instruction.target.type_().borrow();
            if target_type.is_string() {
                write!(self, "readword_str(&{});\n", target)
            } else if target_type.is_int() {
                write!(self, "readword_int(&{});\n", target)
            } else {
                panic!("Invalid type in read instruction: `{}`", target_type.name)
            }
        }
    }

    fn write_return(
        &mut self,
        names: &mut impl CNameRegistry,
        instruction: &ReturnInstruction,
    ) -> fmt::Result {
        let value = self.write_expression(names, &instruction.expression)?;
        if let Some(value) = value {
            write!(self, "return {};\n", value)
        } else {
            write!(self, "return;\n")
        }
    }

    fn write_while(
        &mut self,
        names: &mut impl CNameRegistry,
        instruction: &WhileInstruction,
    ) -> fmt::Result {
        let cond_name = names.expression_name();

        let cond = self.write_expression(names, &instruction.cond)?.unwrap();
        write!(self, "char {} = {};\n", cond_name, cond)?;

        write!(self, "while ({}) {{\n", cond_name)?;
        self.indent();

        self.write_instruction(names, &instruction.body)?;
        let next_cond = self.write_expression(names, &instruction.cond)?.unwrap();
        write!(self, "{} = {};\n", cond_name, next_cond)?;

        self.dedent();
        write!(self, "}}\n")?;

        Ok(())
    }

    fn write_write(
        &mut self,
        names: &mut impl CNameRegistry,
        instruction: &WriteInstruction,
    ) -> fmt::Result {
        // TODO optimize the case where expression is a string literal.
        let value = self
            .write_expression(names, &instruction.expression)?
            .unwrap();
        write!(self, "write_str({});\n", value)
    }

    fn write_expr_value_placeholder(
        &mut self,
        names: &mut impl CNameRegistry,
        type_: &Type,
    ) -> ExprWriteResult {
        // Only non-void expressions create a named result.
        if !type_.is_void() {
            let name = names.expression_name();
            self.write_type_name(names, &type_.type_id)?;
            write!(self, " {};\n", name)?;
            Ok(Some(name))
        } else {
            Ok(None)
        }
    }

    fn write_type_name(&mut self, names: &mut impl CNameRegistry, type_id: &TypeId) -> fmt::Result {
        match type_id {
            TypeId::Int => write!(self, "i32"),
            TypeId::Char => write!(self, "char"),
            TypeId::Bool => write!(self, "char"),
            TypeId::Void => write!(self, "void"),
            TypeId::String => write!(self, "str"),

            TypeId::TemplateInstance(TypeTemplateId::Pointer, type_args) => {
                self.write_type_name(names, &type_args[0])?;
                write!(self, "*")
            }
            TypeId::TemplateInstance(TypeTemplateId::Array, type_args) => {
                write!(self, "vec<")?;
                self.write_type_name(names, &type_args[0])?;
                write!(self, ">*")
            }

            TypeId::Struct(_) => write!(self, "{}", names.type_name(type_id)),
            TypeId::TemplateInstance(TypeTemplateId::Struct(_), _) => {
                write!(self, "{}", names.type_name(type_id))
            }

            TypeId::TypeParameter(_, _) => panic!("Unfilled type parameter encountered"),
            TypeId::Error => panic!("Error type encountered"),
        }
    }

    fn write_function_name(
        &mut self,
        names: &mut impl CNameRegistry,
        function: &Function,
    ) -> fmt::Result {
        match function.id {
            FunctionId::Internal(InternalFunctionTag::AddInt) => write!(self, "add"),
            FunctionId::Internal(InternalFunctionTag::SubInt) => write!(self, "sub"),
            FunctionId::Internal(InternalFunctionTag::MulInt) => write!(self, "mul"),
            FunctionId::Internal(InternalFunctionTag::DivInt) => write!(self, "div"),
            FunctionId::Internal(InternalFunctionTag::ModInt) => write!(self, "mod"),

            FunctionId::Internal(InternalFunctionTag::LessInt) => write!(self, "lt"),
            FunctionId::Internal(InternalFunctionTag::GreaterInt) => write!(self, "gt"),
            FunctionId::Internal(InternalFunctionTag::LessEqInt) => write!(self, "lte"),
            FunctionId::Internal(InternalFunctionTag::GreaterEqInt) => write!(self, "gte"),
            FunctionId::Internal(InternalFunctionTag::EqInt) => write!(self, "eq"),
            FunctionId::Internal(InternalFunctionTag::NotEqInt) => write!(self, "neq"),

            FunctionId::Internal(InternalFunctionTag::Assert) => write!(self, "co_assert"),
            FunctionId::Internal(InternalFunctionTag::AsciiCode) => write!(self, "ascii_code"),
            FunctionId::Internal(InternalFunctionTag::AsciiChar) => write!(self, "ascii_char"),
            FunctionId::Internal(InternalFunctionTag::IntToString) => write!(self, "co_itos"),
            FunctionId::Internal(InternalFunctionTag::StringAdd) => write!(self, "str_add"),
            FunctionId::Internal(InternalFunctionTag::StringIndex) => write!(self, "str_index"),
            FunctionId::Internal(InternalFunctionTag::StringEq) => write!(self, "str_eq"),
            FunctionId::Internal(InternalFunctionTag::StringNotEq) => write!(self, "str_neq"),

            FunctionId::Internal(InternalFunctionTag::ArrayPush(_)) => write!(self, "vec_push"),
            FunctionId::Internal(InternalFunctionTag::ArrayPop(_)) => write!(self, "vec_pop"),
            FunctionId::Internal(InternalFunctionTag::ArrayLen(_)) => write!(self, "vec_len"),
            FunctionId::Internal(InternalFunctionTag::ArrayIndex(_)) => write!(self, "vec_index"),

            FunctionId::UserDefined(_) => write!(self, "{}", names.function_name(function)),
            FunctionId::InstantiatedMethod(_, _) => {
                write!(self, "{}", names.function_name(function))
            }
        }
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level -= 1;
    }
}

impl fmt::Write for CCodePrinter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let lines = s.split('\n');

        for (index, line) in lines.enumerate() {
            if index > 0 {
                write!(&mut self.code, "\n")?;
            }

            if line.len() > 0 {
                if self.code.ends_with('\n') {
                    for _ in 0..self.indent_level {
                        write!(&mut self.code, "  ")?;
                    }
                }

                write!(&mut self.code, "{}", line)?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for CCodePrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code)
    }
}

/// Creates a valid C string literal (potentially using concatenation) that represents the same
/// byte sequence as `s` in UTF-8.
///
/// Hex escape sequences are used to encode any problematic characters.
fn create_c_literal(s: &str) -> String {
    let mut parts = Vec::new();
    let mut current = String::from("\"");

    for b in s.bytes() {
        let c = b as char;
        if c.is_ascii_alphanumeric() || c == ' ' {
            current.push(c);
        } else {
            write!(&mut current, "\\x{:x}\"", b).unwrap();
            parts.push(current);
            current = String::from("\"");
        }
    }

    current.push('"');
    if &current != "\"\"" {
        parts.push(current);
    }
    parts.join(" ")
}
