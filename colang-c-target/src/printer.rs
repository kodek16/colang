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

        for type_ in program.types().all_user_defined() {
            self.write_type_forward_decl(names, &type_.borrow())?;
        }
        write!(self, "\n")?;

        for type_ in program.types().all_user_defined() {
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
            ExpressionKind::Block(ref block) => self.write_block_expr(names, block, &type_),
            ExpressionKind::Call(ref expression) => self.write_call_expr(names, expression),
            ExpressionKind::If(ref expression) => self.write_if_expr(names, expression, &type_),
            ExpressionKind::Literal(ref expression) => self.write_literal_expr(names, expression),
            ExpressionKind::Variable(ref expression) => self.write_variable_expr(names, expression),

            ExpressionKind::Empty(_) => Ok(None),
            _ => unimplemented!(),
        }
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

            // TODO default-init
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

    fn write_literal_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &LiteralExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();

        match expression {
            LiteralExpr::Int(x, _) => write!(self, "i32 {} = {};\n", expression_name, x),
            LiteralExpr::Char(c, _) => write!(self, "char {} = '\\x{}';\n", expression_name, c),
            LiteralExpr::Bool(b, _) => write!(
                self,
                "char {} = {};\n",
                expression_name,
                if *b { "1" } else { "0" }
            ),
            LiteralExpr::String(_s, _) => unimplemented!(),
        }
        .map(|_| Some(expression_name))
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
            _ => unimplemented!(),
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

            FunctionId::Internal(InternalFunctionTag::LessInt) => write!(self, "less"),
            FunctionId::Internal(InternalFunctionTag::GreaterInt) => write!(self, "greater"),
            FunctionId::Internal(InternalFunctionTag::LessEqInt) => write!(self, "less_eq"),
            FunctionId::Internal(InternalFunctionTag::GreaterEqInt) => write!(self, "greater_eq"),
            FunctionId::Internal(InternalFunctionTag::EqInt) => write!(self, "eq"),
            FunctionId::Internal(InternalFunctionTag::NotEqInt) => write!(self, "neq"),

            FunctionId::Internal(_) => unimplemented!(),

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
