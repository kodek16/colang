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
        // Start with prelude.
        write!(self, "{}\n", crate::prelude::PRELUDE_SOURCE)?;

        // Create C names for all user structs and arrays through forward-decls.
        for type_ in program.sorted_types() {
            if type_.borrow().is_user_defined() || type_.borrow().is_array() {
                self.write_type_forward_decl(names, &type_.borrow())?;
                write!(self, "\n")?;
            }
        }

        // Generate definitions for all arrays.
        for type_ in program.sorted_types() {
            if type_.borrow().is_array() {
                self.write_array_def(names, &type_.borrow())?;
            }
        }
        write!(self, "\n")?;

        // Add type definitions for all user structs.
        for type_ in program.sorted_types() {
            if type_.borrow().is_user_defined() {
                self.write_type_def(names, &type_.borrow())?;
                write!(self, "\n")?;
            }
        }

        // Create C names for all user functions through forward-decls.
        for function in program.all_user_functions() {
            self.write_function_forward_decl(names, &function.borrow())?;
        }
        write!(self, "\n")?;

        // Add definitions for all user functions.
        for function in program.all_user_functions() {
            self.write_function_def(names, &function.borrow())?;
            write!(self, "\n")?;
        }

        // Add the `main` stub.
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
        names.add_type(&type_);
        let name = names.type_name(&type_);

        write!(self, "struct /* {} */ s_{};\n", type_.name, name)?;
        write!(self, "typedef struct s_{0} {0};\n", name)?;
        write!(self, "void init_{}({}*);\n", name, name)?;
        Ok(())
    }

    fn write_type_def(&mut self, names: &mut impl CNameRegistry, type_: &Type) -> fmt::Result {
        write!(self, "typedef struct s_{} {{\n", names.type_name(&type_),)?;
        self.indent();

        for field in &type_.fields {
            self.write_field_def(names, &field.borrow())?;
        }

        self.dedent();
        write!(self, "}} {};\n", names.type_name(&type_))?;

        write!(self, "void init_{0}({0}* p) {{\n", names.type_name(&type_))?;
        self.indent();

        for field in &type_.fields {
            self.write_init_function_name(names, &field.borrow().type_.borrow())?;
            write!(self, "(&(p->{}));\n", names.field_name(&field.borrow()))?;
        }

        self.dedent();
        write!(self, "}}\n")?;

        Ok(())
    }

    fn write_array_def(&mut self, names: &mut impl CNameRegistry, type_: &Type) -> fmt::Result {
        write!(self, "define_array(")?;
        self.write_type_name(names, &type_.array_element_type().unwrap().borrow())?;
        write!(self, ", ")?;
        self.write_type_name(names, &type_)?;
        write!(self, ")\n")?;

        Ok(())
    }

    fn write_function_forward_decl(
        &mut self,
        names: &mut impl CNameRegistry,
        function: &Function,
    ) -> fmt::Result {
        names.add_function(function);

        self.write_type_name(names, &function.return_type.borrow())?;
        write!(self, " {}(", names.function_name(function))?;

        for (index, parameter) in function.parameters.iter().enumerate() {
            if index > 0 {
                write!(self, ", ")?;
            }
            self.write_type_name(names, &parameter.borrow().type_.borrow())?;
        }

        write!(self, "); /* {} */\n", function.name)
    }

    fn write_function_def(
        &mut self,
        names: &mut impl CNameRegistry,
        function: &Function,
    ) -> fmt::Result {
        self.write_type_name(names, &function.return_type.borrow())?;
        write!(self, " {}(", names.function_name(function))?;

        for (index, parameter) in function.parameters.iter().enumerate() {
            names.add_variable(&parameter.borrow());

            if index > 0 {
                write!(self, ", ")?;
            }
            self.write_type_name(names, &parameter.borrow().type_.borrow())?;
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

    fn write_field_def(&mut self, names: &mut impl CNameRegistry, field: &Field) -> fmt::Result {
        names.add_field(field);

        self.write_type_name(names, &field.type_.borrow())?;
        write!(
            self,
            " {}; /* {}: {} */\n",
            names.field_name(field),
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
        use ExpressionImpl::*;
        let type_ = expression.type_().borrow();
        match **expression {
            Address(ref expr) => self.write_address_expr(names, expr),
            ArrayFromCopy(ref expr) => self.write_array_from_copy_expr(names, expr, &type_),
            ArrayFromElements(ref expr) => self.write_array_from_elements_expr(names, expr, &type_),
            Block(ref block) => self.write_block_expr(names, block, &type_),
            BooleanOp(ref expr) => self.write_boolean_op_expr(names, expr),
            Call(ref expr) => self.write_call_expr(names, expr),
            Deref(ref expr) => self.write_deref_expr(names, expr),
            FieldAccess(ref expr) => self.write_field_access_expr(names, expr),
            If(ref expr) => self.write_if_expr(names, expr, &type_),
            Is(ref expr) => self.write_is_expr(names, expr),
            Literal(ref expr) => self.write_literal_expr(names, expr),
            New(ref expr) => self.write_new_expr(names, expr),
            Null(ref expr) => self.write_null_expr(names, expr),
            Variable(ref expr) => self.write_variable_expr(names, expr),

            Empty(_) => Ok(None),
            Err(_) => panic!("Error expression encountered"),
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
        array_type: &Type,
    ) -> ExprWriteResult {
        // TODO: emit runtime error if size <= 0.
        let expression_name = names.expression_name();
        let element_type = expression.element.type_().borrow();

        let element = self.write_expression(names, &expression.element)?.unwrap();
        let size = self.write_expression(names, &expression.size)?.unwrap();

        self.write_type_name(names, array_type)?;
        write!(self, " {};\n", expression_name)?;
        write!(self, "{}.data = (", expression_name)?;
        self.write_type_name(names, &element_type)?;
        write!(self, " *) malloc({} * sizeof({}));\n", size, element)?;
        write!(
            self,
            "{0}.len = {0}.capacity = {1};\n",
            expression_name, size
        )?;
        write!(
            self,
            "for (int i = 0; i < {}; ++i) {}.data[i] = {};\n",
            size, expression_name, element
        )?;

        Ok(Some(expression_name))
    }

    fn write_array_from_elements_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &ArrayFromElementsExpr,
        array_type: &Type,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        let element_type = expression.element_type.borrow();

        let elements: Result<Vec<_>, _> = expression
            .elements
            .iter()
            .map(|element| self.write_expression(names, element))
            .collect();

        let elements: Vec<_> = elements?
            .into_iter()
            .map(|element| element.unwrap())
            .collect();

        self.write_type_name(names, array_type)?;
        write!(self, " {};\n", expression_name)?;

        if !elements.is_empty() {
            write!(self, "{}.data = (", expression_name)?;
            self.write_type_name(names, &element_type)?;
            write!(self, " *) malloc({} * sizeof(", elements.len())?;
            self.write_type_name(names, &element_type)?;
            write!(self, "));\n")?;

            write!(
                self,
                "{0}.len = {0}.capacity = {1};\n",
                expression_name,
                elements.len()
            )?;
            for (index, element) in elements.iter().enumerate() {
                write!(self, "{}.data[{}] = {};\n", expression_name, index, element)?;
            }
        } else {
            self.write_init_function_name(names, array_type)?;
            write!(self, "(&{});\n", expression_name)?;
        }

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

            self.write_type_name(names, &variable.type_.borrow())?;
            write!(self, " {};\n", names.variable_name(&variable))?;

            self.write_init_function_name(names, &variable.type_.borrow())?;
            write!(self, "(&{});\n", names.variable_name(&variable))?;
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
            self.write_type_name(names, &return_type)?;
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
        Ok(Some(format!("({}.{})", receiver, names.field_name(&field))))
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
        match &expression.value {
            LiteralValue::Int(x) => Ok(Some(format!("{}", x))),
            LiteralValue::Char(c) => Ok(Some(format!("'\\x{:x}'", c))),
            LiteralValue::Bool(b) => Ok(Some(String::from(if *b { "1" } else { "0" }))),
            LiteralValue::String(s) => {
                let expression_name = names.expression_name();
                write!(
                    self,
                    "str {} = str_from_static({});\n",
                    expression_name,
                    create_c_literal(s),
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

        self.write_type_name(names, &target_type)?;
        write!(self, "* {} = (", expression_name)?;
        self.write_type_name(names, &target_type)?;
        write!(self, "*) malloc(sizeof(")?;
        self.write_type_name(names, &target_type)?;
        write!(self, "));\n")?;

        self.write_init_function_name(names, &target_type)?;
        write!(self, "({});\n", expression_name)?;

        Ok(Some(expression_name))
    }

    fn write_null_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &NullExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        let target_type = expression.target_type.borrow();

        self.write_type_name(names, &target_type)?;
        write!(self, "* {} = ((", expression_name)?;
        self.write_type_name(names, &target_type)?;
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
            self.write_type_name(names, &type_)?;
            write!(self, " {};\n", name)?;
            Ok(Some(name))
        } else {
            Ok(None)
        }
    }

    fn write_type_name(&mut self, names: &mut impl CNameRegistry, type_: &Type) -> fmt::Result {
        match &type_.type_id {
            TypeId::Int => write!(self, "i32"),
            TypeId::Char => write!(self, "char"),
            TypeId::Bool => write!(self, "char"),
            TypeId::Void => write!(self, "void"),
            TypeId::String => write!(self, "str"),

            TypeId::TemplateInstance(TypeTemplateId::Pointer, _) => {
                self.write_type_name(names, &type_.pointer_target_type().unwrap().borrow())?;
                write!(self, "*")
            }

            TypeId::Struct(_)
            | TypeId::TemplateInstance(TypeTemplateId::Struct(_), _)
            | TypeId::TemplateInstance(TypeTemplateId::Array, _) => {
                write!(self, "{}", names.type_name(type_))
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
            FunctionId::Internal(InternalFunctionTag::IntToString) => write!(self, "int_to_string"),
            FunctionId::Internal(InternalFunctionTag::StringAdd) => write!(self, "str_add"),
            FunctionId::Internal(InternalFunctionTag::StringIndex) => write!(self, "str_index"),
            FunctionId::Internal(InternalFunctionTag::StringEq) => write!(self, "str_eq"),
            FunctionId::Internal(InternalFunctionTag::StringNotEq) => write!(self, "str_neq"),

            FunctionId::Internal(InternalFunctionTag::ArrayPush(_)) => {
                let self_parameter = function.parameters[0].borrow();
                let array_type = self_parameter.type_.borrow().pointer_target_type().unwrap();
                write!(self, "push_{}", names.type_name(&array_type.borrow()))?;
                Ok(())
            }
            FunctionId::Internal(InternalFunctionTag::ArrayPop(_)) => {
                let self_parameter = function.parameters[0].borrow();
                let array_type = self_parameter.type_.borrow().pointer_target_type().unwrap();
                write!(self, "pop_{}", names.type_name(&array_type.borrow()))?;
                Ok(())
            }
            FunctionId::Internal(InternalFunctionTag::ArrayLen(_)) => {
                let self_parameter = function.parameters[0].borrow();
                let array_type = self_parameter.type_.borrow();
                write!(self, "len_{}", names.type_name(&array_type))
            }
            FunctionId::Internal(InternalFunctionTag::ArrayIndex(_)) => {
                let self_parameter = function.parameters[0].borrow();
                let array_type = self_parameter.type_.borrow();
                write!(self, "index_{}", names.type_name(&array_type))
            }

            FunctionId::UserDefined(_) => write!(self, "{}", names.function_name(function)),
            FunctionId::InstantiatedMethod(_, _) => {
                write!(self, "{}", names.function_name(function))
            }
        }
    }

    fn write_init_function_name(
        &mut self,
        names: &mut impl CNameRegistry,
        type_: &Type,
    ) -> fmt::Result {
        match &type_.type_id {
            TypeId::Struct(_)
            | TypeId::TemplateInstance(TypeTemplateId::Struct(_), _)
            | TypeId::TemplateInstance(TypeTemplateId::Array, _) => {
                write!(self, "init_")?;
                self.write_type_name(names, type_)
            }
            _ => write!(self, "init_0"),
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
