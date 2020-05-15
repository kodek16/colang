//! Generating C code directly from CO code.

use crate::names::CNameRegistry;
use colang::program::*;
use std::fmt::{self, Write};

// These have to be macros because of the limitations of format! and concat! macros.
macro_rules! version {
    () => {
        env!("CARGO_PKG_VERSION")
    };
}

macro_rules! header {
    () => {
        r#"This C/C++ code was compiled from a higher-level code in CO.
CO is a programming language designed specifically for contests and olympiads.
Check it out at <link not yet available>.

colang v{} was used.
The original program code in CO is as follows:
"#
    };
}

/// A converter from CO program IR to C source code.
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
        source: &str,
        program: &Program,
    ) -> fmt::Result {
        // Include the CO source as a comment at the top of the generated file.
        write!(self, "{}\n", comment_out(&format!(header!(), version!())))?;
        write!(self, "{}\n", comment_out(source))?;

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
            write!(
                self,
                "{}(&(p->{}));\n",
                init_function_name(names, &field.borrow().type_.borrow()),
                names.field_name(&field.borrow())
            )?;
        }

        self.dedent();
        write!(self, "}}\n")?;

        Ok(())
    }

    fn write_array_def(&mut self, names: &mut impl CNameRegistry, type_: &Type) -> fmt::Result {
        write!(
            self,
            "define_array({}, {})\n",
            type_name(names, &type_.array_element_type().unwrap().borrow()),
            type_name(names, &type_)
        )
    }

    fn write_function_forward_decl(
        &mut self,
        names: &mut impl CNameRegistry,
        function: &Function,
    ) -> fmt::Result {
        names.add_function(function);

        write!(
            self,
            "{} {}({}); /* {} */\n",
            type_name(names, &function.return_type.borrow()),
            names.function_name(function),
            function
                .parameters
                .iter()
                .map(|param| type_name(names, &param.borrow().type_.borrow()))
                .collect::<Vec<_>>()
                .join(", "),
            function.name
        )
    }

    fn write_function_def(
        &mut self,
        names: &mut impl CNameRegistry,
        function: &Function,
    ) -> fmt::Result {
        let parameters = function
            .parameters
            .iter()
            .map(|param| {
                names.add_variable(&param.borrow());
                format!(
                    "{} {}",
                    type_name(names, &param.borrow().type_.borrow()),
                    names.variable_name(&param.borrow())
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        write!(
            self,
            "{} {}({}) {{\n",
            type_name(names, &function.return_type.borrow()),
            names.function_name(function),
            parameters
        )?;

        self.indent();

        let return_value = self.write_expression(names, &function.body().borrow())?;
        if let Some(return_value) = return_value {
            write!(self, "return {};\n", return_value)?;
        }

        self.dedent();
        write!(self, "}}\n")?;

        Ok(())
    }

    fn write_field_def(&mut self, names: &mut impl CNameRegistry, field: &Field) -> fmt::Result {
        names.add_field(field);

        write!(
            self,
            "{} {}; /* {}: {} */\n",
            type_name(names, &field.type_.borrow()),
            names.field_name(field),
            field.name,
            field.type_.borrow().name,
        )
    }

    /// Writes the C code produced from a CO expression and returns its value.
    ///
    /// The string returned from this method in case of success must be a C expression valid in the
    /// context directly after `expression` that evaluates in C to the value of the CO expression.
    ///
    /// The returned string must be a primary expression in C. That is, it must be bound together
    /// with maximal tightness.
    ///
    /// If `expression` is an lvalue, the returned string must also be an lvalue in C.
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
        let expression_name = names.expression_name();
        let element_type = expression.element.type_().borrow();

        let element = self.write_expression(names, &expression.element)?.unwrap();
        let size = self.write_expression(names, &expression.size)?.unwrap();

        write!(self, "assert_array_from_copy_size_ok({});\n", size)?;
        write!(
            self,
            "{} {};\n",
            type_name(names, &array_type),
            expression_name
        )?;
        write!(
            self,
            "{}.data = ({} *) malloc({} * sizeof({}));\n",
            expression_name,
            type_name(names, &element_type),
            size,
            element
        )?;
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

        write!(
            self,
            "{} {};\n",
            type_name(names, array_type),
            expression_name
        )?;

        if !elements.is_empty() {
            write!(
                self,
                "{0}.data = ({1} *) malloc({2} * sizeof({1}));\n",
                expression_name,
                type_name(names, &element_type),
                elements.len(),
            )?;

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
            write!(
                self,
                "{}(&{});\n",
                init_function_name(names, array_type),
                expression_name
            )?;
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

            write!(
                self,
                "{} {};\n",
                type_name(names, &variable.type_.borrow()),
                names.variable_name(&variable)
            )?;

            write!(
                self,
                "{}(&{});\n",
                init_function_name(names, &variable.type_.borrow()),
                names.variable_name(&variable)
            )?;
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

        let value = if return_type.is_void() {
            None
        } else {
            let expression_name = names.expression_name();
            write!(
                self,
                "{} {} = ",
                type_name(names, &return_type),
                expression_name
            )?;
            Some(expression_name)
        };

        write!(
            self,
            "{}({});\n",
            function_name(names, &function),
            arguments.join(", ")
        )?;

        Ok(value)
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

        write!(
            self,
            "{0}* {1} = ({0} *) malloc(sizeof({0}));\n",
            type_name(names, &target_type),
            expression_name,
        )?;

        write!(
            self,
            "{}({});\n",
            init_function_name(names, &target_type),
            expression_name
        )?;

        Ok(Some(expression_name))
    }

    fn write_null_expr(
        &mut self,
        names: &mut impl CNameRegistry,
        expression: &NullExpr,
    ) -> ExprWriteResult {
        let expression_name = names.expression_name();
        let target_type = expression.target_type.borrow();

        write!(
            self,
            "{0} *{1} = (({0} *) NULL);\n",
            type_name(names, &target_type),
            expression_name,
        )?;

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
            write!(self, "{} {};\n", type_name(names, &type_), name)?;
            Ok(Some(name))
        } else {
            Ok(None)
        }
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level -= 1;
    }
}

/// Builds the C type name corresponding to a CO `type_`.
///
/// Uses `names` registry for most types, but some types are not stored (e.g. pointers).
fn type_name(names: &impl CNameRegistry, type_: &Type) -> String {
    match &type_.type_id {
        TypeId::Int => String::from("i32"),
        TypeId::Char => String::from("char"),
        TypeId::Bool => String::from("char"),
        TypeId::Void => String::from("void"),
        TypeId::String => String::from("str"),

        TypeId::TemplateInstance(TypeTemplateId::Pointer, _) => format!(
            "{}*",
            type_name(names, &type_.pointer_target_type().unwrap().borrow())
        ),

        TypeId::Struct(_)
        | TypeId::TemplateInstance(TypeTemplateId::Struct(_), _)
        | TypeId::TemplateInstance(TypeTemplateId::Array, _) => {
            String::from(names.type_name(type_))
        }

        TypeId::TypeParameter(_, _) => panic!("Unfilled type parameter encountered"),
        TypeId::Error => panic!("Error type encountered"),
    }
}

/// Builds the C function name corresponding to a CO `function`.
///
/// May use `names` for user-defined functions.
fn function_name(names: &impl CNameRegistry, function: &Function) -> String {
    match function.id {
        FunctionId::Internal(InternalFunctionTag::AddInt) => String::from("add"),
        FunctionId::Internal(InternalFunctionTag::SubInt) => String::from("sub"),
        FunctionId::Internal(InternalFunctionTag::MulInt) => String::from("mul"),
        FunctionId::Internal(InternalFunctionTag::DivInt) => String::from("div"),
        FunctionId::Internal(InternalFunctionTag::ModInt) => String::from("mod"),

        FunctionId::Internal(InternalFunctionTag::LessInt) => String::from("lt"),
        FunctionId::Internal(InternalFunctionTag::GreaterInt) => String::from("gt"),
        FunctionId::Internal(InternalFunctionTag::LessEqInt) => String::from("lte"),
        FunctionId::Internal(InternalFunctionTag::GreaterEqInt) => String::from("gte"),
        FunctionId::Internal(InternalFunctionTag::EqInt) => String::from("eq"),
        FunctionId::Internal(InternalFunctionTag::NotEqInt) => String::from("neq"),

        FunctionId::Internal(InternalFunctionTag::Assert) => String::from("co_assert"),
        FunctionId::Internal(InternalFunctionTag::AsciiCode) => String::from("ascii_code"),
        FunctionId::Internal(InternalFunctionTag::AsciiChar) => String::from("ascii_char"),
        FunctionId::Internal(InternalFunctionTag::IntToString) => String::from("int_to_string"),
        FunctionId::Internal(InternalFunctionTag::StringAdd) => String::from("str_add"),
        FunctionId::Internal(InternalFunctionTag::StringIndex) => String::from("str_index"),
        FunctionId::Internal(InternalFunctionTag::StringEq) => String::from("str_eq"),
        FunctionId::Internal(InternalFunctionTag::StringNotEq) => String::from("str_neq"),

        FunctionId::Internal(InternalFunctionTag::ArrayPush(_)) => {
            let self_parameter = function.parameters[0].borrow();
            let array_type = self_parameter.type_.borrow().pointer_target_type().unwrap();
            format!("push_{}", names.type_name(&array_type.borrow()))
        }
        FunctionId::Internal(InternalFunctionTag::ArrayPop(_)) => {
            let self_parameter = function.parameters[0].borrow();
            let array_type = self_parameter.type_.borrow().pointer_target_type().unwrap();
            format!("pop_{}", names.type_name(&array_type.borrow()))
        }
        FunctionId::Internal(InternalFunctionTag::ArrayLen(_)) => {
            let self_parameter = function.parameters[0].borrow();
            let array_type = self_parameter.type_.borrow();
            format!("len_{}", names.type_name(&array_type))
        }
        FunctionId::Internal(InternalFunctionTag::ArrayIndex(_)) => {
            let self_parameter = function.parameters[0].borrow();
            let array_type = self_parameter.type_.borrow();
            format!("index_{}", names.type_name(&array_type))
        }

        FunctionId::UserDefined(_) => String::from(names.function_name(function)),
        FunctionId::InstantiatedMethod(_, _) => String::from(names.function_name(function)),
    }
}

/// Builds the C function name corresponding to the constructor of a CO `type_`.
fn init_function_name(names: &impl CNameRegistry, type_: &Type) -> String {
    match &type_.type_id {
        TypeId::Struct(_)
        | TypeId::TemplateInstance(TypeTemplateId::Struct(_), _)
        | TypeId::TemplateInstance(TypeTemplateId::Array, _) => {
            format!("init_{}", type_name(names, type_))
        }
        _ => String::from("init_0"),
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

/// Converts a body of text to a C comment by prepending `//` to every line.
fn comment_out(text: &str) -> String {
    let mut result = String::new();
    for line in text.lines() {
        result.push_str("// ");
        result.push_str(line);
        result.push('\n');
    }
    result
}

// Automatic indentation (controlled by `indent()` and `dedent()` methods) for `CCodePrinter`.
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
