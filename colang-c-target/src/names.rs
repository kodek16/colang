use colang::program::{Field, FieldId, Function, FunctionId, TypeId, Variable, VariableId};
use std::collections::HashMap;

pub trait CNameRegistry {
    // TODO once `Type` has links to its type arguments that can be used without a `TypeRegistry`,
    // refactor these methods to accept `Type` instead of `TypeId`.

    fn type_name(&self, type_: &TypeId) -> &str;
    fn variable_name(&self, variable: &Variable) -> &str;
    fn function_name(&self, function: &Function) -> &str;
    fn field_name(&self, field: &Field) -> &str;

    fn add_type(&mut self, type_: &TypeId);
    fn add_variable(&mut self, variable: &Variable);
    fn add_function(&mut self, function: &Function);
    fn add_field(&mut self, field: &Field);

    /// Expression names are transient and not stored in the registry. Every call to this
    /// method returns a new unique name.
    fn expression_name(&mut self) -> String;
}

pub struct NumericCNameRegistry {
    type_names: HashMap<TypeId, String>,
    variable_names: HashMap<VariableId, String>,
    function_names: HashMap<FunctionId, String>,
    field_names: HashMap<FieldId, String>,

    // Names are generated as increasing integer sequence.
    next_type: usize,
    next_variable: usize,
    next_function: usize,
    next_field: usize,
    next_expression: usize,
}

impl NumericCNameRegistry {
    pub fn new() -> NumericCNameRegistry {
        NumericCNameRegistry {
            type_names: HashMap::new(),
            variable_names: HashMap::new(),
            function_names: HashMap::new(),
            field_names: HashMap::new(),
            next_type: 0,
            next_variable: 0,
            next_function: 0,
            next_field: 0,
            next_expression: 0,
        }
    }

    fn generate_type_name(&mut self) -> String {
        let name = format!("t{}", self.next_type);
        self.next_type += 1;
        name
    }

    fn generate_variable_name(&mut self) -> String {
        let name = format!("v{}", self.next_variable);
        self.next_variable += 1;
        name
    }

    fn generate_function_name(&mut self) -> String {
        let name = format!("f{}", self.next_function);
        self.next_function += 1;
        name
    }

    fn generate_field_name(&mut self) -> String {
        let name = format!("fi{}", self.next_field);
        self.next_field += 1;
        name
    }

    fn generate_expression_name(&mut self) -> String {
        let name = format!("e{}", self.next_expression);
        self.next_expression += 1;
        name
    }
}

impl CNameRegistry for NumericCNameRegistry {
    fn type_name(&self, type_: &TypeId) -> &str {
        self.type_names.get(&type_).expect(&format!(
            "Type `{:?}` was not added to C name registry",
            type_
        ))
    }

    fn variable_name(&self, variable: &Variable) -> &str {
        self.variable_names.get(&variable.id).expect(&format!(
            "Variable `{}` was not added to C name registry",
            variable.name
        ))
    }

    fn function_name(&self, function: &Function) -> &str {
        self.function_names.get(&function.id).expect(&format!(
            "Function `{}` was not added to C name registry",
            function.name
        ))
    }

    fn field_name(&self, field: &Field) -> &str {
        self.field_names.get(&field.id).expect(&format!(
            "Field `{}` was not added to C name registry",
            field.name
        ))
    }

    fn add_type(&mut self, type_: &TypeId) {
        let name = self.generate_type_name();
        let previous = self.type_names.insert(type_.clone(), name);
        if previous.is_some() {
            panic!("Type `{:?}` was added to C name registry twice", type_)
        }
    }

    fn add_variable(&mut self, variable: &Variable) {
        let name = self.generate_variable_name();
        let previous = self.variable_names.insert(variable.id.clone(), name);
        if previous.is_some() {
            panic!(
                "Variable `{}` was added to C name registry twice",
                variable.name
            )
        }
    }

    fn add_function(&mut self, function: &Function) {
        let name = self.generate_function_name();
        let previous = self.function_names.insert(function.id.clone(), name);
        if previous.is_some() {
            panic!(
                "Function `{}` was added to C name registry twice",
                function.name
            )
        }
    }

    fn add_field(&mut self, field: &Field) {
        let name = self.generate_field_name();
        let previous = self.field_names.insert(field.id.clone(), name);
        if previous.is_some() {
            panic!("Field `{}` was added to C name registry twice", field.name)
        }
    }

    fn expression_name(&mut self) -> String {
        self.generate_expression_name()
    }
}
