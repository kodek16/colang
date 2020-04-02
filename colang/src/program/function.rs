use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::internal::InternalFunctionTag;
use crate::program::{Expression, SymbolId, Type, Variable};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub enum Function {
    UserDefined(UserDefinedFunction),
    Internal(InternalFunction),
}

pub trait Parameter {
    fn name(&self) -> &str;
    fn type_(&self) -> &Rc<RefCell<Type>>;
}

impl Function {
    pub fn name(&self) -> &str {
        match self {
            Function::UserDefined(function) => &function.name,
            Function::Internal(function) => &function.name,
        }
    }

    pub fn definition_site(&self) -> Option<InputSpan> {
        match self {
            Function::UserDefined(function) => Some(function.definition_site),
            Function::Internal(_) => None,
        }
    }

    // To implement this method normally, https://github.com/rust-lang/rust/issues/27732
    // needs to be stable: it is required if we want to coerce Ref<Variable> to Ref<dyn Parameter>.
    // In its absence, the best thing we can do is clone the parameter data manually to break
    // away from RefCell.
    pub fn parameters(&self) -> Box<dyn ExactSizeIterator<Item = impl Parameter> + '_> {
        struct SizedParameter {
            name: String,
            type_: Rc<RefCell<Type>>,
        }
        impl Parameter for SizedParameter {
            fn name(&self) -> &str {
                &self.name
            }

            fn type_(&self) -> &Rc<RefCell<Type>> {
                &self.type_
            }
        }

        match self {
            Function::UserDefined(function) => {
                Box::new(function.parameters.iter().map(|parameter| {
                    let parameter = parameter.borrow();
                    SizedParameter {
                        name: parameter.name().to_string(),
                        type_: Rc::clone(&parameter.type_),
                    }
                }))
            }
            Function::Internal(function) => {
                Box::new(function.parameters.iter().map(|parameter| SizedParameter {
                    name: parameter.name.clone(),
                    type_: Rc::clone(&parameter.type_),
                }))
            }
        }
    }

    pub fn return_type(&self) -> &Rc<RefCell<Type>> {
        match self {
            Function::UserDefined(ref function) => &function.return_type,
            Function::Internal(ref function) => &function.return_type,
        }
    }

    pub fn as_user_defined(&self) -> &UserDefinedFunction {
        match self {
            Function::UserDefined(ref function) => function,
            Function::Internal(ref function) => panic!(
                "Attempt to treat internal function `{}` as user-defined",
                function.name
            ),
        }
    }

    pub fn as_user_defined_mut(&mut self) -> &mut UserDefinedFunction {
        match self {
            Function::UserDefined(ref mut function) => function,
            Function::Internal(ref function) => panic!(
                "Attempt to treat internal function `{}` as user-defined",
                function.name
            ),
        }
    }
}

impl Parameter for Variable {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &Rc<RefCell<Type>> {
        &self.type_
    }
}

#[derive(Debug)]
pub struct UserDefinedFunction {
    pub name: String,
    pub definition_site: InputSpan,
    pub(crate) id: Option<SymbolId>,
    parameters: Vec<Rc<RefCell<Variable>>>,
    return_type: Rc<RefCell<Type>>,
    body: Option<Expression>,
}

impl UserDefinedFunction {
    /// Initialize a new, empty function. Parameters and body are filled later.
    pub fn new(
        name: String,
        return_type: Rc<RefCell<Type>>,
        definition_site: InputSpan,
    ) -> Function {
        let function = UserDefinedFunction {
            name,
            definition_site,
            parameters: vec![],
            return_type,
            body: None,
            id: None,
        };

        Function::UserDefined(function)
    }

    pub fn fill_parameters(&mut self, parameters: Vec<Rc<RefCell<Variable>>>) {
        self.parameters = parameters
    }

    // We need to accept `body_type` as a separate parameter, because computing
    // the type for recursive functions involves borrowing the function immutably,
    // so if we do this inside the method, where we have a mutable borrow, it would cause a panic.
    #[must_use]
    pub fn fill_body(
        &mut self,
        body: Expression,
        body_type: Rc<RefCell<Type>>,
    ) -> Result<(), CompilationError> {
        if body_type != self.return_type {
            let error = CompilationError::function_body_type_mismatch(
                &self.return_type.borrow().name(),
                &body_type.borrow().name(),
                self.definition_site,
            );
            return Err(error);
        }

        self.body = Some(body);
        Ok(())
    }

    pub fn parameters(&self) -> impl Iterator<Item = impl Deref<Target = Variable> + '_> {
        self.parameters.iter().map(|parameter| parameter.borrow())
    }

    pub fn return_type(&self) -> impl Deref<Target = Type> + '_ {
        self.return_type.borrow()
    }

    pub fn body(&self) -> &Expression {
        &self.body.as_ref().expect("function body was not filled")
    }
}

#[derive(Debug)]
pub struct InternalFunction {
    pub name: String,
    pub tag: InternalFunctionTag,
    parameters: Vec<InternalParameter>,
    return_type: Rc<RefCell<Type>>,
}

#[derive(Debug)]
pub struct InternalParameter {
    pub name: String,
    pub type_: Rc<RefCell<Type>>,
}

impl InternalFunction {
    pub fn new(
        name: String,
        tag: InternalFunctionTag,
        parameters: Vec<InternalParameter>,
        return_type: Rc<RefCell<Type>>,
    ) -> Function {
        let function = InternalFunction {
            name,
            tag,
            parameters,
            return_type,
        };

        Function::Internal(function)
    }
}

impl Parameter for InternalParameter {
    fn name(&self) -> &str {
        &self.name
    }

    fn type_(&self) -> &Rc<RefCell<Type>> {
        &self.type_
    }
}
