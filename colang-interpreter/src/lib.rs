//! Interpreter backend for CO can run the code right after it is compiled.

mod cin;
mod internal;

use cin::Cin;
use colang::backends::Backend;
use colang::program::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub struct InterpreterBackend;

impl Backend for InterpreterBackend {
    fn run(&self, program: Program) -> Result<(), ()> {
        let mut state = State::new();

        let main = program.main_function();
        let result = run_user_function(main, &mut state);
        match result {
            Ok(_) => Ok(()),
            Err(error) => {
                eprintln!(
                    "Error occurred while running the program:\n{}",
                    error.to_string()
                );
                Err(())
            }
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Lvalue(Lvalue),
    Rvalue(Rvalue),
}

impl Value {
    pub fn into_lvalue(self) -> Lvalue {
        match self {
            Value::Lvalue(value) => value,
            Value::Rvalue(_) => panic!("Rvalue accessed as lvalue."),
        }
    }

    pub fn into_rvalue(self) -> Rvalue {
        match self {
            Value::Lvalue(value) => (*value.borrow()).clone(),
            Value::Rvalue(value) => value,
        }
    }
}

#[derive(Clone)]
pub struct Lvalue(Rc<RefCell<Rvalue>>);

impl Lvalue {
    pub fn store(rvalue: Rvalue) -> Lvalue {
        Lvalue(Rc::new(RefCell::new(rvalue)))
    }

    pub fn clone_contents(&self) -> Lvalue {
        Lvalue::store(self.0.borrow().clone())
    }

    pub fn detach(&self) -> Rvalue {
        self.0.borrow().clone()
    }

    pub fn borrow(&self) -> impl Deref<Target = Rvalue> + '_ {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> impl DerefMut<Target = Rvalue> + '_ {
        self.0.borrow_mut()
    }
}

/// Every value that exists in the program belongs to this type.
pub enum Rvalue {
    Int(i32),
    Bool(bool),
    Array(Rc<RefCell<Vec<Lvalue>>>),
    Pointer(Option<Lvalue>),
    Struct(HashMap<SymbolId, Lvalue>),
    Void,
}

impl Rvalue {
    pub fn as_int(&self) -> i32 {
        match self {
            Rvalue::Int(x) => *x,
            _ => panic_wrong_type("int", self.type_()),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Rvalue::Bool(b) => *b,
            _ => panic_wrong_type("bool", self.type_()),
        }
    }

    pub fn into_array(self) -> Rc<RefCell<Vec<Lvalue>>> {
        match self {
            Rvalue::Array(v) => v,
            _ => panic_wrong_type("array", self.type_()),
        }
    }

    pub fn into_pointer(self) -> Option<Lvalue> {
        match self {
            Rvalue::Pointer(p) => p,
            _ => panic_wrong_type("pointer", self.type_()),
        }
    }

    pub fn into_pointer_to_self(self) -> RunResult<Lvalue> {
        match self.into_pointer() {
            Some(pointer) => Ok(pointer),
            None => {
                let error = "`self` is null in method";
                return Err(error.into());
            }
        }
    }

    pub fn as_struct(&self) -> &HashMap<SymbolId, Lvalue> {
        match self {
            Rvalue::Struct(fields) => fields,
            _ => panic_wrong_type("struct", self.type_()),
        }
    }

    /// A quick-and-dirty information source about value types in runtime.
    /// This is _not_ meant to be an actual RTTI solution, it is only meant
    /// to be used in type mismatch panics, which should not occur under
    /// normal circumstances.
    pub fn type_(&self) -> &'static str {
        use Rvalue::*;
        match self {
            Int(_) => "int",
            Bool(_) => "bool",
            Array(_) => "array",
            Pointer(_) => "pointer",
            Struct(_) => "struct",
            Void => "void",
        }
    }
}

impl Clone for Rvalue {
    fn clone(&self) -> Self {
        use Rvalue::*;
        match self {
            Int(x) => Rvalue::Int(*x),
            Bool(b) => Rvalue::Bool(*b),
            Array(v) => Rvalue::Array(Rc::clone(v)),
            Pointer(p) => Rvalue::Pointer(p.clone()),
            Struct(fields) => {
                let fields_copy = fields
                    .iter()
                    .map(|(id, lvalue)| (*id, lvalue.clone_contents()))
                    .collect();
                Rvalue::Struct(fields_copy)
            }
            Void => Rvalue::Void,
        }
    }
}

struct State {
    variables: HashMap<SymbolId, Vec<Lvalue>>,
    cin: Cin,

    /// Value to be returned from the innermost current context.
    return_value: Option<Rvalue>,
}

impl State {
    fn new() -> State {
        State {
            variables: HashMap::new(),
            cin: Cin::new(),
            return_value: None,
        }
    }

    fn push(&mut self, variable_id: SymbolId, value: Rvalue) {
        self.variables
            .entry(variable_id)
            .or_default()
            .push(Lvalue::store(value))
    }

    fn pop(&mut self, variable_id: SymbolId) {
        self.variables
            .get_mut(&variable_id)
            .expect("variable deallocated before allocation")
            .pop()
            .expect("variable deallocated twice");
    }

    fn get(&self, variable_id: SymbolId) -> Value {
        Value::Lvalue(
            self.variables
                .get(&variable_id)
                .expect("variable accessed before allocation")
                .last()
                .expect("variable accessed after deallocation")
                .clone(),
        )
    }
}

type RunResult<T> = Result<T, Box<dyn Error>>;

fn run_user_function(
    function: impl Deref<Target = Function>,
    state: &mut State,
) -> RunResult<Value> {
    let body = function.as_user_defined().body();
    run_expression(body, state)
}

fn run_internal_function(function: &InternalFunction, arguments: Vec<Value>) -> RunResult<Value> {
    use InternalFunctionTag::*;
    match function.tag {
        Assert => internal::assert(arguments),
        AddInt => internal::add_int(arguments),
        SubInt => internal::sub_int(arguments),
        MulInt => internal::mul_int(arguments),
        LessInt => internal::less_int(arguments),
        GreaterInt => internal::greater_int(arguments),
        LessEqInt => internal::less_eq_int(arguments),
        GreaterEqInt => internal::greater_eq_int(arguments),
        EqInt => internal::eq_int(arguments),
        NotEqInt => internal::not_eq_int(arguments),
        ArrayPush(_) => internal::array_push(arguments),
        ArrayPop(_) => internal::array_pop(arguments),
        ArrayLen(_) => internal::array_len(arguments),
    }
}

fn run_instruction(instruction: &Instruction, state: &mut State) -> RunResult<()> {
    match instruction {
        Instruction::Alloc(ref s) => run_alloc(s, state),
        Instruction::Dealloc(ref s) => run_dealloc(s, state),
        Instruction::Read(ref s) => run_read(s, state),
        Instruction::Write(ref s) => run_write(s, state),
        Instruction::While(ref s) => run_while(s, state),
        Instruction::Assign(ref s) => run_assign(s, state),
        Instruction::Return(ref s) => run_return(s, state),
        Instruction::Eval(ref s) => run_eval(s, state),
    }
}

fn run_alloc(instruction: &AllocInstruction, state: &mut State) -> RunResult<()> {
    let variable_id = instruction.variable().id;

    let initial_value = match instruction.initializer() {
        Some(initializer) => run_expression(initializer, state)?.into_rvalue(),
        None => {
            let variable = instruction.variable();
            let variable_type = variable.type_();
            default_value_for_type(&variable_type)
        }
    };

    state.push(variable_id, initial_value);
    Ok(())
}

fn run_dealloc(instruction: &DeallocInstruction, state: &mut State) -> RunResult<()> {
    let variable_id = instruction.variable().id;
    state.pop(variable_id);
    Ok(())
}

fn run_read(instruction: &ReadInstruction, state: &mut State) -> RunResult<()> {
    let target = run_expression(instruction.target(), state)?.into_lvalue();
    let word = state.cin.read_word()?;
    let new_value: i32 = word
        .parse()
        .map_err(|_| format!("Could not parse `{}` to an integer.", word))?;
    *target.borrow_mut() = Rvalue::Int(new_value);
    Ok(())
}

fn run_write(instruction: &WriteInstruction, state: &mut State) -> RunResult<()> {
    let value = run_expression(instruction.expression(), state)?.into_rvalue();
    match value {
        Rvalue::Int(x) => println!("{}", x),
        _ => panic_wrong_type("int", value.type_()),
    }
    Ok(())
}

fn run_while(instruction: &WhileInstruction, state: &mut State) -> RunResult<()> {
    let mut cond = run_expression(instruction.cond(), state)?
        .into_rvalue()
        .as_bool();
    while cond {
        run_instruction(instruction.body(), state)?;
        cond = run_expression(instruction.cond(), state)?
            .into_rvalue()
            .as_bool();
    }
    Ok(())
}

fn run_assign(instruction: &AssignInstruction, state: &mut State) -> RunResult<()> {
    let target = run_expression(instruction.target(), state)?.into_lvalue();
    let new_value = run_expression(instruction.value(), state)?.into_rvalue();
    *target.borrow_mut() = new_value;
    Ok(())
}

fn run_return(instruction: &ReturnInstruction, state: &mut State) -> RunResult<()> {
    let value = run_expression(instruction.expression(), state)?.into_rvalue();
    state.return_value = Some(value);
    Ok(())
}

fn run_eval(instruction: &EvalInstruction, state: &mut State) -> RunResult<()> {
    let _ = run_expression(instruction.expression(), state)?;
    Ok(())
}

fn run_expression(expression: &Expression, state: &mut State) -> RunResult<Value> {
    match &expression.kind {
        ExpressionKind::Variable(e) => run_variable_expr(e, state),
        ExpressionKind::Literal(e) => run_literal_expr(e, state),
        ExpressionKind::Address(e) => run_address_expr(e, state),
        ExpressionKind::Deref(e) => run_deref_expr(e, state),
        ExpressionKind::New(e) => run_new_expr(e, state),
        ExpressionKind::ArrayFromElements(e) => run_array_from_elements_expr(e, state),
        ExpressionKind::ArrayFromCopy(e) => run_array_from_copy_expr(e, state),
        ExpressionKind::Index(e) => run_index_expr(e, state),
        ExpressionKind::Call(e) => run_call_expr(e, state),
        ExpressionKind::FieldAccess(e) => run_field_access_expr(e, state),
        ExpressionKind::If(e) => run_if_expr(e, state),
        ExpressionKind::Block(e) => run_block_expr(e, state),
        ExpressionKind::Empty => Ok(Value::Rvalue(Rvalue::Void)),
        ExpressionKind::Error => panic_error(),
    }
}

fn run_variable_expr(expression: &VariableExpr, state: &State) -> RunResult<Value> {
    let variable_id = expression.variable().id;
    let result = state.get(variable_id).clone();
    Ok(result)
}

fn run_literal_expr(expression: &LiteralExpr, _: &State) -> RunResult<Value> {
    let value = match expression {
        LiteralExpr::Int(value) => Value::Rvalue(Rvalue::Int(*value)),
        LiteralExpr::Bool(value) => Value::Rvalue(Rvalue::Bool(*value)),
    };
    Ok(value)
}

fn run_address_expr(expression: &AddressExpr, state: &mut State) -> RunResult<Value> {
    let lvalue = run_expression(expression.target(), state)?.into_lvalue();
    Ok(Value::Rvalue(Rvalue::Pointer(Some(lvalue))))
}

fn run_deref_expr(expression: &DerefExpr, state: &mut State) -> RunResult<Value> {
    let lvalue = run_expression(expression.pointer(), state)?
        .into_rvalue()
        .into_pointer();
    let lvalue = match lvalue {
        Some(lvalue) => lvalue,
        None => {
            let error = "Attempted to dereference null pointer.";
            return Err(error.into());
        }
    };
    Ok(Value::Lvalue(lvalue))
}

fn run_new_expr(expression: &NewExpr, _: &mut State) -> RunResult<Value> {
    let target = default_value_for_type(&expression.target_type().borrow());
    Ok(Value::Rvalue(Rvalue::Pointer(Some(Lvalue::store(target)))))
}

fn run_array_from_elements_expr(
    expression: &ArrayFromElementsExpr,
    state: &mut State,
) -> RunResult<Value> {
    let elements: RunResult<Vec<Value>> = expression
        .elements()
        .map(|element| run_expression(element, state))
        .collect();
    let elements: Vec<Lvalue> = elements?
        .into_iter()
        .map(|value| Lvalue::store(value.into_rvalue()))
        .collect();

    Ok(Value::Rvalue(Rvalue::Array(Rc::new(RefCell::new(
        elements,
    )))))
}

fn run_array_from_copy_expr(expression: &ArrayFromCopyExpr, state: &mut State) -> RunResult<Value> {
    let element = run_expression(expression.element(), state)?.into_rvalue();
    let size = run_expression(expression.size(), state)?
        .into_rvalue()
        .as_int();

    if size <= 0 {
        let error = format!("attempted to create array of non-positive size: {}", size);
        return Err(error.into());
    }
    let size = size as usize;

    let array = vec![element; size];
    let array = array.into_iter().map(Lvalue::store).collect();

    Ok(Value::Rvalue(Rvalue::Array(Rc::new(RefCell::new(array)))))
}

fn run_index_expr(expression: &IndexExpr, state: &mut State) -> RunResult<Value> {
    let collection = run_expression(expression.collection(), state)?
        .into_rvalue()
        .into_array();
    let collection = collection.borrow();
    let index = run_expression(expression.index(), state)?
        .into_rvalue()
        .as_int();

    if index < 0 || index >= collection.len() as i32 {
        let error = format!(
            "array index out of bounds: array size is {}, index is {}",
            collection.len(),
            index
        );
        return Err(error.into());
    }

    Ok(Value::Lvalue(collection[index as usize].clone()))
}

fn run_call_expr(expression: &CallExpr, state: &mut State) -> RunResult<Value> {
    let function = expression.function();

    let arguments: RunResult<Vec<Value>> = expression
        .arguments()
        .map(|argument| run_expression(argument, state))
        .collect();
    let arguments = arguments?;

    match *function {
        Function::UserDefined(ref function) => {
            let parameters = function.parameters();

            for (parameter, value) in parameters.zip(arguments.into_iter()) {
                let variable_id = parameter.id;
                state.push(variable_id, value.into_rvalue())
            }

            let function_result = run_user_function(expression.function(), state);

            for parameter in function.parameters() {
                let variable_id = parameter.id;
                state.pop(variable_id)
            }

            function_result
        }
        Function::Internal(ref function) => run_internal_function(function, arguments),
    }
}

fn run_field_access_expr(expression: &FieldAccessExpr, state: &mut State) -> RunResult<Value> {
    let receiver = run_expression(expression.receiver(), state)?;
    let field_id = expression.field().id;
    let result = match receiver {
        Value::Lvalue(lvalue) => Value::Lvalue(lvalue.borrow().as_struct()[&field_id].clone()),
        Value::Rvalue(rvalue) => Value::Rvalue(rvalue.as_struct()[&field_id].detach()),
    };
    Ok(result)
}

fn run_if_expr(expression: &IfExpr, state: &mut State) -> RunResult<Value> {
    let cond = run_expression(expression.cond(), state)?
        .into_rvalue()
        .as_bool();
    if cond {
        run_expression(expression.then(), state)
    } else {
        run_expression(expression.else_(), state)
    }
}

fn run_block_expr(block: &BlockExpr, state: &mut State) -> RunResult<Value> {
    if !state.return_value.is_none() {
        panic!("Interpreter is in an invalid state: return value is not None before block.")
    }

    for instruction in block.instructions() {
        run_instruction(instruction, state)?;
    }

    let result = state.return_value.take().unwrap_or(Rvalue::Void);
    Ok(Value::Rvalue(result))
}

fn default_value_for_type(type_: &Type) -> Rvalue {
    match type_.type_id() {
        TypeId::Void => panic!("Tried to default-initialize a value of type `void`"),
        TypeId::Int => Rvalue::Int(0),
        TypeId::Bool => Rvalue::Bool(false),
        TypeId::TemplateInstance(TypeTemplateId::Array, _) => {
            Rvalue::Array(Rc::new(RefCell::new(vec![])))
        }
        TypeId::TemplateInstance(TypeTemplateId::Pointer, _) => Rvalue::Pointer(None),
        TypeId::Struct(_) => {
            let fields = type_
                .fields()
                .map(|field| {
                    let field = field.borrow();
                    let value = default_value_for_type(&field.type_());
                    (field.id, Lvalue::store(value))
                })
                .collect();
            Rvalue::Struct(fields)
        }
        TypeId::Error => panic_error(),
    }
}

fn panic_error() -> ! {
    panic!("Error-containing program passed to interpreter backend.")
}

fn panic_wrong_type(expected_type: &str, actual_type: &str) -> ! {
    panic!(
        "Runtime type mismatch: expected `{}`, got `{}`",
        expected_type, actual_type
    )
}
