use super::error_codes;
use super::Ast;
use super::Binary;
use super::Expr;
use super::Function as FunctionDecl;
use super::Literal;
use super::Logical;
use super::Stmt;
use super::Unary;

use std::cell::Cell;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::rc::Weak;

use flexi_parse::error::Error;
use flexi_parse::group::Delimiters as _;
use flexi_parse::group::Parentheses;
use flexi_parse::new_error;
use flexi_parse::token::Ident;
use flexi_parse::token::Token;
use flexi_parse::Punct;
use flexi_parse::Result;

mod natives {
    use super::Value;

    use std::time::SystemTime;

    use flexi_parse::Result;

    pub(super) fn clock(_: Vec<Value>) -> Result<Value> {
        Ok(Value::Number(
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        ))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Ordering {
    Less,
    Equal,
    Greater,
    None,
}

impl Ordering {
    fn gt(self) -> bool {
        self == Ordering::Greater
    }

    fn ge(self) -> bool {
        self == Ordering::Greater || self == Ordering::Equal
    }

    fn lt(self) -> bool {
        self == Ordering::Less
    }

    fn le(self) -> bool {
        self == Ordering::Less || self == Ordering::Equal
    }
}

impl From<cmp::Ordering> for Ordering {
    fn from(value: cmp::Ordering) -> Self {
        match value {
            cmp::Ordering::Less => Ordering::Less,
            cmp::Ordering::Equal => Ordering::Equal,
            cmp::Ordering::Greater => Ordering::Greater,
        }
    }
}

impl From<Option<cmp::Ordering>> for Ordering {
    fn from(value: Option<cmp::Ordering>) -> Self {
        if let Some(ordering) = value {
            ordering.into()
        } else {
            Ordering::None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NativeFunction {
    function: fn(Vec<Value>) -> Result<Value>,
    arity: usize,
    name: &'static str,
}

#[derive(Debug, Clone)]
struct Function {
    declaration: FunctionDecl,
    closure: State,
    is_initialiser: bool,
}

impl Function {
    fn new(declaration: FunctionDecl, closure: State, is_initialiser: bool) -> Function {
        Function {
            declaration,
            closure,
            is_initialiser,
        }
    }

    fn call(&self, arguments: Vec<Value>, parentheses: &Parentheses) -> Result<Value> {
        let state = self.closure.start_scope();
        if self.declaration.params.len() != arguments.len() {
            return Err(arity_error(
                parentheses,
                self.declaration.params.len(),
                arguments.len(),
            ));
        }
        for (name, arg) in self.declaration.params.iter().zip(arguments) {
            state
                .environment
                .borrow_mut()
                .define(name.string().to_owned(), arg);
        }
        for stmt in &self.declaration.body {
            if let Some(value) = stmt.execute(&state)? {
                return Ok(value);
            }
        }
        Ok(Value::Nil)
    }

    fn bind(&self, instance: &'static RefCell<Instance>) -> Function {
        let scope = self.closure.start_scope();
        scope
            .environment
            .borrow_mut()
            .define("this".to_string(), Value::Instance(instance));
        Function {
            declaration: self.declaration.clone(),
            closure: scope,
            is_initialiser: self.is_initialiser,
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.declaration == other.declaration
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Class {
    name: String,
    superclass: Option<Rc<Class>>,
    methods: HashMap<String, Function>,
}

impl Class {
    fn new(
        name: String,
        superclass: Option<Rc<Class>>,
        methods: HashMap<String, Function>,
    ) -> Class {
        Class {
            name,
            superclass,
            methods,
        }
    }

    fn find_method(&self, name: &str) -> Option<&Function> {
        if let Some(method) = self.methods.get(name) {
            Some(method)
        } else if let Some(superclass) = &self.superclass {
            superclass.find_method(name)
        } else {
            None
        }
    }

    fn arity(&self) -> usize {
        self.find_method("init")
            .map(|f| f.declaration.params.len())
            .unwrap_or(0)
    }
}

#[derive(Clone, PartialEq)]
struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

impl Instance {
    fn new(class: Rc<Class>) -> Instance {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }

    fn set(&mut self, name: &Ident, value: Value) {
        self.fields.insert(name.string().to_string(), value);
    }
}

fn get(instance: &'static RefCell<Instance>, name: &Ident) -> Result<Value> {
    let this = instance.borrow();
    if let Some(value) = this.fields.get(name.string()) {
        Ok(value.to_owned())
    } else if let Some(function) = this.class.find_method(name.string()) {
        Ok(Value::Function(function.bind(instance)))
    } else {
        Err(new_error(
            format!("Undefined property '{}'", name.string()),
            name,
            error_codes::UNDEFINED_NAME,
        ))
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instance")
            .field("class", &self.class.name)
            .field("fields", &self.fields)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Nil,
    String(String),
    Number(f64),
    Bool(bool),
    Native(NativeFunction),
    Function(Function),
    Class(Rc<Class>),
    Instance(&'static RefCell<Instance>),
}

fn arity_error(parentheses: &Parentheses, expected: usize, actual: usize) -> Error {
    new_error(
        format!("Expected {} arguments but got {}", expected, actual),
        parentheses.span().to_owned(),
        error_codes::INCORRECT_ARITY,
    )
}

impl Value {
    fn as_class(&self) -> Option<&Rc<Class>> {
        if let Value::Class(class) = self {
            Some(class)
        } else {
            None
        }
    }

    fn as_instance(&self) -> Option<&'static RefCell<Instance>> {
        if let Value::Instance(instance) = self {
            Some(*instance)
        } else {
            None
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }

    fn add(&self, op: &Punct!["+"], other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1.to_owned() + s2)),
            _ => Err(new_error(
                format!("Can't add '{}' to '{}'", self, other),
                op,
                error_codes::TYPE_ERROR,
            )),
        }
    }

    fn sub(&self, op: &Punct!["-"], other: &Value) -> Result<Value> {
        if let (Value::Number(n1), Value::Number(n2)) = (self, other) {
            Ok(Value::Number(n1 - n2))
        } else {
            Err(new_error(
                format!("Can't subtract '{}' from '{}'", other, self),
                op,
                error_codes::TYPE_ERROR,
            ))
        }
    }

    fn mul(&self, op: &Punct!["*"], other: &Value) -> Result<Value> {
        if let (Value::Number(n1), Value::Number(n2)) = (self, other) {
            Ok(Value::Number(n1 * n2))
        } else {
            Err(new_error(
                format!("Can't multiply '{}' by '{}'", self, other),
                op,
                error_codes::TYPE_ERROR,
            ))
        }
    }

    fn div(&self, op: &Punct!["/"], other: &Value) -> Result<Value> {
        if let (Value::Number(n1), Value::Number(n2)) = (self, other) {
            Ok(Value::Number(n1 / n2))
        } else {
            Err(new_error(
                format!("Can't divide '{}' by '{}'", other, self),
                op,
                error_codes::TYPE_ERROR,
            ))
        }
    }

    fn neg(&self, op: &Punct!["-"]) -> Result<Value> {
        if let Value::Number(n) = self {
            Ok(Value::Number(-n))
        } else {
            Err(new_error(
                format!("Can't negate '{}'", self),
                op,
                error_codes::TYPE_ERROR,
            ))
        }
    }

    fn cmp<T: Token>(&self, op: &T, other: &Value) -> Result<Ordering> {
        if self == other {
            return Ok(Ordering::Equal);
        }

        match (self, other) {
            (Value::String(s1), Value::String(s2)) => Ok(s1.cmp(s2).into()),
            (Value::Number(n1), Value::Number(n2)) => Ok(n1.partial_cmp(n2).into()),
            (Value::Bool(b1), Value::Bool(b2)) => Ok(b1.cmp(b2).into()),
            _ => Err(new_error(
                format!("Can't compare '{}' with '{}'", self, other),
                op,
                error_codes::TYPE_ERROR,
            )),
        }
    }

    fn call(
        &self,
        state: &State,
        arguments: Vec<Value>,
        parentheses: &Parentheses,
    ) -> Result<Value> {
        match self {
            Value::Native(NativeFunction {
                function, arity, ..
            }) => {
                if *arity != arguments.len() {
                    return Err(arity_error(parentheses, *arity, arguments.len()));
                }
                function(arguments)
            }
            Value::Function(function) => function.call(arguments, parentheses),
            Value::Class(class) => {
                if class.arity() != arguments.len() {
                    return Err(arity_error(parentheses, class.arity(), arguments.len()));
                }

                let instance = state.allocate(Instance::new(Rc::clone(class)));
                if let Some(initialiser) = class.find_method("init") {
                    initialiser.bind(instance).call(arguments, parentheses)?;
                }
                Ok(Value::Instance(instance))
            }
            Value::Nil
            | Value::String(_)
            | Value::Number(_)
            | Value::Bool(_)
            | Value::Instance(_) => Err(new_error(
                format!("Can't call '{}'", self),
                parentheses.span().clone(),
                error_codes::TYPE_ERROR,
            )),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::String(s) => f.write_str(s),
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Native(NativeFunction { name, .. }) => write!(f, "<native fn '{name}'>"),
            Value::Function(Function {
                declaration: FunctionDecl { name, .. },
                ..
            }) => write!(f, "<fn {}>", name.string()),
            Value::Class(class) => f.write_str(&class.name),
            Value::Instance(instance) => write!(f, "'{}' instance", instance.borrow().class.name),
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
}

impl State {
    fn new() -> Self {
        let mut values = HashMap::new();

        values.insert(
            "clock".to_string(),
            Value::Native(NativeFunction {
                function: natives::clock,
                arity: 0,
                name: "clock",
            }),
        );

        let environment = Rc::new(RefCell::new(Environment {
            values,
            enclosing: None,
            sub: vec![],
            mark: Cell::new(false),
        }));

        State {
            environment: Rc::clone(&environment),
            globals: environment,
        }
    }

    #[must_use]
    fn start_scope(&self) -> State {
        let environment = Rc::new(RefCell::new(Environment {
            values: HashMap::new(),
            enclosing: Some(Rc::clone(&self.environment)),
            sub: vec![],
            mark: Cell::new(false),
        }));
        self.environment
            .borrow_mut()
            .sub
            .push(Rc::downgrade(&environment));
        State {
            environment,
            globals: Rc::clone(&self.globals),
        }
    }

    fn super_scope(self) -> Option<State> {
        Some(State {
            environment: Rc::clone(self.environment.borrow().enclosing.as_ref()?),
            globals: self.globals,
        })
    }

    fn allocate<T>(&self, value: T) -> &'static RefCell<T> {
        Box::leak(Box::new(RefCell::new(value)))
    }
}

struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
    sub: Vec<Weak<RefCell<Environment>>>,
    mark: Cell<bool>,
}

impl Environment {
    fn get(&self, name: &Ident) -> Result<Value> {
        if let Some(value) = self.values.get(name.string()) {
            Ok(value.to_owned())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(new_error(
                format!("Undefined variable '{}'", name.string()),
                name,
                error_codes::UNDEFINED_NAME,
            ))
        }
    }

    fn get_at(&self, name: &Ident, distance: usize) -> Result<Value> {
        if distance == 0 {
            self.get(name)
        } else {
            self.enclosing
                .as_ref()
                .unwrap()
                .borrow()
                .get_at(name, distance - 1)
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, name: &Ident, value: Value) -> Result<()> {
        if self.values.contains_key(name.string()) {
            self.values.insert(name.string().clone(), value);
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(new_error(
                format!("Undefined variable '{}'", name.string()),
                name,
                error_codes::UNDEFINED_NAME,
            ))
        }
    }

    fn assign_at(&mut self, name: &Ident, value: Value, distance: usize) -> Result<()> {
        if distance == 0 {
            self.assign(name, value)
        } else {
            self.enclosing
                .as_ref()
                .unwrap()
                .borrow_mut()
                .assign_at(name, value, distance - 1)
        }
    }
}

struct EnvValuesDebug<'a>(&'a HashMap<String, Value>);

impl fmt::Debug for EnvValuesDebug<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.0.iter().map(|(k, v)| (k, v.to_string())))
            .finish()
    }
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Environment")
            .field("values", &EnvValuesDebug(&self.values))
            .field("enclosing", &self.enclosing)
            .field("sub", &self.sub)
            .field("mark", &self.mark)
            .finish()
    }
}

impl Binary {
    fn evaluate(&self, state: &State) -> Result<Value> {
        match self {
            Binary::Mul(left, op, right) => left.evaluate(state)?.mul(op, &right.evaluate(state)?),
            Binary::Div(left, op, right) => left.evaluate(state)?.div(op, &right.evaluate(state)?),
            Binary::Add(left, op, right) => left.evaluate(state)?.add(op, &right.evaluate(state)?),
            Binary::Sub(left, op, right) => left.evaluate(state)?.sub(op, &right.evaluate(state)?),
            Binary::Equal(left, _, right) => {
                Ok(Value::Bool(left.evaluate(state)? == right.evaluate(state)?))
            }
            Binary::NotEqual(left, _, right) => {
                Ok(Value::Bool(left.evaluate(state)? != right.evaluate(state)?))
            }
            Binary::Greater(left, op, right) => Ok(Value::Bool(
                left.evaluate(state)?.cmp(op, &right.evaluate(state)?)?.gt(),
            )),
            Binary::GreaterEqual(left, op, right) => Ok(Value::Bool(
                left.evaluate(state)?.cmp(op, &right.evaluate(state)?)?.ge(),
            )),
            Binary::Less(left, op, right) => Ok(Value::Bool(
                left.evaluate(state)?.cmp(op, &right.evaluate(state)?)?.lt(),
            )),
            Binary::LessEqual(left, op, right) => Ok(Value::Bool(
                left.evaluate(state)?.cmp(op, &right.evaluate(state)?)?.le(),
            )),
        }
    }
}

impl Literal {
    fn evaluate(&self) -> Value {
        match self {
            Literal::False(_) => Value::Bool(false),
            Literal::Float(value) => Value::Number(value.value()),
            Literal::Int(value) => Value::Number(value.value() as f64),
            Literal::Nil(_) => Value::Nil,
            Literal::String(string) => Value::String(string.string().clone()),
            Literal::True(_) => Value::Bool(true),
        }
    }
}

impl Logical {
    fn evaluate(&self, state: &State) -> Result<Value> {
        match self {
            Logical::And(left, _, right) => {
                let left = left.evaluate(state)?;
                if left.is_truthy() {
                    Ok(left)
                } else {
                    right.evaluate(state)
                }
            }
            Logical::Or(left, _, right) => {
                let left = left.evaluate(state)?;
                if !left.is_truthy() {
                    Ok(left)
                } else {
                    right.evaluate(state)
                }
            }
        }
    }
}

impl Unary {
    fn evaluate(&self, state: &State) -> Result<Value> {
        match self {
            Unary::Neg(op, expr) => expr.evaluate(state)?.neg(op),
            Unary::Not(_, expr) => Ok(Value::Bool(!expr.evaluate(state)?.is_truthy())),
        }
    }
}

impl Expr {
    fn evaluate(&self, state: &State) -> Result<Value> {
        match self {
            Expr::Assign {
                name,
                value,
                distance,
            } => {
                let value = value.evaluate(state)?;
                if let Some(distance) = distance.get() {
                    state
                        .environment
                        .borrow_mut()
                        .assign_at(name, value.clone(), distance)?;
                } else {
                    state.globals.borrow_mut().assign(name, value.clone())?;
                }
                Ok(value)
            }
            Expr::Binary(binary) => binary.evaluate(state),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = callee.evaluate(state)?;
                let mut evaluated_args = Vec::with_capacity(arguments.len());
                for arg in arguments {
                    evaluated_args.push(arg.evaluate(state)?);
                }
                callee.call(state, evaluated_args, paren)
            }
            Expr::Get { object, name } => {
                if let Value::Instance(instance) = object.evaluate(state)? {
                    get(instance, name)
                } else {
                    Err(new_error(
                        "Only instances have properties".to_string(),
                        name,
                        error_codes::TYPE_ERROR,
                    ))
                }
            }
            Expr::Group(expr) => expr.evaluate(state),
            Expr::Literal(literal) => Ok(literal.evaluate()),
            Expr::Logical(logical) => logical.evaluate(state),
            Expr::Set {
                object,
                name,
                value,
            } => {
                if let Value::Instance(instance) = object.evaluate(state)? {
                    let value = value.evaluate(state)?;
                    instance.borrow_mut().set(name, value.clone());
                    Ok(value)
                } else {
                    Err(new_error(
                        "Only instances have fields".to_string(),
                        name,
                        error_codes::TYPE_ERROR,
                    ))
                }
            }
            Expr::Super {
                keyword,
                distance,
                dot: _,
                method,
            } => {
                let superclass = state
                    .environment
                    .borrow()
                    .get_at(keyword.ident(), distance.clone().into_inner().unwrap())?;

                let object = state.environment.borrow().get_at(
                    &Ident::new("this".to_string(), keyword.span().clone()),
                    distance.clone().into_inner().unwrap() - 1,
                )?;

                if let Some(method) = superclass.as_class().unwrap().find_method(method.string()) {
                    Ok(Value::Function(method.bind(object.as_instance().unwrap())))
                } else {
                    Err(new_error(
                        format!("Undefined property '{}'", method.string()),
                        method,
                        error_codes::UNDEFINED_NAME,
                    ))
                }
            }
            Expr::This { keyword, distance } => {
                if let Some(distance) = distance.get() {
                    state.environment.borrow().get_at(keyword.ident(), distance)
                } else {
                    state.globals.borrow().get(keyword.ident())
                }
            }
            Expr::Unary(unary) => unary.evaluate(state),
            Expr::Variable { name, distance } => {
                if let Some(distance) = distance.get() {
                    state.environment.borrow().get_at(name, distance)
                } else {
                    state.globals.borrow().get(name)
                }
            }
        }
    }
}

macro_rules! propagate_return {
    ( $expr:expr ) => {
        if let Some(_tmp) = $expr {
            return Ok(Some(_tmp));
        }
    };
}

impl Stmt {
    fn execute(&self, state: &State) -> Result<Option<Value>> {
        match self {
            Stmt::Block(stmts) => {
                let inner_state = state.start_scope();
                for stmt in stmts {
                    propagate_return!(stmt.execute(&inner_state)?);
                }
            }
            Stmt::Class {
                name,
                superclass,
                superclass_distance,
                methods,
            } => {
                let superclass = if let Some(superclass_name) = superclass {
                    let superclass = Expr::Variable {
                        name: superclass_name.clone(),
                        distance: superclass_distance.clone(),
                    }
                    .evaluate(state)?;
                    if let Value::Class(superclass) = superclass {
                        Some(superclass)
                    } else {
                        return Err(new_error(
                            "Superclass must be a class".to_string(),
                            superclass_name,
                            error_codes::INHERIT_FROM_VALUE,
                        ));
                    }
                } else {
                    None
                };

                state
                    .environment
                    .borrow_mut()
                    .define(name.string().to_owned(), Value::Nil);

                let superclass_is_some = superclass.is_some();
                let mut state = if let Some(superclass) = &superclass {
                    let state = state.start_scope();
                    state
                        .environment
                        .borrow_mut()
                        .define("super".to_string(), Value::Class(Rc::clone(superclass)));
                    state
                } else {
                    state.clone()
                };

                let methods = methods
                    .iter()
                    .map(|declaration| {
                        (
                            declaration.name.string().to_owned(),
                            Function::new(
                                declaration.clone(),
                                state.clone(),
                                declaration.name.string() == "init",
                            ),
                        )
                    })
                    .collect();
                let class = Class::new(name.string().to_owned(), superclass, methods);

                if superclass_is_some {
                    state = state.super_scope().unwrap();
                }

                state
                    .environment
                    .borrow_mut()
                    .assign(name, Value::Class(Rc::new(class)))?;
            }
            Stmt::Expr(expr) => {
                expr.evaluate(state)?;
            }
            Stmt::Function(function) => {
                let value =
                    Value::Function(Function::new(function.to_owned(), state.clone(), false));
                state
                    .environment
                    .borrow_mut()
                    .define(function.name.string().to_owned(), value);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if condition.evaluate(state)?.is_truthy() {
                    propagate_return!(then_branch.execute(state)?);
                } else if let Some(else_branch) = else_branch {
                    propagate_return!(else_branch.execute(state)?);
                }
            }
            Stmt::Print(expr) => {
                let value = expr.evaluate(state)?;
                println!("{}", value);
            }
            Stmt::Return { keyword: _, value } => {
                return Ok(Some(
                    value
                        .as_ref()
                        .map(|v| v.evaluate(state))
                        .unwrap_or(Ok(Value::Nil))?,
                ));
            }
            Stmt::Variable { name, initialiser } => {
                let value = if let Some(initialiser) = initialiser {
                    initialiser.evaluate(state)?
                } else {
                    Value::Nil
                };
                state
                    .environment
                    .borrow_mut()
                    .define(name.string().to_owned(), value);
            }
            Stmt::While { condition, body } => {
                while condition.evaluate(state)?.is_truthy() {
                    propagate_return!(body.execute(state)?);
                }
            }
        }

        Ok(None)
    }
}

pub(super) fn interpret(ast: Ast) -> Result<()> {
    let state = State::new();
    for stmt in ast.0 {
        stmt.execute(&state)?;
    }
    Ok(())
}
