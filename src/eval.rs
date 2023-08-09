use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    io::{stderr, stdin, stdout, BufRead, BufReader, Read, Write},
    rc::Rc,
};

use crate::{error::EvalError, parser::Ast};

// TODO: Find out how to increase this limit, since the stack of the main thread can overflow if
// this is too large.
const CALL_STACK_SIZE_LIMIT: usize = 100;

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Return(Value),
    Continue,
    Break,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    String,
    Function,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Number => write!(f, "number"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            Function => write!(f, "function"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(String),
    Function(Rc<Function>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Number(n) => {
                if n.is_nan() {
                    write!(f, "nan")
                } else {
                    write!(f, "{}", n)
                }
            }
            Bool(b) => write!(f, "{}", b),
            String(s) => write!(f, r#""{}""#, s),
            Function(func) => write!(f, "{:?}", func),
        }
    }
}

impl Value {
    fn get_type(&self) -> Type {
        use Value::*;
        match self {
            Number(..) => Type::Number,
            Bool(..) => Type::Bool,
            String(..) => Type::String,
            Function(..) => Type::Function,
        }
    }

    fn assert_type(&self, expected: Type) -> Result<(), EvalError> {
        let got = self.get_type();
        if got != expected {
            return Err(EvalError::WrongType { expected, got });
        }
        Ok(())
    }

    fn convert_to_string(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            other => other.to_string(),
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            Value::Number(n) => *n != 0.0,
            Value::Bool(b) => *b,
            Value::String(s) => !s.is_empty(),
            Value::Function(..) => true,
        }
    }

    fn call(&self, ctx: &mut Context, args: Vec<Value>) -> Result<Value, EvalError> {
        match self {
            Value::Function(func) => {
                let maybe_expected_args = func.get_arg_count();
                let got_args = args.len();
                if let Some(expected_args) = maybe_expected_args {
                    if got_args != expected_args {
                        return Err(EvalError::FunctionWrongArgAmount {
                            name: func.get_name(),
                            expected: expected_args,
                            got: got_args,
                        });
                    }
                }
                func.call(ctx, &args)
            }
            other => {
                let error = format!("Cannot call value of type {}", other.get_type());
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn add(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l + r)),
            (String(l), String(r)) => Ok(String(l + &r)),
            (l, r) => {
                let error = format!(
                    "Cannot add value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn subtract(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l - r)),
            (l, r) => {
                let error = format!(
                    "Cannot subtract value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn multiply(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l * r)),
            (l, r) => {
                let error = format!(
                    "Cannot multiply value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn divide(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l / r)),
            (l, r) => {
                let error = format!(
                    "Cannot divide value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn modulo(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l % r)),
            (l, r) => {
                let error = format!(
                    "Cannot modulo value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn power(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l.powf(r))),
            (l, r) => {
                let error = format!(
                    "Cannot raise power of value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn negate(self) -> Result<Value, EvalError> {
        use Value::*;
        match self {
            Number(l) => Ok(Number(-l)),
            _ => {
                let error = format!("Cannot negate value of type {}", self.get_type());
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn bool_negate(self) -> Result<Value, EvalError> {
        Ok(Value::Bool(!self.as_bool()))
    }

    fn equal(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        let eq = match (self, rhs) {
            (Number(l), Number(r)) => l == r,
            (Bool(l), Bool(r)) => l == r,
            (String(l), String(r)) => l == r,
            _ => false,
        };
        Ok(Bool(eq))
    }

    fn unequal(self, rhs: Self) -> Result<Value, EvalError> {
        let Value::Bool(eq) = self.equal(rhs)? else {
            unreachable!()
        };
        Ok(Value::Bool(!eq))
    }

    fn less_than(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        let lt = match (self, rhs) {
            (Number(l), Number(r)) => l < r,
            (String(l), String(r)) => l < r,
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                return Err(EvalError::TypeError(error));
            }
        };
        Ok(Bool(lt))
    }

    fn greater_than(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        let gt = match (self, rhs) {
            (Number(l), Number(r)) => l > r,
            (String(l), String(r)) => l > r,
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                return Err(EvalError::TypeError(error));
            }
        };
        Ok(Bool(gt))
    }

    fn less_than_or_equal(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        let le = match (self, rhs) {
            (Number(l), Number(r)) => l <= r,
            (String(l), String(r)) => l <= r,
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                return Err(EvalError::TypeError(error));
            }
        };
        Ok(Bool(le))
    }

    fn greater_than_or_equal(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        let ge = match (self, rhs) {
            (Number(l), Number(r)) => l >= r,
            (String(l), String(r)) => l >= r,
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                return Err(EvalError::TypeError(error));
            }
        };
        Ok(Bool(ge))
    }

    fn and(
        self,
        rhs_evaluator: impl FnOnce() -> Result<Self, EvalError>,
    ) -> Result<Value, EvalError> {
        let result = self.as_bool() && rhs_evaluator()?.as_bool();
        Ok(Value::Bool(result))
    }

    fn or(
        self,
        rhs_evaluator: impl FnOnce() -> Result<Self, EvalError>,
    ) -> Result<Value, EvalError> {
        let result = self.as_bool() || rhs_evaluator()?.as_bool();
        Ok(Value::Bool(result))
    }
}

const NULL_VALUE: Value = Value::Number(0.0);

type BuiltinFunctionClosure = Box<dyn Fn(&mut Context, &[Value]) -> Result<Value, EvalError>>;

pub enum Function {
    Builtin {
        name: String,
        n_args: Option<usize>,
        func: BuiltinFunctionClosure,
    },
    UserDefined {
        name: Option<String>,
        arg_names: Vec<String>,
        body: Ast,
    },
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Builtin { name, .. } => {
                write!(f, "built-in function '{}'", name)
            }
            Function::UserDefined { name, .. } => {
                if let Some(name) = name {
                    write!(f, "function '{}'", name)
                } else {
                    write!(f, "unnamed function")
                }
            }
        }
    }
}

impl Function {
    pub fn new_builtin<F>(name: impl Into<String>, n_args: Option<usize>, func: F) -> Self
    where
        F: Fn(&mut Context, &[Value]) -> Result<Value, EvalError> + 'static,
    {
        Self::Builtin {
            name: name.into(),
            n_args,
            func: Box::new(func),
        }
    }

    pub fn new_user_defined(
        func_name: Option<&str>,
        arg_names: Vec<String>,
        body: Ast,
    ) -> Result<Self, EvalError> {
        let func_name = func_name.map(|name| name.to_string());

        let mut arg_set = HashSet::new();
        for name in &arg_names {
            if !arg_set.insert(name) {
                return Err(EvalError::DuplicateArgName {
                    func_name,
                    arg_name: name.clone(),
                });
            }
        }
        Ok(Self::UserDefined {
            name: func_name,
            arg_names,
            body,
        })
    }

    pub fn get_name(&self) -> Option<String> {
        match self {
            Function::Builtin { name, .. } => Some(name.clone()),
            Function::UserDefined { name, .. } => name.clone(),
        }
    }

    pub fn call(&self, ctx: &mut Context, args: &[Value]) -> Result<Value, EvalError> {
        match self {
            Function::Builtin { func, .. } => func(ctx, args),
            Function::UserDefined {
                arg_names, body, ..
            } => {
                if ctx.call_stack.len() >= CALL_STACK_SIZE_LIMIT - 1 {
                    return Err(EvalError::CallStackOverflow);
                }

                let mut scope = Scope::new();
                for (name, value) in arg_names.iter().zip(args.iter()) {
                    scope.set_var(name, value.clone());
                }

                if let Some(function_scope) = ctx.function_scope.take() {
                    ctx.call_stack.push(function_scope);
                }
                ctx.function_scope = Some(scope);

                let call_result = match evaluate(body, ctx) {
                    Err(EvalError::InternalControlFlow(ControlFlow::Return(val))) => Ok(val),
                    Err(EvalError::InternalControlFlow(ControlFlow::Continue)) => {
                        Err(EvalError::ContinueOutsideOfLoop)
                    }
                    Err(EvalError::InternalControlFlow(ControlFlow::Break)) => {
                        Err(EvalError::BreakOutsideOfLoop)
                    }
                    other => other,
                };
                ctx.function_scope = ctx.call_stack.pop();
                call_result
            }
        }
    }

    fn get_arg_count(&self) -> Option<usize> {
        match self {
            Function::Builtin { n_args, .. } => *n_args,
            Function::UserDefined { arg_names, .. } => Some(arg_names.len()),
        }
    }
}

struct Scope {
    variables: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.variables.get(name).cloned()
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Value) {
        self.variables.insert(name.into(), val);
    }
}

pub struct ContextBuilder {
    stdin: Box<dyn Read>,
    stdout: Box<dyn Write>,
    stderr: Box<dyn Write>,
    standard_variables: bool,
    standard_functions: bool,
}

impl Default for ContextBuilder {
    fn default() -> Self {
        Self {
            stdin: Box::new(stdin()),
            stdout: Box::new(stdout()),
            stderr: Box::new(stderr()),
            standard_variables: true,
            standard_functions: true,
        }
    }
}

impl ContextBuilder {
    pub fn build(self) -> Context {
        let mut ctx = Context {
            global_scope: Scope::new(),
            function_scope: None,
            call_stack: Vec::new(),
            stdin: BufReader::new(self.stdin),
            stdout: self.stdout,
            stderr: self.stderr,
            _internal_side_effect_flag: false,
        };

        if self.standard_variables {
            ctx.add_standard_variables();
        }
        if self.standard_functions {
            ctx.add_standard_functions()
                .expect("Failed to add standard functions to context");
        }

        ctx
    }

    // TODO: Remove `#[allow(unused)]` once this is used outside of tests
    #[allow(unused)]
    pub fn stdin(mut self, stdin: impl Read + 'static) -> Self {
        self.stdin = Box::new(stdin);
        self
    }

    // TODO: Remove `#[allow(unused)]` once this is used outside of tests
    #[allow(unused)]
    pub fn stdout(mut self, stdout: impl Write + 'static) -> Self {
        self.stdout = Box::new(stdout);
        self
    }

    // TODO: Remove `#[allow(unused)]` once this is used outside of tests
    #[allow(unused)]
    pub fn stderr(mut self, stderr: impl Write + 'static) -> Self {
        self.stderr = Box::new(stderr);
        self
    }

    // TODO: Remove `#[allow(unused)]` once this is used outside of tests
    #[allow(unused)]
    pub fn standard_variables(mut self, b: bool) -> Self {
        self.standard_variables = b;
        self
    }

    // TODO: Remove `#[allow(unused)]` once this is used outside of tests
    #[allow(unused)]
    pub fn standard_functions(mut self, b: bool) -> Self {
        self.standard_functions = b;
        self
    }
}

pub struct Context {
    global_scope: Scope,
    function_scope: Option<Scope>,
    call_stack: Vec<Scope>,
    stdin: BufReader<Box<dyn Read>>,
    stdout: Box<dyn Write>,
    stderr: Box<dyn Write>,
    /// This flag is used during tests until observable side effects apart from writing to stdout
    /// are introduced.
    // TODO: Remove this when some form of obvservable side effects is implemented
    pub _internal_side_effect_flag: bool,
}

impl Default for Context {
    fn default() -> Self {
        Self::builder().build()
    }
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn builder() -> ContextBuilder {
        ContextBuilder::default()
    }

    fn add_standard_variables(&mut self) {
        use std::f64::consts::{E, PI};

        // TODO: Scope constants to a separate namespace like `math`, so they can be used via `math.pi`
        self.set_var("pi", Value::Number(PI));
        self.set_var("e", Value::Number(E));
        self.set_var("nan", Value::Number(f64::NAN));
        self.set_var("inf", Value::Number(f64::INFINITY));
    }

    fn add_standard_functions(&mut self) -> Result<(), EvalError> {
        self.add_builtin_function("_set_internal_side_effect_flag", Some(0), |ctx, _args| {
            ctx._internal_side_effect_flag = true;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("print", None, |ctx, args| {
            print_values(&mut ctx.stdout, args)?;
            ctx.stdout.flush()?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("println", None, |ctx, args| {
            print_values(&mut ctx.stdout, args)?;
            writeln!(ctx.stdout, "")?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("eprint", None, |ctx, args| {
            print_values(&mut ctx.stderr, args)?;
            ctx.stderr.flush()?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("eprintln", None, |ctx, args| {
            print_values(&mut ctx.stderr, args)?;
            writeln!(ctx.stderr, "")?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("read_line", Some(0), |ctx, _args| {
            let mut str = String::new();
            ctx.stdin.read_line(&mut str)?;
            str.pop();
            Ok(Value::String(str))
        });

        self.add_builtin_function("to_string", Some(1), |_ctx, args| {
            Ok(Value::String(args[0].convert_to_string()))
        });

        self.add_builtin_function("parse_number", Some(1), |_ctx, args| {
            args[0].assert_type(Type::String)?;
            let Value::String(ref arg) = args[0] else {
                unreachable!()
            };
            let num = arg.parse().unwrap_or(f64::NAN);
            Ok(Value::Number(num))
        });

        // TODO: Scope functions to a separate namespace like `math`, so they can be used via
        // `math.is_nan(42)`
        self.add_builtin_function("is_nan", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Bool(arg.is_nan()))
        });

        self.add_builtin_function("is_infinite", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Bool(arg.is_infinite()))
        });

        self.add_builtin_function("is_finite", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Bool(arg.is_finite()))
        });

        self.add_builtin_function("sin", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.sin()))
        });
        self.add_builtin_function("cos", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.cos()))
        });
        self.add_builtin_function("tan", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.tan()))
        });
        self.add_builtin_function("asin", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.asin()))
        });
        self.add_builtin_function("acos", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.acos()))
        });
        self.add_builtin_function("atan", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.atan()))
        });
        self.add_builtin_function("atan2", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            args[1].assert_type(Type::Number)?;
            let Value::Number(arg1) = args[0] else {
                unreachable!()
            };
            let Value::Number(arg2) = args[1] else {
                unreachable!()
            };
            Ok(Value::Number(arg1.atan2(arg2)))
        });
        self.add_builtin_function("tanh", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.tanh()))
        });
        self.add_builtin_function("sinh", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.sinh()))
        });
        self.add_builtin_function("cosh", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.cosh()))
        });

        self.add_builtin_function("ln", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.ln()))
        });
        self.add_builtin_function("log2", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.log2()))
        });
        self.add_builtin_function("log10", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.log10()))
        });
        self.add_builtin_function("log", Some(2), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            args[1].assert_type(Type::Number)?;
            let Value::Number(arg1) = args[0] else {
                unreachable!()
            };
            let Value::Number(arg2) = args[1] else {
                unreachable!()
            };
            Ok(Value::Number(arg1.log(arg2)))
        });

        self.add_builtin_function("abs", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.abs()))
        });
        self.add_builtin_function("min", Some(2), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            args[1].assert_type(Type::Number)?;
            let Value::Number(arg1) = args[0] else {
                unreachable!()
            };
            let Value::Number(arg2) = args[1] else {
                unreachable!()
            };
            Ok(Value::Number(arg1.min(arg2)))
        });
        self.add_builtin_function("max", Some(2), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            args[1].assert_type(Type::Number)?;
            let Value::Number(arg1) = args[0] else {
                unreachable!()
            };
            let Value::Number(arg2) = args[1] else {
                unreachable!()
            };
            Ok(Value::Number(arg1.max(arg2)))
        });
        self.add_builtin_function("floor", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.floor()))
        });
        self.add_builtin_function("ceil", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.ceil()))
        });
        self.add_builtin_function("round", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.round()))
        });

        self.add_builtin_function("sqrt", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.sqrt()))
        });
        self.add_builtin_function("exp", Some(1), |_ctx, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.exp()))
        });

        self.add_builtin_function("inspect", Some(1), |_ctx, args| {
            println!("{}", args[0]);
            Ok(args[0].clone())
        });

        Ok(())
    }

    pub fn add_builtin_function<F>(
        &mut self,
        name: impl Into<String> + Clone,
        n_args: Option<usize>,
        func: F,
    ) where
        F: Fn(&mut Context, &[Value]) -> Result<Value, EvalError> + 'static,
    {
        self.set_var(
            name.clone(),
            Value::Function(Rc::new(Function::new_builtin(name, n_args, func))),
        )
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.function_scope
            .as_ref()
            .and_then(|s| s.get_var(name))
            .or_else(|| self.global_scope.get_var(name))
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Value) {
        let scope = self
            .function_scope
            .as_mut()
            .unwrap_or(&mut self.global_scope);
        scope.set_var(name, val);
    }
}

pub fn evaluate(ast: &Ast, ctx: &mut Context) -> Result<Value, EvalError> {
    let result = match ast {
        Ast::FunctionDefinition {
            name,
            arg_names,
            body,
        } => {
            let func =
                Function::new_user_defined(Some(name.as_str()), arg_names.clone(), *body.clone())?;
            ctx.set_var(name, Value::Function(Rc::new(func)));
            NULL_VALUE
        }
        Ast::UnnamedFunction { arg_names, body } => {
            let func = Function::new_user_defined(None, arg_names.clone(), *body.clone())?;
            Value::Function(Rc::new(func))
        }
        Ast::Lines(lines) => {
            let mut result = NULL_VALUE;
            for line in lines {
                result = evaluate(line, ctx)?;
            }
            result
        }
        Ast::NumberLiteral(n) => Value::Number(*n),
        Ast::BooleanLiteral(b) => Value::Bool(*b),
        Ast::StringLiteral(s) => Value::String(s.clone()),
        Ast::Variable(name) => ctx
            .get_var(name)
            .ok_or_else(|| EvalError::VariableNotDefined(name.clone()))?,
        Ast::Add(lhs, rhs) => evaluate(lhs, ctx)?.add(evaluate(rhs, ctx)?)?,
        Ast::Subtract(lhs, rhs) => evaluate(lhs, ctx)?.subtract(evaluate(rhs, ctx)?)?,
        Ast::Multiply(lhs, rhs) => evaluate(lhs, ctx)?.multiply(evaluate(rhs, ctx)?)?,
        Ast::Divide(lhs, rhs) => evaluate(lhs, ctx)?.divide(evaluate(rhs, ctx)?)?,
        Ast::Modulo(lhs, rhs) => evaluate(lhs, ctx)?.modulo(evaluate(rhs, ctx)?)?,
        Ast::Power(lhs, rhs) => evaluate(lhs, ctx)?.power(evaluate(rhs, ctx)?)?,
        Ast::UnaryMinus(rhs) => evaluate(rhs, ctx)?.negate()?,
        Ast::BooleanNegate(rhs) => evaluate(rhs, ctx)?.bool_negate()?,
        Ast::Equality(lhs, rhs) => evaluate(lhs, ctx)?.equal(evaluate(rhs, ctx)?)?,
        Ast::Inequality(lhs, rhs) => evaluate(lhs, ctx)?.unequal(evaluate(rhs, ctx)?)?,
        Ast::LessThan(lhs, rhs) => evaluate(lhs, ctx)?.less_than(evaluate(rhs, ctx)?)?,
        Ast::GreaterThan(lhs, rhs) => evaluate(lhs, ctx)?.greater_than(evaluate(rhs, ctx)?)?,
        Ast::LessThanOrEqual(lhs, rhs) => {
            evaluate(lhs, ctx)?.less_than_or_equal(evaluate(rhs, ctx)?)?
        }
        Ast::GreaterThanOrEqual(lhs, rhs) => {
            evaluate(lhs, ctx)?.greater_than_or_equal(evaluate(rhs, ctx)?)?
        }
        Ast::And(lhs, rhs) => evaluate(lhs, ctx)?.and(|| evaluate(rhs, ctx))?,
        Ast::Or(lhs, rhs) => evaluate(lhs, ctx)?.or(|| evaluate(rhs, ctx))?,
        Ast::Brackets(inner) => evaluate(inner, ctx)?,
        Ast::Assign(name, rhs) => {
            let rval = evaluate(rhs, ctx)?;
            ctx.set_var(name, rval.clone());
            rval
        }
        Ast::FunctionCall { value, args } => {
            let val = evaluate(value, ctx)?;
            let args: Vec<_> = args
                .iter()
                .map(|ast| evaluate(ast, ctx))
                .collect::<Result<_, _>>()?;
            val.call(ctx, args)?
        }
        Ast::IfStatement {
            condition,
            if_body,
            else_body,
        } => {
            let condition = evaluate(condition, ctx)?;
            if condition.as_bool() {
                evaluate(if_body, ctx)?;
            } else if let Some(else_body) = else_body {
                evaluate(else_body, ctx)?;
            }
            NULL_VALUE
        }
        Ast::WhileLoop { condition, body } => {
            while evaluate(condition, ctx)?.as_bool() {
                match evaluate(body, ctx) {
                    Err(EvalError::InternalControlFlow(ControlFlow::Continue)) => continue,
                    Err(EvalError::InternalControlFlow(ControlFlow::Break)) => break,
                    e @ Err(_) => return e,
                    Ok(_) => (),
                }
            }
            NULL_VALUE
        }
        Ast::Continue => {
            return Err(EvalError::InternalControlFlow(ControlFlow::Continue));
        }
        Ast::Break => {
            return Err(EvalError::InternalControlFlow(ControlFlow::Break));
        }
        Ast::Return(expr) => {
            let val = match expr {
                None => NULL_VALUE,
                Some(expr) => evaluate(expr, ctx)?,
            };
            return Err(EvalError::InternalControlFlow(ControlFlow::Return(val)));
        }
    };

    Ok(result)
}

fn print_values<W: Write>(to: &mut W, values: &[Value]) -> Result<(), EvalError> {
    let mut str_iter = values.iter().map(|v| v.convert_to_string());
    if let Some(first) = str_iter.next() {
        write!(to, "{}", first)?;
    }
    for str in str_iter {
        write!(to, " {}", str)?;
    }
    Ok(())
}
