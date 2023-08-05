use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

use crate::{error::EvalError, parser::AST};

// TODO: Find out how to increase this limit, since the stack of the main thread can overflow if
// this is too large.
const CALL_STACK_SIZE_LIMIT: usize = 100;

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Return(Value),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Number => write!(f, "number"),
            Bool => write!(f, "bool"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
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
        }
    }
}

impl Value {
    fn get_type(&self) -> Type {
        use Value::*;
        match self {
            Number(..) => Type::Number,
            Bool(..) => Type::Bool,
        }
    }

    fn assert_type(&self, expected: Type) -> Result<(), EvalError> {
        let got = self.get_type();
        if got != expected {
            return Err(EvalError::WrongType { expected, got });
        }
        Ok(())
    }

    fn as_bool(&self) -> bool {
        match self {
            Value::Number(n) => *n != 0.0,
            Value::Bool(b) => *b,
        }
    }

    fn add(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l + r)),
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
            _ => false,
        };
        Ok(Bool(eq))
    }

    fn unequal(self, rhs: Self) -> Result<Value, EvalError> {
        let Value::Bool(eq) = self.equal(rhs)? else { unreachable!() };
        Ok(Value::Bool(!eq))
    }

    fn less_than(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Bool(l < r)),
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn greater_than(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Bool(l > r)),
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn less_than_or_equal(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Bool(l <= r)),
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn greater_than_or_equal(self, rhs: Self) -> Result<Value, EvalError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Bool(l >= r)),
            (l, r) => {
                let error = format!(
                    "Cannot compare value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
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

pub enum Function {
    Builtin {
        n_args: usize,
        func: Box<dyn Fn(&mut Context, &[Value]) -> Result<Value, EvalError>>,
    },
    UserDefined {
        arg_names: Vec<String>,
        body: AST,
    },
}

impl Function {
    pub fn new_builtin<F>(n_args: usize, func: F) -> Self
    where
        F: Fn(&mut Context, &[Value]) -> Result<Value, EvalError> + 'static,
    {
        Self::Builtin {
            n_args,
            func: Box::new(func),
        }
    }

    pub fn new_user_defined(
        func_name: &str,
        arg_names: Vec<String>,
        body: AST,
    ) -> Result<Self, EvalError> {
        let mut arg_set = HashSet::new();
        for name in &arg_names {
            if !arg_set.insert(name) {
                return Err(EvalError::DuplicateArgName {
                    func_name: func_name.to_owned(),
                    arg_name: name.clone(),
                });
            }
        }
        Ok(Self::UserDefined { arg_names, body })
    }

    pub fn call(&self, ctx: &mut Context, args: &[Value]) -> Result<Value, EvalError> {
        match self {
            Function::Builtin { n_args, func } => {
                debug_assert!(args.len() == *n_args, "Invalid number of arguments");
                func(ctx, args)
            }
            Function::UserDefined { arg_names, body } => {
                debug_assert!(args.len() == arg_names.len(), "Invalid number of arguments");

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

                let call_result = evaluate_catch_return(body, ctx);
                ctx.function_scope = ctx.call_stack.pop();
                call_result
            }
        }
    }

    fn get_arg_count(&self) -> usize {
        match self {
            Function::Builtin { n_args, .. } => *n_args,
            Function::UserDefined { arg_names, .. } => arg_names.len(),
        }
    }
}

struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Rc<Function>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<Function>> {
        self.functions.get(name).cloned()
    }

    pub fn add_function(
        &mut self,
        name: impl Into<String> + AsRef<str>,
        func: Function,
    ) -> Result<(), EvalError> {
        if self.functions.get(name.as_ref()).is_some() {
            return Err(EvalError::FunctionAlreadyDefined(name.into()));
        }
        self.functions.insert(name.into(), Rc::new(func));
        Ok(())
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.variables.get(name).cloned()
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Value) {
        self.variables.insert(name.into(), val);
    }
}

pub struct Context {
    global_scope: Scope,
    function_scope: Option<Scope>,
    call_stack: Vec<Scope>,
    /// This flag is used during tests until observable side effects apart from writing to stdout
    /// are introduced.
    // TODO: Remove this when some form of obvservable side effects is implemented
    pub _internal_side_effect_flag: bool,
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Self {
            global_scope: Scope::new(),
            function_scope: None,
            call_stack: Vec::new(),
            _internal_side_effect_flag: false,
        };
        ctx.add_standard_variables();
        ctx.add_standard_functions()
            .expect("Failed to add standard functions");
        ctx
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
        self.add_function(
            "_set_internal_side_effect_flag",
            Function::new_builtin(0, |ctx, _args| {
                ctx._internal_side_effect_flag = true;
                Ok(NULL_VALUE)
            }),
        )?;

        // TODO: Scope functions to a separate namespace like `math`, so they can be used via
        // `math.is_nan(42)`
        self.add_function(
            "is_nan",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Bool(arg.is_nan()))
            }),
        )?;

        self.add_function(
            "is_infinite",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Bool(arg.is_infinite()))
            }),
        )?;

        self.add_function(
            "is_finite",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Bool(arg.is_finite()))
            }),
        )?;

        self.add_function(
            "sin",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.sin()))
            }),
        )?;
        self.add_function(
            "cos",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.cos()))
            }),
        )?;
        self.add_function(
            "tan",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.tan()))
            }),
        )?;
        self.add_function(
            "asin",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.asin()))
            }),
        )?;
        self.add_function(
            "acos",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.acos()))
            }),
        )?;
        self.add_function(
            "atan",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.atan()))
            }),
        )?;
        self.add_function(
            "atan2",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                args[1].assert_type(Type::Number)?;
                let Value::Number(arg1) = args[0] else { unreachable!() };
                let Value::Number(arg2) = args[1] else { unreachable!() };
                Ok(Value::Number(arg1.atan2(arg2)))
            }),
        )?;
        self.add_function(
            "tanh",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.tanh()))
            }),
        )?;
        self.add_function(
            "sinh",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.sinh()))
            }),
        )?;
        self.add_function(
            "cosh",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.cosh()))
            }),
        )?;

        self.add_function(
            "ln",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.ln()))
            }),
        )?;
        self.add_function(
            "log2",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.log2()))
            }),
        )?;
        self.add_function(
            "log10",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.log10()))
            }),
        )?;
        self.add_function(
            "log",
            Function::new_builtin(2, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                args[1].assert_type(Type::Number)?;
                let Value::Number(arg1) = args[0] else { unreachable!() };
                let Value::Number(arg2) = args[1] else { unreachable!() };
                Ok(Value::Number(arg1.log(arg2)))
            }),
        )?;

        self.add_function(
            "abs",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.abs()))
            }),
        )?;
        self.add_function(
            "min",
            Function::new_builtin(2, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                args[1].assert_type(Type::Number)?;
                let Value::Number(arg1) = args[0] else { unreachable!() };
                let Value::Number(arg2) = args[1] else { unreachable!() };
                Ok(Value::Number(arg1.min(arg2)))
            }),
        )?;
        self.add_function(
            "max",
            Function::new_builtin(2, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                args[1].assert_type(Type::Number)?;
                let Value::Number(arg1) = args[0] else { unreachable!() };
                let Value::Number(arg2) = args[1] else { unreachable!() };
                Ok(Value::Number(arg1.max(arg2)))
            }),
        )?;
        self.add_function(
            "floor",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.floor()))
            }),
        )?;
        self.add_function(
            "ceil",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.ceil()))
            }),
        )?;
        self.add_function(
            "round",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.round()))
            }),
        )?;

        self.add_function(
            "sqrt",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.sqrt()))
            }),
        )?;
        self.add_function(
            "exp",
            Function::new_builtin(1, |_ctx, args| {
                args[0].assert_type(Type::Number)?;
                let Value::Number(arg) = args[0] else { unreachable!() };
                Ok(Value::Number(arg.exp()))
            }),
        )?;

        self.add_function(
            "inspect",
            Function::new_builtin(1, |_ctx, args| {
                println!("{}", args[0]);
                Ok(args[0].clone())
            }),
        )?;

        Ok(())
    }

    pub fn get_function(&mut self, name: &str) -> Option<Rc<Function>> {
        self.function_scope
            .as_ref()
            .and_then(|s| s.get_function(name))
            .or_else(|| self.global_scope.get_function(name))
    }

    pub fn add_function(
        &mut self,
        name: impl Into<String> + AsRef<str>,
        func: Function,
    ) -> Result<(), EvalError> {
        let scope = self
            .function_scope
            .as_mut()
            .unwrap_or(&mut self.global_scope);
        scope.add_function(name, func)
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

pub fn evaluate(ast: &AST, ctx: &mut Context) -> Result<Value, EvalError> {
    let result = match ast {
        AST::FunctionDefinition {
            name,
            arg_names: args,
            body,
        } => {
            let func = Function::new_user_defined(name, args.clone(), *body.clone())?;
            ctx.add_function(name, func)?;
            NULL_VALUE
        }
        AST::Lines(lines) => {
            let mut result = NULL_VALUE;
            for line in lines.into_iter() {
                result = evaluate(line, ctx)?;
            }
            result
        }
        AST::NumberLiteral(n) => {
            Value::Number(*n)
        }
        AST::BooleanLiteral(b) => Value::Bool(*b),
        AST::Variable(name) => ctx
            .get_var(name)
            .ok_or_else(|| EvalError::VariableNotDefined(name.clone()))?,
        AST::Add(lhs, rhs) => evaluate(lhs, ctx)?.add(evaluate(rhs, ctx)?)?,
        AST::Subtract(lhs, rhs) => evaluate(lhs, ctx)?.subtract(evaluate(rhs, ctx)?)?,
        AST::Multiply(lhs, rhs) => evaluate(lhs, ctx)?.multiply(evaluate(rhs, ctx)?)?,
        AST::Divide(lhs, rhs) => evaluate(lhs, ctx)?.divide(evaluate(rhs, ctx)?)?,
        AST::Modulo(lhs, rhs) => evaluate(lhs, ctx)?.modulo(evaluate(rhs, ctx)?)?,
        AST::Power(lhs, rhs) => evaluate(lhs, ctx)?.power(evaluate(rhs, ctx)?)?,
        AST::UnaryMinus(rhs) => evaluate(rhs, ctx)?.negate()?,
        AST::BooleanNegate(rhs) => evaluate(rhs, ctx)?.bool_negate()?,
        AST::Equality(lhs, rhs) => evaluate(lhs, ctx)?.equal(evaluate(rhs, ctx)?)?,
        AST::Inequality(lhs, rhs) => evaluate(lhs, ctx)?.unequal(evaluate(rhs, ctx)?)?,
        AST::LessThan(lhs, rhs) => evaluate(lhs, ctx)?.less_than(evaluate(rhs, ctx)?)?,
        AST::GreaterThan(lhs, rhs) => evaluate(lhs, ctx)?.greater_than(evaluate(rhs, ctx)?)?,
        AST::LessThanOrEqual(lhs, rhs) => {
            evaluate(lhs, ctx)?.less_than_or_equal(evaluate(rhs, ctx)?)?
        }
        AST::GreaterThanOrEqual(lhs, rhs) => {
            evaluate(lhs, ctx)?.greater_than_or_equal(evaluate(rhs, ctx)?)?
        }
        AST::And(lhs, rhs) => evaluate(lhs, ctx)?.and(|| evaluate(rhs, ctx))?,
        AST::Or(lhs, rhs) => evaluate(lhs, ctx)?.or(|| evaluate(rhs, ctx))?,
        AST::Brackets(inner) => evaluate(inner, ctx)?,
        AST::Assign(name, rhs) => {
            let rval = evaluate(rhs, ctx)?;
            ctx.set_var(name, rval.clone());
            rval
        }
        AST::FunctionCall(name, args_ast) => {
            let func = ctx
                .get_function(name)
                .ok_or_else(|| EvalError::FunctionNotDefined(name.clone()))?;

            let expected_args = func.get_arg_count();
            let got_args = args_ast.len();
            if got_args != expected_args {
                return Err(EvalError::FunctionWrongArgAmount {
                    name: name.clone(),
                    expected: expected_args,
                    got: got_args,
                });
            }

            let args: Vec<_> = args_ast
                .iter()
                .map(|ast| evaluate(ast, ctx))
                .collect::<Result<_, _>>()?;
            func.call(ctx, &args)?
        }
        AST::IfStatement {
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
        AST::WhileLoop { condition, body } => {
            while evaluate(condition, ctx)?.as_bool() {
                evaluate(body, ctx)?;
            }
            NULL_VALUE
        }
        AST::Return(expr) => {
            let val = match expr {
                None => NULL_VALUE,
                Some(expr) => evaluate(expr, ctx)?,
            };
            return Err(EvalError::InternalControlFlow(ControlFlow::Return(val)));
        }
    };

    Ok(result)
}

fn evaluate_catch_return(ast: &AST, ctx: &mut Context) -> Result<Value, EvalError> {
    match evaluate(ast, ctx) {
        Err(EvalError::InternalControlFlow(ControlFlow::Return(val))) => Ok(val),
        other => other,
    }
}
