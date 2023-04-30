use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{error::EvalError, parser::AST};

// TODO: Find out how to increase this limit, since the stack of the main thread can overflow if
// this is too large.
const CALL_STACK_SIZE_LIMIT: usize = 100;

pub type Number = f64;

pub enum Function {
    Builtin {
        n_args: usize,
        func: Box<dyn Fn(&mut Context, &[Number]) -> Number>,
    },
    UserDefined {
        arg_names: Vec<String>,
        body: AST,
    },
}

impl Function {
    pub fn new_builtin<F>(n_args: usize, func: F) -> Self
    where
        F: Fn(&mut Context, &[Number]) -> Number + 'static,
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

    pub fn call(&self, ctx: &mut Context, args: &[Number]) -> Result<Number, EvalError> {
        match self {
            Function::Builtin { n_args, func } => {
                debug_assert!(args.len() == *n_args, "Invalid number of arguments");
                Ok(func(ctx, args))
            }
            Function::UserDefined { arg_names, body } => {
                debug_assert!(args.len() == arg_names.len(), "Invalid number of arguments");

                if ctx.call_stack.len() >= CALL_STACK_SIZE_LIMIT - 1 {
                    return Err(EvalError::CallStackOverflow);
                }

                let mut scope = Scope::new();
                for (name, value) in arg_names.iter().zip(args.iter()) {
                    scope.set_var(name, *value);
                }

                if let Some(function_scope) = ctx.function_scope.take() {
                    ctx.call_stack.push(function_scope);
                }
                ctx.function_scope = Some(scope);

                let call_result = evaluate(body, ctx);
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
    variables: HashMap<String, Number>,
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

    pub fn get_var(&self, name: &str) -> Option<Number> {
        self.variables.get(name).copied()
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Number) {
        self.variables.insert(name.into(), val);
    }
}

pub struct Context {
    global_scope: Scope,
    function_scope: Option<Scope>,
    call_stack: Vec<Scope>,
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Self {
            global_scope: Scope::new(),
            function_scope: None,
            call_stack: Vec::new(),
        };
        ctx.add_standard_variables();
        ctx.add_standard_functions()
            .expect("Failed to add standard functions");
        ctx
    }

    fn add_standard_variables(&mut self) {
        use std::f64::consts::{E, PI};

        self.set_var("pi", PI);
        self.set_var("e", E);
    }

    fn add_standard_functions(&mut self) -> Result<(), EvalError> {
        self.add_function("sin", Function::new_builtin(1, |_ctx, args| args[0].sin()))?;
        self.add_function("cos", Function::new_builtin(1, |_ctx, args| args[0].cos()))?;
        self.add_function("tan", Function::new_builtin(1, |_ctx, args| args[0].tan()))?;
        self.add_function(
            "asin",
            Function::new_builtin(1, |_ctx, args| args[0].asin()),
        )?;
        self.add_function(
            "acos",
            Function::new_builtin(1, |_ctx, args| args[0].acos()),
        )?;
        self.add_function(
            "atan",
            Function::new_builtin(1, |_ctx, args| args[0].atan()),
        )?;
        self.add_function(
            "atan2",
            Function::new_builtin(2, |_ctx, args| args[0].atan2(args[1])),
        )?;
        self.add_function(
            "tanh",
            Function::new_builtin(1, |_ctx, args| args[0].tanh()),
        )?;
        self.add_function(
            "sinh",
            Function::new_builtin(1, |_ctx, args| args[0].sinh()),
        )?;
        self.add_function(
            "cosh",
            Function::new_builtin(1, |_ctx, args| args[0].cosh()),
        )?;

        self.add_function("ln", Function::new_builtin(1, |_ctx, args| args[0].ln()))?;
        self.add_function(
            "log2",
            Function::new_builtin(1, |_ctx, args| args[0].log2()),
        )?;
        self.add_function(
            "log10",
            Function::new_builtin(1, |_ctx, args| args[0].log10()),
        )?;
        self.add_function(
            "log",
            Function::new_builtin(2, |_ctx, args| args[0].log(args[1])),
        )?;

        self.add_function("abs", Function::new_builtin(1, |_ctx, args| args[0].abs()))?;
        self.add_function(
            "min",
            Function::new_builtin(2, |_ctx, args| args[0].min(args[1])),
        )?;
        self.add_function(
            "max",
            Function::new_builtin(2, |_ctx, args| args[0].max(args[1])),
        )?;
        self.add_function(
            "floor",
            Function::new_builtin(1, |_ctx, args| args[0].floor()),
        )?;
        self.add_function(
            "ceil",
            Function::new_builtin(1, |_ctx, args| args[0].ceil()),
        )?;
        self.add_function(
            "round",
            Function::new_builtin(1, |_ctx, args| args[0].round()),
        )?;

        self.add_function(
            "sqrt",
            Function::new_builtin(1, |_ctx, args| args[0].sqrt()),
        )?;
        self.add_function("exp", Function::new_builtin(1, |_ctx, args| args[0].exp()))?;

        self.add_function(
            "inspect",
            Function::new_builtin(1, |_ctx, args| {
                println!("{}", args[0]);
                args[0]
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

    pub fn get_var(&self, name: &str) -> Option<Number> {
        self.function_scope
            .as_ref()
            .and_then(|s| s.get_var(name))
            .or_else(|| self.global_scope.get_var(name))
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Number) {
        let scope = self
            .function_scope
            .as_mut()
            .unwrap_or(&mut self.global_scope);
        scope.set_var(name, val);
    }
}

pub fn evaluate(ast: &AST, ctx: &mut Context) -> Result<Number, EvalError> {
    let result = match ast {
        AST::FunctionDefinition {
            name,
            arg_names: args,
            body,
        } => {
            let func = Function::new_user_defined(name, args.clone(), *body.clone())?;
            ctx.add_function(name, func)?;
            0.0
        }
        AST::Lines(lines) => {
            let mut result = 0.0;
            for line in lines.into_iter().flatten() {
                result = evaluate(line, ctx)?;
            }
            result
        }
        AST::Number(n) => n.parse().map_err(|_| EvalError::Overflow)?,
        AST::Variable(name) => ctx
            .get_var(name)
            .ok_or_else(|| EvalError::VariableNotDefined(name.clone()))?,
        AST::Add(lhs, rhs) => evaluate(lhs, ctx)? + evaluate(rhs, ctx)?,
        AST::Subtract(lhs, rhs) => evaluate(lhs, ctx)? - evaluate(rhs, ctx)?,
        AST::Multiply(lhs, rhs) => evaluate(lhs, ctx)? * evaluate(rhs, ctx)?,
        AST::Divide(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            if rval == 0.0 {
                return Err(EvalError::DivideByZero);
            }
            lval / rval
        }
        AST::Modulo(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            if rval == 0.0 {
                return Err(EvalError::DivideByZero);
            }
            lval % rval
        }
        AST::Power(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            lval.powf(rval)
        }
        AST::UnaryPlus(rhs) => evaluate(rhs, ctx)?,
        AST::UnaryMinus(rhs) => -evaluate(rhs, ctx)?,
        AST::Brackets(inner) => evaluate(inner, ctx)?,
        AST::Assign(name, rhs) => {
            let rval = evaluate(rhs, ctx)?;
            ctx.set_var(name, rval);
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
        AST::IfStatement { condition, body } => {
            let condition = evaluate(condition, ctx)?;
            if condition != 0.0 {
                evaluate(body, ctx)?;
            }
            0.0
        }
    };

    if !result.is_finite() {
        return Err(EvalError::Overflow);
    }

    Ok(result)
}
