use std::{collections::HashMap, rc::Rc};

use crate::{error::EvalError, parser::AST};

pub type Number = f64;

#[derive(Clone)]
pub struct Function {
    n_args: usize,
    func: Rc<dyn Fn(&mut Context, &[Number]) -> Number>,
}

impl Function {
    pub fn new<F>(n_args: usize, func: F) -> Self
    where
        F: Fn(&mut Context, &[Number]) -> Number + 'static,
    {
        Self {
            n_args,
            func: Rc::new(func),
        }
    }

    pub fn call(&self, ctx: &mut Context, args: &[Number]) -> Number {
        (self.func)(ctx, args)
    }
}

pub struct Context {
    variables: HashMap<String, Number>,
    functions: HashMap<String, Function>,
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        };
        ctx.add_standard_variables();
        ctx.add_standard_functions();
        ctx
    }

    fn add_standard_variables(&mut self) {
        use std::f64::consts::{E, PI};

        self.set_var("pi", PI);
        self.set_var("e", E);
    }

    fn add_standard_functions(&mut self) {
        self.add_function("sin", Function::new(1, |_ctx, args| args[0].sin()));
        self.add_function("cos", Function::new(1, |_ctx, args| args[0].cos()));
        self.add_function("tan", Function::new(1, |_ctx, args| args[0].tan()));
        self.add_function("asin", Function::new(1, |_ctx, args| args[0].asin()));
        self.add_function("acos", Function::new(1, |_ctx, args| args[0].acos()));
        self.add_function("atan", Function::new(1, |_ctx, args| args[0].atan()));
        self.add_function(
            "atan2",
            Function::new(2, |_ctx, args| args[0].atan2(args[1])),
        );
        self.add_function("tanh", Function::new(1, |_ctx, args| args[0].tanh()));
        self.add_function("sinh", Function::new(1, |_ctx, args| args[0].sinh()));
        self.add_function("cosh", Function::new(1, |_ctx, args| args[0].cosh()));

        self.add_function("ln", Function::new(1, |_ctx, args| args[0].ln()));
        self.add_function("log2", Function::new(1, |_ctx, args| args[0].log2()));
        self.add_function("log10", Function::new(1, |_ctx, args| args[0].log10()));
        self.add_function("log", Function::new(2, |_ctx, args| args[0].log(args[1])));

        self.add_function("abs", Function::new(1, |_ctx, args| args[0].abs()));
        self.add_function("min", Function::new(2, |_ctx, args| args[0].min(args[1])));
        self.add_function("max", Function::new(2, |_ctx, args| args[0].max(args[1])));
        self.add_function("floor", Function::new(1, |_ctx, args| args[0].floor()));
        self.add_function("ceil", Function::new(1, |_ctx, args| args[0].ceil()));
        self.add_function("round", Function::new(1, |_ctx, args| args[0].round()));

        self.add_function("sqrt", Function::new(1, |_ctx, args| args[0].sqrt()));
        self.add_function("exp", Function::new(1, |_ctx, args| args[0].exp()));
    }

    pub fn add_function(&mut self, name: impl Into<String>, func: Function) {
        self.functions.insert(name.into(), func);
    }

    pub fn get_var(&self, name: &str) -> Option<Number> {
        self.variables.get(name).copied()
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Number) {
        self.variables.insert(name.into(), val);
    }
}

pub fn evaluate(ast: &AST, ctx: &mut Context) -> Result<Number, EvalError> {
    let result = match ast {
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
                .functions
                .get(name)
                .ok_or_else(|| EvalError::FunctionNotDefined(name.clone()))?
                .clone();

            let expected_args = func.n_args;
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
            func.call(ctx, &args)
        }
    };

    if !result.is_finite() {
        return Err(EvalError::Overflow);
    }

    Ok(result)
}
