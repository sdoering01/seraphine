use std::{collections::HashMap, rc::Rc};

use crate::{error::EvalError, parser::AST};

pub type Number = f64;

#[derive(Clone)]
pub struct Function {
    n_args: usize,
    func: Rc<dyn Fn(&mut Context, &[Number]) -> Number>,
}

impl Function {
    pub fn new(n_args: usize, func: Rc<dyn Fn(&mut Context, &[Number]) -> Number>) -> Self {
        Self { n_args, func }
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
        Self {
            variables: HashMap::new(),
            functions: HashMap::from([(
                "add".to_string(),
                Function::new(2, Rc::new(|_ctx, args| args[0] + args[1])),
            )]),
        }
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
