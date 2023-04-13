use std::{collections::HashMap, rc::Rc};

use crate::{error::EvalError, parser::AST};

pub type Number = i64;

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
        AST::Add(lhs, rhs) => evaluate(lhs, ctx)?
            .checked_add(evaluate(rhs, ctx)?)
            .ok_or(EvalError::Overflow)?,
        AST::Subtract(lhs, rhs) => evaluate(lhs, ctx)?
            .checked_sub(evaluate(rhs, ctx)?)
            .ok_or(EvalError::Overflow)?,
        AST::Multiply(lhs, rhs) => evaluate(lhs, ctx)?
            .checked_mul(evaluate(rhs, ctx)?)
            .ok_or(EvalError::Overflow)?,
        AST::Divide(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            if rval == 0 {
                return Err(EvalError::DivideByZero);
            }
            lval / rval
        }
        AST::Modulo(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            if rval == 0 {
                return Err(EvalError::DivideByZero);
            }
            lval % rval
        }
        AST::Power(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            // a ^ -b is defined as 1 / (a ^ b)
            if rval < 0 {
                match lval {
                    0 => return Err(EvalError::DivideByZero),
                    // 1 / (1 ^ N) = 1
                    1 => 1,
                    // 1 / (-1 ^ N) = { 1 if N even, -1 if N uneven }
                    -1 => {
                        if rval % 2 == 0 {
                            1
                        } else {
                            -1
                        }
                    }
                    // 1 / (n ^ N) = 0 where abs(n) >= 2
                    _ => 0,
                }
            } else {
                let rval = u32::try_from(rval).map_err(|_| EvalError::Overflow)?;
                lval.checked_pow(rval).ok_or(EvalError::Overflow)?
            }
        }
        AST::UnaryPlus(rhs) => evaluate(rhs, ctx)?,
        AST::UnaryMinus(rhs) => evaluate(rhs, ctx)?
            .checked_neg()
            .ok_or(EvalError::Overflow)?,
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

    Ok(result)
}
