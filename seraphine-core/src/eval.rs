use std::{
    cell::RefCell,
    collections::BTreeMap,
    io::{stderr, stdin, stdout, BufReader, Read, Write},
    rc::Rc,
};

use crate::{
    common::Span,
    error::{EvalError, SeraphineError, StdlibError},
    parser::{parse, Ast, AstKind},
    runtime::common::RuntimeContext,
    stdlib::{get_standard_functions, get_standard_variables},
    tokenizer::tokenize,
    value::{Function, FunctionKind, Value, NULL_VALUE},
};

// TODO: Find out how to increase this limit, since the stack of the main thread can overflow if
// this is too large.
const CALL_STACK_SIZE_LIMIT: usize = 100;

// Convenience trait, so we don't have to write out the same convert call for each Stdlib Result
trait ConvertableToEvalResult<Output> {
    fn convert(self, span: Span) -> Result<Output, EvalError>;
}

impl<T> ConvertableToEvalResult<T> for Result<T, StdlibError> {
    fn convert(self, span: Span) -> Result<T, EvalError> {
        self.map_err(|error| EvalError::StdlibError { error, span })
    }
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Return(Value),
    Continue,
    Break,
}

#[derive(Debug)]
struct InnerScope {
    variables: BTreeMap<String, Value>,
    parent_scope: Option<Scope>,
}

#[derive(Debug, Clone)]
pub struct Scope(Rc<RefCell<InnerScope>>);

impl Default for Scope {
    fn default() -> Self {
        Scope::new()
    }
}

impl Scope {
    pub fn new() -> Scope {
        Scope(Rc::new(RefCell::new(InnerScope {
            variables: BTreeMap::new(),
            parent_scope: None,
        })))
    }

    pub fn with_parent(parent: &Scope) -> Scope {
        Scope(Rc::new(RefCell::new(InnerScope {
            variables: BTreeMap::new(),
            parent_scope: Some(parent.clone()),
        })))
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        let self_borrow = self.0.borrow();
        self_borrow.variables.get(name).cloned().or_else(|| {
            self_borrow
                .parent_scope
                .as_ref()
                .and_then(|s| s.get_var(name))
        })
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Value) {
        self.0.borrow_mut().variables.insert(name.into(), val);
    }
}

pub struct EvaluatorBuilder {
    stdin: Box<dyn Read>,
    stdout: Box<dyn Write>,
    stderr: Box<dyn Write>,
    debug_writer: Option<Box<dyn Write>>,
    standard_variables: bool,
    standard_functions: bool,
}

impl Default for EvaluatorBuilder {
    fn default() -> Self {
        Self {
            stdin: Box::new(stdin()),
            stdout: Box::new(stdout()),
            stderr: Box::new(stderr()),
            debug_writer: None,
            standard_variables: true,
            standard_functions: true,
        }
    }
}

impl EvaluatorBuilder {
    pub fn build(self) -> Evaluator {
        let mut eval = Evaluator {
            scope: Scope::new(),
            function_call_depth: 0,
            current_span: Span::default(),
            ctx: RuntimeContext::new(
                BufReader::new(self.stdin),
                self.stdout,
                self.stderr,
                self.debug_writer,
            ),
        };

        if self.standard_variables {
            eval.add_standard_variables();
        }
        if self.standard_functions {
            eval.add_standard_functions();
        }

        eval
    }

    pub fn stdin(mut self, stdin: impl Read + 'static) -> Self {
        self.stdin = Box::new(stdin);
        self
    }

    pub fn stdout(mut self, stdout: impl Write + 'static) -> Self {
        self.stdout = Box::new(stdout);
        self
    }

    pub fn stderr(mut self, stderr: impl Write + 'static) -> Self {
        self.stderr = Box::new(stderr);
        self
    }

    pub fn debug_writer(mut self, debug_writer: Option<impl Write + 'static>) -> Self {
        if let Some(debug_writer) = debug_writer {
            self.debug_writer = Some(Box::new(debug_writer));
        } else {
            self.debug_writer = None;
        }
        self
    }

    pub fn standard_variables(mut self, b: bool) -> Self {
        self.standard_variables = b;
        self
    }

    pub fn standard_functions(mut self, b: bool) -> Self {
        self.standard_functions = b;
        self
    }
}

pub struct Evaluator {
    scope: Scope,
    function_call_depth: usize,
    current_span: Span,
    pub(crate) ctx: RuntimeContext,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::builder().build()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn builder() -> EvaluatorBuilder {
        EvaluatorBuilder::default()
    }

    pub fn eval_str(&mut self, s: &str) -> Result<Value, SeraphineError> {
        let tokens = tokenize(s)?;

        #[cfg(debug_assertions)]
        if let Some(debug_writer) = &mut self.ctx.debug_writer {
            writeln!(debug_writer, "Tokens: {:?}", tokens)?;
        }

        let ast = parse(&tokens)?;

        #[cfg(debug_assertions)]
        if let Some(debug_writer) = &mut self.ctx.debug_writer {
            writeln!(debug_writer, "AST: {:#?}", ast)?;
        }

        let result = evaluate(&ast, self)?;
        Ok(result)
    }

    fn add_standard_variables(&mut self) {
        for (name, value) in get_standard_variables() {
            self.set_var(name, value);
        }
    }

    fn add_standard_functions(&mut self) {
        for (name, func_value) in get_standard_functions() {
            self.set_var(name, func_value);
        }
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.scope.get_var(name)
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Value) {
        self.scope.set_var(name, val);
    }

    fn with_span(
        &mut self,
        span: Span,
        func: impl FnOnce(&mut Evaluator) -> Result<Value, EvalError>,
    ) -> Result<Value, EvalError> {
        let prev_span = self.current_span;
        self.current_span = span;
        let ret = func(self);
        self.current_span = prev_span;
        ret
    }

    fn call_value(&mut self, value: Value, args: Vec<Value>) -> Result<Value, StdlibError> {
        match value {
            Value::Function(func) => {
                let maybe_expected_args = func.get_arg_count();
                let got_args = args.len();
                if let Some(expected_args) = maybe_expected_args {
                    if got_args != expected_args {
                        return Err(StdlibError::FunctionWrongArgAmount {
                            name: func.get_name(),
                            expected: expected_args,
                            got: got_args,
                        });
                    }
                }
                self.call_function(func, args)
            }
            other => {
                let error = format!("Cannot call value of type {}", other.get_type());
                Err(StdlibError::TypeError(error))
            }
        }
    }

    fn call_function(&mut self, func: Function, args: Vec<Value>) -> Result<Value, StdlibError> {
        let receiver = func.receiver.map(|r| r.as_ref().to_owned());

        match func.kind.as_ref() {
            FunctionKind::Builtin { func, .. } => func(&mut self.ctx, receiver, args),
            FunctionKind::UserDefinedAst {
                arg_names,
                body,
                parent_scope,
                ..
            } => {
                let result = if self.function_call_depth >= CALL_STACK_SIZE_LIMIT - 1 {
                    Err(EvalError::CallStackOverflow)
                } else {
                    let mut scope = Scope::with_parent(parent_scope);
                    if let Some(recv) = receiver {
                        scope.set_var("this", recv);
                    }
                    for (name, value) in arg_names.iter().zip(args) {
                        scope.set_var(name, value);
                    }

                    self.function_call_depth += 1;
                    std::mem::swap(&mut scope, &mut self.scope);
                    let call_result = match evaluate(body, self) {
                        Err(EvalError::InternalControlFlow {
                            kind: ControlFlow::Return(val),
                            ..
                        }) => Ok(val),
                        Err(EvalError::InternalControlFlow {
                            kind: ControlFlow::Continue,
                            span,
                        }) => Err(EvalError::ContinueOutsideOfLoop(span)),
                        Err(EvalError::InternalControlFlow {
                            kind: ControlFlow::Break,
                            span,
                        }) => Err(EvalError::BreakOutsideOfLoop(span)),
                        other => other,
                    };
                    std::mem::swap(&mut scope, &mut self.scope);
                    self.function_call_depth -= 1;

                    call_result
                };

                result.map_err(|err| StdlibError::FunctionCall(Box::new(err)))
            }
            FunctionKind::UserDefinedVm { .. } => Err(StdlibError::GenericError(
                "got user defined vm function in AST walking mode".to_string(),
            )),
        }
    }
}

pub fn evaluate(ast: &Ast, eval: &mut Evaluator) -> Result<Value, EvalError> {
    eval.with_span(ast.span, |eval| {
        let result = match &ast.kind {
            AstKind::FunctionDefinition {
                name,
                arg_names,
                body,
            } => {
                let func = Function::new_user_defined_ast(
                    Some(name.as_str()),
                    arg_names.clone(),
                    *body.clone(),
                    eval.scope.clone(),
                )
                .convert(eval.current_span)?;
                eval.set_var(name, Value::Function(func));
                NULL_VALUE
            }
            AstKind::UnnamedFunction { arg_names, body } => {
                let func = Function::new_user_defined_ast(
                    None,
                    arg_names.clone(),
                    *body.clone(),
                    eval.scope.clone(),
                )
                .convert(eval.current_span)?;
                Value::Function(func)
            }
            AstKind::MemberAccess {
                value: value_ast,
                member,
            } => {
                let value = evaluate(value_ast, eval)?;
                value.get_member(member).convert(eval.current_span)?
            }
            AstKind::Indexing {
                value: value_ast,
                index: index_ast,
            } => {
                let value = evaluate(value_ast, eval)?;
                let index = evaluate(index_ast, eval)?;
                value.get_index(index).convert(eval.current_span)?
            }
            AstKind::Block(lines) => {
                let mut result = NULL_VALUE;
                for line in lines {
                    result = evaluate(line, eval)?;
                }
                result
            }
            AstKind::Null => NULL_VALUE,
            AstKind::NumberLiteral(n) => Value::Number(*n),
            AstKind::BooleanLiteral(b) => Value::Bool(*b),
            AstKind::StringLiteral(s) => Value::String(s.clone()),
            AstKind::ListLiteral(values) => {
                let values = values
                    .iter()
                    .map(|ast| evaluate(ast, eval))
                    .collect::<Result<Vec<_>, _>>()?;
                Value::List(Rc::new(RefCell::new(values)))
            }
            AstKind::ObjectLiteral(key_value_pairs) => {
                let mut object = BTreeMap::new();
                for (key, value) in key_value_pairs {
                    let value = evaluate(value, eval)?;
                    object.insert(key.clone(), value);
                }
                Value::Object(Rc::new(RefCell::new(object)))
            }
            AstKind::Variable(name) => {
                eval.get_var(name)
                    .ok_or_else(|| EvalError::VariableNotDefined {
                        name: name.clone(),
                        span: eval.current_span,
                    })?
            }
            AstKind::Add(lhs, rhs) => evaluate(lhs, eval)?
                .add(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::Subtract(lhs, rhs) => evaluate(lhs, eval)?
                .subtract(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::Multiply(lhs, rhs) => evaluate(lhs, eval)?
                .multiply(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::Divide(lhs, rhs) => evaluate(lhs, eval)?
                .divide(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::Modulo(lhs, rhs) => evaluate(lhs, eval)?
                .modulo(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::Power(lhs, rhs) => evaluate(lhs, eval)?
                .power(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::UnaryMinus(rhs) => evaluate(rhs, eval)?.negate().convert(eval.current_span)?,
            AstKind::BooleanNegate(rhs) => evaluate(rhs, eval)?
                .bool_negate()
                .convert(eval.current_span)?,
            AstKind::Equality(lhs, rhs) => evaluate(lhs, eval)?
                .equal(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::Inequality(lhs, rhs) => evaluate(lhs, eval)?
                .unequal(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::LessThan(lhs, rhs) => evaluate(lhs, eval)?
                .less_than(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::GreaterThan(lhs, rhs) => evaluate(lhs, eval)?
                .greater_than(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::LessThanOrEqual(lhs, rhs) => evaluate(lhs, eval)?
                .less_than_or_equal(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::GreaterThanOrEqual(lhs, rhs) => evaluate(lhs, eval)?
                .greater_than_or_equal(evaluate(rhs, eval)?)
                .convert(eval.current_span)?,
            AstKind::And(lhs, rhs) => lazy_and(evaluate(lhs, eval)?, || evaluate(rhs, eval))?,
            AstKind::Or(lhs, rhs) => lazy_or(evaluate(lhs, eval)?, || evaluate(rhs, eval))?,
            AstKind::Brackets(inner) => evaluate(inner, eval)?,
            AstKind::Assign(name, rhs) => {
                let rval = evaluate(rhs, eval)?;
                eval.set_var(name, rval.clone());
                NULL_VALUE
            }
            AstKind::IndexingAssign { value, index, rhs } => {
                let value = evaluate(value, eval)?;
                let index = evaluate(index, eval)?;
                let rval = evaluate(rhs, eval)?;
                value
                    .set_index(index, rval.clone())
                    .convert(eval.current_span)?;
                NULL_VALUE
            }
            AstKind::MemberAssign { value, member, rhs } => {
                let value = evaluate(value, eval)?;
                let rval = evaluate(rhs, eval)?;
                value
                    .set_member(member, rval.clone())
                    .convert(eval.current_span)?;
                NULL_VALUE
            }
            AstKind::FunctionCall { value, args } => {
                let val = evaluate(value, eval)?;
                let args: Vec<_> = args
                    .iter()
                    .map(|ast| evaluate(ast, eval))
                    .collect::<Result<_, _>>()?;
                eval.call_value(val, args).convert(eval.current_span)?
            }
            AstKind::IfStatement {
                condition,
                if_body,
                else_body,
            } => {
                let condition = evaluate(condition, eval)?;
                if condition.as_bool() {
                    evaluate(if_body, eval)?;
                } else if let Some(else_body) = else_body {
                    evaluate(else_body, eval)?;
                }
                NULL_VALUE
            }
            AstKind::WhileLoop { condition, body } => {
                while evaluate(condition, eval)?.as_bool() {
                    match evaluate(body, eval) {
                        Err(EvalError::InternalControlFlow {
                            kind: ControlFlow::Continue,
                            ..
                        }) => continue,
                        Err(EvalError::InternalControlFlow {
                            kind: ControlFlow::Break,
                            ..
                        }) => break,
                        e @ Err(_) => return e,
                        Ok(_) => (),
                    }
                }
                NULL_VALUE
            }
            AstKind::ForLoop {
                variable,
                iterable,
                body,
            } => {
                let iterable_val = evaluate(iterable, eval)?;
                let iterator = iterable_val.make_iterator().convert(iterable.span)?;
                for value in iterator.borrow_mut().into_iter() {
                    eval.set_var(variable, value);
                    match evaluate(body, eval) {
                        Err(EvalError::InternalControlFlow {
                            kind: ControlFlow::Continue,
                            ..
                        }) => continue,
                        Err(EvalError::InternalControlFlow {
                            kind: ControlFlow::Break,
                            ..
                        }) => break,
                        e @ Err(_) => return e,
                        Ok(_) => (),
                    }
                }
                NULL_VALUE
            }
            AstKind::Continue => {
                return Err(EvalError::InternalControlFlow {
                    kind: ControlFlow::Continue,
                    span: eval.current_span,
                });
            }
            AstKind::Break => {
                return Err(EvalError::InternalControlFlow {
                    kind: ControlFlow::Break,
                    span: eval.current_span,
                });
            }
            AstKind::Return(expr) => {
                let val = match expr {
                    None => NULL_VALUE,
                    Some(expr) => evaluate(expr, eval)?,
                };
                return Err(EvalError::InternalControlFlow {
                    kind: ControlFlow::Return(val),
                    span: eval.current_span,
                });
            }
        };
        Ok(result)
    })
}

fn lazy_and(
    lhs: Value,
    rhs_evaluator: impl FnOnce() -> Result<Value, EvalError>,
) -> Result<Value, EvalError> {
    let result = lhs.as_bool() && rhs_evaluator()?.as_bool();
    Ok(Value::Bool(result))
}

pub(crate) fn lazy_or(
    lhs: Value,
    rhs_evaluator: impl FnOnce() -> Result<Value, EvalError>,
) -> Result<Value, EvalError> {
    let result = lhs.as_bool() || rhs_evaluator()?.as_bool();
    Ok(Value::Bool(result))
}
