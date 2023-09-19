use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    io::{stderr, stdin, stdout, BufRead, BufReader, Read, Write},
    rc::Rc,
};

use crate::{
    error::{EvalError, SeraphineError},
    parser::{parse, Ast},
    tokenizer::tokenize,
};

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
    Null,
    Number,
    Bool,
    String,
    Function,
    List,
    Object,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Null => write!(f, "null"),
            Number => write!(f, "number"),
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            Function => write!(f, "function"),
            List => write!(f, "list"),
            Object => write!(f, "object"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Number(f64),
    Bool(bool),
    String(String),
    Function(Function),
    List(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<HashMap<String, Value>>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;
        match self {
            Null => write!(f, "null"),
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
            List(_) | Object(_) => print_potentially_self_referential(self, f),
        }
    }
}

fn print_potentially_self_referential(
    value: &Value,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    print_potentially_self_referential_recursive(value, f, &mut Vec::new())
}

fn print_potentially_self_referential_recursive(
    value: &Value,
    f: &mut std::fmt::Formatter<'_>,
    refs: &mut Vec<usize>,
) -> std::fmt::Result {
    match value {
        Value::List(lst) => {
            write!(f, "[")?;

            let pointer = lst.as_ptr() as usize;
            if refs.contains(&pointer) {
                write!(f, "...")?;
            } else {
                refs.push(pointer);
                let mut first = true;
                for item in lst.borrow().iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    print_potentially_self_referential_recursive(item, f, refs)?;
                }
                refs.pop();
            }

            write!(f, "]")
        }
        Value::Object(obj) => {
            write!(f, "{{ ")?;

            let pointer = obj.as_ptr() as usize;
            if refs.contains(&pointer) {
                write!(f, "...")?;
            } else {
                refs.push(pointer);
                let mut first = true;
                for (key, value) in obj.borrow().iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}: ", key)?;
                    print_potentially_self_referential_recursive(value, f, refs)?;
                }
                refs.pop();
            }

            write!(f, " }}")
        }
        non_referential => write!(f, "{}", non_referential),
    }
}

impl Value {
    fn get_type(&self) -> Type {
        use Value::*;
        match self {
            Null => Type::Null,
            Number(..) => Type::Number,
            Bool(..) => Type::Bool,
            String(..) => Type::String,
            Function(..) => Type::Function,
            List(..) => Type::List,
            Object(..) => Type::Object,
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
            Value::Null => false,
            Value::Number(n) => *n != 0.0,
            Value::Bool(b) => *b,
            Value::String(s) => !s.is_empty(),
            Value::Function(..) => true,
            Value::List(lst) => !lst.borrow().is_empty(),
            Value::Object(obj) => !obj.borrow().is_empty(),
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

    fn get_member(&self, member: &str) -> Result<Value, EvalError> {
        match self {
            Value::String(s) => match member {
                "length" => Ok(Value::Number(s.len() as f64)),
                "trim" => {
                    let func = Function::new_builtin(
                        "trim",
                        Some(self.clone()),
                        Some(0),
                        |_ctx, this, _args| {
                            let Some(Value::String(s)) = this else {
                                unreachable!()
                            };
                            let trimmed = s.trim().to_string();
                            Ok(Value::String(trimmed))
                        },
                    );
                    Ok(Value::Function(func))
                }
                "concat" => {
                    let func = Function::new_builtin(
                        "concat",
                        Some(self.clone()),
                        Some(1),
                        |_ctx, this, args| {
                            let Some(Value::String(mut s)) = this else {
                                unreachable!()
                            };
                            args[0].assert_type(Type::String)?;
                            let Value::String(other) = &args[0] else {
                                unreachable!()
                            };
                            s.push_str(other);
                            Ok(Value::String(s))
                        },
                    );
                    Ok(Value::Function(func))
                }
                "slice" => {
                    let func = Function::new_builtin(
                        "slice",
                        Some(self.clone()),
                        Some(2),
                        |_ctx, this, args| {
                            let Some(Value::String(s)) = this else {
                                unreachable!()
                            };
                            args[0].assert_type(Type::Number)?;
                            args[1].assert_type(Type::Number)?;
                            let Value::Number(start) = &args[0] else {
                                unreachable!()
                            };
                            let Value::Number(end) = &args[1] else {
                                unreachable!()
                            };
                            // TODO: Add overflow checks
                            let start = *start as usize;
                            let end = *end as usize;
                            let sliced = if end > start { &s[start..end] } else { "" };
                            Ok(Value::String(sliced.to_string()))
                        },
                    );
                    Ok(Value::Function(func))
                }
                _ => Err(EvalError::NoSuchMember {
                    r#type: Type::String,
                    member_name: member.to_string(),
                }),
            },
            Value::List(l) => match member {
                "length" => Ok(Value::Number(l.borrow().len() as f64)),
                "get" => {
                    let func = Function::new_builtin(
                        "get",
                        Some(self.clone()),
                        Some(1),
                        |_ctx, this, args| {
                            let Some(Value::List(l)) = this else {
                                unreachable!()
                            };
                            args[0].assert_type(Type::Number)?;
                            let Value::Number(index) = &args[0] else {
                                unreachable!()
                            };
                            // TODO: Add overflow checks
                            let index = *index as usize;
                            let item = l.borrow().get(index).cloned().unwrap_or(NULL_VALUE);
                            Ok(item)
                        },
                    );
                    Ok(Value::Function(func))
                }
                "set" => {
                    let func = Function::new_builtin(
                        "set",
                        Some(self.clone()),
                        Some(2),
                        |_ctx, this, args| {
                            let Some(Value::List(l)) = this else {
                                unreachable!()
                            };
                            args[0].assert_type(Type::Number)?;
                            let Value::Number(index) = &args[0] else {
                                unreachable!()
                            };

                            // TODO: Add overflow checks
                            let index = *index as usize;
                            let mut list = l.borrow_mut();
                            if index >= list.len() {
                                return Err(EvalError::IndexOutOfBounds {
                                    index,
                                    length: list.len(),
                                });
                            }
                            list[index] = args[1].clone();

                            Ok(NULL_VALUE)
                        },
                    );
                    Ok(Value::Function(func))
                }
                "push" => {
                    let func = Function::new_builtin(
                        "push",
                        Some(self.clone()),
                        Some(1),
                        |_ctx, this, args| {
                            let Some(Value::List(l)) = this else {
                                unreachable!()
                            };
                            l.borrow_mut().push(args[0].clone());
                            Ok(Value::List(l))
                        },
                    );
                    Ok(Value::Function(func))
                }
                _ => Err(EvalError::NoSuchMember {
                    r#type: Type::List,
                    member_name: member.to_string(),
                }),
            },
            Value::Object(o) => {
                let obj = o.borrow();
                // TODO: Put this functionality in a method of a new Object type
                if let Some(value) = obj.get(member) {
                    match value.clone() {
                        Value::Function(f) => Ok(Value::Function(f.with_this(Some(self.clone())))),
                        v => Ok(v),
                    }
                } else {
                    Err(EvalError::NoSuchMember {
                        r#type: Type::Object,
                        member_name: member.to_string(),
                    })
                }
            }
            _ => Err(EvalError::NoSuchMember {
                r#type: self.get_type(),
                member_name: member.to_string(),
            }),
        }
    }

    fn set_member(self, member: &str, value: Value) -> Result<(), EvalError> {
        match self {
            Value::Object(o) => {
                let mut obj = o.borrow_mut();
                obj.insert(member.to_string(), value);
                Ok(())
            }
            _ => {
                let error = format!("Cannot set member of type {}", self.get_type());
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn get_index(self, idx: Value) -> Result<Value, EvalError> {
        match self {
            Value::List(l) => {
                idx.assert_type(Type::Number)?;
                let Value::Number(index) = idx else {
                    unreachable!()
                };

                // TODO: Add overflow checks
                let index = index as usize;
                let list = l.borrow();
                if index >= list.len() {
                    return Err(EvalError::IndexOutOfBounds {
                        index,
                        length: list.len(),
                    });
                }
                Ok(list[index].clone())
            }
            Value::Object(ref o) => {
                idx.assert_type(Type::String)?;
                let Value::String(index) = idx else {
                    unreachable!()
                };

                // TODO: Put this functionality in a method of a new Object type
                if let Some(value) = o.borrow().get(&index) {
                    match value.clone() {
                        Value::Function(f) => Ok(Value::Function(f.with_this(Some(self.clone())))),
                        v => Ok(v),
                    }
                } else {
                    Err(EvalError::NoSuchMember {
                        r#type: Type::Object,
                        member_name: index,
                    })
                }
            }
            _ => {
                let error = format!("Cannot index value of type {}", self.get_type());
                Err(EvalError::TypeError(error))
            }
        }
    }

    fn set_index(self, idx: Value, value: Value) -> Result<(), EvalError> {
        match self {
            Value::List(l) => {
                idx.assert_type(Type::Number)?;
                let Value::Number(index) = idx else {
                    unreachable!()
                };

                // TODO: Add overflow checks
                let index = index as usize;
                let mut list = l.borrow_mut();
                if index >= list.len() {
                    return Err(EvalError::IndexOutOfBounds {
                        index,
                        length: list.len(),
                    });
                }
                list[index] = value;
                Ok(())
            }
            Value::Object(o) => {
                idx.assert_type(Type::String)?;
                let Value::String(index) = idx else {
                    unreachable!()
                };

                o.borrow_mut().insert(index, value);
                Ok(())
            }
            _ => {
                let error = format!("Cannot index value of type {}", self.get_type());
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

    fn inner_equal(&self, rhs: &Self) -> bool {
        use Value::*;

        match (self, rhs) {
            (Number(l), Number(r)) => l == r,
            (Bool(l), Bool(r)) => l == r,
            (String(l), String(r)) => l == r,
            (Function(l), Function(r)) => l.is_equal(r),
            (List(l), List(r)) => Rc::ptr_eq(l, r),
            (Object(l), Object(r)) => Rc::ptr_eq(l, r),
            (Null, Null) => true,
            _ => false,
        }
    }

    fn equal(self, rhs: Self) -> Result<Value, EvalError> {
        Ok(Value::Bool(self.inner_equal(&rhs)))
    }

    fn unequal(self, rhs: Self) -> Result<Value, EvalError> {
        Ok(Value::Bool(!self.inner_equal(&rhs)))
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

const NULL_VALUE: Value = Value::Null;

#[derive(Clone)]
pub struct Function {
    kind: Rc<FunctionKind>,
    receiver: Option<Box<Value>>,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)?;
        if let Some(receiver) = &self.receiver {
            write!(f, " on {}", receiver)?;
        }
        Ok(())
    }
}

impl Function {
    pub fn new_builtin<F>(
        name: impl Into<String>,
        receiver: Option<Value>,
        n_args: Option<usize>,
        func: F,
    ) -> Self
    where
        F: Fn(&mut Context, Option<Value>, &[Value]) -> Result<Value, EvalError> + 'static,
    {
        Function {
            kind: Rc::new(FunctionKind::Builtin {
                name: name.into(),
                n_args,
                func: Box::new(func),
            }),
            receiver: receiver.map(Box::new),
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

        Ok(Function {
            kind: Rc::new(FunctionKind::UserDefined {
                name: func_name,
                arg_names,
                body,
            }),
            receiver: None,
        })
    }

    pub fn get_name(&self) -> Option<String> {
        match self.kind.as_ref() {
            FunctionKind::Builtin { name, .. } => Some(name.clone()),
            FunctionKind::UserDefined { name, .. } => name.clone(),
        }
    }

    pub fn call(&self, ctx: &mut Context, args: &[Value]) -> Result<Value, EvalError> {
        let receiver = self.receiver.as_ref().map(|r| r.as_ref().to_owned());

        match self.kind.as_ref() {
            FunctionKind::Builtin { func, .. } => func(ctx, receiver, args),
            FunctionKind::UserDefined {
                arg_names, body, ..
            } => {
                if ctx.call_stack.len() >= CALL_STACK_SIZE_LIMIT - 1 {
                    return Err(EvalError::CallStackOverflow);
                }

                let mut scope = Scope::new();
                if let Some(recv) = receiver {
                    scope.set_var("this", recv);
                }
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
        match self.kind.as_ref() {
            FunctionKind::Builtin { n_args, .. } => *n_args,
            FunctionKind::UserDefined { arg_names, .. } => Some(arg_names.len()),
        }
    }

    fn is_equal(&self, other: &Function) -> bool {
        Rc::ptr_eq(&self.kind, &other.kind)
            && match (&self.receiver, &other.receiver) {
                (Some(l), Some(r)) => l.inner_equal(r),
                (None, None) => true,
                _ => false,
            }
    }

    fn with_this(self, this: Option<Value>) -> Self {
        Function {
            kind: self.kind,
            receiver: this.map(Box::new),
        }
    }
}

type BuiltinFunctionClosure =
    Box<dyn Fn(&mut Context, Option<Value>, &[Value]) -> Result<Value, EvalError>>;

pub enum FunctionKind {
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

impl Debug for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Builtin { name, .. } => {
                write!(f, "built-in function '{}'", name)
            }
            FunctionKind::UserDefined { name, .. } => {
                if let Some(name) = name {
                    write!(f, "function '{}'", name)
                } else {
                    write!(f, "unnamed function")
                }
            }
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
    debug_writer: Option<Box<dyn Write>>,
    standard_variables: bool,
    standard_functions: bool,
}

impl Default for ContextBuilder {
    fn default() -> Self {
        Self {
            stdin: Box::new(stdin()),
            stdout: Box::new(stdout()),
            stderr: Box::new(stderr()),
            debug_writer: Some(Box::new(stdout())),
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
            debug_writer: self.debug_writer,
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

    pub fn debug_writer(mut self, debug_writer: Option<impl Write + 'static>) -> Self {
        if let Some(debug_writer) = debug_writer {
            self.debug_writer = Some(Box::new(debug_writer));
        } else {
            self.debug_writer = None;
        }
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
    #[allow(dead_code)]
    debug_writer: Option<Box<dyn Write>>,
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

    pub fn eval_str(&mut self, s: &str) -> Result<Value, SeraphineError> {
        let tokens = tokenize(s)?;

        #[cfg(debug_assertions)]
        if !cfg!(test) {
            if let Some(debug_writer) = &mut self.debug_writer {
                writeln!(debug_writer, "Tokens: {:?}", tokens)?;
            }
        } else {
            println!("Tokens: {:?}", tokens);
        }

        let ast = parse(&tokens)?;

        #[cfg(debug_assertions)]
        if !cfg!(test) {
            if let Some(debug_writer) = &mut self.debug_writer {
                writeln!(debug_writer, "AST: {:#?}", ast)?;
            }
        } else {
            println!("AST: {:#?}", ast);
        }

        let result = evaluate(&ast, self)?;
        Ok(result)
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
        self.add_builtin_function(
            "_set_internal_side_effect_flag",
            Some(0),
            |ctx, _this, _args| {
                ctx._internal_side_effect_flag = true;
                Ok(NULL_VALUE)
            },
        );

        self.add_builtin_function("print", None, |ctx, _this, args| {
            print_values(&mut ctx.stdout, args)?;
            ctx.stdout.flush()?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("println", None, |ctx, _this, args| {
            print_values(&mut ctx.stdout, args)?;
            writeln!(ctx.stdout, "")?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("eprint", None, |ctx, _this, args| {
            print_values(&mut ctx.stderr, args)?;
            ctx.stderr.flush()?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("eprintln", None, |ctx, _this, args| {
            print_values(&mut ctx.stderr, args)?;
            writeln!(ctx.stderr, "")?;
            Ok(NULL_VALUE)
        });

        self.add_builtin_function("read_line", Some(0), |ctx, _this, _args| {
            let mut str = String::new();
            ctx.stdin.read_line(&mut str)?;
            str.pop();
            Ok(Value::String(str))
        });

        self.add_builtin_function("to_string", Some(1), |_ctx, _this, args| {
            Ok(Value::String(args[0].convert_to_string()))
        });

        self.add_builtin_function("parse_number", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::String)?;
            let Value::String(ref arg) = args[0] else {
                unreachable!()
            };
            let num = arg.parse().unwrap_or(f64::NAN);
            Ok(Value::Number(num))
        });

        // TODO: Scope functions to a separate namespace like `math`, so they can be used via
        // `math.is_nan(42)`
        self.add_builtin_function("is_nan", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Bool(arg.is_nan()))
        });

        self.add_builtin_function("is_infinite", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Bool(arg.is_infinite()))
        });

        self.add_builtin_function("is_finite", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Bool(arg.is_finite()))
        });

        self.add_builtin_function("sin", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.sin()))
        });
        self.add_builtin_function("cos", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.cos()))
        });
        self.add_builtin_function("tan", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.tan()))
        });
        self.add_builtin_function("asin", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.asin()))
        });
        self.add_builtin_function("acos", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.acos()))
        });
        self.add_builtin_function("atan", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.atan()))
        });
        self.add_builtin_function("atan2", Some(1), |_ctx, _this, args| {
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
        self.add_builtin_function("tanh", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.tanh()))
        });
        self.add_builtin_function("sinh", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.sinh()))
        });
        self.add_builtin_function("cosh", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.cosh()))
        });

        self.add_builtin_function("ln", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.ln()))
        });
        self.add_builtin_function("log2", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.log2()))
        });
        self.add_builtin_function("log10", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.log10()))
        });
        self.add_builtin_function("log", Some(2), |_ctx, _this, args| {
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

        self.add_builtin_function("abs", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.abs()))
        });
        self.add_builtin_function("min", Some(2), |_ctx, _this, args| {
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
        self.add_builtin_function("max", Some(2), |_ctx, _this, args| {
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
        self.add_builtin_function("floor", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.floor()))
        });
        self.add_builtin_function("ceil", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.ceil()))
        });
        self.add_builtin_function("round", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.round()))
        });

        self.add_builtin_function("sqrt", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.sqrt()))
        });
        self.add_builtin_function("exp", Some(1), |_ctx, _this, args| {
            args[0].assert_type(Type::Number)?;
            let Value::Number(arg) = args[0] else {
                unreachable!()
            };
            Ok(Value::Number(arg.exp()))
        });

        self.add_builtin_function("inspect", Some(1), |_ctx, _this, args| {
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
        F: Fn(&mut Context, Option<Value>, &[Value]) -> Result<Value, EvalError> + 'static,
    {
        self.set_var(
            name.clone(),
            Value::Function(Function::new_builtin(name, None, n_args, func)),
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
            ctx.set_var(name, Value::Function(func));
            NULL_VALUE
        }
        Ast::UnnamedFunction { arg_names, body } => {
            let func = Function::new_user_defined(None, arg_names.clone(), *body.clone())?;
            Value::Function(func)
        }
        Ast::MemberAccess {
            value: value_ast,
            member,
        } => {
            let value = evaluate(value_ast, ctx)?;
            value.get_member(member)?
        }
        Ast::Indexing {
            value: value_ast,
            index: index_ast,
        } => {
            let value = evaluate(value_ast, ctx)?;
            let index = evaluate(index_ast, ctx)?;
            value.get_index(index)?
        }
        Ast::Lines(lines) => {
            let mut result = NULL_VALUE;
            for line in lines {
                result = evaluate(line, ctx)?;
            }
            result
        }
        Ast::Null => NULL_VALUE,
        Ast::NumberLiteral(n) => Value::Number(*n),
        Ast::BooleanLiteral(b) => Value::Bool(*b),
        Ast::StringLiteral(s) => Value::String(s.clone()),
        Ast::ListLiteral(values) => {
            let values: Vec<_> = values
                .iter()
                .map(|ast| evaluate(ast, ctx))
                .collect::<Result<_, _>>()?;
            Value::List(Rc::new(RefCell::new(values)))
        }
        Ast::ObjectLiteral(key_value_pairs) => {
            let mut object = HashMap::new();
            for (key, value) in key_value_pairs {
                let value = evaluate(value, ctx)?;
                object.insert(key.clone(), value);
            }
            Value::Object(Rc::new(RefCell::new(object)))
        }
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
            NULL_VALUE
        }
        Ast::IndexingAssign { value, index, rhs } => {
            let value = evaluate(value, ctx)?;
            let index = evaluate(index, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            value.set_index(index, rval.clone())?;
            NULL_VALUE
        }
        Ast::MemberAssign { value, member, rhs } => {
            let value = evaluate(value, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            value.set_member(member, rval.clone())?;
            NULL_VALUE
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
