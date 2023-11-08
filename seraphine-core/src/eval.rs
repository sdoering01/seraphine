use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    io::{stderr, stdin, stdout, BufReader, Read, Write},
    rc::Rc,
};

use crate::{
    error::{EvalError, SeraphineError},
    parser::{parse, Ast},
    stdlib::{get_standard_functions, get_standard_variables},
    tokenizer::tokenize,
    runtime::common::RuntimeContext,
    vm::Scope as VmScope,
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
    Iterator,
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
            Iterator => write!(f, "iterator"),
        }
    }
}

pub trait SeraphineIterator: Iterator<Item = Value> + Debug {}

impl<T> SeraphineIterator for T where T: Iterator<Item = Value> + Debug {}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Number(f64),
    Bool(bool),
    String(String),
    Function(Function),
    List(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<BTreeMap<String, Value>>>),
    Iterator(Rc<RefCell<dyn SeraphineIterator>>),
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
            Iterator(i) => write!(f, "iterator at {:?}", i.as_ptr()),
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
    pub fn get_type(&self) -> Type {
        use Value::*;
        match self {
            Null => Type::Null,
            Number(..) => Type::Number,
            Bool(..) => Type::Bool,
            String(..) => Type::String,
            Function(..) => Type::Function,
            List(..) => Type::List,
            Object(..) => Type::Object,
            Iterator(..) => Type::Iterator,
        }
    }

    pub(crate) fn assert_type(&self, expected: Type) -> Result<(), EvalError> {
        let got = self.get_type();
        if got != expected {
            return Err(EvalError::WrongType { expected, got });
        }
        Ok(())
    }

    pub(crate) fn convert_to_string(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            other => other.to_string(),
        }
    }

    pub(crate) fn as_bool(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Number(n) => *n != 0.0,
            Value::Bool(b) => *b,
            Value::String(s) => !s.is_empty(),
            Value::Function(..) => true,
            Value::List(lst) => !lst.borrow().is_empty(),
            Value::Object(obj) => !obj.borrow().is_empty(),
            Value::Iterator(_) => true,
        }
    }

    pub(crate) fn call(&self, eval: &mut Evaluator, args: Vec<Value>) -> Result<Value, EvalError> {
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
                func.call(eval, args)
            }
            other => {
                let error = format!("Cannot call value of type {}", other.get_type());
                Err(EvalError::TypeError(error))
            }
        }
    }

    pub(crate) fn get_member(&self, member: &str) -> Result<Value, EvalError> {
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

    pub(crate) fn set_member(self, member: &str, value: Value) -> Result<(), EvalError> {
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

    pub(crate) fn get_index(self, idx: Value) -> Result<Value, EvalError> {
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

    pub(crate) fn set_index(self, idx: Value, value: Value) -> Result<(), EvalError> {
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

    pub(crate) fn make_iterator(self) -> Result<Rc<RefCell<dyn SeraphineIterator>>, EvalError> {
        match self {
            Value::List(l) => Ok(Rc::new(RefCell::new(l.borrow().clone().into_iter()))),
            Value::Iterator(i) => Ok(i),
            _ => {
                let error = format!("Cannot iterate over value of type {}", self.get_type());
                Err(EvalError::TypeError(error))
            }
        }
    }

    pub(crate) fn advance_iterator(&self) -> Result<Option<Value>, EvalError> {
        match self {
            Value::Iterator(i) => Ok(i.borrow_mut().next()),
            _ => {
                let error = format!(
                    "Cannot advance type of {} since it is not an iterator",
                    self.get_type()
                );
                Err(EvalError::TypeError(error))
            }
        }
    }

    pub(crate) fn add(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn subtract(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn multiply(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn divide(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn modulo(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn power(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn negate(self) -> Result<Value, EvalError> {
        use Value::*;
        match self {
            Number(l) => Ok(Number(-l)),
            _ => {
                let error = format!("Cannot negate value of type {}", self.get_type());
                Err(EvalError::TypeError(error))
            }
        }
    }

    pub(crate) fn bool_negate(self) -> Result<Value, EvalError> {
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

    pub(crate) fn equal(self, rhs: Self) -> Result<Value, EvalError> {
        Ok(Value::Bool(self.inner_equal(&rhs)))
    }

    pub(crate) fn unequal(self, rhs: Self) -> Result<Value, EvalError> {
        Ok(Value::Bool(!self.inner_equal(&rhs)))
    }

    pub(crate) fn less_than(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn greater_than(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn less_than_or_equal(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn greater_than_or_equal(self, rhs: Self) -> Result<Value, EvalError> {
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

    pub(crate) fn and(
        self,
        rhs_evaluator: impl FnOnce() -> Result<Self, EvalError>,
    ) -> Result<Value, EvalError> {
        let result = self.as_bool() && rhs_evaluator()?.as_bool();
        Ok(Value::Bool(result))
    }

    pub(crate) fn or(
        self,
        rhs_evaluator: impl FnOnce() -> Result<Self, EvalError>,
    ) -> Result<Value, EvalError> {
        let result = self.as_bool() || rhs_evaluator()?.as_bool();
        Ok(Value::Bool(result))
    }
}

pub const NULL_VALUE: Value = Value::Null;

#[derive(Clone)]
pub struct Function {
    pub kind: Rc<FunctionKind>,
    pub receiver: Option<Box<Value>>,
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
        F: Fn(&mut RuntimeContext, Option<Value>, Vec<Value>) -> Result<Value, EvalError> + 'static,
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

    pub fn new_user_defined_ast(
        func_name: Option<&str>,
        arg_names: Vec<String>,
        body: Ast,
    ) -> Result<Self, EvalError> {
        let func_name = func_name.map(|name| name.to_string());

        let mut arg_set = BTreeSet::new();
        for name in &arg_names {
            if !arg_set.insert(name) {
                return Err(EvalError::DuplicateArgName {
                    func_name,
                    arg_name: name.clone(),
                });
            }
        }

        Ok(Function {
            kind: Rc::new(FunctionKind::UserDefinedAst {
                name: func_name,
                arg_names,
                body,
            }),
            receiver: None,
        })
    }

    pub fn new_user_defined_vm(
        name: Option<impl Into<String>>,
        param_count: usize,
        entrypoint: usize,
        parent_scope: VmScope,
    ) -> Function {
        Function {
            kind: Rc::new(FunctionKind::UserDefinedVm {
                param_count,
                entrypoint,
                name: name.map(|n| n.into()),
                parent_scope,
            }),
            receiver: None,
        }
    }

    pub fn get_name(&self) -> Option<String> {
        match self.kind.as_ref() {
            FunctionKind::Builtin { name, .. } => Some(name.clone()),
            FunctionKind::UserDefinedAst { name, .. }
            | FunctionKind::UserDefinedVm { name, .. } => name.clone(),
        }
    }

    pub fn call(&self, eval: &mut Evaluator, args: Vec<Value>) -> Result<Value, EvalError> {
        let receiver = self.receiver.as_ref().map(|r| r.as_ref().to_owned());

        match self.kind.as_ref() {
            FunctionKind::Builtin { func, .. } => func(&mut eval.ctx, receiver, args),
            FunctionKind::UserDefinedAst {
                arg_names, body, ..
            } => {
                if eval.call_stack.len() >= CALL_STACK_SIZE_LIMIT - 1 {
                    return Err(EvalError::CallStackOverflow);
                }

                let mut scope = Scope::new();
                if let Some(recv) = receiver {
                    scope.set_var("this", recv);
                }
                for (name, value) in arg_names.iter().zip(args.into_iter()) {
                    scope.set_var(name, value);
                }

                if let Some(function_scope) = eval.function_scope.take() {
                    eval.call_stack.push(function_scope);
                }
                eval.function_scope = Some(scope);

                let call_result = match evaluate(body, eval) {
                    Err(EvalError::InternalControlFlow(ControlFlow::Return(val))) => Ok(val),
                    Err(EvalError::InternalControlFlow(ControlFlow::Continue)) => {
                        Err(EvalError::ContinueOutsideOfLoop)
                    }
                    Err(EvalError::InternalControlFlow(ControlFlow::Break)) => {
                        Err(EvalError::BreakOutsideOfLoop)
                    }
                    other => other,
                };
                eval.function_scope = eval.call_stack.pop();
                call_result
            }
            FunctionKind::UserDefinedVm { .. } => {
                return Err(EvalError::GenericError(
                    "got user defined vm function in AST walking mode".to_string(),
                ));
            }
        }
    }

    pub(crate) fn get_arg_count(&self) -> Option<usize> {
        match self.kind.as_ref() {
            FunctionKind::Builtin { n_args, .. } => *n_args,
            FunctionKind::UserDefinedAst { arg_names, .. } => Some(arg_names.len()),
            FunctionKind::UserDefinedVm { param_count, .. } => Some(*param_count),
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

// TODO: Replace Context with something that can be shared across AST-based evaluation and the VM
pub(crate) type BuiltinFunctionClosure =
    Box<dyn Fn(&mut RuntimeContext, Option<Value>, Vec<Value>) -> Result<Value, EvalError>>;

pub enum FunctionKind {
    Builtin {
        name: String,
        n_args: Option<usize>,
        func: BuiltinFunctionClosure,
    },
    UserDefinedAst {
        name: Option<String>,
        arg_names: Vec<String>,
        body: Ast,
    },
    UserDefinedVm {
        param_count: usize,
        entrypoint: usize,
        name: Option<String>,
        parent_scope: VmScope,
    },
}

impl Debug for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Builtin { name, .. } => {
                write!(f, "built-in function '{}'", name)
            }
            FunctionKind::UserDefinedVm { name, .. }
            | FunctionKind::UserDefinedAst { name, .. } => {
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
    variables: BTreeMap<String, Value>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: BTreeMap::new(),
        }
    }

    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.variables.get(name).cloned()
    }

    pub fn set_var(&mut self, name: impl Into<String>, val: Value) {
        self.variables.insert(name.into(), val);
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
            debug_writer: Some(Box::new(stdout())),
            standard_variables: true,
            standard_functions: true,
        }
    }
}

impl EvaluatorBuilder {
    pub fn build(self) -> Evaluator {
        let mut eval = Evaluator {
            global_scope: Scope::new(),
            function_scope: None,
            call_stack: Vec::new(),
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

pub struct Evaluator {
    global_scope: Scope,
    function_scope: Option<Scope>,
    call_stack: Vec<Scope>,
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
        if !cfg!(test) {
            if let Some(debug_writer) = &mut self.ctx.debug_writer {
                writeln!(debug_writer, "Tokens: {:?}", tokens)?;
            }
        } else {
            println!("Tokens: {:?}", tokens);
        }

        let ast = parse(&tokens)?;

        #[cfg(debug_assertions)]
        if !cfg!(test) {
            if let Some(debug_writer) = &mut self.ctx.debug_writer {
                writeln!(debug_writer, "AST: {:#?}", ast)?;
            }
        } else {
            println!("AST: {:#?}", ast);
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

pub fn evaluate(ast: &Ast, eval: &mut Evaluator) -> Result<Value, EvalError> {
    let result = match ast {
        Ast::FunctionDefinition {
            name,
            arg_names,
            body,
        } => {
            let func = Function::new_user_defined_ast(
                Some(name.as_str()),
                arg_names.clone(),
                *body.clone(),
            )?;
            eval.set_var(name, Value::Function(func));
            NULL_VALUE
        }
        Ast::UnnamedFunction { arg_names, body } => {
            let func = Function::new_user_defined_ast(None, arg_names.clone(), *body.clone())?;
            Value::Function(func)
        }
        Ast::MemberAccess {
            value: value_ast,
            member,
        } => {
            let value = evaluate(value_ast, eval)?;
            value.get_member(member)?
        }
        Ast::Indexing {
            value: value_ast,
            index: index_ast,
        } => {
            let value = evaluate(value_ast, eval)?;
            let index = evaluate(index_ast, eval)?;
            value.get_index(index)?
        }
        Ast::Lines(lines) => {
            let mut result = NULL_VALUE;
            for line in lines {
                result = evaluate(line, eval)?;
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
                .map(|ast| evaluate(ast, eval))
                .collect::<Result<_, _>>()?;
            Value::List(Rc::new(RefCell::new(values)))
        }
        Ast::ObjectLiteral(key_value_pairs) => {
            let mut object = BTreeMap::new();
            for (key, value) in key_value_pairs {
                let value = evaluate(value, eval)?;
                object.insert(key.clone(), value);
            }
            Value::Object(Rc::new(RefCell::new(object)))
        }
        Ast::Variable(name) => eval
            .get_var(name)
            .ok_or_else(|| EvalError::VariableNotDefined(name.clone()))?,
        Ast::Add(lhs, rhs) => evaluate(lhs, eval)?.add(evaluate(rhs, eval)?)?,
        Ast::Subtract(lhs, rhs) => evaluate(lhs, eval)?.subtract(evaluate(rhs, eval)?)?,
        Ast::Multiply(lhs, rhs) => evaluate(lhs, eval)?.multiply(evaluate(rhs, eval)?)?,
        Ast::Divide(lhs, rhs) => evaluate(lhs, eval)?.divide(evaluate(rhs, eval)?)?,
        Ast::Modulo(lhs, rhs) => evaluate(lhs, eval)?.modulo(evaluate(rhs, eval)?)?,
        Ast::Power(lhs, rhs) => evaluate(lhs, eval)?.power(evaluate(rhs, eval)?)?,
        Ast::UnaryMinus(rhs) => evaluate(rhs, eval)?.negate()?,
        Ast::BooleanNegate(rhs) => evaluate(rhs, eval)?.bool_negate()?,
        Ast::Equality(lhs, rhs) => evaluate(lhs, eval)?.equal(evaluate(rhs, eval)?)?,
        Ast::Inequality(lhs, rhs) => evaluate(lhs, eval)?.unequal(evaluate(rhs, eval)?)?,
        Ast::LessThan(lhs, rhs) => evaluate(lhs, eval)?.less_than(evaluate(rhs, eval)?)?,
        Ast::GreaterThan(lhs, rhs) => evaluate(lhs, eval)?.greater_than(evaluate(rhs, eval)?)?,
        Ast::LessThanOrEqual(lhs, rhs) => {
            evaluate(lhs, eval)?.less_than_or_equal(evaluate(rhs, eval)?)?
        }
        Ast::GreaterThanOrEqual(lhs, rhs) => {
            evaluate(lhs, eval)?.greater_than_or_equal(evaluate(rhs, eval)?)?
        }
        Ast::And(lhs, rhs) => evaluate(lhs, eval)?.and(|| evaluate(rhs, eval))?,
        Ast::Or(lhs, rhs) => evaluate(lhs, eval)?.or(|| evaluate(rhs, eval))?,
        Ast::Brackets(inner) => evaluate(inner, eval)?,
        Ast::Assign(name, rhs) => {
            let rval = evaluate(rhs, eval)?;
            eval.set_var(name, rval.clone());
            NULL_VALUE
        }
        Ast::IndexingAssign { value, index, rhs } => {
            let value = evaluate(value, eval)?;
            let index = evaluate(index, eval)?;
            let rval = evaluate(rhs, eval)?;
            value.set_index(index, rval.clone())?;
            NULL_VALUE
        }
        Ast::MemberAssign { value, member, rhs } => {
            let value = evaluate(value, eval)?;
            let rval = evaluate(rhs, eval)?;
            value.set_member(member, rval.clone())?;
            NULL_VALUE
        }
        Ast::FunctionCall { value, args } => {
            let val = evaluate(value, eval)?;
            let args: Vec<_> = args
                .iter()
                .map(|ast| evaluate(ast, eval))
                .collect::<Result<_, _>>()?;
            val.call(eval, args)?
        }
        Ast::IfStatement {
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
        Ast::WhileLoop { condition, body } => {
            while evaluate(condition, eval)?.as_bool() {
                match evaluate(body, eval) {
                    Err(EvalError::InternalControlFlow(ControlFlow::Continue)) => continue,
                    Err(EvalError::InternalControlFlow(ControlFlow::Break)) => break,
                    e @ Err(_) => return e,
                    Ok(_) => (),
                }
            }
            NULL_VALUE
        }
        Ast::ForLoop {
            variable,
            iterable,
            body,
        } => {
            let iterable = evaluate(iterable, eval)?;
            for value in iterable.make_iterator()?.borrow_mut().into_iter() {
                eval.set_var(variable, value);
                match evaluate(body, eval) {
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
                Some(expr) => evaluate(expr, eval)?,
            };
            return Err(EvalError::InternalControlFlow(ControlFlow::Return(val)));
        }
    };

    Ok(result)
}

pub(crate) fn print_values<W: Write>(to: &mut W, values: &[Value]) -> Result<(), EvalError> {
    let mut str_iter = values.iter().map(|v| v.convert_to_string());
    if let Some(first) = str_iter.next() {
        write!(to, "{}", first)?;
    }
    for str in str_iter {
        write!(to, " {}", str)?;
    }
    Ok(())
}
