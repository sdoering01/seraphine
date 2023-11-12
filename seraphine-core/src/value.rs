use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    io::Write,
    rc::Rc,
};

use crate::{
    error::StdlibError, eval::Scope as EvaluatorScope, parser::Ast,
    runtime::common::RuntimeContext, vm::Scope as VmScope,
};

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

pub(crate) fn print_potentially_self_referential(
    value: &Value,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    print_potentially_self_referential_recursive(value, f, &mut Vec::new())
}

pub(crate) fn print_potentially_self_referential_recursive(
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

    pub(crate) fn assert_type(&self, expected: Type) -> Result<(), StdlibError> {
        let got = self.get_type();
        if got != expected {
            return Err(StdlibError::WrongType { expected, got });
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

    pub(crate) fn get_member(&self, member: &str) -> Result<Value, StdlibError> {
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
                _ => Err(StdlibError::NoSuchMember {
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
                                return Err(StdlibError::IndexOutOfBounds {
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
                _ => Err(StdlibError::NoSuchMember {
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
                    Err(StdlibError::NoSuchMember {
                        r#type: Type::Object,
                        member_name: member.to_string(),
                    })
                }
            }
            _ => Err(StdlibError::NoSuchMember {
                r#type: self.get_type(),
                member_name: member.to_string(),
            }),
        }
    }

    pub(crate) fn set_member(self, member: &str, value: Value) -> Result<(), StdlibError> {
        match self {
            Value::Object(o) => {
                let mut obj = o.borrow_mut();
                obj.insert(member.to_string(), value);
                Ok(())
            }
            _ => {
                let error = format!("Cannot set member of type {}", self.get_type());
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn get_index(self, idx: Value) -> Result<Value, StdlibError> {
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
                    return Err(StdlibError::IndexOutOfBounds {
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
                    Err(StdlibError::NoSuchMember {
                        r#type: Type::Object,
                        member_name: index,
                    })
                }
            }
            _ => {
                let error = format!("Cannot index value of type {}", self.get_type());
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn set_index(self, idx: Value, value: Value) -> Result<(), StdlibError> {
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
                    return Err(StdlibError::IndexOutOfBounds {
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
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn make_iterator(self) -> Result<Rc<RefCell<dyn SeraphineIterator>>, StdlibError> {
        match self {
            Value::List(l) => Ok(Rc::new(RefCell::new(l.borrow().clone().into_iter()))),
            Value::Iterator(i) => Ok(i),
            _ => {
                let error = format!("Cannot iterate over value of type {}", self.get_type());
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn advance_iterator(&self) -> Result<Option<Value>, StdlibError> {
        match self {
            Value::Iterator(i) => Ok(i.borrow_mut().next()),
            _ => {
                let error = format!(
                    "Cannot advance type of {} since it is not an iterator",
                    self.get_type()
                );
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn add(self, rhs: Self) -> Result<Value, StdlibError> {
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
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn subtract(self, rhs: Self) -> Result<Value, StdlibError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l - r)),
            (l, r) => {
                let error = format!(
                    "Cannot subtract value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn multiply(self, rhs: Self) -> Result<Value, StdlibError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l * r)),
            (l, r) => {
                let error = format!(
                    "Cannot multiply value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn divide(self, rhs: Self) -> Result<Value, StdlibError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l / r)),
            (l, r) => {
                let error = format!(
                    "Cannot divide value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn modulo(self, rhs: Self) -> Result<Value, StdlibError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l % r)),
            (l, r) => {
                let error = format!(
                    "Cannot modulo value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn power(self, rhs: Self) -> Result<Value, StdlibError> {
        use Value::*;
        match (self, rhs) {
            (Number(l), Number(r)) => Ok(Number(l.powf(r))),
            (l, r) => {
                let error = format!(
                    "Cannot raise power of value of type {} and value of type {}",
                    l.get_type(),
                    r.get_type()
                );
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn negate(self) -> Result<Value, StdlibError> {
        use Value::*;
        match self {
            Number(l) => Ok(Number(-l)),
            _ => {
                let error = format!("Cannot negate value of type {}", self.get_type());
                Err(StdlibError::TypeError(error))
            }
        }
    }

    pub(crate) fn bool_negate(self) -> Result<Value, StdlibError> {
        Ok(Value::Bool(!self.as_bool()))
    }

    pub(crate) fn inner_equal(&self, rhs: &Self) -> bool {
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

    pub(crate) fn equal(self, rhs: Self) -> Result<Value, StdlibError> {
        Ok(Value::Bool(self.inner_equal(&rhs)))
    }

    pub(crate) fn unequal(self, rhs: Self) -> Result<Value, StdlibError> {
        Ok(Value::Bool(!self.inner_equal(&rhs)))
    }

    pub(crate) fn less_than(self, rhs: Self) -> Result<Value, StdlibError> {
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
                return Err(StdlibError::TypeError(error));
            }
        };
        Ok(Bool(lt))
    }

    pub(crate) fn greater_than(self, rhs: Self) -> Result<Value, StdlibError> {
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
                return Err(StdlibError::TypeError(error));
            }
        };
        Ok(Bool(gt))
    }

    pub(crate) fn less_than_or_equal(self, rhs: Self) -> Result<Value, StdlibError> {
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
                return Err(StdlibError::TypeError(error));
            }
        };
        Ok(Bool(le))
    }

    pub(crate) fn greater_than_or_equal(self, rhs: Self) -> Result<Value, StdlibError> {
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
                return Err(StdlibError::TypeError(error));
            }
        };
        Ok(Bool(ge))
    }

    pub(crate) fn and(self, rhs: Value) -> Result<Value, StdlibError> {
        let result = self.as_bool() && rhs.as_bool();
        Ok(Value::Bool(result))
    }

    pub(crate) fn or(self, rhs: Value) -> Result<Value, StdlibError> {
        let result = self.as_bool() || rhs.as_bool();
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
        F: Fn(&mut RuntimeContext, Option<Value>, Vec<Value>) -> Result<Value, StdlibError>
            + 'static,
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
        parent_scope: EvaluatorScope,
    ) -> Result<Self, StdlibError> {
        let func_name = func_name.map(|name| name.to_string());

        let mut arg_set = BTreeSet::new();
        for name in &arg_names {
            if !arg_set.insert(name) {
                return Err(StdlibError::DuplicateArgName {
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
                parent_scope,
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

    pub(crate) fn get_arg_count(&self) -> Option<usize> {
        match self.kind.as_ref() {
            FunctionKind::Builtin { n_args, .. } => *n_args,
            FunctionKind::UserDefinedAst { arg_names, .. } => Some(arg_names.len()),
            FunctionKind::UserDefinedVm { param_count, .. } => Some(*param_count),
        }
    }

    pub(crate) fn is_equal(&self, other: &Function) -> bool {
        Rc::ptr_eq(&self.kind, &other.kind)
            && match (&self.receiver, &other.receiver) {
                (Some(l), Some(r)) => l.inner_equal(r),
                (None, None) => true,
                _ => false,
            }
    }

    pub(crate) fn with_this(self, this: Option<Value>) -> Self {
        Function {
            kind: self.kind,
            receiver: this.map(Box::new),
        }
    }
}

// TODO: Replace Context with something that can be shared across AST-based evaluation and the VM
pub(crate) type BuiltinFunctionClosure =
    Box<dyn Fn(&mut RuntimeContext, Option<Value>, Vec<Value>) -> Result<Value, StdlibError>>;

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
        parent_scope: EvaluatorScope,
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

pub(crate) fn print_values<W: Write>(to: &mut W, values: &[Value]) -> Result<(), StdlibError> {
    let mut str_iter = values.iter().map(|v| v.convert_to_string());
    if let Some(first) = str_iter.next() {
        write!(to, "{}", first)?;
    }
    for str in str_iter {
        write!(to, " {}", str)?;
    }
    Ok(())
}
