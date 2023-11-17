use std::{
    cell::RefCell,
    collections::BTreeMap,
    io::{stderr, stdin, stdout, BufReader},
    rc::Rc,
};

use crate::{
    bytecode::{BinaryOp, Bytecode, InstructionKind, UnaryOp},
    codegen::VariableLookupTable,
    common::Span,
    error::{FormattableWithContext, StdlibError, VmError},
    runtime::common::RuntimeContext,
    stdlib::{get_standard_functions, get_standard_variables},
    value::{Function, FunctionKind, Value},
};

// Convenience trait, so we don't have to write out the same convert call for each Stdlib Result
trait ConvertableToVmResult<Output> {
    fn convert(self, vm: &Vm) -> Result<Output, VmError>;
}

impl<T> ConvertableToVmResult<T> for Result<T, StdlibError> {
    fn convert(self, vm: &Vm) -> Result<T, VmError> {
        self.map_err(|error| VmError::StdlibError {
            error,
            span: vm.bytecode.instructions[vm.instruction_pointer].span,
        })
    }
}

#[derive(Debug)]
enum StackError {
    Underflow,
}

impl<T> ConvertableToVmResult<T> for Result<T, StackError> {
    fn convert(self, vm: &Vm) -> Result<T, VmError> {
        self.map_err(|error| {
            let instruction_idx = vm.instruction_pointer;
            match error {
                StackError::Underflow => VmError::StackUnderflow {
                    instruction: vm.bytecode.instructions[instruction_idx].clone(),
                    instruction_idx,
                },
            }
        })
    }
}

#[derive(Debug)]
struct Stack {
    stack: Vec<Value>,
}

impl From<Vec<Value>> for Stack {
    fn from(value: Vec<Value>) -> Self {
        Self { stack: value }
    }
}

impl Stack {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn peek(&self) -> Result<&Value, StackError> {
        self.stack.last().ok_or(StackError::Underflow)
    }

    fn pop(&mut self) -> Result<Value, StackError> {
        self.stack.pop().ok_or(StackError::Underflow)
    }

    fn pop_n(&mut self, n: usize) -> Result<Vec<Value>, StackError> {
        if n > self.stack.len() {
            return Err(StackError::Underflow);
        }

        let drain_start = self.stack.len() - n;
        Ok(self.stack.drain(drain_start..).collect())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn trim(&mut self, new_len: usize) -> Result<(), StackError> {
        if self.stack.len() < new_len {
            return Err(StackError::Underflow);
        }

        self.stack.drain(new_len..self.stack.len());
        Ok(())
    }
}

#[derive(Debug)]
struct InnerScope {
    variables: Vec<Option<Value>>,
    parent_scope: Option<Scope>,
}

#[derive(Debug, Clone)]
pub struct Scope(Rc<RefCell<InnerScope>>);

impl Scope {
    fn with_len(n: usize) -> Scope {
        Scope(Rc::new(RefCell::new(InnerScope {
            variables: vec![None; n],
            parent_scope: None,
        })))
    }

    fn with_parent(parent: &Scope) -> Scope {
        Scope(Rc::new(RefCell::new(InnerScope {
            variables: vec![None; parent.len()],
            parent_scope: Some(parent.clone()),
        })))
    }

    fn get(&self, idx: usize) -> Option<Value> {
        let self_borrow = self.0.borrow();
        self_borrow.variables[idx]
            .clone()
            .or_else(|| self_borrow.parent_scope.as_ref().and_then(|s| s.get(idx)))
    }

    fn set(&self, idx: usize, value: Value) {
        self.0.borrow_mut().variables[idx] = Some(value);
    }

    fn len(&self) -> usize {
        self.0.borrow().variables.len()
    }
}

#[derive(Debug)]
struct CallStackItem {
    prev_stack: Stack,
    prev_scope: Scope,
    continue_at_instruction: usize,
    stack_save_slots: Vec<usize>,
    caller_span: Span,
}

pub struct Vm {
    bytecode: Bytecode,
    variable_names: VariableLookupTable,
    stack: Stack,
    scope: Scope,
    instruction_pointer: usize,
    call_stack: Vec<CallStackItem>,
    stack_save_slots: Vec<usize>,
    this_idx: usize,
    ctx: RuntimeContext,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Result<Vm, VmError> {
        let variable_names = VariableLookupTable::from(bytecode.variable_names.clone());

        let global_scope = Scope::with_len(variable_names.len());

        let mut standard_values = get_standard_variables();
        standard_values.append(&mut get_standard_functions());
        for (name, val) in standard_values {
            if let Some(idx) = variable_names.lookup(name) {
                global_scope.set(idx, val);
            }
        }

        let Some(this_idx) = variable_names.lookup("this") else {
            return Err(VmError::BytecodeNoThis);
        };

        let stack_save_slots = vec![0; bytecode.stack_save_slots];

        // TODO: Make configurable via builder
        let ctx = RuntimeContext::new(
            BufReader::new(Box::new(stdin())),
            Box::new(stdout()),
            Box::new(stderr()),
            Some(Box::new(stdout())),
        );

        Ok(Vm {
            bytecode,
            variable_names,
            stack: Stack::new(),
            scope: global_scope,
            instruction_pointer: 0,
            call_stack: Vec::new(),
            stack_save_slots,
            this_idx,
            ctx,
        })
    }

    pub fn format_error(&self, error: VmError) -> String {
        error.format(&self.bytecode.code, &self.bytecode.code_file_name, true)
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        match self.inner_run() {
            result @ Ok(_) => result,
            Err(mut err) => {
                // Build up tracback for error, starting with most recent call
                while let Some(call_stack_item) = self.call_stack.pop() {
                    err = VmError::StdlibError {
                        error: StdlibError::FunctionCall(Box::new(err)),
                        span: call_stack_item.caller_span,
                    }
                }
                Err(err)
            }
        }
    }

    fn inner_run(&mut self) -> Result<(), VmError> {
        while self.instruction_pointer < self.bytecode.instructions.len() {
            let instruction = &self.bytecode.instructions[self.instruction_pointer];

            match &instruction.kind {
                InstructionKind::InternalPlaceholder(kind) => {
                    return Err(VmError::BytecodeInternalPlaceholder(kind.clone()));
                }
                InstructionKind::End => break,
                InstructionKind::UnaryOp(op) => {
                    let operand = self.stack.pop().convert(self)?;
                    let result = match op {
                        UnaryOp::Negate => operand.negate().convert(self)?,
                        UnaryOp::Not => operand.bool_negate().convert(self)?,
                    };
                    self.stack.push(result);
                }
                InstructionKind::BinaryOp(op) => {
                    let rhs = self.stack.pop().convert(self)?;
                    let lhs = self.stack.pop().convert(self)?;
                    let result = match op {
                        BinaryOp::Add => lhs.add(rhs).convert(self)?,
                        BinaryOp::Subtract => lhs.subtract(rhs).convert(self)?,
                        BinaryOp::Multiply => lhs.multiply(rhs).convert(self)?,
                        BinaryOp::Divide => lhs.divide(rhs).convert(self)?,
                        BinaryOp::Modulo => lhs.modulo(rhs).convert(self)?,
                        BinaryOp::Power => lhs.power(rhs).convert(self)?,
                        BinaryOp::Equal => lhs.equal(rhs).convert(self)?,
                        BinaryOp::Unequal => lhs.unequal(rhs).convert(self)?,
                        BinaryOp::LessThan => lhs.less_than(rhs).convert(self)?,
                        BinaryOp::GreaterThan => lhs.greater_than(rhs).convert(self)?,
                        BinaryOp::LessThanOrEqual => lhs.less_than_or_equal(rhs).convert(self)?,
                        BinaryOp::GreaterThanOrEqual => {
                            lhs.greater_than_or_equal(rhs).convert(self)?
                        }
                        BinaryOp::And => lhs.and(rhs).convert(self)?,
                        BinaryOp::Or => lhs.or(rhs).convert(self)?,
                    };
                    self.stack.push(result);
                }
                InstructionKind::PushNull => {
                    self.stack.push(Value::Null);
                }
                InstructionKind::PushNumber(n) => {
                    self.stack.push(Value::Number(*n));
                }
                InstructionKind::PushBool(b) => {
                    self.stack.push(Value::Bool(*b));
                }
                InstructionKind::PushString(s) => {
                    self.stack.push(Value::String(s.clone()));
                }
                InstructionKind::MakeList { n_elems } => {
                    let list = self.stack.pop_n(*n_elems).convert(self)?;
                    self.stack.push(Value::List(Rc::new(RefCell::new(list))));
                }
                InstructionKind::MakeObject { n_keys } => {
                    let mut object = BTreeMap::new();

                    let mut kv_list = self.stack.pop_n(*n_keys * 2).convert(self)?;
                    kv_list.reverse();

                    assert_eq!(kv_list.len(), *n_keys * 2);
                    for _ in 0..*n_keys {
                        let key_value = kv_list.pop().unwrap();
                        let Value::String(key) = key_value else {
                            unreachable!("corrupt bytecode -- non-string object key")
                        };
                        let value = kv_list.pop().unwrap();
                        object.insert(key, value);
                    }
                    self.stack
                        .push(Value::Object(Rc::new(RefCell::new(object))));
                }
                InstructionKind::LoadVariable(idx) => match self.scope.get(*idx) {
                    Some(value) => self.stack.push(value.clone()),
                    None => {
                        return Err(VmError::UndefinedVariable {
                            name: self.variable_names.get_name(*idx).to_string(),
                            span: instruction.span,
                        })
                    }
                },
                InstructionKind::StoreVariable(idx) => {
                    let value = self.stack.pop().convert(self)?;
                    self.scope.set(*idx, value);
                }
                InstructionKind::GetIndex => {
                    let index = self.stack.pop().convert(self)?;
                    let value = self.stack.pop().convert(self)?;
                    let result = value.get_index(index).convert(self)?;
                    self.stack.push(result);
                }
                InstructionKind::SetIndex => {
                    let rhs = self.stack.pop().convert(self)?;
                    let index = self.stack.pop().convert(self)?;
                    let value = self.stack.pop().convert(self)?;
                    value.set_index(index, rhs).convert(self)?;
                }
                InstructionKind::GetMember => {
                    let Value::String(member) = self.stack.pop().convert(self)? else {
                        unreachable!("corrupt bytecode -- non-string member")
                    };
                    let value = self.stack.pop().convert(self)?;
                    let result = value.get_member(&member).convert(self)?;
                    self.stack.push(result);
                }
                InstructionKind::SetMember => {
                    let rhs = self.stack.pop().convert(self)?;
                    let Value::String(member) = self.stack.pop().convert(self)? else {
                        unreachable!("corrupt bytecode -- non-string member")
                    };
                    let value = self.stack.pop().convert(self)?;
                    value.set_member(&member, rhs).convert(self)?;
                }
                InstructionKind::Jump(dest) => {
                    self.instruction_pointer = *dest;
                    continue;
                }
                InstructionKind::JumpIfTrue(dest) => {
                    let cond = self.stack.pop().convert(self)?;
                    if cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                InstructionKind::JumpIfFalse(dest) => {
                    let cond = self.stack.pop().convert(self)?;
                    if !cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                InstructionKind::JumpIfTrueNoPop(dest) => {
                    let cond = self.stack.peek().convert(self)?;
                    if cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                InstructionKind::JumpIfFalseNoPop(dest) => {
                    let cond = self.stack.peek().convert(self)?;
                    if !cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                InstructionKind::CastBool => {
                    let value = self.stack.pop().convert(self)?;
                    self.stack.push(Value::Bool(value.as_bool()));
                }
                InstructionKind::PushFunction {
                    param_count,
                    entrypoint,
                    name_idx,
                } => self
                    .stack
                    .push(Value::Function(Function::new_user_defined_vm(
                        name_idx.map(|idx| self.variable_names.get_name(idx).to_string()),
                        *param_count,
                        *entrypoint,
                        self.scope.clone(),
                    ))),
                InstructionKind::FunctionCall { arg_count } => {
                    let args = self.stack.pop_n(*arg_count).convert(self)?;
                    let callable = self.stack.pop().convert(self)?;

                    let Value::Function(func) = callable else {
                        let error_string =
                            format!("Cannot call value of type {}", callable.get_type());
                        return Err(VmError::StdlibError {
                            error: StdlibError::TypeError(error_string),
                            span: instruction.span,
                        });
                    };

                    if let Some(want_arg_count) = func.get_arg_count() {
                        if want_arg_count != *arg_count {
                            return Err(VmError::StdlibError {
                                error: StdlibError::FunctionWrongArgAmount {
                                    name: func.get_name(),
                                    expected: want_arg_count,
                                    got: *arg_count,
                                },
                                span: instruction.span,
                            });
                        }
                    }

                    match func.kind.as_ref() {
                        FunctionKind::UserDefinedAst { .. } => {
                            return Err(VmError::StdlibError {
                                error: StdlibError::GenericError(
                                    "Cannot call user defined ast function in VM".to_string(),
                                ),
                                span: instruction.span,
                            });
                        }
                        FunctionKind::Builtin {
                            func: rust_func, ..
                        } => {
                            let receiver = func.receiver.as_ref().map(|r| r.as_ref().to_owned());

                            // TODO: Create CallStackItem, when runtime error messages with stack
                            // trace are implemented

                            let val = rust_func(&mut self.ctx, receiver, args).convert(self)?;
                            self.stack.push(val);
                        }
                        FunctionKind::UserDefinedVm {
                            entrypoint,
                            parent_scope,
                            ..
                        } => {
                            let mut scope = Scope::with_parent(parent_scope);
                            if let Some(receiver) = func.receiver {
                                scope.set(self.this_idx, receiver.as_ref().clone());
                            }
                            std::mem::swap(&mut scope, &mut self.scope);

                            let mut stack = Stack::from(args);
                            std::mem::swap(&mut stack, &mut self.stack);

                            let mut stack_save_slots = vec![0; self.bytecode.stack_save_slots];
                            std::mem::swap(&mut stack_save_slots, &mut self.stack_save_slots);

                            self.call_stack.push(CallStackItem {
                                prev_stack: stack,
                                prev_scope: scope,
                                continue_at_instruction: self.instruction_pointer + 1,
                                stack_save_slots,
                                caller_span: instruction.span,
                            });

                            self.instruction_pointer = *entrypoint;
                            continue;
                        }
                    }
                }
                InstructionKind::Return => {
                    let value = self.stack.pop().convert(self)?;

                    let mut previous_state = self.call_stack.pop().ok_or(
                        VmError::BytecodeReturnOutsideOfFunctionCall {
                            span: instruction.span,
                        },
                    )?;

                    std::mem::swap(&mut previous_state.prev_stack, &mut self.stack);
                    std::mem::swap(&mut previous_state.prev_scope, &mut self.scope);
                    std::mem::swap(
                        &mut previous_state.stack_save_slots,
                        &mut self.stack_save_slots,
                    );
                    self.stack.push(value);
                    self.instruction_pointer = previous_state.continue_at_instruction;
                    continue;
                }
                InstructionKind::MakeIterator => {
                    let value = self.stack.pop().convert(self)?;
                    let iterator = value.make_iterator().convert(self)?;
                    self.stack.push(Value::Iterator(iterator));
                }
                InstructionKind::AdvanceIteratorJumpIfDrained(dest) => {
                    match self
                        .stack
                        .peek()
                        // Workaround, so that `self` can be borrowed in `convert`
                        .cloned()
                        .convert(self)?
                        .advance_iterator()
                        .convert(self)?
                    {
                        Some(value) => self.stack.push(value),
                        None => {
                            self.instruction_pointer = *dest;
                            continue;
                        }
                    };
                }
                InstructionKind::SaveStackSize { slot_idx } => {
                    self.stack_save_slots[*slot_idx] = self.stack.len();
                }
                InstructionKind::TrimStackSize { slot_idx } => {
                    self.stack
                        .trim(self.stack_save_slots[*slot_idx])
                        .convert(self)?;
                }
            }
            self.instruction_pointer += 1;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        codegen::generate,
        error::SeraphineError,
        macros::{
            assert_eq_bool, assert_eq_num, assert_eq_num_list, assert_eq_num_object, assert_eq_str,
            assert_null,
        },
        parser::parse,
        tokenizer::tokenize,
    };

    fn run_str(s: &str) -> Result<Value, SeraphineError> {
        let tokens = tokenize(s)?;
        let ast = parse(&tokens)?;
        let instructions = generate(&ast, s, "<test>")?;
        println!("{:#?}", instructions);
        let mut vm = Vm::new(instructions)?;
        vm.run()?;
        let tos = vm.stack.pop().unwrap();
        Ok(tos)
    }

    #[test]
    fn test_basic_arithmetic() {
        assert_eq_num!(run_str("2").unwrap(), 2.0);
        assert_eq_num!(run_str("2 - 3").unwrap(), -1.0);
        assert_eq_num!(run_str("2-3").unwrap(), -1.0);
        assert_eq_num!(run_str("2 + 2 * 2").unwrap(), 6.0);
        assert_eq_num!(run_str("3 * 2 * 5 + 10 / 5 - 8").unwrap(), 24.0);
        assert_eq_num!(run_str("1 - (3 + 2)").unwrap(), -4.0);
    }

    #[test]
    fn test_basic_boolean_arithmetic() {
        assert_eq_bool!(run_str("true").unwrap(), true);
        assert_eq_bool!(run_str("false").unwrap(), false);
        assert_eq_bool!(run_str("true && false").unwrap(), false);
        assert_eq_bool!(run_str("true || false").unwrap(), true);
        assert_eq_bool!(run_str("true && false || true").unwrap(), true);
        assert_eq_bool!(run_str("true || (false && true)").unwrap(), true);
        assert_eq_bool!(run_str("true || true && false").unwrap(), true);
    }

    #[test]
    fn test_push_null() {
        assert_null!(run_str("null").unwrap());
    }

    #[test]
    fn test_push_string() {
        assert_eq_str!(run_str(r#""hello""#).unwrap(), "hello");
    }

    #[test]
    fn test_make_list() {
        assert_eq_num_list!(run_str("[1, 2, 3]").unwrap(), [1.0, 2.0, 3.0]);
    }

    #[test]
    fn test_make_object() {
        assert_eq_num_object!(
            run_str(r#"{a: 1, b: (4 - 1) / 1.5, c: 3}"#).unwrap(),
            {"a" => 1.0, "b" => 2.0, "c" => 3.0}
        );
    }

    #[test]
    fn test_load_variable_undefined() {
        assert!(run_str("undefined_variable").is_err());
    }

    #[test]
    fn test_load_variable() {
        let code = "\
            a = 1
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 1.0);
    }

    #[test]
    fn test_get_index() {
        let code = "\
            a = [1, 2, 3]
            a[1]
        ";
        assert_eq_num!(run_str(code).unwrap(), 2.0);
    }

    #[test]
    fn test_set_index() {
        let code = "\
            a = [1, 2, 3]
            a[1] = 4
            a
        ";
        assert_eq_num_list!(run_str(code).unwrap(), [1.0, 4.0, 3.0]);
    }

    #[test]
    fn test_get_member() {
        let code = "\
            a = {a: 1, b: 2, c: 3}
            a.b
        ";
        assert_eq_num!(run_str(code).unwrap(), 2.0);
    }

    #[test]
    fn test_set_member() {
        let code = "\
            a = {a: 1, b: 2, c: 3}
            a.b = 4
            a
        ";
        assert_eq_num_object!(run_str(code).unwrap(), {"a" => 1.0, "b" => 4.0, "c" => 3.0});
    }

    #[test]
    fn test_plain_if_condition() {
        let code = "\
            a = 1
            if (true) {
                a = 2
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 2.0);
    }

    #[test]
    fn test_if_condition_with_else() {
        let code = "\
            a = 1
            if (true) {
                a = 2
            } else {
                a = 3
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 2.0);

        let code = "\
            a = 1
            if (false) {
                a = 2
            } else {
                a = 3
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 3.0);
    }

    #[test]
    fn test_if_condition_with_else_if() {
        let code = "\
            a = 1
            if (true) {
                a = 2
            } else if (true) {
                a = 3
            } else {
                a = 4
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 2.0);

        let code = "\
            a = 1
            if (false) {
                a = 2
            } else if (true) {
                a = 3
            } else {
                a = 4
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 3.0);

        let code = "\
            a = 1
            if (false) {
                a = 2
            } else if (false) {
                a = 3
            } else {
                a = 4
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 4.0);
    }

    #[test]
    fn test_while_loop() {
        let code = "\
            a = 0
            while (a < 10) {
                a = a + 1
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 10.0);
    }

    #[test]
    fn test_basic_function() {
        let code = "\
            fn add(a, b) {
                a + b
            }

            add(1, 2)
        ";
        assert_eq_num!(run_str(code).unwrap(), 3.0);
    }

    #[test]
    fn test_function_access_to_global_scope() {
        let code = "\
            global = 42

            fn get_global() {
                global
            }

            get_global()
        ";
        assert_eq_num!(run_str(code).unwrap(), 42.0);
    }

    #[test]
    fn test_closure() {
        let code = "\
            outer_var = 1
            fn outer() {
                outer_var = 2
                fn () {
                    outer_var
                }
            }

            outer()()
        ";
        assert_eq_num!(run_str(code).unwrap(), 2.0);

        let code = "\
            outer_var = 1
            fn outer() {
                fn () {
                    outer_var
                }
            }

            outer()()
        ";
        assert_eq_num!(run_str(code).unwrap(), 1.0);
    }

    #[test]
    fn test_closure_read_only_parent_scope_access() {
        let code = "\
            fn factory() {
                private = 1
                fn () {
                    private = private + 1
                    private
                }
            }

            get_private = factory()
            // Should always return 2 since outer `private` isn't mutated
            get_private() + get_private()
        ";
        assert_eq_num!(run_str(code).unwrap(), 4.0);
    }

    #[test]
    fn test_methods() {
        let code = "\
            obj = {
                num: 0,
                increment() {
                    this.num = this.num + 1
                }
            }
            obj.increment()
            obj.increment()
            obj.num
        ";
        assert_eq_num!(run_str(code).unwrap(), 2.0);
    }

    #[test]
    fn test_for_loop() {
        let code = "\
            result = 0
            for (i in [1, 2, 3]) {
                result = result + i
            }
            result
        ";
        assert_eq_num!(run_str(code).unwrap(), 6.0);
    }

    #[test]
    fn test_builtin_function_call() {
        let code = "\
            inspect(\"Hello, world!\")
        ";
        assert_eq_str!(run_str(code).unwrap(), "Hello, world!");
    }

    #[test]
    fn test_continue() {
        let code = "\
            continue
        ";
        assert!(run_str(code).is_err());

        let code = "\
            fn continue_func() {
                continue
            }

            for (i in range(3)) {
                continue_func()
            }
        ";
        assert!(run_str(code).is_err());

        let code = "\
            a = 0
            for (i in range(10)) {
                if (i % 2 == 0) {
                    continue
                }
                a = a + 1
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 5.0);

        let code = "\
            a = 0
            i = 0
            while (i < 10) {
                i = i + 1
                if (i % 2 == 0) {
                    continue
                }
                a = a + 1
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 5.0);

        let code = "\
            a = 0
            for (i in range(10)) {
                if (i % 2 == 0) {
                    continue
                }

                for (j in range(20)) {
                    if (j % 2 == 0) {
                        continue
                    }

                    for (k in range(50)) {
                        if (k % 2 == 0) {
                            continue
                        }

                        a = a + 1
                    }
                }
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 1250.0);

        let code = "\
            a = 0
            i = 0
            while (i < 10) {
                i = i + 1
                if (i % 2 == 0) {
                    continue
                }

                j = 0
                while (j < 20) {
                    j = j + 1
                    if (j % 2 == 0) {
                        continue
                    }

                    k = 0
                    while (k < 50) {
                        k = k + 1
                        if (k % 2 == 0) {
                            continue
                        }

                        a = a + 1
                    }
                }
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 1250.0);

        let code = "\
            fn recur(x) {
                if (x <= 1) {
                    return 1
                }

                ret = 0
                for (i in range(x)) {
                    if (i % 2 == 0) {
                        continue
                    }
                    ret = ret + recur(i)
                }
                ret
            }

            recur(10)
        ";
        assert_eq_num!(run_str(code).unwrap(), 16.0);
    }

    #[test]
    fn test_break() {
        let code = "\
            break
        ";
        assert!(run_str(code).is_err());

        let code = "\
            fn break_func() {
                break
            }

            for (i in range(3)) {
                break_func()
            }
        ";
        assert!(run_str(code).is_err());

        let code = "\
            a = 0
            for (i in range(10)) {
                if (i == 5) {
                    break
                }
                a = a + 1
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 5.0);

        let code = "\
            a = 0
            i = 0
            while (i < 10) {
                if (i == 5) {
                    break
                }
                a = a + 1
                i = i + 1
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 5.0);

        let code = "\
            a = 0
            for (i in range(10)) {
                if (i == 5) {
                    break
                }

                for (j in range(20)) {
                    if (j == 10) {
                        break
                    }

                    for (k in range(50)) {
                        if (k == 25) {
                            break
                        }

                        a = a + 1
                    }
                }
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 1250.0);

        let code = "\
            a = 0
            i = 0
            while (i < 10) {
                if (i == 5) {
                    break
                }
                i = i + 1

                j = 0
                while (j < 20) {
                    if (j == 10) {
                        break
                    }
                    j = j + 1

                    k = 0
                    while (k < 50) {
                        if (k == 25) {
                            break
                        }
                        k = k + 1

                        a = a + 1
                    }
                }
            }
            a
        ";
        assert_eq_num!(run_str(code).unwrap(), 1250.0);

        let code = "\
            fn recur(x) {
                if (x <= 1) {
                    return 1
                }

                ret = 0
                for (i in range(x)) {
                    if (i > x / 2) {
                        break
                    }
                    ret = ret + recur(i)
                }
                ret
            }

            recur(10)
        ";
        assert_eq_num!(run_str(code).unwrap(), 14.0);
    }

    #[test]
    fn test_traceback() {
        let code = concat!(
            "fn a() {\n",
            "   [] + 1\n",
            "}\n",
            "\n",
            "fn b() {\n",
            "   a()\n",
            "}\n",
            "\n",
            "b()\n",
        );

        let tokens = tokenize(code).unwrap();
        let ast = parse(&tokens).unwrap();
        let instructions = generate(&ast, code, "<test>").unwrap();
        let mut vm = Vm::new(instructions).unwrap();
        let err = vm.run().unwrap_err();
        let error_string = vm.format_error(err);

        // TODO: Use snapshot testing crate `insta` for this
        let want_error = concat!(
            "Traceback\n",
            "  <test>:9:1\n",
            "  9 | b()\n",
            "      ^\n",
            "  <test>:6:4\n",
            "  6 |    a()\n",
            "         ^\n",
            "  <test>:2:4\n",
            "  2 |    [] + 1\n",
            "         ^\n",
            "Cannot add value of type list and value of type number"
        );

        assert_eq!(error_string, want_error);
    }
}
