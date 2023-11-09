use std::{
    cell::RefCell,
    collections::BTreeMap,
    io::{stderr, stdin, stdout, BufReader},
    rc::Rc,
};

use crate::{
    bytecode::{BinaryOp, Bytecode, Instruction, UnaryOp, VariableLookupTable},
    error::VmError,
    eval::{Function, FunctionKind, Value},
    runtime::common::RuntimeContext,
    stdlib::{get_standard_functions, get_standard_variables},
};

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

    fn peek(&self) -> Result<&Value, VmError> {
        self.stack.last().ok_or(VmError::StackUnderflow)
    }

    fn peek_mut(&mut self) -> Result<&mut Value, VmError> {
        self.stack.last_mut().ok_or(VmError::StackUnderflow)
    }

    fn pop(&mut self) -> Result<Value, VmError> {
        self.stack.pop().ok_or(VmError::StackUnderflow)
    }

    fn pop_n(&mut self, n: usize) -> Result<Vec<Value>, VmError> {
        if n > self.stack.len() {
            return Err(VmError::StackUnderflow);
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

    fn trim(&mut self, new_len: usize) -> Result<(), VmError> {
        if self.stack.len() < new_len {
            return Err(VmError::StackUnderflow);
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
    pub fn new(bytecode: Bytecode) -> Vm {
        let mut variable_names = VariableLookupTable::from(bytecode.variable_names.clone());

        let mut standard_values = get_standard_variables();
        standard_values.append(&mut get_standard_functions());
        for (name, _val) in &standard_values {
            variable_names.lookup_or_insert(name);
        }

        let global_scope = Scope::with_len(variable_names.len());
        for (name, val) in standard_values {
            // Safety: Value name was inserted already, so the lookup can't fail
            let idx = variable_names.lookup(name).unwrap();
            global_scope.set(idx, val);
        }

        let Some(this_idx) = variable_names.lookup("this") else {
            // TODO: Replace with Result
            panic!("corrupt bytecode: `this` not in variable names");
        };

        let stack_save_slots = vec![0; bytecode.stack_save_slots];

        // TODO: Make configurable via builder
        let ctx = RuntimeContext::new(
            BufReader::new(Box::new(stdin())),
            Box::new(stdout()),
            Box::new(stderr()),
            Some(Box::new(stdout())),
        );

        Vm {
            bytecode,
            variable_names,
            stack: Stack::new(),
            scope: global_scope,
            instruction_pointer: 0,
            call_stack: Vec::new(),
            stack_save_slots,
            this_idx,
            ctx,
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        while self.instruction_pointer < self.bytecode.instructions.len() {
            let instruction = &self.bytecode.instructions[self.instruction_pointer];
            match instruction {
                Instruction::InternalPlaceholder(_) => {
                    panic!("corrupt bytecode -- internal placeholder instruction found")
                }
                Instruction::End => break,
                Instruction::UnaryOp(op) => {
                    let operand = self.stack.pop()?;
                    let result = match op {
                        UnaryOp::Negate => operand.negate()?,
                        UnaryOp::Not => operand.bool_negate()?,
                    };
                    self.stack.push(result);
                }
                Instruction::BinaryOp(op) => {
                    let rhs = self.stack.pop()?;
                    let lhs = self.stack.pop()?;
                    let result = match op {
                        BinaryOp::Add => lhs.add(rhs)?,
                        BinaryOp::Subtract => lhs.subtract(rhs)?,
                        BinaryOp::Multiply => lhs.multiply(rhs)?,
                        BinaryOp::Divide => lhs.divide(rhs)?,
                        BinaryOp::Modulo => lhs.modulo(rhs)?,
                        BinaryOp::Power => lhs.power(rhs)?,
                        BinaryOp::Equal => lhs.equal(rhs)?,
                        BinaryOp::Unequal => lhs.unequal(rhs)?,
                        BinaryOp::LessThan => lhs.less_than(rhs)?,
                        BinaryOp::GreaterThan => lhs.greater_than(rhs)?,
                        BinaryOp::LessThanOrEqual => lhs.less_than_or_equal(rhs)?,
                        BinaryOp::GreaterThanOrEqual => lhs.greater_than_or_equal(rhs)?,
                        // TODO: Short-circuiting with jumps
                        BinaryOp::And => lhs.and(|| Ok(rhs))?,
                        // TODO: Short-circuiting with jumps
                        BinaryOp::Or => lhs.or(|| Ok(rhs))?,
                    };
                    self.stack.push(result);
                }
                Instruction::PushNull => {
                    self.stack.push(Value::Null);
                }
                Instruction::PushNumber(n) => {
                    self.stack.push(Value::Number(*n));
                }
                Instruction::PushBool(b) => {
                    self.stack.push(Value::Bool(*b));
                }
                Instruction::PushString(s) => {
                    self.stack.push(Value::String(s.clone()));
                }
                Instruction::MakeList { n_elems } => {
                    let list = self.stack.pop_n(*n_elems)?;
                    self.stack.push(Value::List(Rc::new(RefCell::new(list))));
                }
                Instruction::MakeObject { n_keys } => {
                    let mut object = BTreeMap::new();

                    let mut kv_list = self.stack.pop_n(*n_keys * 2)?;
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
                Instruction::LoadVariable(idx) => match self.scope.get(*idx) {
                    Some(value) => self.stack.push(value.clone()),
                    None => {
                        return Err(VmError::UndefinedVariable(
                            self.variable_names.get_name(*idx).to_string(),
                        ))
                    }
                },
                Instruction::StoreVariable(idx) => {
                    let value = self.stack.pop()?;
                    self.scope.set(*idx, value);
                }
                Instruction::GetIndex => {
                    let index = self.stack.pop()?;
                    let value = self.stack.pop()?;
                    let result = value.get_index(index)?;
                    self.stack.push(result);
                }
                Instruction::SetIndex => {
                    let rhs = self.stack.pop()?;
                    let index = self.stack.pop()?;
                    let value = self.stack.pop()?;
                    value.set_index(index, rhs)?;
                }
                Instruction::GetMember => {
                    let Value::String(member) = self.stack.pop()? else {
                        unreachable!("corrupt bytecode -- non-string member")
                    };
                    let value = self.stack.pop()?;
                    let result = value.get_member(&member)?;
                    self.stack.push(result);
                }
                Instruction::SetMember => {
                    let rhs = self.stack.pop()?;
                    let Value::String(member) = self.stack.pop()? else {
                        unreachable!("corrupt bytecode -- non-string member")
                    };
                    let value = self.stack.pop()?;
                    value.set_member(&member, rhs)?;
                }
                Instruction::Jump(dest) => {
                    self.instruction_pointer = *dest;
                    continue;
                }
                Instruction::JumpIfTrue(dest) => {
                    let cond = self.stack.pop()?;
                    if cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                Instruction::JumpIfFalse(dest) => {
                    let cond = self.stack.pop()?;
                    if !cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                Instruction::JumpIfTrueNoPop(dest) => {
                    let cond = self.stack.peek()?;
                    if cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                Instruction::JumpIfFalseNoPop(dest) => {
                    let cond = self.stack.peek()?;
                    if !cond.as_bool() {
                        self.instruction_pointer = *dest;
                        continue;
                    }
                }
                Instruction::CastBool => {
                    let value = self.stack.pop()?;
                    self.stack.push(Value::Bool(value.as_bool()));
                }
                Instruction::PushFunction {
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
                Instruction::FunctionCall { arg_count } => {
                    let args = self.stack.pop_n(*arg_count)?;
                    let callable = self.stack.pop()?;

                    // TODO: Replace with Result
                    let Value::Function(func) = callable else {
                        panic!("cannot call value of type {}", callable.get_type());
                    };

                    if let Some(want_arg_count) = func.get_arg_count() {
                        if want_arg_count != *arg_count {
                            // TODO: Replace with Result
                            panic!(
                                "function wants {} parameters, but only {} were provided",
                                want_arg_count, arg_count
                            );
                        }
                    }

                    match func.kind.as_ref() {
                        // TODO: Replace with Result
                        FunctionKind::UserDefinedAst { .. } => {
                            panic!("cannot call user defined ast functions in vm")
                        }
                        // TODO: Implement this without `eval::Context`
                        FunctionKind::Builtin {
                            func: rust_func, ..
                        } => {
                            let receiver = func.receiver.as_ref().map(|r| r.as_ref().to_owned());

                            // TODO: Create CallStackItem, when runtime error messages with stack
                            // trace are implemented

                            let val = rust_func(&mut self.ctx, receiver, args)?;
                            self.stack.push(val);
                        }
                        FunctionKind::UserDefinedVm {
                            entrypoint,
                            parent_scope,
                            ..
                        } => {
                            let mut scope = Scope::with_parent(&parent_scope);
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
                            });

                            self.instruction_pointer = *entrypoint;
                            continue;
                        }
                    }
                }
                Instruction::Return => {
                    let value = self.stack.pop()?;

                    let mut previous_state = self
                        .call_stack
                        .pop()
                        // TODO: Replace with Result
                        .unwrap_or_else(|| panic!("return outside of function"));

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
                Instruction::MakeIterator => {
                    let value = self.stack.pop()?;
                    let iterator = value.make_iterator()?;
                    self.stack.push(Value::Iterator(iterator));
                }
                Instruction::AdvanceIteratorJumpIfDrained(dest) => {
                    match self.stack.peek_mut()?.advance_iterator()? {
                        Some(value) => self.stack.push(value),
                        None => {
                            self.instruction_pointer = *dest;
                            continue;
                        }
                    };
                }
                Instruction::SaveStackSize { slot_idx } => {
                    self.stack_save_slots[*slot_idx] = self.stack.len();
                }
                Instruction::TrimStackSize { slot_idx } => {
                    self.stack.trim(self.stack_save_slots[*slot_idx])?;
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
        bytecode,
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
        let instructions = bytecode::generate(&ast);
        println!("{:#?}", instructions);
        let mut vm = Vm::new(instructions);
        vm.run()?;
        let tos = vm.stack.pop()?;
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
}
