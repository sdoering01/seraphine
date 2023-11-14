use std::collections::BTreeSet;

use crate::{
    common::Span,
    parser::{Ast, AstKind},
};

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Equal,
    Unequal,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum PlaceholderInstructionKind {
    Placeholder,
    PushFunction {
        function_idx: usize,
        param_count: usize,
        name_idx: Option<usize>,
    },
    Break,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub span: Span,
}

impl Instruction {
    pub fn new(kind: InstructionKind, span: Span) -> Instruction {
        Instruction { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    InternalPlaceholder(PlaceholderInstructionKind),
    End,
    PushNull,
    PushNumber(f64),
    PushBool(bool),
    PushString(String),
    MakeList {
        n_elems: usize,
    },
    MakeObject {
        n_keys: usize,
    },
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    LoadVariable(usize),
    StoreVariable(usize),
    GetIndex,
    SetIndex,
    GetMember,
    SetMember,
    Jump(usize),
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    JumpIfTrueNoPop(usize),
    JumpIfFalseNoPop(usize),
    CastBool,
    PushFunction {
        param_count: usize,
        entrypoint: usize,
        name_idx: Option<usize>,
    },
    FunctionCall {
        arg_count: usize,
    },
    Return,
    MakeIterator,
    AdvanceIteratorJumpIfDrained(usize),
    SaveStackSize {
        slot_idx: usize,
    },
    TrimStackSize {
        slot_idx: usize,
    },
}

const PLACEHOLDER_INSTRUCTION_KIND: InstructionKind =
    InstructionKind::InternalPlaceholder(PlaceholderInstructionKind::Placeholder);

pub fn generate(ast: &Ast, code: &str, code_file_name: &str) -> Bytecode {
    let code_generator = CodeGenerator::new();
    code_generator.generate_bytecode(ast, code, code_file_name)
}

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub variable_names: Vec<String>,
    pub functions_start_idx: usize,
    pub stack_save_slots: usize,
    // Currently only used for printing contexts of errors
    pub code: String,
    pub code_file_name: String,
}

impl Bytecode {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Copy)]
enum InstructionContext {
    Script,
    Function(usize),
}

#[derive(Debug, Clone, Default)]
pub(crate) struct VariableLookupTable {
    variable_names: Vec<String>,
}

impl From<VariableLookupTable> for Vec<String> {
    fn from(value: VariableLookupTable) -> Self {
        value.variable_names
    }
}

impl From<Vec<String>> for VariableLookupTable {
    fn from(value: Vec<String>) -> Self {
        VariableLookupTable {
            variable_names: value,
        }
    }
}

impl VariableLookupTable {
    pub(crate) fn lookup_or_insert(&mut self, var: &str) -> usize {
        match self.lookup(var) {
            Some(idx) => idx,
            None => {
                self.variable_names.push(var.to_string());
                self.variable_names.len() - 1
            }
        }
    }

    pub(crate) fn lookup(&self, var: &str) -> Option<usize> {
        self.variable_names.iter().position(|v| v == var)
    }

    pub(crate) fn get_name(&self, idx: usize) -> &str {
        &self.variable_names[idx]
    }

    pub(crate) fn len(&self) -> usize {
        self.variable_names.len()
    }
}

#[derive(Debug, Clone)]
pub struct CodeGenerator {
    script_instructions: Vec<Instruction>,
    functions: Vec<Vec<Instruction>>,
    variable_names: VariableLookupTable,
    stack_save_slots: usize,

    current_instruction_context: InstructionContext,
    current_loop_continue_idx: Option<usize>,
    current_span: Span,
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator {
            script_instructions: Vec::new(),
            functions: Vec::new(),
            variable_names: VariableLookupTable::default(),
            stack_save_slots: 0,
            current_instruction_context: InstructionContext::Script,
            current_loop_continue_idx: None,
            current_span: Span { start: 0, len: 0 },
        }
    }

    pub fn generate_bytecode(mut self, ast: &Ast, code: &str, code_file_name: &str) -> Bytecode {
        self.generate(ast);

        // make sure `this` is in variable names, this makes things easier in the VM
        let _ = self.variable_names.lookup_or_insert("this");

        self.script_instructions
            .push(Instruction::new(InstructionKind::End, Span::default()));

        let script_len = self.script_instructions.len();
        let mut instructions = self.script_instructions;

        let mut current_function_entrypoint = script_len;
        let mut function_entrypoints = Vec::with_capacity(self.functions.len());
        for mut function_instructions in self.functions {
            function_entrypoints.push(current_function_entrypoint);
            // correct absolute jump positions in function instructions
            for instruction in &mut function_instructions {
                match instruction.kind {
                    InstructionKind::Jump(idx) => {
                        instruction.kind = InstructionKind::Jump(idx + current_function_entrypoint)
                    }
                    InstructionKind::JumpIfTrue(idx) => {
                        instruction.kind =
                            InstructionKind::JumpIfTrue(idx + current_function_entrypoint)
                    }
                    InstructionKind::JumpIfFalse(idx) => {
                        instruction.kind =
                            InstructionKind::JumpIfFalse(idx + current_function_entrypoint)
                    }
                    InstructionKind::JumpIfFalseNoPop(idx) => {
                        instruction.kind =
                            InstructionKind::JumpIfFalseNoPop(idx + current_function_entrypoint)
                    }
                    InstructionKind::JumpIfTrueNoPop(idx) => {
                        instruction.kind =
                            InstructionKind::JumpIfTrueNoPop(idx + current_function_entrypoint)
                    }
                    InstructionKind::AdvanceIteratorJumpIfDrained(idx) => {
                        instruction.kind = InstructionKind::AdvanceIteratorJumpIfDrained(
                            idx + current_function_entrypoint,
                        )
                    }
                    _ => (),
                }
            }
            current_function_entrypoint += function_instructions.len();
            instructions.append(&mut function_instructions);
        }

        // replace internal function definition placeholders
        for instruction in &mut instructions {
            if let InstructionKind::InternalPlaceholder(
                PlaceholderInstructionKind::PushFunction {
                    function_idx,
                    param_count,
                    name_idx,
                },
            ) = instruction.kind
            {
                instruction.kind = InstructionKind::PushFunction {
                    param_count,
                    entrypoint: function_entrypoints[function_idx],
                    name_idx,
                }
            }
        }

        Bytecode {
            instructions,
            variable_names: self.variable_names.into(),
            functions_start_idx: script_len,
            stack_save_slots: self.stack_save_slots,
            code: code.to_string(),
            code_file_name: code_file_name.to_string(),
        }
    }

    pub fn generate(&mut self, ast: &Ast) {
        let prev_span = self.current_span;
        self.current_span = ast.span;

        match &ast.kind {
            AstKind::Block(l) => {
                for line in l {
                    self.generate(line);
                }
            }
            AstKind::Brackets(ast) => {
                self.generate(ast);
            }
            op @ (AstKind::Add(lhs, rhs)
            | AstKind::Subtract(lhs, rhs)
            | AstKind::Multiply(lhs, rhs)
            | AstKind::Divide(lhs, rhs)
            | AstKind::Modulo(lhs, rhs)
            | AstKind::Power(lhs, rhs)
            | AstKind::Equality(lhs, rhs)
            | AstKind::Inequality(lhs, rhs)
            | AstKind::LessThan(lhs, rhs)
            | AstKind::GreaterThan(lhs, rhs)
            | AstKind::LessThanOrEqual(lhs, rhs)
            | AstKind::GreaterThanOrEqual(lhs, rhs)) => {
                self.generate(lhs);
                self.generate(rhs);

                let binary_op = match op {
                    AstKind::Add(_, _) => BinaryOp::Add,
                    AstKind::Subtract(_, _) => BinaryOp::Subtract,
                    AstKind::Multiply(_, _) => BinaryOp::Multiply,
                    AstKind::Divide(_, _) => BinaryOp::Divide,
                    AstKind::Modulo(_, _) => BinaryOp::Modulo,
                    AstKind::Power(_, _) => BinaryOp::Power,
                    AstKind::Equality(_, _) => BinaryOp::Equal,
                    AstKind::Inequality(_, _) => BinaryOp::Unequal,
                    AstKind::LessThan(_, _) => BinaryOp::LessThan,
                    AstKind::GreaterThan(_, _) => BinaryOp::GreaterThan,
                    AstKind::LessThanOrEqual(_, _) => BinaryOp::LessThanOrEqual,
                    AstKind::GreaterThanOrEqual(_, _) => BinaryOp::GreaterThanOrEqual,
                    _ => unreachable!(),
                };
                self.push_instruction(InstructionKind::BinaryOp(binary_op));
            }
            op @ (AstKind::And(lhs, rhs) | AstKind::Or(lhs, rhs)) => {
                self.generate(lhs);
                // Make sure that lhs always is always coerced to a bool, so short-circuiting
                // doesn't break language semantics
                self.push_instruction(InstructionKind::CastBool);
                self.push_instruction(PLACEHOLDER_INSTRUCTION_KIND);
                let short_circuit_jump_instruction = self.current_instructions().len() - 1;
                self.generate(rhs);
                let binary_op = match op {
                    AstKind::And(_, _) => BinaryOp::And,
                    AstKind::Or(_, _) => BinaryOp::Or,
                    _ => unreachable!(),
                };
                self.push_instruction(InstructionKind::BinaryOp(binary_op));
                let after_and_idx = self.current_instructions().len();
                // We want to leave the value on the stack, so the operator or the following
                // instructions can use it
                let jump_instruction_kind = match op {
                    AstKind::And(_, _) => InstructionKind::JumpIfFalseNoPop(after_and_idx),
                    AstKind::Or(_, _) => InstructionKind::JumpIfTrueNoPop(after_and_idx),
                    _ => unreachable!(),
                };
                self.current_instructions_mut()[short_circuit_jump_instruction].kind =
                    jump_instruction_kind;
            }
            AstKind::UnaryMinus(ast) => {
                self.generate(ast);
                self.push_instruction(InstructionKind::UnaryOp(UnaryOp::Negate));
            }
            AstKind::BooleanNegate(ast) => {
                self.generate(ast);
                self.push_instruction(InstructionKind::UnaryOp(UnaryOp::Not));
            }
            AstKind::Null => self.push_instruction(InstructionKind::PushNull),
            AstKind::NumberLiteral(n) => {
                self.push_instruction(InstructionKind::PushNumber(*n));
            }
            AstKind::BooleanLiteral(b) => {
                self.push_instruction(InstructionKind::PushBool(*b));
            }
            AstKind::StringLiteral(s) => {
                self.push_instruction(InstructionKind::PushString(s.clone()));
            }
            AstKind::ListLiteral(elems) => {
                let n_elems = elems.len();
                for elem in elems {
                    self.generate(elem);
                }
                self.push_instruction(InstructionKind::MakeList { n_elems });
            }
            AstKind::ObjectLiteral(kv_pairs) => {
                let n_keys = kv_pairs.len();
                for (key, value) in kv_pairs {
                    self.push_instruction(InstructionKind::PushString(key.clone()));
                    self.generate(value);
                }
                self.push_instruction(InstructionKind::MakeObject { n_keys });
            }
            AstKind::Variable(name) => {
                let idx = self.variable_names.lookup_or_insert(name);
                self.push_instruction(InstructionKind::LoadVariable(idx));
            }
            AstKind::Assign(name, value) => {
                self.generate(value);
                let idx = self.variable_names.lookup_or_insert(name);
                self.push_instruction(InstructionKind::StoreVariable(idx));
            }
            AstKind::Indexing { value, index } => {
                self.generate(value);
                self.generate(index);
                self.push_instruction(InstructionKind::GetIndex);
            }
            AstKind::IndexingAssign { value, index, rhs } => {
                self.generate(value);
                self.generate(index);
                self.generate(rhs);
                self.push_instruction(InstructionKind::SetIndex);
            }
            AstKind::MemberAccess { value, member } => {
                self.generate(value);
                self.push_instruction(InstructionKind::PushString(member.clone()));
                self.push_instruction(InstructionKind::GetMember);
            }
            AstKind::MemberAssign { value, member, rhs } => {
                self.generate(value);
                self.push_instruction(InstructionKind::PushString(member.clone()));
                self.generate(rhs);
                self.push_instruction(InstructionKind::SetMember);
            }
            AstKind::IfStatement {
                condition,
                if_body,
                else_body,
            } => {
                self.generate(condition);
                self.push_instruction(PLACEHOLDER_INSTRUCTION_KIND);
                let jump_to_else_instruction = self.current_instructions().len() - 1;
                self.generate(if_body);

                let else_start_idx;
                match else_body {
                    Some(else_body) => {
                        self.push_instruction(PLACEHOLDER_INSTRUCTION_KIND);
                        let jump_to_end_instruction = self.current_instructions().len() - 1;

                        else_start_idx = self.current_instructions().len();

                        self.generate(else_body);
                        let after_else_idx = self.current_instructions().len();
                        self.current_instructions_mut()[jump_to_end_instruction].kind =
                            InstructionKind::Jump(after_else_idx);
                    }
                    None => {
                        else_start_idx = self.current_instructions().len();
                    }
                }

                self.current_instructions_mut()[jump_to_else_instruction].kind =
                    InstructionKind::JumpIfFalse(else_start_idx);
            }
            AstKind::FunctionCall { value, args } => {
                self.generate(value);
                for arg in args {
                    self.generate(arg);
                }
                self.push_instruction(InstructionKind::FunctionCall {
                    arg_count: args.len(),
                });
            }
            AstKind::FunctionDefinition {
                name,
                arg_names,
                body,
            } => self.generate_function_definition(Some(name), arg_names, body),
            AstKind::UnnamedFunction { arg_names, body } => {
                self.generate_function_definition(None, arg_names, body)
            }
            AstKind::Return(value) => {
                if let InstructionContext::Script = self.current_instruction_context {
                    // TODO: Replace with Result
                    panic!("return used outside of function");
                }

                match value {
                    Some(ast) => self.generate(ast),
                    None => self.push_instruction(InstructionKind::PushNull),
                }
                self.push_instruction(InstructionKind::Return);
            }
            AstKind::WhileLoop { condition, body } => {
                let condition_start_idx = self.current_instructions().len();
                self.generate(condition);
                self.push_instruction(PLACEHOLDER_INSTRUCTION_KIND);
                let jump_after_loop_instruction = self.current_instructions().len() - 1;

                let body_start_idx = self.current_instructions().len();
                self.with_loop_continue_idx(Some(condition_start_idx), |self_| {
                    self_.generate(body)
                });

                self.push_instruction(InstructionKind::Jump(condition_start_idx));
                let after_loop_idx = self.current_instructions().len();
                self.current_instructions_mut()[jump_after_loop_instruction].kind =
                    InstructionKind::JumpIfFalse(after_loop_idx);
                self.replace_break_instructions(body_start_idx, after_loop_idx, after_loop_idx);
            }
            AstKind::ForLoop {
                variable,
                iterable,
                body,
            } => {
                self.generate(iterable);
                self.push_instruction(InstructionKind::MakeIterator);
                let slot_idx = self.new_stack_save_slot();
                self.push_instruction(InstructionKind::SaveStackSize { slot_idx });

                self.push_instruction(InstructionKind::TrimStackSize { slot_idx });
                let loop_start_idx = self.current_instructions().len() - 1;
                self.push_instruction(PLACEHOLDER_INSTRUCTION_KIND);
                let advance_iterator_instruction = self.current_instructions().len() - 1;
                let variable_idx = self.variable_names.lookup_or_insert(variable);
                self.push_instruction(InstructionKind::StoreVariable(variable_idx));

                self.with_loop_continue_idx(Some(loop_start_idx), |self_| self_.generate(body));

                self.push_instruction(InstructionKind::Jump(loop_start_idx));
                let after_loop_idx = self.current_instructions().len();
                self.current_instructions_mut()[advance_iterator_instruction].kind =
                    InstructionKind::AdvanceIteratorJumpIfDrained(after_loop_idx);
                self.replace_break_instructions(loop_start_idx, after_loop_idx, after_loop_idx);
            }
            AstKind::Continue => match self.current_loop_continue_idx {
                Some(idx) => self.push_instruction(InstructionKind::Jump(idx)),
                // TODO: Replace with Result
                None => panic!("continue used outside of loop"),
            },
            AstKind::Break => {
                match self.current_loop_continue_idx {
                    Some(_) => self.push_instruction(InstructionKind::InternalPlaceholder(
                        PlaceholderInstructionKind::Break,
                    )),
                    // TODO: Replace with Result
                    None => panic!("break used outside of loop"),
                }
            }
        };

        self.current_span = prev_span;
    }

    fn current_instructions(&self) -> &[Instruction] {
        match self.current_instruction_context {
            InstructionContext::Script => &self.script_instructions,
            InstructionContext::Function(idx) => &self.functions[idx],
        }
    }

    fn current_instructions_mut(&mut self) -> &mut Vec<Instruction> {
        match self.current_instruction_context {
            InstructionContext::Script => &mut self.script_instructions,
            InstructionContext::Function(idx) => &mut self.functions[idx],
        }
    }

    fn generate_function_definition(&mut self, name: Option<&str>, params: &[String], body: &Ast) {
        let mut param_set = BTreeSet::new();
        for param in params {
            if !param_set.insert(param) {
                // TODO: Replace with Result, since a user could trigger this panic with bad input
                panic!(
                    "duplicate parameter name {} when generating function {:?}",
                    param, name
                );
            }
        }

        let function_idx = self.generate_function_body(params, body);

        let placeholder_instruction =
            InstructionKind::InternalPlaceholder(PlaceholderInstructionKind::PushFunction {
                function_idx,
                param_count: params.len(),
                name_idx: name.map(|n| self.variable_names.lookup_or_insert(n)),
            });
        self.push_instruction(placeholder_instruction);

        if let Some(name) = name {
            let instruction =
                InstructionKind::StoreVariable(self.variable_names.lookup_or_insert(name));
            self.push_instruction(instruction);
        }
    }

    fn generate_function_body(&mut self, params: &[String], body: &Ast) -> usize {
        self.functions.push(Vec::new());
        let function_idx = self.functions.len() - 1;

        let prev_instruction_context = self.current_instruction_context;
        self.current_instruction_context = InstructionContext::Function(function_idx);

        // Params are expected on top of stack, when the function is called
        //
        // VM evaluates arguments in order and puts them on the stack. Due to the nature of a
        // stack, we have to pop them in reverse order.
        for param in params.iter().rev() {
            let instruction =
                InstructionKind::StoreVariable(self.variable_names.lookup_or_insert(param));
            self.push_instruction(instruction);
        }

        self.with_loop_continue_idx(None, |self_| self_.generate(body));

        let AstKind::Block(lines) = &body.kind else {
            panic!("tried generating function for AST that doesn't represent a block");
        };
        let should_push_null = match lines.last() {
            // Empty function body returns null implicitly
            None => true,
            Some(last_line) => returns_implicit_null(last_line),
        };
        if should_push_null {
            self.push_instruction(InstructionKind::PushNull);
        }
        // TODO: This could be omitted, when all possible branches end with an explicit return
        // statement. For now this just takes up extra space in the bytecode.
        self.push_instruction(InstructionKind::Return);

        self.current_instruction_context = prev_instruction_context;

        function_idx
    }

    fn push_instruction(&mut self, instruction_kind: InstructionKind) {
        let instruction = Instruction::new(instruction_kind, self.current_span);
        self.current_instructions_mut().push(instruction);
    }

    fn new_stack_save_slot(&mut self) -> usize {
        let new_slot_idx = self.stack_save_slots;
        self.stack_save_slots += 1;
        new_slot_idx
    }

    fn with_loop_continue_idx(
        &mut self,
        loop_continue_idx: Option<usize>,
        func: impl FnOnce(&mut CodeGenerator),
    ) {
        let prev_loop_continue_idx = self.current_loop_continue_idx;
        self.current_loop_continue_idx = loop_continue_idx;
        func(self);
        self.current_loop_continue_idx = prev_loop_continue_idx;
    }

    fn replace_break_instructions(
        &mut self,
        start_idx: usize,
        end_idx: usize,
        jump_destination: usize,
    ) {
        for instruction in &mut self.current_instructions_mut()[start_idx..end_idx] {
            if let InstructionKind::InternalPlaceholder(PlaceholderInstructionKind::Break) =
                instruction.kind
            {
                instruction.kind = InstructionKind::Jump(jump_destination);
            }
        }
    }
}

fn returns_implicit_null(ast: &Ast) -> bool {
    // List all variants explicitly, so behavior of new variants has to be checked explicitly
    match ast.kind {
        AstKind::Block(_)
        | AstKind::Assign(_, _)
        | AstKind::IndexingAssign { .. }
        | AstKind::MemberAssign { .. }
        | AstKind::FunctionDefinition { .. }
        | AstKind::IfStatement { .. }
        | AstKind::WhileLoop { .. }
        | AstKind::ForLoop { .. }
        | AstKind::Continue
        | AstKind::Break => true,

        // `return` returns a value, even though it is a statement
        AstKind::Return(_)
        | AstKind::Null
        | AstKind::NumberLiteral(_)
        | AstKind::BooleanLiteral(_)
        | AstKind::StringLiteral(_)
        | AstKind::ListLiteral(_)
        | AstKind::ObjectLiteral(_)
        | AstKind::Variable(_)
        | AstKind::Add(_, _)
        | AstKind::Subtract(_, _)
        | AstKind::Multiply(_, _)
        | AstKind::Divide(_, _)
        | AstKind::Modulo(_, _)
        | AstKind::Power(_, _)
        | AstKind::UnaryMinus(_)
        | AstKind::BooleanNegate(_)
        | AstKind::Equality(_, _)
        | AstKind::Inequality(_, _)
        | AstKind::LessThan(_, _)
        | AstKind::GreaterThan(_, _)
        | AstKind::LessThanOrEqual(_, _)
        | AstKind::GreaterThanOrEqual(_, _)
        | AstKind::And(_, _)
        | AstKind::Or(_, _)
        | AstKind::Brackets(_)
        | AstKind::FunctionCall { .. }
        | AstKind::MemberAccess { .. }
        | AstKind::Indexing { .. }
        | AstKind::UnnamedFunction { .. } => false,
    }
}
