use crate::common::Span;

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

pub(crate) const PLACEHOLDER_INSTRUCTION_KIND: InstructionKind =
    InstructionKind::InternalPlaceholder(PlaceholderInstructionKind::Placeholder);

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
