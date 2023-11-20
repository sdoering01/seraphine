mod deserialize;
mod serialize;

use crate::{
    common::Span,
    error::{DeserializeError, SerializeError},
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
    PushNull,
    PushNumber(f64),
    PushBool(bool),
    PushString(String),
    PushFunction {
        param_count: usize,
        entrypoint: usize,
        name_idx: Option<usize>,
    },
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
    InternalPlaceholder(PlaceholderInstructionKind),
    End,
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

    pub fn serialize(self) -> Result<Vec<u8>, SerializeError> {
        serialize::serialize(self)
    }

    pub fn deserialize(serialized_bytecode: Vec<u8>) -> Result<Self, DeserializeError> {
        deserialize::deserialize(serialized_bytecode)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{codegen::generate, parser::parse, tokenizer::tokenize};

    #[test]
    fn test_serialize_deserialize() {
        let code = r#"
            fn new_counter(initial_count) {{
                count: initial_count,
                previous_counts: [],
                increment() {
                    this.previous_counts.push(this.count)
                    this.count = this.count + 1
                },
                for_each_count(func) {
                    if (func) {
                        for (c in this.previous_counts) {
                            func(c)
                        }
                    }
                },
                get_initial_count() {
                    if (this.previous_counts.length == 0) {
                        return this.count
                    } else {
                        return this.previous_counts[0]
                    }
                },
            }}

            counter = new_counter(42)
            println("Counter at", counter.count)
            while (counter.count < 45) {
                counter.increment()
            }
            println("Counter at", counter.count)

            counter.for_each_count(fn (count) {
                println("Counter was", count)
            })

            println("Initial count was", counter.get_initial_count())
        "#;

        let tokens = tokenize(code).unwrap();
        let ast = parse(&tokens).unwrap();
        let bytecode = generate(&ast, code, "<test>").unwrap();
        let bytecode_repr_before = format!("{:?}", bytecode);
        let serialized_bytecode = bytecode.serialize().unwrap();
        let deserialized_bytecode = Bytecode::deserialize(serialized_bytecode).unwrap();
        let bytecode_repr_after = format!("{:?}", deserialized_bytecode);
        assert_eq!(
            bytecode_repr_before, bytecode_repr_after,
            "bytecode changed after serializing and deserializing"
        );
    }
}
