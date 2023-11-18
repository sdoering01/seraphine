use crate::common::Span;

use super::{BinaryOp, Bytecode, Instruction, InstructionKind, UnaryOp};

pub(super) fn deserialize(serialized_bytecode: Vec<u8>) -> Bytecode {
    let mut d = Deserializer::new(serialized_bytecode);
    d.deserialize()
}

struct Deserializer {
    buf: Vec<u8>,
    read_pointer: usize,
}

impl Deserializer {
    fn new(buf: Vec<u8>) -> Deserializer {
        Deserializer {
            buf,
            read_pointer: 0,
        }
    }

    fn deserialize(&mut self) -> Bytecode {
        let n_variables = self.deserialize_usize();
        if n_variables > (1 << 32) {
            panic!("corrupt bytecode -- too many variable names");
        }
        let variable_names = (0..n_variables)
            .map(|_| self.deserialize_string())
            .collect();

        let functions_start_idx = self.deserialize_usize();

        let stack_save_slots = self.deserialize_usize();

        let n_instructions = self.deserialize_usize();
        if n_variables > (1 << 32) {
            panic!("corrupt bytecode -- too many instructions");
        }
        let instructions = (0..n_instructions)
            .map(|_| self.deserialize_instruction())
            .collect();

        let code_file_name = self.deserialize_string();

        let code = self.deserialize_string();

        Bytecode {
            instructions,
            variable_names,
            functions_start_idx,
            stack_save_slots,
            code,
            code_file_name,
        }
    }

    fn take_bytes(&mut self, n_bytes: usize) -> &[u8] {
        let bytes_available = self.buf.len() - self.read_pointer;
        if bytes_available < n_bytes {
            // TODO: Replace with Result
            panic!("malformed bytecode -- unexpected ending");
        }
        let start_idx = self.read_pointer;
        self.read_pointer += n_bytes;
        &self.buf[start_idx..self.read_pointer]
    }

    fn deserialize_string(&mut self) -> String {
        let len = self.deserialize_usize();
        let bytes = self.take_bytes(len);
        // TODO: Replace with Result
        String::from_utf8(bytes.to_owned())
            .expect("malformed bytecode -- contains non UTF-8 string")
    }

    fn deserialize_u64(&mut self) -> u64 {
        let byte_array = self.take_bytes(8).try_into().unwrap();
        u64::from_le_bytes(byte_array)
    }

    fn deserialize_usize(&mut self) -> usize {
        self.deserialize_u64() as usize
    }

    fn deserialize_f64(&mut self) -> f64 {
        let byte_array = self.take_bytes(8).try_into().unwrap();
        f64::from_le_bytes(byte_array)
    }

    fn deserialize_span(&mut self) -> Span {
        let start = self.deserialize_usize();
        let len = self.deserialize_usize();
        Span::new(start, len)
    }

    fn deserialize_instruction(&mut self) -> Instruction {
        let span = self.deserialize_span();
        let opcode = self.take_bytes(1)[0];

        let kind = match opcode {
            0 => InstructionKind::PushNull,
            1 => {
                let num = self.deserialize_f64();
                InstructionKind::PushNumber(num)
            }
            2 => {
                let b = self.take_bytes(1)[0] != 0;
                InstructionKind::PushBool(b)
            }
            3 => {
                let s = self.deserialize_string();
                InstructionKind::PushString(s)
            }
            4 => {
                let param_count = self.deserialize_usize();
                let entrypoint = self.deserialize_usize();
                let name_idx = match self.deserialize_u64() {
                    u64::MAX => None,
                    idx => Some(idx as usize),
                };
                InstructionKind::PushFunction {
                    param_count,
                    entrypoint,
                    name_idx,
                }
            }
            10 => {
                let n_elems = self.deserialize_usize();
                InstructionKind::MakeList { n_elems }
            }
            11 => {
                let n_keys = self.deserialize_usize();
                InstructionKind::MakeObject { n_keys }
            }
            20 => {
                let op = self.deserialize_unary_op();
                InstructionKind::UnaryOp(op)
            }
            21 => {
                let op = self.deserialize_binary_op();
                InstructionKind::BinaryOp(op)
            }
            30 => {
                let idx = self.deserialize_usize();
                InstructionKind::LoadVariable(idx)
            }
            31 => {
                let idx = self.deserialize_usize();
                InstructionKind::StoreVariable(idx)
            }
            40 => InstructionKind::GetIndex,
            41 => InstructionKind::SetIndex,
            42 => InstructionKind::GetMember,
            43 => InstructionKind::SetMember,
            50 => {
                let addr = self.deserialize_usize();
                InstructionKind::Jump(addr)
            }
            51 => {
                let addr = self.deserialize_usize();
                InstructionKind::JumpIfTrue(addr)
            }
            52 => {
                let addr = self.deserialize_usize();
                InstructionKind::JumpIfFalse(addr)
            }
            53 => {
                let addr = self.deserialize_usize();
                InstructionKind::JumpIfTrueNoPop(addr)
            }
            54 => {
                let addr = self.deserialize_usize();
                InstructionKind::JumpIfFalseNoPop(addr)
            }
            55 => InstructionKind::CastBool,
            60 => {
                let arg_count = self.deserialize_usize();
                InstructionKind::FunctionCall { arg_count }
            }
            61 => InstructionKind::Return,
            70 => InstructionKind::MakeIterator,
            71 => {
                let addr = self.deserialize_usize();
                InstructionKind::AdvanceIteratorJumpIfDrained(addr)
            }
            72 => {
                let slot_idx = self.deserialize_usize();
                InstructionKind::SaveStackSize { slot_idx }
            }
            73 => {
                let slot_idx = self.deserialize_usize();
                InstructionKind::TrimStackSize { slot_idx }
            }
            251 => InstructionKind::End,
            _ => {
                // TODO: Replace with Result
                panic!("corrupt bytecode -- unknown opcode {}", opcode);
            }
        };

        Instruction::new(kind, span)
    }

    fn deserialize_unary_op(&mut self) -> UnaryOp {
        match self.take_bytes(1)[0] {
            0 => UnaryOp::Negate,
            1 => UnaryOp::Not,
            // TODO: Replace with Result
            op => panic!("corrupt bytecode -- unknown unary op {}", op),
        }
    }

    fn deserialize_binary_op(&mut self) -> BinaryOp {
        match self.take_bytes(1)[0] {
            0 => BinaryOp::Add,
            1 => BinaryOp::Subtract,
            2 => BinaryOp::Multiply,
            3 => BinaryOp::Divide,
            4 => BinaryOp::Modulo,
            5 => BinaryOp::Power,
            20 => BinaryOp::Equal,
            21 => BinaryOp::Unequal,
            22 => BinaryOp::LessThan,
            23 => BinaryOp::GreaterThan,
            24 => BinaryOp::LessThanOrEqual,
            25 => BinaryOp::GreaterThanOrEqual,
            40 => BinaryOp::And,
            41 => BinaryOp::Or,
            // TODO: Replace with Result
            op => panic!("corrupt bytecode -- unknown binary op {}", op),
        }
    }
}
