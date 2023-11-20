use crate::{common::Span, error::DeserializeError};

use super::{BinaryOp, Bytecode, Instruction, InstructionKind, UnaryOp};

pub(super) fn deserialize(serialized_bytecode: Vec<u8>) -> Result<Bytecode, DeserializeError> {
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

    fn deserialize(&mut self) -> Result<Bytecode, DeserializeError> {
        let n_variables = self.deserialize_usize()?;
        if n_variables > (1 << 32) {
            return Err(DeserializeError::TooManyVariables(n_variables));
        }
        let variable_names = (0..n_variables)
            .map(|_| self.deserialize_string())
            .collect::<Result<_, _>>()?;

        let functions_start_idx = self.deserialize_usize()?;

        let stack_save_slots = self.deserialize_usize()?;

        let n_instructions = self.deserialize_usize()?;
        if n_variables > (1 << 32) {
            return Err(DeserializeError::TooManyInstructions(n_instructions));
        }
        let instructions = (0..n_instructions)
            .map(|_| self.deserialize_instruction())
            .collect::<Result<_, _>>()?;

        let code_file_name = self.deserialize_string()?;

        let code = self.deserialize_string()?;

        Ok(Bytecode {
            instructions,
            variable_names,
            functions_start_idx,
            stack_save_slots,
            code,
            code_file_name,
        })
    }

    fn take_bytes(&mut self, n_bytes: usize) -> Result<&[u8], DeserializeError> {
        let bytes_available = self.buf.len() - self.read_pointer;
        if bytes_available < n_bytes {
            return Err(DeserializeError::UnexpectedEndOfInput);
        }
        let start_idx = self.read_pointer;
        self.read_pointer += n_bytes;
        Ok(&self.buf[start_idx..self.read_pointer])
    }

    fn deserialize_string(&mut self) -> Result<String, DeserializeError> {
        let len = self.deserialize_usize()?;
        let bytes = self.take_bytes(len)?;
        String::from_utf8(bytes.to_owned())
            .map_err(|_| DeserializeError::InvalidUtf8String(bytes.to_owned()))
    }

    fn deserialize_u64(&mut self) -> Result<u64, DeserializeError> {
        let byte_array = self.take_bytes(8)?.try_into().unwrap();
        Ok(u64::from_le_bytes(byte_array))
    }

    fn deserialize_usize(&mut self) -> Result<usize, DeserializeError> {
        Ok(self.deserialize_u64()? as usize)
    }

    fn deserialize_f64(&mut self) -> Result<f64, DeserializeError> {
        let byte_array = self.take_bytes(8)?.try_into().unwrap();
        Ok(f64::from_le_bytes(byte_array))
    }

    fn deserialize_span(&mut self) -> Result<Span, DeserializeError> {
        let start = self.deserialize_usize()?;
        let len = self.deserialize_usize()?;
        Ok(Span::new(start, len))
    }

    fn deserialize_instruction(&mut self) -> Result<Instruction, DeserializeError> {
        let span = self.deserialize_span()?;
        let opcode = self.take_bytes(1)?[0];

        let kind = match opcode {
            0 => InstructionKind::PushNull,
            1 => {
                let num = self.deserialize_f64()?;
                InstructionKind::PushNumber(num)
            }
            2 => {
                let b = self.take_bytes(1)?[0] != 0;
                InstructionKind::PushBool(b)
            }
            3 => {
                let s = self.deserialize_string()?;
                InstructionKind::PushString(s)
            }
            4 => {
                let param_count = self.deserialize_usize()?;
                let entrypoint = self.deserialize_usize()?;
                let name_idx = match self.deserialize_u64()? {
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
                let n_elems = self.deserialize_usize()?;
                InstructionKind::MakeList { n_elems }
            }
            11 => {
                let n_keys = self.deserialize_usize()?;
                InstructionKind::MakeObject { n_keys }
            }
            20 => {
                let op = self.deserialize_unary_op()?;
                InstructionKind::UnaryOp(op)
            }
            21 => {
                let op = self.deserialize_binary_op()?;
                InstructionKind::BinaryOp(op)
            }
            30 => {
                let idx = self.deserialize_usize()?;
                InstructionKind::LoadVariable(idx)
            }
            31 => {
                let idx = self.deserialize_usize()?;
                InstructionKind::StoreVariable(idx)
            }
            40 => InstructionKind::GetIndex,
            41 => InstructionKind::SetIndex,
            42 => InstructionKind::GetMember,
            43 => InstructionKind::SetMember,
            50 => {
                let addr = self.deserialize_usize()?;
                InstructionKind::Jump(addr)
            }
            51 => {
                let addr = self.deserialize_usize()?;
                InstructionKind::JumpIfTrue(addr)
            }
            52 => {
                let addr = self.deserialize_usize()?;
                InstructionKind::JumpIfFalse(addr)
            }
            53 => {
                let addr = self.deserialize_usize()?;
                InstructionKind::JumpIfTrueNoPop(addr)
            }
            54 => {
                let addr = self.deserialize_usize()?;
                InstructionKind::JumpIfFalseNoPop(addr)
            }
            55 => InstructionKind::CastBool,
            60 => {
                let arg_count = self.deserialize_usize()?;
                InstructionKind::FunctionCall { arg_count }
            }
            61 => InstructionKind::Return,
            70 => InstructionKind::MakeIterator,
            71 => {
                let addr = self.deserialize_usize()?;
                InstructionKind::AdvanceIteratorJumpIfDrained(addr)
            }
            72 => {
                let slot_idx = self.deserialize_usize()?;
                InstructionKind::SaveStackSize { slot_idx }
            }
            73 => {
                let slot_idx = self.deserialize_usize()?;
                InstructionKind::TrimStackSize { slot_idx }
            }
            251 => InstructionKind::End,
            _ => {
                return Err(DeserializeError::UnknownOpCode(opcode));
            }
        };

        Ok(Instruction::new(kind, span))
    }

    fn deserialize_unary_op(&mut self) -> Result<UnaryOp, DeserializeError> {
        let op = match self.take_bytes(1)?[0] {
            0 => UnaryOp::Negate,
            1 => UnaryOp::Not,
            op => {
                return Err(DeserializeError::UnknownUnaryOpCode(op));
            }
        };

        Ok(op)
    }

    fn deserialize_binary_op(&mut self) -> Result<BinaryOp, DeserializeError> {
        let op = match self.take_bytes(1)?[0] {
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
            op => {
                return Err(DeserializeError::UnknownBinaryOpCode(op));
            }
        };

        Ok(op)
    }
}
