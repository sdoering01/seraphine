use crate::{common::Span, error::SerializeError};

use super::{BinaryOp, Bytecode, Instruction, InstructionKind, UnaryOp};

pub(super) fn serialize(bytecode: Bytecode) -> Result<Vec<u8>, SerializeError> {
    let mut buf = Vec::new();

    serialize_usize(&mut buf, bytecode.variable_names.len());
    for var_name in bytecode.variable_names {
        serialize_string(&mut buf, var_name);
    }

    serialize_usize(&mut buf, bytecode.functions_start_idx);

    serialize_usize(&mut buf, bytecode.stack_save_slots);

    serialize_usize(&mut buf, bytecode.instructions.len());
    for instruction in bytecode.instructions {
        serialize_instruction(&mut buf, instruction)?;
    }

    serialize_string(&mut buf, bytecode.code_file_name);

    serialize_string(&mut buf, bytecode.code);

    Ok(buf)
}

fn serialize_string(buf: &mut Vec<u8>, string: String) {
    let bytes = string.bytes();
    let len = bytes.len();
    serialize_usize(buf, len);
    push_bytes(buf, bytes);
}

fn serialize_usize(buf: &mut Vec<u8>, num: usize) {
    push_bytes(buf, (num as u64).to_le_bytes());
}

fn serialize_span(buf: &mut Vec<u8>, span: Span) {
    serialize_usize(buf, span.start);
    serialize_usize(buf, span.len);
}

fn push_bytes(buf: &mut Vec<u8>, bytes: impl IntoIterator<Item = u8>) {
    for byte in bytes {
        buf.push(byte);
    }
}

fn serialize_instruction(
    buf: &mut Vec<u8>,
    instruction: Instruction,
) -> Result<(), SerializeError> {
    serialize_span(buf, instruction.span);
    buf.push(instruction_opcode(&instruction));
    match instruction.kind {
        InstructionKind::PushNull => {}
        InstructionKind::PushNumber(n) => push_bytes(buf, n.to_le_bytes()),
        InstructionKind::PushBool(b) => buf.push(b as u8),
        InstructionKind::PushString(s) => serialize_string(buf, s),
        InstructionKind::PushFunction {
            param_count,
            entrypoint,
            name_idx,
        } => {
            serialize_usize(buf, param_count);
            serialize_usize(buf, entrypoint);
            match name_idx {
                Some(idx) => serialize_usize(buf, idx),
                None => push_bytes(buf, [0xff; 8]),
            }
        }
        InstructionKind::MakeList { n_elems } => serialize_usize(buf, n_elems),
        InstructionKind::MakeObject { n_keys } => serialize_usize(buf, n_keys),
        InstructionKind::UnaryOp(op) => serialize_unary_op(buf, op),
        InstructionKind::BinaryOp(op) => serialize_binary_op(buf, op),
        InstructionKind::LoadVariable(idx) => serialize_usize(buf, idx),
        InstructionKind::StoreVariable(idx) => serialize_usize(buf, idx),
        InstructionKind::GetIndex => {}
        InstructionKind::SetIndex => {}
        InstructionKind::GetMember => {}
        InstructionKind::SetMember => {}
        InstructionKind::Jump(addr) => serialize_usize(buf, addr),
        InstructionKind::JumpIfTrue(addr) => serialize_usize(buf, addr),
        InstructionKind::JumpIfFalse(addr) => serialize_usize(buf, addr),
        InstructionKind::JumpIfTrueNoPop(addr) => serialize_usize(buf, addr),
        InstructionKind::JumpIfFalseNoPop(addr) => serialize_usize(buf, addr),
        InstructionKind::CastBool => {}
        InstructionKind::FunctionCall { arg_count } => serialize_usize(buf, arg_count),
        InstructionKind::Return => {}
        InstructionKind::MakeIterator => {}
        InstructionKind::AdvanceIteratorJumpIfDrained(addr) => serialize_usize(buf, addr),
        InstructionKind::SaveStackSize { slot_idx } => serialize_usize(buf, slot_idx),
        InstructionKind::TrimStackSize { slot_idx } => serialize_usize(buf, slot_idx),
        InstructionKind::InternalPlaceholder(kind) => {
            return Err(SerializeError::InternalPlaceholder(kind));
        }
        InstructionKind::End => {}
    };

    Ok(())
}

fn serialize_unary_op(buf: &mut Vec<u8>, op: UnaryOp) {
    let serialized = match op {
        UnaryOp::Negate => 0,
        UnaryOp::Not => 1,
    };
    buf.push(serialized);
}

fn serialize_binary_op(buf: &mut Vec<u8>, op: BinaryOp) {
    let serialized = match op {
        BinaryOp::Add => 0,
        BinaryOp::Subtract => 1,
        BinaryOp::Multiply => 2,
        BinaryOp::Divide => 3,
        BinaryOp::Modulo => 4,
        BinaryOp::Power => 5,
        BinaryOp::Equal => 20,
        BinaryOp::Unequal => 21,
        BinaryOp::LessThan => 22,
        BinaryOp::GreaterThan => 23,
        BinaryOp::LessThanOrEqual => 24,
        BinaryOp::GreaterThanOrEqual => 25,
        BinaryOp::And => 40,
        BinaryOp::Or => 41,
    };
    buf.push(serialized);
}

fn instruction_opcode(instruction: &Instruction) -> u8 {
    match instruction.kind {
        InstructionKind::PushNull => 0,
        InstructionKind::PushNumber(_) => 1,
        InstructionKind::PushBool(_) => 2,
        InstructionKind::PushString(_) => 3,
        InstructionKind::PushFunction { .. } => 4,
        InstructionKind::MakeList { .. } => 10,
        InstructionKind::MakeObject { .. } => 11,
        InstructionKind::UnaryOp(_) => 20,
        InstructionKind::BinaryOp(_) => 21,
        InstructionKind::LoadVariable(_) => 30,
        InstructionKind::StoreVariable(_) => 31,
        InstructionKind::GetIndex => 40,
        InstructionKind::SetIndex => 41,
        InstructionKind::GetMember => 42,
        InstructionKind::SetMember => 43,
        InstructionKind::Jump(_) => 50,
        InstructionKind::JumpIfTrue(_) => 51,
        InstructionKind::JumpIfFalse(_) => 52,
        InstructionKind::JumpIfTrueNoPop(_) => 53,
        InstructionKind::JumpIfFalseNoPop(_) => 54,
        InstructionKind::CastBool => 55,
        InstructionKind::FunctionCall { .. } => 60,
        InstructionKind::Return => 61,
        InstructionKind::MakeIterator => 70,
        InstructionKind::AdvanceIteratorJumpIfDrained(_) => 71,
        InstructionKind::SaveStackSize { .. } => 72,
        InstructionKind::TrimStackSize { .. } => 73,
        InstructionKind::InternalPlaceholder(_) => 250,
        InstructionKind::End => 251,
    }
}
