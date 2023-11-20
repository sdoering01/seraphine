use std::{
    fmt::{self, Debug, Display, Formatter},
    io,
};

use crate::{
    bytecode::{Instruction, PlaceholderInstructionKind},
    common::{Pos, Span},
    eval::ControlFlow,
    parser::AstKind,
    tokenizer::{Operator, Token, TokenKind},
    value::Type,
};

pub trait FormattableWithContext: Display {
    fn error_context(&self, input: &str, file_name: &str) -> Option<ErrorContext>;

    fn format(&self, input: &str, file_name: &str, _with_traceback: bool) -> String {
        match Self::error_context(self, input, file_name) {
            Some(error_ctx) => format_error(self.to_string(), error_ctx),
            None => self.to_string(),
        }
    }
}

#[derive(Debug)]
pub enum SeraphineError {
    TokenizeError(TokenizeError),
    ParseError(ParseError),
    EvalError(EvalError),
    CodegenError(CodegenError),
    VmError(VmError),
    SerializeError(SerializeError),
    DeserializeError(DeserializeError),
    IoError(io::Error),
}

impl Display for SeraphineError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use SeraphineError::*;
        match self {
            TokenizeError(e) => write!(f, "Tokenize error: {}", e),
            ParseError(e) => write!(f, "Parse error: {}", e),
            EvalError(e) => write!(f, "Eval error: {}", e),
            VmError(e) => write!(f, "VM error: {}", e),
            CodegenError(e) => write!(f, "Codegen error: {}", e),
            SerializeError(e) => write!(f, "Serialize error: {}", e),
            DeserializeError(e) => write!(f, "Deserialize error: {}", e),
            IoError(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl SeraphineError {
    pub fn format(&self, input: &str, file_name: &str, with_traceback: bool) -> String {
        match self {
            SeraphineError::TokenizeError(e) => e.format(input, file_name, with_traceback),
            SeraphineError::ParseError(e) => e.format(input, file_name, with_traceback),
            SeraphineError::EvalError(e) => e.format(input, file_name, with_traceback),
            SeraphineError::CodegenError(e) => e.format(input, file_name, with_traceback),
            SeraphineError::VmError(e) => e.format(input, file_name, with_traceback),
            SeraphineError::SerializeError(e) => e.to_string(),
            SeraphineError::DeserializeError(e) => e.to_string(),
            SeraphineError::IoError(e) => e.to_string(),
        }
    }
}

impl From<TokenizeError> for SeraphineError {
    fn from(e: TokenizeError) -> Self {
        Self::TokenizeError(e)
    }
}

impl From<ParseError> for SeraphineError {
    fn from(e: ParseError) -> Self {
        Self::ParseError(e)
    }
}

impl From<EvalError> for SeraphineError {
    fn from(e: EvalError) -> Self {
        Self::EvalError(e)
    }
}

impl From<CodegenError> for SeraphineError {
    fn from(e: CodegenError) -> Self {
        Self::CodegenError(e)
    }
}

impl From<VmError> for SeraphineError {
    fn from(e: VmError) -> Self {
        Self::VmError(e)
    }
}

impl From<SerializeError> for SeraphineError {
    fn from(e: SerializeError) -> Self {
        Self::SerializeError(e)
    }
}

impl From<DeserializeError> for SeraphineError {
    fn from(e: DeserializeError) -> Self {
        Self::DeserializeError(e)
    }
}

impl From<io::Error> for SeraphineError {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

#[derive(Debug)]
pub enum TokenizeError {
    UnexpectedChar { got: char, pos: Pos },
    MalformedNumber { number_str: String, pos: Pos },
    UnterminatedString { pos: Pos },
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TokenizeError::*;
        match self {
            UnexpectedChar { got, .. } => write!(f, "Unexpected char {:?}", got),
            MalformedNumber { number_str, .. } => write!(f, "Malformed number {}", number_str),
            UnterminatedString { .. } => write!(f, "Unterminated string"),
        }
    }
}

impl FormattableWithContext for TokenizeError {
    fn error_context(&self, input: &str, file_name: &str) -> Option<ErrorContext> {
        match self {
            TokenizeError::UnexpectedChar { pos, .. }
            | TokenizeError::MalformedNumber { pos, .. }
            | TokenizeError::UnterminatedString { pos } => {
                Some(error_pos_to_error_context(input, file_name, *pos))
            }
        }
    }
}

#[derive(Debug)]
pub enum OperatorKind {
    Unary,
    Binary,
}

impl Display for OperatorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OperatorKind::Unary => write!(f, "unary"),
            OperatorKind::Binary => write!(f, "binary"),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        token: Token,
        expected: Option<TokenKind>,
    },
    ExpectedIdentifier {
        pos: Pos,
    },
    InvalidOperator {
        op: Operator,
        kind: OperatorKind,
        pos: Pos,
    },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ParseError::*;
        match self {
            UnexpectedToken { token, expected } => {
                write!(f, "Unexpected token '{:?}'", token.kind)?;
                if let Some(expected) = expected {
                    write!(f, ", expected token '{:?}'", expected)?;
                }
                Ok(())
            }
            ExpectedIdentifier { .. } => write!(f, "Expected identifier"),
            InvalidOperator { op, kind, .. } => write!(f, "Invalid {} operator {:?}", kind, op),
        }
    }
}

impl FormattableWithContext for ParseError {
    fn error_context(&self, input: &str, file_name: &str) -> Option<ErrorContext> {
        match self {
            ParseError::UnexpectedToken {
                token:
                    Token {
                        span: Span { start: pos, .. },
                        ..
                    },
                ..
            }
            | ParseError::ExpectedIdentifier { pos }
            | ParseError::InvalidOperator { pos, .. } => {
                Some(error_pos_to_error_context(input, file_name, *pos))
            }
        }
    }
}

pub trait RuntimeError: Display + Debug + FormattableWithContext {
    fn cause(&self) -> Option<&dyn RuntimeError>;

    fn traceback(&self, input: &str, file_name: &str) -> Vec<ErrorContext> {
        let mut traceback = Vec::new();
        if let Some(error_ctx) = self.error_context(input, file_name) {
            traceback.push(error_ctx);
        }

        let mut maybe_error = self.cause();
        while let Some(error) = maybe_error {
            if let Some(error_ctx) = error.error_context(input, file_name) {
                traceback.push(error_ctx);
            }
            maybe_error = error.cause();
        }

        traceback
    }
}

impl RuntimeError for EvalError {
    fn cause(&self) -> Option<&dyn RuntimeError> {
        match self {
            EvalError::StdlibError {
                error: StdlibError::FunctionCall(error),
                ..
            } => Some(error.as_ref()),
            _ => None,
        }
    }
}

impl RuntimeError for VmError {
    fn cause(&self) -> Option<&dyn RuntimeError> {
        match self {
            VmError::StdlibError {
                error: StdlibError::FunctionCall(error),
                ..
            } => Some(error.as_ref()),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum StdlibError {
    GenericError(String),
    FunctionWrongArgAmount {
        name: Option<String>,
        expected: usize,
        got: usize,
    },
    DuplicateArgName {
        func_name: Option<String>,
        arg_name: String,
    },
    NoSuchMember {
        r#type: Type,
        member_name: String,
    },
    TypeError(String),
    WrongType {
        expected: Type,
        got: Type,
    },
    IndexOutOfBounds {
        index: usize,
        length: usize,
    },
    FunctionCall(Box<dyn RuntimeError>),
    Io(io::Error),
}

impl From<io::Error> for StdlibError {
    fn from(e: io::Error) -> Self {
        Self::Io(e)
    }
}

impl Display for StdlibError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use StdlibError::*;
        match self {
            GenericError(e) => write!(f, "{}", e),
            FunctionWrongArgAmount {
                name,
                expected,
                got,
                ..
            } => {
                match name {
                    Some(name) => write!(f, "Function '{}'", name)?,
                    None => write!(f, "Unnamed function")?,
                };
                write!(
                    f,
                    " was called with {} arguments but expects {}",
                    got, expected
                )
            }
            DuplicateArgName {
                func_name,
                arg_name,
                ..
            } => {
                match func_name {
                    Some(name) => write!(f, "Function '{}'", name)?,
                    None => write!(f, "Unnamed function")?,
                };
                write!(f, " has duplicate argument name '{}'", arg_name)
            }
            NoSuchMember {
                r#type: t,
                member_name,
                ..
            } => {
                write!(f, "Type '{}' has no member named '{}'", t, member_name)
            }
            TypeError(e) => write!(f, "{}", e),
            WrongType { expected, got } => {
                write!(
                    f,
                    "Expected value of type '{}' but got value of type ‘{}' instead",
                    expected, got
                )
            }
            IndexOutOfBounds { index, length } => {
                write!(
                    f,
                    "Index {} is out of bounds for list of length {}",
                    index, length
                )
            }
            FunctionCall(err) => {
                write!(f, "{}", err)
            }
            Io(e) => {
                write!(f, "IO error: {}", e)
            }
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    VariableNotDefined { name: String, span: Span },
    StdlibError { error: StdlibError, span: Span },
    CallStackOverflow,
    ContinueOutsideOfLoop(Span),
    BreakOutsideOfLoop(Span),
    InternalControlFlow { kind: ControlFlow, span: Span },
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use EvalError::*;
        match self {
            VariableNotDefined { name, .. } => {
                write!(f, "Variable with name '{}' is not defined", name)
            }
            StdlibError { error, .. } => write!(f, "{}", error),
            CallStackOverflow => write!(f, "Call stack overflow (too many nested function calls)"),
            ContinueOutsideOfLoop(_) => {
                write!(f, "Continue statement outside of loop")
            }
            BreakOutsideOfLoop(_) => {
                write!(f, "Break statement outside of loop")
            }
            // TODO: Change this once returns are allowed outside of functions
            InternalControlFlow {
                kind: ControlFlow::Return(_),
                ..
            } => {
                write!(f, "Return statement outside of function")
            }
            InternalControlFlow {
                kind: ControlFlow::Continue,
                ..
            } => {
                write!(f, "Continue statement outside of loop")
            }
            InternalControlFlow {
                kind: ControlFlow::Break,
                ..
            } => {
                write!(f, "Break statement outside of loop")
            }
        }
    }
}

impl FormattableWithContext for EvalError {
    fn error_context(&self, input: &str, file_name: &str) -> Option<ErrorContext> {
        match self {
            EvalError::VariableNotDefined { span, .. }
            | EvalError::StdlibError { span, .. }
            | EvalError::ContinueOutsideOfLoop(span)
            | EvalError::BreakOutsideOfLoop(span)
            | EvalError::InternalControlFlow { span, .. } => {
                Some(error_pos_to_error_context(input, file_name, span.start))
            }
            EvalError::CallStackOverflow => None,
        }
    }

    fn format(&self, input: &str, file_name: &str, with_traceback: bool) -> String {
        match self {
            EvalError::StdlibError {
                error: StdlibError::FunctionCall(_),
                ..
            } if with_traceback => {
                let traceback = self.traceback(input, file_name);
                format_traceback(&traceback, &self.to_string())
            }
            EvalError::StdlibError {
                error: StdlibError::FunctionCall(_),
                span,
            } => {
                let error_ctx = error_pos_to_error_context(input, file_name, span.start);
                format_error_omitted_traceback(self.to_string(), error_ctx)
            }
            _ => match self.error_context(input, file_name) {
                Some(error_ctx) => format_error(self.to_string(), error_ctx),
                None => self.to_string(),
            },
        }
    }
}

#[derive(Debug)]
pub enum CodegenError {
    ReturnOutsideOfFunction {
        span: Span,
    },
    ContinueOutsideOfLoop {
        span: Span,
    },
    BreakOutsideOfLoop {
        span: Span,
    },
    DuplicateArgName {
        func_name: Option<String>,
        arg_name: String,
        function_span: Span,
    },
    // It makes no sense to include a Span here, since this will only happen when the user provides
    // a custom AST. The parser will always generate a block as the function body.
    FunctionBodyNoBlock {
        got_kind: AstKind,
    },
}

impl Display for CodegenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CodegenError::ReturnOutsideOfFunction { .. } => {
                write!(f, "return used outside of function")
            }
            CodegenError::ContinueOutsideOfLoop { .. } => {
                write!(f, "continue used outside of loop")
            }
            CodegenError::BreakOutsideOfLoop { .. } => {
                write!(f, "break used outside of loop")
            }
            CodegenError::DuplicateArgName {
                func_name,
                arg_name,
                ..
            } => {
                match func_name {
                    Some(name) => write!(f, "Function '{}'", name)?,
                    None => write!(f, "Unnamed function")?,
                };
                write!(f, " has duplicate argument name '{}'", arg_name)
            }
            CodegenError::FunctionBodyNoBlock { got_kind } => {
                write!(
                    f,
                    "Function body isn't a block, got function body {:?}",
                    got_kind
                )
            }
        }
    }
}

impl FormattableWithContext for CodegenError {
    fn error_context(&self, input: &str, file_name: &str) -> Option<ErrorContext> {
        match self {
            CodegenError::ReturnOutsideOfFunction { span }
            | CodegenError::ContinueOutsideOfLoop { span }
            | CodegenError::BreakOutsideOfLoop { span }
            | CodegenError::DuplicateArgName {
                function_span: span,
                ..
            } => Some(error_pos_to_error_context(input, file_name, span.start)),
            CodegenError::FunctionBodyNoBlock { .. } => None,
        }
    }
}

#[derive(Debug)]
pub enum VmError {
    StdlibError {
        error: StdlibError,
        span: Span,
    },
    StackUnderflow {
        instruction: Instruction,
        instruction_idx: usize,
    },
    UndefinedVariable {
        name: String,
        span: Span,
    },
    BytecodeReturnOutsideOfFunctionCall {
        span: Span,
    },
    BytecodeNoThis,
    BytecodeInternalPlaceholder(PlaceholderInstructionKind),
}

impl Display for VmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use VmError::*;
        match self {
            StdlibError { error, .. } => write!(f, "{}", error),
            StackUnderflow {
                instruction,
                instruction_idx,
                ..
            } => write!(
                f,
                "Stack underflow at instruction index {} ({:?})",
                instruction_idx, instruction.kind
            ),
            UndefinedVariable { name, .. } => write!(f, "Variable '{}' is not defined", name),
            BytecodeReturnOutsideOfFunctionCall { .. } => write!(
                f,
                "Corrupt bytecode -- `return` encountered outside of function call"
            ),
            BytecodeNoThis => write!(f, "Corrupt bytecode -- no `this` in variable names"),
            BytecodeInternalPlaceholder(kind) => write!(
                f,
                "Corrupt bytecode -- contains internal placeholder instruction {:?}",
                kind
            ),
        }
    }
}

impl FormattableWithContext for VmError {
    fn error_context(&self, input: &str, file_name: &str) -> Option<ErrorContext> {
        match self {
            VmError::StdlibError { span, .. }
            | VmError::UndefinedVariable { span, .. }
            | VmError::StackUnderflow {
                instruction: Instruction { span, .. },
                ..
            }
            | VmError::BytecodeReturnOutsideOfFunctionCall { span } => {
                Some(error_pos_to_error_context(input, file_name, span.start))
            }
            VmError::BytecodeNoThis | VmError::BytecodeInternalPlaceholder(_) => None,
        }
    }

    fn format(&self, input: &str, file_name: &str, with_traceback: bool) -> String {
        match self {
            VmError::StdlibError {
                error: StdlibError::FunctionCall(_),
                ..
            } if with_traceback => {
                let traceback = self.traceback(input, file_name);
                format_traceback(&traceback, &self.to_string())
            }
            VmError::StdlibError {
                error: StdlibError::FunctionCall(_),
                span,
            } => {
                let error_ctx = error_pos_to_error_context(input, file_name, span.start);
                format_error_omitted_traceback(self.to_string(), error_ctx)
            }
            _ => match self.error_context(input, file_name) {
                Some(error_ctx) => format_error(self.to_string(), error_ctx),
                None => self.to_string(),
            },
        }
    }
}

#[derive(Debug)]
pub enum SerializeError {
    InternalPlaceholder(PlaceholderInstructionKind),
}

impl Display for SerializeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SerializeError::InternalPlaceholder(kind) => write!(
                f,
                "Internal placeholder instruction {:?} found in bytecode",
                kind
            ),
        }
    }
}

#[derive(Debug)]
pub enum DeserializeError {
    UnknownOpCode(u8),
    UnknownUnaryOpCode(u8),
    UnknownBinaryOpCode(u8),
    TooManyVariables(usize),
    TooManyInstructions(usize),
    UnexpectedEndOfInput,
    InvalidUtf8String(Vec<u8>),
}

impl Display for DeserializeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DeserializeError::UnknownOpCode(op_code) => {
                write!(f, "Unknown op code 0x{:02x} in bytecode", op_code)
            }
            DeserializeError::UnknownUnaryOpCode(op_code) => {
                write!(f, "Unknown unary op code 0x{:02x} in bytecode", op_code)
            }
            DeserializeError::UnknownBinaryOpCode(op_code) => {
                write!(f, "Unknown binary op code 0x{:02x} in bytecode", op_code)
            }
            DeserializeError::TooManyVariables(n_variables) => {
                write!(f, "Too many variables in bytecode ({})", n_variables)
            }
            DeserializeError::TooManyInstructions(n_instructions) => {
                write!(f, "Too many instructions in bytecode ({})", n_instructions)
            }
            DeserializeError::UnexpectedEndOfInput => {
                write!(f, "Unexpected end of bytecode")
            }
            DeserializeError::InvalidUtf8String(bytes) => {
                write!(f, "Invalid UTF-8 string in bytecode: {:?}", bytes)
            }
        }
    }
}

pub struct ErrorContext {
    file_name: String,
    line_num: usize,
    column_num: usize,
    line: String,
}

fn error_pos_to_error_context(input: &str, file_name: &str, input_pos: Pos) -> ErrorContext {
    let mut line_num = 0;
    let mut column_num = 0;
    let mut line_chars = Vec::new();

    let mut chars = input.chars();
    for _ in 0..input_pos {
        let c = chars.next();
        match c {
            // TODO: Replace with Result (e.g. with new error type FormatError)
            None => panic!(
                "trying to get error context for position {} that is outside of input of length {}",
                input_pos,
                input.len()
            ),
            // TODO: Handle "\r\n" once tokenizer can handle it
            Some('\n') => {
                line_num += 1;
                column_num = 0;
                line_chars.clear();
            }
            Some(c) => {
                column_num += 1;
                line_chars.push(c);
            }
        }
    }

    chars
        .take_while(|c| c != &'\n')
        .for_each(|c| line_chars.push(c));

    ErrorContext {
        file_name: file_name.to_string(),
        line_num,
        column_num,
        line: line_chars.into_iter().collect(),
    }
}

fn highlight_pos(line: &str, line_num: usize, column_num: usize, pad_start: usize) -> String {
    let one_based_line_num = line_num + 1;
    let one_based_line_num_string = one_based_line_num.to_string();
    let delimiter = " | ";

    let mut padding_start = String::new();
    for _ in 0..pad_start {
        padding_start.push(' ');
    }

    let mut error_string = format!(
        "{}{}{}{}\n",
        padding_start, one_based_line_num_string, delimiter, line
    );
    let padding = one_based_line_num_string.len() + delimiter.len() + column_num;
    for _ in 0..(pad_start + padding) {
        error_string.push(' ');
    }
    error_string.push('^');
    error_string
}

fn format_pos(file_name: &str, line_num: usize, column_num: usize) -> String {
    format!("{}:{}:{}", file_name, line_num + 1, column_num + 1)
}

pub fn format_error(error_message_prefix: impl Into<String>, error_ctx: ErrorContext) -> String {
    let mut error_message = error_message_prefix.into();
    error_message.push_str(&format!(
        " at {}\n",
        format_pos(
            &error_ctx.file_name,
            error_ctx.line_num,
            error_ctx.column_num
        )
    ));
    error_message.push_str(&highlight_pos(
        &error_ctx.line,
        error_ctx.line_num,
        error_ctx.column_num,
        0,
    ));
    error_message
}

pub fn format_traceback(traceback: &[ErrorContext], error: &str) -> String {
    let mut formatted_error = String::new();
    formatted_error.push_str("Traceback\n");
    let padding = 2;
    for error_ctx in traceback {
        for _ in 0..padding {
            formatted_error.push(' ');
        }
        formatted_error.push_str(&format_pos(
            &error_ctx.file_name,
            error_ctx.line_num,
            error_ctx.column_num,
        ));
        formatted_error.push('\n');
        formatted_error.push_str(&highlight_pos(
            &error_ctx.line,
            error_ctx.line_num,
            error_ctx.column_num,
            padding,
        ));
        formatted_error.push('\n');
    }
    formatted_error.push_str(error);
    formatted_error
}

fn format_error_omitted_traceback(error: impl Into<String>, error_ctx: ErrorContext) -> String {
    let mut formatted_error = format_error(error.into(), error_ctx);
    formatted_error.push_str("\nTraceback omitted");
    formatted_error
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_error() {
        let input = concat!(
            "fn some_function {\n",
            "    return 42\n",
            "}\n",
            "42 + error_here\n"
        );
        let error_ctx = error_pos_to_error_context(input, "test_file.sr", 40);
        let got_error = format_error("A test error occured", error_ctx);
        let want_error = concat!(
            "A test error occured at test_file.sr:4:6\n",
            "4 | 42 + error_here\n",
            "         ^"
        );
        assert_eq!(got_error, want_error);
    }
}
