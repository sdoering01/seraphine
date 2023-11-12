use std::{
    fmt::{self, Debug, Display, Formatter},
    io,
};

use crate::{
    common::{Pos, Span},
    eval::{ControlFlow, Type},
    tokenizer::{Operator, Token, TokenKind},
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
// Clippy warns that the Error suffix should be removed, but it makes sense here
#[allow(clippy::enum_variant_names)]
pub enum SeraphineError {
    TokenizeError(TokenizeError),
    ParseError(ParseError),
    EvalError(EvalError),
    VmError(VmError),
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
            SeraphineError::VmError(e) => e.format(input, file_name, with_traceback),
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

impl From<VmError> for SeraphineError {
    fn from(e: VmError) -> Self {
        Self::VmError(e)
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
    fn push_traceback(&self, traceback: &mut Vec<ErrorContext>, input: &str, file_name: &str);
}

impl RuntimeError for EvalError {
    fn push_traceback(&self, trackback: &mut Vec<ErrorContext>, input: &str, file_name: &str) {
        if let Some(error_ctx) = self.error_context(input, file_name) {
            trackback.push(error_ctx);
        }

        if let EvalError::StdlibError {
            error: StdlibError::FunctionCall(error),
            ..
        } = self
        {
            error.push_traceback(trackback, input, file_name);
        }
    }
}

impl RuntimeError for VmError {
    fn push_traceback(&self, traceback: &mut Vec<ErrorContext>, input: &str, file_name: &str) {
        if let Some(error_ctx) = self.error_context(input, file_name) {
            traceback.push(error_ctx);
        }

        // TODO: Handle StdlibError::FunctionCall for traceback
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
                error: StdlibError::FunctionCall(inner_error),
                span,
            } if with_traceback => {
                let mut traceback = vec![error_pos_to_error_context(input, file_name, span.start)];
                inner_error.push_traceback(&mut traceback, input, file_name);

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
                formatted_error.push_str(&self.to_string());

                formatted_error
            }
            EvalError::StdlibError {
                error: StdlibError::FunctionCall(_),
                span,
            } => {
                let error_ctx = error_pos_to_error_context(input, file_name, span.start);
                let mut formatted_error = format_error(self.to_string(), error_ctx);
                formatted_error.push_str("\nTraceback omitted");
                formatted_error
            }
            _ => match self.error_context(input, file_name) {
                Some(error_ctx) => format_error(self.to_string(), error_ctx),
                None => self.to_string(),
            },
        }
    }
}

#[derive(Debug)]
pub enum VmError {
    StdlibError(StdlibError),
    StackUnderflow,
    UndefinedVariable(String),
}

impl Display for VmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use VmError::*;
        match self {
            StdlibError(e) => write!(f, "{}", e),
            StackUnderflow => write!(f, "Stack underflow"),
            UndefinedVariable(name) => write!(f, "Variable '{}' is not defined", name),
        }
    }
}

impl FormattableWithContext for VmError {
    fn error_context(&self, _input: &str, _file_name: &str) -> Option<ErrorContext> {
        // TODO: Implement this
        None
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
