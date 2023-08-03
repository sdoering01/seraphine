use std::{
    fmt::{self, Display, Formatter},
    io,
};

use crate::{
    common::Pos,
    eval::ControlFlow,
    tokenizer::{Operator, Token, TokenKind},
};

#[derive(Debug)]
pub enum CalcError {
    TokenizeError(TokenizeError),
    ParseError(ParseError),
    EvalError(EvalError),
    IoError(io::Error),
}

impl Display for CalcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use CalcError::*;
        match self {
            TokenizeError(e) => write!(f, "Tokenize error: {}", e),
            ParseError(e) => write!(f, "Parse error: {}", e),
            EvalError(e) => write!(f, "Eval error: {}", e),
            IoError(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl CalcError {
    pub fn format(&self, input: &str, file_name: &str) -> String {
        use CalcError::*;
        match self {
            TokenizeError(e) => e.format(input, file_name),
            ParseError(e) => e.format(input, file_name),
            e => e.to_string(),
        }
    }
}

impl From<TokenizeError> for CalcError {
    fn from(e: TokenizeError) -> Self {
        Self::TokenizeError(e)
    }
}

impl From<ParseError> for CalcError {
    fn from(e: ParseError) -> Self {
        Self::ParseError(e)
    }
}

impl From<EvalError> for CalcError {
    fn from(e: EvalError) -> Self {
        Self::EvalError(e)
    }
}

impl From<io::Error> for CalcError {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

#[derive(Debug)]
pub enum TokenizeError {
    UnexpectedChar { got: char, pos: Pos },
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TokenizeError::*;
        match self {
            UnexpectedChar { got, .. } => write!(f, "Unexpected char '{}'", got),
        }
    }
}

impl TokenizeError {
    fn format(&self, input: &str, file_name: &str) -> String {
        let error = self.to_string();
        match self {
            Self::UnexpectedChar { pos, .. } => format_error(error, input, file_name, *pos),
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
    NoTokensLeft,
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
            NoTokensLeft => write!(f, "No tokens left to parse"),
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

impl ParseError {
    fn format(&self, input: &str, file_name: &str) -> String {
        let error = self.to_string();
        match self {
            Self::NoTokensLeft => error,
            Self::UnexpectedToken { token, .. } => format_error(error, input, file_name, token.pos),
            Self::ExpectedIdentifier { pos } => format_error(error, input, file_name, *pos),
            Self::InvalidOperator { pos, .. } => format_error(error, input, file_name, *pos),
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    DivideByZero,
    Overflow,
    VariableNotDefined(String),
    FunctionNotDefined(String),
    FunctionAlreadyDefined(String),
    FunctionWrongArgAmount {
        name: String,
        expected: usize,
        got: usize,
    },
    DuplicateArgName {
        func_name: String,
        arg_name: String,
    },
    CallStackOverflow,
    InternalControlFlow(ControlFlow),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use EvalError::*;
        match self {
            DivideByZero => write!(f, "Divide by zero"),
            Overflow => write!(f, "Overflow"),
            VariableNotDefined(name) => write!(f, "Variable with name '{}' is not defined", name),
            FunctionNotDefined(name) => write!(f, "Function with name '{}' is not defined", name),
            FunctionAlreadyDefined(name) => {
                write!(f, "Function with name '{}' is already defined", name)
            }
            FunctionWrongArgAmount {
                name,
                expected,
                got,
            } => write!(
                f,
                "Function '{}' was called with {} arguments but expects {}",
                name, got, expected
            ),
            DuplicateArgName {
                func_name,
                arg_name,
            } => write!(
                f,
                "Function '{}' has duplicate argument name '{}'",
                func_name, arg_name
            ),
            CallStackOverflow => write!(f, "Call stack overflow (too many nested function calls)"),
            // TODO: Change this once returns are allowed outside of functions
            InternalControlFlow(ControlFlow::Return(_)) => {
                write!(f, "Return statement outside of function")
            }
        }
    }
}

struct ErrorContext {
    line_num: usize,
    column_num: usize,
    line: String,
}

fn error_pos_to_error_context(input: &str, input_pos: Pos) -> Option<ErrorContext> {
    let mut line_num = 0;
    let mut column_num = 0;
    let mut line_chars = Vec::new();

    let mut chars = input.chars();
    for _ in 0..input_pos {
        let c = chars.next();
        match c {
            None => return None,
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

    Some(ErrorContext {
        line_num,
        column_num,
        line: line_chars.into_iter().collect(),
    })
}

fn highlight_pos(line: &str, line_num: usize, column_num: usize) -> String {
    let one_based_line_num = line_num + 1;
    let one_based_line_num_string = one_based_line_num.to_string();
    let delimiter = " | ";

    let mut error_string = format!("{}{}{}\n", one_based_line_num_string, delimiter, line);
    let padding = one_based_line_num_string.len() + delimiter.len() + column_num;
    for _ in 0..padding {
        error_string.push(' ');
    }
    error_string.push('^');
    error_string
}

fn format_pos(file_name: &str, line_num: usize, column_num: usize) -> String {
    format!("{}:{}:{}", file_name, line_num + 1, column_num + 1)
}

fn format_error(
    error_message_prefix: impl Into<String>,
    input: &str,
    file_name: &str,
    pos: Pos,
) -> String {
    let mut error_message = error_message_prefix.into();
    match error_pos_to_error_context(input, pos) {
        Some(ErrorContext {
            line_num,
            column_num,
            line,
        }) => {
            error_message.push_str(&format!(
                " at {}\n",
                format_pos(file_name, line_num, column_num)
            ));
            error_message.push_str(&highlight_pos(&line, line_num, column_num));
        }
        None => {
            error_message.push_str(&format!(
                " at position {} which is outside of the input",
                pos
            ));
        }
    }
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
        let got_error = format_error("A test error occured", input, "test_file.sr", 40);
        let want_error = concat!(
            "A test error occured at test_file.sr:4:6\n",
            "4 | 42 + error_here\n",
            "         ^"
        );
        assert_eq!(got_error, want_error);
    }
}

