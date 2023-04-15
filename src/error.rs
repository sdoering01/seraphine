use std::{
    fmt::{self, Display, Formatter},
    io,
};

use crate::tokenizer::Token;

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
    UnexpectedChar(char),
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TokenizeError::*;
        match self {
            UnexpectedChar(c) => write!(f, "Unexpected char {}", c),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    NoTokensLeft,
    UnexpectedToken(Token),
    ExpectedToken(Token),
    ExpectedIdentifier,
    UnmatchedBracket(Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ParseError::*;
        match self {
            NoTokensLeft => write!(f, "No tokens left to parse"),
            UnexpectedToken(t) => write!(f, "Unexpected token {:?}", t),
            ExpectedToken(t) => write!(f, "Expected token {:?}", t),
            ExpectedIdentifier => write!(f, "Expected identifier"),
            UnmatchedBracket(t) => write!(f, "Unmatched bracket {:?}", t),
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
    DuplicateArgName{ func_name: String, arg_name: String },
    CallStackOverflow,
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
            DuplicateArgName{ func_name, arg_name } => write!(f, "Function '{}' has duplicate argument name '{}'", func_name, arg_name),
            CallStackOverflow => write!(f, "Call stack overflow (too many nested function calls)"),
        }
    }
}
