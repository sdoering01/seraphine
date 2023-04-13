use std::fmt::{self, Display, Formatter};

use crate::tokenizer::Token;

#[derive(Debug)]
pub enum CalcError {
    TokenizeError(TokenizeError),
    ParseError(ParseError),
    EvalError(EvalError),
}

impl Display for CalcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use CalcError::*;
        match self {
            TokenizeError(e) => write!(f, "Tokenize error: {}", e),
            ParseError(e) => write!(f, "Parse error: {}", e),
            EvalError(e) => write!(f, "Eval error: {}", e),
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
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ParseError::*;
        match self {
            NoTokensLeft => write!(f, "No tokens left to parse"),
            UnexpectedToken(t) => write!(f, "Unexpected token {:?}", t),
            ExpectedToken(t) => write!(f, "Expected token {:?}", t),
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    DivideByZero,
    Overflow,
    VariableNotDefined(String),
    FunctionNotDefined(String),
    FunctionWrongArgAmount {
        name: String,
        expected: usize,
        got: usize,
    },
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use EvalError::*;
        match self {
            DivideByZero => write!(f, "Divide by zero"),
            Overflow => write!(f, "Overflow"),
            VariableNotDefined(name) => write!(f, "Variable with name '{}' is not defined", name),
            FunctionNotDefined(name) => write!(f, "Function with name '{}' is not defined", name),
            FunctionWrongArgAmount {
                name,
                expected,
                got,
            } => write!(
                f,
                "Function '{}' was called with {} arguments but expects {}",
                name, got, expected
            ),
        }
    }
}