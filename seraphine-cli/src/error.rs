use std::{fmt::Display, io};

#[derive(Debug)]
pub(crate) enum CliError {
    Io(io::Error),
    Formatted(String),
}

impl Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliError::Io(e) => write!(f, "{}", e),
            CliError::Formatted(e) => write!(f, "{}", e),
        }
    }
}

impl From<io::Error> for CliError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<String> for CliError {
    fn from(value: String) -> Self {
        Self::Formatted(value)
    }
}
