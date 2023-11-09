use std::fmt::Display;

const HELP_TEXT: &str = "\
Seraphine CLI

Usage:
    seraphine [options] [input-file]

Options:
    --vm            Run the program using the virtual machine
    --evaluator     Run the program using the evaluator [default]
    --help          Print this help text

If no input file is specified, a REPL is started";

#[derive(Debug, Clone)]
pub(crate) enum OptionParseAction {
    Continue,
    Help,
}

#[derive(Debug, Clone)]
pub(crate) enum OptionParseError {
    UnknownOption(String),
}

impl Display for OptionParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptionParseError::UnknownOption(option) => {
                write!(f, "Unknown option: {}", option)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum Runtime {
    Evaluator,
    Vm,
}

#[derive(Debug, Clone)]
pub(crate) struct OptionParser {
    runtime: Runtime,
    input_file: Option<String>,
    input_arguments: Option<Vec<String>>,
}

impl OptionParser {
    pub(crate) fn new() -> Self {
        Self {
            runtime: Runtime::Evaluator,
            input_file: None,
            input_arguments: None,
        }
    }

    pub(crate) fn parse(
        &mut self,
        args: impl IntoIterator<Item = String>,
    ) -> Result<OptionParseAction, OptionParseError> {
        let mut args_iter = args.into_iter();
        while let Some(arg) = args_iter.next() {
            if arg.starts_with('-') {
                match arg.as_str() {
                    "--vm" => {
                        self.runtime = Runtime::Vm;
                    }
                    "--evaluator" => {
                        self.runtime = Runtime::Evaluator;
                    }
                    "--help" => {
                        return Ok(OptionParseAction::Help);
                    }
                    _ => {
                        return Err(OptionParseError::UnknownOption(arg));
                    }
                }
            } else {
                self.input_file = Some(arg);
                self.input_arguments = Some(args_iter.collect());
                break;
            }
        }

        Ok(OptionParseAction::Continue)
    }

    pub(crate) fn help(&self) -> &'static str {
        HELP_TEXT
    }

    pub(crate) fn runtime(&self) -> Runtime {
        self.runtime
    }

    pub(crate) fn input_file(&self) -> Option<&String> {
        self.input_file.as_ref()
    }
}
