use std::fmt::Display;

const HELP_TEXT: &str = "\
Seraphine CLI

Usage:
    seraphine [options] <action> [action-options] [input-file]

Options:
    -h
    --help          Print this help text
    -V
    --version       Print the version of the Seraphine CLI

Action:
    eval            Evaluate a Seraphine file directly
      --evaluator       Evaluate the file with the evaluator [default]
      --vm              Evaluate the file with the VM without producing a bytecode file

    compile         Compile a Seraphine file to bytecode
      -o <file>
      --output <file>   Write bytecode to <file> [default: input file with extension .src]

    run             Run a Seraphine bytecode file

    repl            Start the interactive REPL";

const HELP_HINT_TEXT: &str = "For help try --help";

const VERSION_TEXT: &str = concat!("Seraphine ", env!("CARGO_PKG_VERSION"));

#[derive(Debug, Clone)]
pub(crate) enum OptionParseAction {
    Help,
    Version,
    Eval {
        input_file: String,
        runtime: Runtime,
    },
    Compile {
        input_file: String,
        output_file: Option<String>,
    },
    Run {
        input_file: String,
    },
    Repl,
}

#[derive(Debug, Clone)]
pub(crate) enum OptionParseError {
    GenericError(String),
    UnknownOption(String),
    UnknownAction(String),
    NoActionSpecified,
}

impl Display for OptionParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptionParseError::GenericError(msg) => {
                write!(f, "{}", msg)
            }
            OptionParseError::UnknownOption(option) => {
                write!(f, "Unknown option: {}", option)
            }
            OptionParseError::UnknownAction(action) => {
                write!(f, "Unknown action: {}", action)
            }
            OptionParseError::NoActionSpecified => {
                write!(f, "No action specified")
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
pub(crate) struct OptionParser<A>
where
    A: Iterator<Item = String>,
{
    args: A,
}

impl<A> OptionParser<A>
where
    A: Iterator<Item = String>,
{
    pub(crate) fn new<I>(args: I) -> Self
    where
        I: IntoIterator<IntoIter = A>,
    {
        Self {
            args: args.into_iter(),
        }
    }

    pub(crate) fn parse(&mut self) -> Result<OptionParseAction, OptionParseError> {
        if let Some(arg) = self.args.next() {
            match arg.as_str() {
                "-h" | "--help" => {
                    return Ok(OptionParseAction::Help);
                }
                "-V" | "--version" => {
                    return Ok(OptionParseAction::Version);
                }
                _ if arg.starts_with('-') => {
                    return Err(OptionParseError::UnknownOption(arg));
                }
                "eval" => return self.parse_eval(),
                "compile" => return self.parse_compile(),
                "run" => return self.parse_run(),
                "repl" => return self.parse_repl(),
                _ => return Err(OptionParseError::UnknownAction(arg)),
            }
        }

        Err(OptionParseError::NoActionSpecified)
    }

    fn parse_eval(&mut self) -> Result<OptionParseAction, OptionParseError> {
        let mut runtime = Runtime::Evaluator;
        let mut input_file = None;

        for arg in self.args.by_ref() {
            match arg.as_str() {
                "--vm" => {
                    runtime = Runtime::Vm;
                }
                "--evaluator" => {
                    runtime = Runtime::Evaluator;
                }
                _ if arg.starts_with('-') => {
                    return Err(OptionParseError::UnknownOption(arg));
                }
                _ => {
                    if input_file.is_none() {
                        input_file = Some(arg);
                    } else {
                        let msg = format!("Argument after input file not allowed: {}", arg);
                        return Err(OptionParseError::GenericError(msg));
                    }
                }
            }
        }

        match input_file {
            None => Err(OptionParseError::GenericError(
                "No input file specified".to_string(),
            )),
            Some(input_file) => Ok(OptionParseAction::Eval {
                input_file,
                runtime,
            }),
        }
    }

    fn parse_compile(&mut self) -> Result<OptionParseAction, OptionParseError> {
        let mut input_file = None;
        let mut output_file = None;

        while let Some(arg) = self.args.next() {
            match arg.as_str() {
                "-o" | "--output" => {
                    output_file = match self.args.next() {
                        output @ Some(_) => output,
                        None => {
                            return Err(OptionParseError::GenericError(
                                "No output file specified".to_string(),
                            ))
                        }
                    };
                }
                _ if arg.starts_with('-') => {
                    return Err(OptionParseError::UnknownOption(arg));
                }
                _ => {
                    if input_file.is_none() {
                        input_file = Some(arg);
                    } else {
                        let msg = format!("Argument after input file not allowed: {}", arg);
                        return Err(OptionParseError::GenericError(msg));
                    }
                }
            }
        }

        match input_file {
            None => Err(OptionParseError::GenericError(
                "No input file specified".to_string(),
            )),
            Some(input_file) => Ok(OptionParseAction::Compile {
                input_file,
                output_file,
            }),
        }
    }

    fn parse_run(&mut self) -> Result<OptionParseAction, OptionParseError> {
        let mut input_file = None;

        for arg in self.args.by_ref() {
            match arg.as_str() {
                _ if arg.starts_with('-') => {
                    return Err(OptionParseError::UnknownOption(arg));
                }
                _ => {
                    if input_file.is_none() {
                        input_file = Some(arg);
                    } else {
                        let msg = format!("Argument after input file not allowed: {}", arg);
                        return Err(OptionParseError::GenericError(msg));
                    }
                }
            }
        }

        match input_file {
            None => Err(OptionParseError::GenericError(
                "No input file specified".to_string(),
            )),
            Some(input_file) => Ok(OptionParseAction::Run { input_file }),
        }
    }

    fn parse_repl(&mut self) -> Result<OptionParseAction, OptionParseError> {
        if let Some(arg) = self.args.next() {
            return Err(OptionParseError::UnknownOption(arg));
        }

        Ok(OptionParseAction::Repl)
    }

    pub(crate) fn help(&self) -> &'static str {
        HELP_TEXT
    }

    pub(crate) fn help_hint(&self) -> &'static str {
        HELP_HINT_TEXT
    }

    pub(crate) fn version(&self) -> &'static str {
        VERSION_TEXT
    }
}
