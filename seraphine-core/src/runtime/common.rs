use std::io::{BufReader, Read, Write};

pub struct RuntimeContext {
    pub(crate) stdin: BufReader<Box<dyn Read>>,
    pub(crate) stdout: Box<dyn Write>,
    pub(crate) stderr: Box<dyn Write>,
    #[allow(dead_code)]
    pub(crate) debug_writer: Option<Box<dyn Write>>,
    /// This flag is used during tests until observable side effects apart from writing to stdout
    /// are introduced.
    // TODO: Remove this when some form of obvservable side effects is implemented
    pub(crate) _internal_side_effect_flag: bool,
}

impl RuntimeContext {
    pub(crate) fn new(
        stdin: BufReader<Box<dyn Read>>,
        stdout: Box<dyn Write>,
        stderr: Box<dyn Write>,
        debug_writer: Option<Box<dyn Write>>,
    ) -> RuntimeContext {
        RuntimeContext {
            stdin,
            stdout,
            stderr,
            debug_writer,
            _internal_side_effect_flag: false,
        }
    }
}
