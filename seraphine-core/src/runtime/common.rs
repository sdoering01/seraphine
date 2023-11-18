use std::io::{BufReader, Read, Write};

pub struct RuntimeContext {
    pub(crate) stdin: BufReader<Box<dyn Read>>,
    pub(crate) stdout: Box<dyn Write>,
    pub(crate) stderr: Box<dyn Write>,
    #[allow(dead_code)]
    pub(crate) debug_writer: Option<Box<dyn Write>>,
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
        }
    }
}
