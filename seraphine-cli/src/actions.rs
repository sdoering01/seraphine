use std::{
    fs::File,
    io::{stdout, BufReader, Read, Write},
    path::PathBuf,
};

use seraphine_core::{
    bytecode::Bytecode, codegen::generate, error::SeraphineError, eval::Evaluator, parser::parse,
    tokenizer::tokenize, vm::Vm,
};

use crate::{common::COLOR_DEBUG_INFO, error::CliError, option_parser::Runtime, repl::Repl};

struct ColoredWriter<W: Write> {
    writer: W,
    fg_color_string: String,
}

impl<W: Write> ColoredWriter<W> {
    fn new(writer: W, fg_color_string: impl Into<String>) -> ColoredWriter<W> {
        ColoredWriter {
            writer,
            fg_color_string: fg_color_string.into(),
        }
    }
}

impl<W: Write> Write for ColoredWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.writer.write_all(self.fg_color_string.as_bytes())?;
        self.writer.write_all(buf)?;
        self.writer
            .write_all(termion::color::Reset.fg_str().as_bytes())?;
        self.writer.flush()?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}

fn load_code(input_file: &str) -> std::io::Result<String> {
    std::fs::read_to_string(input_file)
}

fn generate_bytecode(code: &str, code_file_name: &str) -> Result<Bytecode, SeraphineError> {
    let tokens = tokenize(code)?;
    let ast = parse(&tokens)?;
    Ok(generate(&ast, code, code_file_name)?)
}

fn run_with_vm(code: &str, code_file_name: &str) -> Result<(), SeraphineError> {
    let bytecode = generate_bytecode(code, code_file_name)?;
    let mut vm = Vm::new(bytecode)?;
    vm.run()?;
    Ok(())
}

pub(crate) fn eval(input_file: &str, runtime: Runtime) -> Result<(), CliError> {
    let code = load_code(input_file)?;

    let result = match runtime {
        Runtime::Evaluator => {
            let debug_writer = ColoredWriter::new(stdout(), COLOR_DEBUG_INFO.fg_str());
            let mut eval = Evaluator::builder()
                .debug_writer(Some(Box::new(debug_writer)))
                .build();
            eval.eval_str(&code).map(|_| ())
        }
        Runtime::Vm => run_with_vm(&code, input_file),
    };

    result.map_err(|e| e.format(&code, input_file, true).into())
}

pub(crate) fn compile(input_file: &str, output_file: Option<String>) -> Result<(), CliError> {
    let code = load_code(input_file)?;
    let bytecode =
        generate_bytecode(&code, input_file).map_err(|e| e.format(&code, input_file, true))?;
    let serialized_bytecode = bytecode.serialize().map_err(|e| e.to_string())?;

    let output_file = output_file
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(input_file).with_extension("src"));

    File::create(output_file)?.write_all(&serialized_bytecode)?;

    Ok(())
}

pub(crate) fn run(input_file: &str) -> Result<(), CliError> {
    let bytecode_file = File::open(input_file)?;
    let mut bytecode_file = BufReader::new(bytecode_file);
    let mut serialized_bytecode = Vec::new();
    bytecode_file.read_to_end(&mut serialized_bytecode)?;
    let bytecode = Bytecode::deserialize(serialized_bytecode).map_err(|e| e.to_string())?;
    let mut vm = Vm::new(bytecode).map_err(|e| e.to_string())?;
    vm.run().map_err(|e| vm.format_error(e))?;
    Ok(())
}

pub(crate) fn repl() -> Result<(), CliError> {
    Repl::new()?.start()?;
    Ok(())
}
