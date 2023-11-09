use std::process;

use termion::color;

use seraphine_core::{
    bytecode::generate, error::SeraphineError, eval::Evaluator, parser::parse, tokenizer::tokenize,
    vm::Vm,
};

use option_parser::{OptionParseAction, OptionParser, Runtime};

mod option_parser;
mod repl;

fn run_with_vm(code: &str) -> Result<(), SeraphineError> {
    let tokens = tokenize(code)?;
    let ast = parse(&tokens)?;
    let bytecode = generate(&ast);
    let mut vm = Vm::new(bytecode);
    vm.run().map_err(|e| e.into())
}

fn eval_code(code: &str, runtime: Runtime) -> Result<(), SeraphineError> {
    match runtime {
        Runtime::Evaluator => {
            let mut eval = Evaluator::new();
            eval.eval_str(code).map(|_| ())
        }
        Runtime::Vm => run_with_vm(code),
    }
}

pub fn main() {
    // Skip the first argument, which is the path to the CLI executable
    let args = std::env::args().skip(1);

    let mut option_parser = OptionParser::new();
    match option_parser.parse(args) {
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
        Ok(OptionParseAction::Help) => {
            println!("{}", option_parser.help());
            process::exit(0);
        }
        _ => {}
    }

    match option_parser.input_file() {
        Some(input_file) => {
            let code = match std::fs::read_to_string(input_file) {
                Ok(code) => code,
                Err(e) => {
                    eprintln!("Can't open file {}: {}", input_file, e);
                    process::exit(1);
                }
            };

            if let Err(e) = eval_code(&code, option_parser.runtime()) {
                eprintln!(
                    "{}{}{}",
                    color::Fg(color::Red),
                    e.format(&code, input_file),
                    color::Fg(color::Reset)
                );
                process::exit(1);
            }
        }
        None => {
            if let Err(e) = repl::repl(option_parser.runtime()) {
                eprintln!("Encountered error in REPL: {}", e);
                process::exit(1);
            }
        }
    }
}
