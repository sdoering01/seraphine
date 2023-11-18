use std::process;

use crate::option_parser::{OptionParseAction, OptionParser};

mod actions;
mod error;
mod option_parser;
mod repl;

macro_rules! eprintln_red {
    () => {
        std::eprintln!()
    };
    ($($arg:tt)*) => {{
        std::eprint!("{}", termion::color::Fg(termion::color::Red));
        std::eprint!($($arg)*);
        std::eprintln!("{}", termion::color::Fg(termion::color::Reset));
    }};
}

pub fn main() {
    // Skip the first argument, which is the path to the CLI executable
    let args = std::env::args().skip(1);

    let mut option_parser = OptionParser::new(args);
    let (action, result) = match option_parser.parse() {
        Err(e) => {
            eprintln_red!("seraphine: {}", e);
            // TODO: Only print, when interactive
            eprintln!("{}", option_parser.help_hint());
            process::exit(1);
        }
        Ok(OptionParseAction::Help) => {
            eprintln!("{}", option_parser.help());
            process::exit(0);
        }
        Ok(OptionParseAction::Eval {
            input_file,
            runtime,
        }) => ("eval", actions::eval(&input_file, runtime)),
        Ok(OptionParseAction::Compile {
            input_file,
            output_file,
        }) => ("compile", actions::compile(&input_file, output_file)),
        Ok(OptionParseAction::Run { input_file }) => ("eval", actions::run(&input_file)),
        Ok(OptionParseAction::Repl) => ("repl", actions::repl()),
    };

    if let Err(e) = result {
        eprintln_red!("seraphine {}: {}", action, e,);
        process::exit(1);
    }
}
