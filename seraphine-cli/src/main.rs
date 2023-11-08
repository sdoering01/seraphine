use std::process;

use termion::color;

use seraphine_core::{error::SeraphineError, eval::Evaluator};

mod repl;

fn eval_file(path: &str) -> Result<(), SeraphineError> {
    let mut eval = Evaluator::new();
    let contents = std::fs::read_to_string(path)?;
    if let Err(e) = eval.eval_str(&contents) {
        eprintln!(
            "{}{}{}",
            color::Fg(color::Red),
            e.format(&contents, path),
            color::Fg(color::Reset)
        );
        Err(e)
    } else {
        Ok(())
    }
}

pub fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        if eval_file(&args[1]).is_err() {
            process::exit(1);
        }
        Ok(())
    } else {
        repl::repl()
    }
}
