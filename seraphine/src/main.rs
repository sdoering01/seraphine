use std::process;

use seraphine_core::{error::SeraphineError, eval::Context};

mod repl;

fn eval_file(path: &str) -> Result<(), SeraphineError> {
    let mut ctx = Context::new();
    let contents = std::fs::read_to_string(path)?;
    match ctx.eval_str(&contents) {
        Ok(result) => {
            println!("{}", result);
            Ok(())
        }
        Err(e) => {
            eprintln!("{}", e.format(&contents, path));
            Err(e)
        }
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
