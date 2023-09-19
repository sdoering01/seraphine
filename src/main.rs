use std::process;

#[cfg(feature = "repl")]
use seraphine::repl;
use seraphine::{error::SeraphineError, eval::Context};

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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        if eval_file(&args[1]).is_err() {
            process::exit(1);
        }
        Ok(())
    } else {
        #[cfg(feature = "repl")]
        {
            repl::repl()
        }
        #[cfg(not(feature = "repl"))]
        {
            eprintln!("No file specified");
            process::exit(1);
        }
    }
}
