mod error;
mod eval;
mod parser;
mod tokenizer;

use error::CalcError;
use eval::{evaluate, Context, Number};
use parser::parse;
use tokenizer::tokenize;

fn eval_str_ctx(s: &str, ctx: &mut Context) -> Result<Number, CalcError> {
    let tokens = tokenize(s)?;
    let ast = parse(&tokens)?;
    let result = evaluate(&ast, ctx)?;
    Ok(result)
}

fn main() {
    let mut ctx = Context::new();

    loop {
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }
                match eval_str_ctx(input, &mut ctx) {
                    Ok(result) => println!("{}", result),
                    Err(err) => eprintln!("{}", err),
                }
            }
            Err(err) => eprintln!("Error: {}", err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval_str(s: &str) -> Result<Number, CalcError> {
        eval_str_ctx(s, &mut Context::new())
    }

    #[test]
    fn test_eval_str() {
        assert!(eval_str("").is_err());
        assert!(eval_str("-").is_err());
        assert!(eval_str("* 2").is_err());
        assert!(eval_str("2 +").is_err());
        assert_eq!(eval_str("2").unwrap(), 2);
        assert_eq!(eval_str("2 - 3").unwrap(), -1);
        assert_eq!(eval_str("2-3").unwrap(), -1);
        assert_eq!(eval_str("2 + 2 * 2").unwrap(), 6);
        assert_eq!(eval_str("3 * 2 * 5 + 10 / 5 - 8").unwrap(), 24);
    }

    #[test]
    fn test_unary_plus() {
        assert_eq!(eval_str("+2").unwrap(), 2);
        assert_eq!(eval_str("2-+2").unwrap(), 0);
        assert_eq!(eval_str("2++2").unwrap(), 4);
        assert_eq!(eval_str("+2++2").unwrap(), 4);
        assert!(eval_str("2+++2").is_err());
        assert!(eval_str("2*-+2").is_err());
    }

    #[test]
    fn test_unary_minus() {
        assert_eq!(eval_str("-2").unwrap(), -2);
        assert_eq!(eval_str("2--2").unwrap(), 4);
        assert_eq!(eval_str("2+-2").unwrap(), 0);
        assert_eq!(eval_str("-2+-2").unwrap(), -4);
        assert!(eval_str("2---2").is_err());
        assert!(eval_str("2*+-2").is_err());
    }

    #[test]
    fn test_brackets() {
        assert_eq!(eval_str("4 * (5 - 1)").unwrap(), 16);
        assert_eq!(eval_str("(2 + 2) * (3 + 3)").unwrap(), 24);
        assert_eq!(eval_str("(2 + 2)").unwrap(), 4);
        assert_eq!(eval_str("-(2 + 2)").unwrap(), -4);
        assert_eq!(eval_str("-((2 + 3) * 4)").unwrap(), -20);
        assert_eq!(eval_str("-((2 + -4) * 5) / 2").unwrap(), 5);
        assert_eq!(eval_str("(1 + 2) + 3").unwrap(), 6);
        assert!(eval_str("-2 + 2)").is_err());
        assert!(eval_str("-(2 + 2").is_err());
        assert!(eval_str("()").is_err());
    }

    #[test]
    fn test_power() {
        assert!(eval_str("4 ^").is_err());
        assert!(eval_str("^ 3").is_err());
        assert_eq!(eval_str("1 ^ -3").unwrap(), 1);
        assert_eq!(eval_str("(-1) ^ -3").unwrap(), -1);
        assert_eq!(eval_str("(-1) ^ -4").unwrap(), 1);
        assert_eq!(eval_str("2 ^ -3").unwrap(), 0);
        assert_eq!(eval_str("2 ^ 0").unwrap(), 1);
        assert_eq!(eval_str("3 ^ 5").unwrap(), 243);
        assert_eq!(eval_str("-1 ^ 4").unwrap(), 1);
        assert_eq!(eval_str("-1 ^ 5").unwrap(), -1);
        assert_eq!(eval_str("-1 ^ -5").unwrap(), -1);
        assert_eq!(eval_str("(1 + 1) ^ (4 * 2)").unwrap(), 256);
    }

    #[test]
    fn test_overflow() {
        assert!(eval_str("2^63").is_err());
        assert!(eval_str("2^32 * 2^31").is_err());
        // 2^62 = 4611686018427387904
        assert!(eval_str("2^62 + 4611686018427387904").is_err());
        assert_eq!(eval_str("-2^63").unwrap(), (-2i64).pow(63));
        assert!(eval_str("-(2^62 * -2)").is_err());
        assert!(eval_str("-1 * (2^62 * -2)").is_err());
    }

    #[test]
    fn test_mod() {
        assert!(eval_str("2 %").is_err());
        assert!(eval_str("% 3").is_err());
        assert!(eval_str("100 % 0").is_err());
        assert_eq!(eval_str("7 % 3").unwrap(), 1);
        assert_eq!(eval_str("7 % -3").unwrap(), 1);
        assert_eq!(eval_str("-7 % 3").unwrap(), -1);
        assert_eq!(eval_str("-9 % -3").unwrap(), 0);
        assert_eq!(eval_str("42 % 1337").unwrap(), 42);
        assert_eq!(eval_str("2 + 3 * 4 % 5").unwrap(), 4);
    }

    #[test]
    fn test_variables() {
        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("a = 2", &mut ctx).unwrap(), 2);
        assert_eq!(eval_str_ctx("b = a + 1", &mut ctx).unwrap(), 3);
        assert_eq!(eval_str_ctx("c = a + b", &mut ctx).unwrap(), 5);
        assert_eq!(ctx.get_var("a"), Some(2));
        assert_eq!(ctx.get_var("b"), Some(3));
        assert_eq!(ctx.get_var("c"), Some(5));

        assert!(eval_str("not_defined").is_err());

        let mut ctx = Context::new();
        assert_eq!(
            eval_str_ctx("a = -(b = ((c = -8) * 5) - 2)", &mut ctx).unwrap(),
            42
        );
        assert_eq!(ctx.get_var("a"), Some(42));
        assert_eq!(ctx.get_var("b"), Some(-42));
        assert_eq!(ctx.get_var("c"), Some(-8));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("some_longer_name = 2", &mut ctx).unwrap(), 2);
        assert_eq!(ctx.get_var("some_longer_name"), Some(2));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("(a = 2)", &mut ctx).unwrap(), 2);
        assert_eq!(ctx.get_var("a"), Some(2));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("(a = 2) * a", &mut ctx).unwrap(), 4);
        assert_eq!(ctx.get_var("a"), Some(2));

        assert!(eval_str("a * (a = 2)").is_err());

        assert!(eval_str("a b = 2").is_err());
        assert!(eval_str("2 = 2").is_err());
        assert!(eval_str("* = 2").is_err());
        assert!(eval_str("() = 2").is_err());
    }

    #[test]
    fn test_functions() {
        assert!(eval_str("add()").is_err());
        assert!(eval_str("add(1)").is_err());
        assert!(eval_str("add(1,)").is_err());
        assert!(eval_str("add(,1)").is_err());
        assert!(eval_str("add(1 1)").is_err());
        assert_eq!(eval_str("add(1, 2)").unwrap(), 3);
        assert!(eval_str("add(1, 2, 3)").is_err());
        assert_eq!(eval_str("add(1, add(2, 3))").unwrap(), 6);
    }
}
