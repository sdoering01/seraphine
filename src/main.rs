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
        assert_eq!(eval_str("2").unwrap(), 2.0);
        assert_eq!(eval_str("2 - 3").unwrap(), -1.0);
        assert_eq!(eval_str("2-3").unwrap(), -1.0);
        assert_eq!(eval_str("2 + 2 * 2").unwrap(), 6.0);
        assert_eq!(eval_str("3 * 2 * 5 + 10 / 5 - 8").unwrap(), 24.0);
    }

    #[test]
    fn test_number_parsing() {
        assert!(eval_str(".1").is_ok());
        assert!(eval_str("1.1").is_ok());
        assert!(eval_str("1.").is_ok());

        assert!(eval_str("2.3.4").is_err());
        assert!(eval_str("..").is_err());
        assert!(eval_str("..1").is_err());
        assert!(eval_str("1..").is_err());
        assert!(eval_str(".1.").is_err());
    }

    #[test]
    fn test_unary_plus() {
        assert_eq!(eval_str("+2").unwrap(), 2.0);
        assert_eq!(eval_str("2-+2").unwrap(), 0.0);
        assert_eq!(eval_str("2++2").unwrap(), 4.0);
        assert_eq!(eval_str("+2++2").unwrap(), 4.0);
        assert!(eval_str("2+++2").is_err());
        assert!(eval_str("2*-+2").is_err());
    }

    #[test]
    fn test_unary_minus() {
        assert_eq!(eval_str("-2").unwrap(), -2.0);
        assert_eq!(eval_str("2--2").unwrap(), 4.0);
        assert_eq!(eval_str("2+-2").unwrap(), 0.0);
        assert_eq!(eval_str("-2+-2").unwrap(), -4.0);
        assert!(eval_str("2---2").is_err());
        assert!(eval_str("2*+-2").is_err());
    }

    #[test]
    fn test_brackets() {
        assert_eq!(eval_str("4 * (5 - 1)").unwrap(), 16.0);
        assert_eq!(eval_str("(2 + 2) * (3 + 3)").unwrap(), 24.0);
        assert_eq!(eval_str("(2 + 2)").unwrap(), 4.0);
        assert_eq!(eval_str("-(2 + 2)").unwrap(), -4.0);
        assert_eq!(eval_str("-((2 + 3) * 4)").unwrap(), -20.0);
        assert_eq!(eval_str("-((2 + -4) * 5) / 2").unwrap(), 5.0);
        assert_eq!(eval_str("(1 + 2) + 3").unwrap(), 6.0);
        assert!(eval_str("-2 + 2)").is_err());
        assert!(eval_str("-(2 + 2").is_err());
        assert!(eval_str("()").is_err());
    }

    #[test]
    fn test_power() {
        assert!(eval_str("4 ^").is_err());
        assert!(eval_str("^ 3").is_err());
        assert_eq!(eval_str("1 ^ -3").unwrap(), 1.0);
        assert_eq!(eval_str("(-1) ^ -3").unwrap(), -1.0);
        assert_eq!(eval_str("(-1) ^ -4").unwrap(), 1.0);
        assert_eq!(eval_str("2 ^ -3").unwrap(), 0.125);
        assert_eq!(eval_str("2 ^ 0").unwrap(), 1.0);
        assert_eq!(eval_str("3 ^ 5").unwrap(), 243.0);
        assert_eq!(eval_str("-1 ^ 4").unwrap(), 1.0);
        assert_eq!(eval_str("-1 ^ 5").unwrap(), -1.0);
        assert_eq!(eval_str("-1 ^ -5").unwrap(), -1.0);
        assert_eq!(eval_str("(1 + 1) ^ (4 * 2)").unwrap(), 256.0);
    }

    #[test]
    fn test_mod() {
        assert!(eval_str("2 %").is_err());
        assert!(eval_str("% 3").is_err());
        assert!(eval_str("100 % 0").is_err());
        assert_eq!(eval_str("7 % 3").unwrap(), 1.0);
        assert_eq!(eval_str("7 % -3").unwrap(), 1.0);
        assert_eq!(eval_str("-7 % 3").unwrap(), -1.0);
        assert_eq!(eval_str("-9 % -3").unwrap(), 0.0);
        assert_eq!(eval_str("42 % 1337").unwrap(), 42.0);
        assert_eq!(eval_str("2 + 3 * 4 % 5").unwrap(), 4.0);
    }

    #[test]
    fn test_variables() {
        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("a = 2", &mut ctx).unwrap(), 2.0);
        assert_eq!(eval_str_ctx("b = a + 1", &mut ctx).unwrap(), 3.0);
        assert_eq!(eval_str_ctx("c = a + b", &mut ctx).unwrap(), 5.0);
        assert_eq!(ctx.get_var("a"), Some(2.0));
        assert_eq!(ctx.get_var("b"), Some(3.0));
        assert_eq!(ctx.get_var("c"), Some(5.0));

        assert!(eval_str("not_defined").is_err());

        let mut ctx = Context::new();
        assert_eq!(
            eval_str_ctx("a = -(b = ((c = -8) * 5) - 2)", &mut ctx).unwrap(),
            42.0
        );
        assert_eq!(ctx.get_var("a"), Some(42.0));
        assert_eq!(ctx.get_var("b"), Some(-42.0));
        assert_eq!(ctx.get_var("c"), Some(-8.0));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("some_longer_name = 2", &mut ctx).unwrap(), 2.0);
        assert_eq!(ctx.get_var("some_longer_name"), Some(2.0));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("(a = 2)", &mut ctx).unwrap(), 2.0);
        assert_eq!(ctx.get_var("a"), Some(2.0));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("(a = 2) * a", &mut ctx).unwrap(), 4.0);
        assert_eq!(ctx.get_var("a"), Some(2.0));

        assert!(eval_str("a * (a = 2)").is_err());

        assert!(eval_str("a b = 2").is_err());
        assert!(eval_str("2 = 2").is_err());
        assert!(eval_str("* = 2").is_err());
        assert!(eval_str("() = 2").is_err());
    }

    #[test]
    fn test_builtin_functions() {
        use std::f64::consts;

        let eps = 1e-10;
        assert!((eval_str("sin(pi/2)").unwrap() - 1.0).abs() < eps);
        assert!((eval_str("cos(pi/2)").unwrap() - 0.0).abs() < eps);
        assert!((eval_str("tan(pi/4)").unwrap() - 1.0).abs() < eps);
        assert!((eval_str("asin(1)").unwrap() - consts::FRAC_PI_2).abs() < eps);
        assert!((eval_str("acos(1)").unwrap() - 0.0).abs() < eps);
        assert!((eval_str("atan(1)").unwrap() - consts::FRAC_PI_4).abs() < eps);
        assert!((eval_str("sinh(1)").unwrap() - 1_f64.sinh()).abs() < eps);
        assert!((eval_str("cosh(1)").unwrap() - 1_f64.cosh()).abs() < eps);
        assert!((eval_str("tanh(1)").unwrap() - 1_f64.tanh()).abs() < eps);

        assert!((eval_str("ln(e)").unwrap() - 1.0).abs() < eps);
        assert!((eval_str("log2(1024)").unwrap() - 10.0).abs() < eps);
        assert!((eval_str("log10(1000)").unwrap() - 3.0).abs() < eps);
        assert!((eval_str("log(27, 3)").unwrap() - 3.0).abs() < eps);

        assert!((eval_str("abs(-1)").unwrap() - 1.0).abs() < eps);
        assert!((eval_str("abs(1)").unwrap() - 1.0).abs() < eps);
        assert!((eval_str("min(1, 5)").unwrap() - 1.0).abs() < eps);
        assert!((eval_str("max(1, 5)").unwrap() - 5.0).abs() < eps);
        // TODO: Implement floating point parsing first
        // assert!((eval_str("floor(1.5)").unwrap() - 1.0).abs() < eps);
        // assert!((eval_str("ceil(1.5)").unwrap() - 2.0).abs() < eps);
        // assert!((eval_str("round(1.5)").unwrap() - 2.0).abs() < eps);
        // assert!((eval_str("round(1.4)").unwrap() - 1.0).abs() < eps);
        // assert!((eval_str("round(1.6)").unwrap() - 2.0).abs() < eps);

        assert!(eval_str("sqrt(-1)").is_err());
        assert!((eval_str("sqrt(4)").unwrap() - 2.0).abs() < eps);
        assert!((eval_str("exp(2)").unwrap() - 7.389056099).abs() < eps);
    }

    #[test]
    fn test_functions() {
        use crate::eval::Function;

        let mut ctx = Context::new();
        ctx.add_function("add", Function::new(2, |_ctx, args| args[0] + args[1]));

        assert!(eval_str_ctx("add()", &mut ctx).is_err());
        assert!(eval_str_ctx("add(1)", &mut ctx).is_err());
        assert!(eval_str_ctx("add(1,)", &mut ctx).is_err());
        assert!(eval_str_ctx("add(,1)", &mut ctx).is_err());
        assert!(eval_str_ctx("add(1 1)", &mut ctx).is_err());
        assert_eq!(eval_str_ctx("add(1, 2)", &mut ctx).unwrap(), 3.0);
        assert!(eval_str_ctx("add(1, 2, 3)", &mut ctx).is_err());
        assert_eq!(eval_str_ctx("add(1, add(2, 3))", &mut ctx).unwrap(), 6.0);
    }
}
