use std::{io, process};

mod common;
mod error;
mod eval;
mod parser;
mod tokenizer;

use error::CalcError;
use eval::{evaluate, Context, Value};
use parser::parse;
use tokenizer::tokenize;

fn eval_str_ctx(s: &str, ctx: &mut Context) -> Result<Value, CalcError> {
    let tokens = tokenize(s)?;
    if cfg!(debug_assertions) {
        println!("Tokens: {:?}", tokens);
    }

    let ast = parse(&tokens)?;
    if cfg!(debug_assertions) {
        println!("AST: {:#?}", ast);
    }

    let result = evaluate(&ast, ctx)?;
    Ok(result)
}

fn eval_file(path: &str) -> Result<(), CalcError> {
    let mut ctx = Context::new();
    let contents = std::fs::read_to_string(path)?;
    match eval_str_ctx(&contents, &mut ctx) {
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

fn repl() {
    // TODO: Implement proper multi-line support
    let mut ctx = Context::new();
    let _stdout = io::stdout();
    let mut input = String::new();

    loop {
        let mut line = String::new();
        match std::io::stdin().read_line(&mut line) {
            Ok(_) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                input.push_str(line);
                match eval_str_ctx(&input, &mut ctx) {
                    Ok(result) => {
                        println!("{}", result);
                        input.clear();
                    }
                    // Artifact from previous band aid multi-line support
                    // input incomplete => {
                    //     input.push('\n');
                    //     print!("> ");
                    //     stdout.lock().flush().expect("Failed to flush stdout");
                    // }
                    Err(err) => {
                        eprintln!("{}", err.format(&input, "<repl>"));
                        input.clear();
                    }
                }
            }
            Err(err) => eprintln!("Error: {}", err),
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        if eval_file(&args[1]).is_err() {
            process::exit(1);
        }
    } else {
        repl();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eval::Value;

    fn eval_str(s: &str) -> Result<Value, CalcError> {
        eval_str_ctx(s, &mut Context::new())
    }

    macro_rules! assert_eq_num {
        ( $left:expr, $right:expr ) => {
            match ($left, $right) {
                (value, expected) => {
                    let $crate::eval::Value::Number(got) = value else {
                        ::std::panic!("value is not a number");
                    };
                    ::std::assert_eq!(got, expected);
                }
            }
        };
        ( $left:expr, $right:expr, $eps:expr ) => {
            match ($left, $right) {
                (value, expected) => {
                    let $crate::eval::Value::Number(got) = value else {
                        ::std::panic!("value is not a number");
                    };
                    ::std::assert!((got - expected).abs() < $eps);
                }
            }
        };
    }

    macro_rules! assert_eq_bool {
        ( $left:expr, $right:expr ) => {
            match ($left, $right) {
                (value, expected) => {
                    let $crate::eval::Value::Bool(got) = value else {
                        ::std::panic!("value is not a bool");
                    };
                    ::std::assert_eq!(got, expected);
                }
            }
        };
    }

    #[test]
    fn test_eval_str() {
        assert!(eval_str("").is_ok());
        assert!(eval_str("-").is_err());
        assert!(eval_str("* 2").is_err());
        assert!(eval_str("2 +").is_err());
        assert_eq_num!(eval_str("2").unwrap(), 2.0);
        assert_eq_num!(eval_str("2 - 3").unwrap(), -1.0);
        assert_eq_num!(eval_str("2-3").unwrap(), -1.0);
        assert_eq_num!(eval_str("2 + 2 * 2").unwrap(), 6.0);
        assert_eq_num!(eval_str("3 * 2 * 5 + 10 / 5 - 8").unwrap(), 24.0);
    }

    #[test]
    fn test_precedence_bug_fix() {
        assert_eq_num!(eval_str("1 + 2 ^ 2 * 2").unwrap(), 9.0);
    }

    #[test]
    fn test_number_parsing() {
        assert!(eval_str(".1").is_ok());
        assert!(eval_str("1.1").is_ok());
        assert!(eval_str("1.").is_ok());
        assert!(eval_str("1e9").is_ok());
        assert!(eval_str(".1e9").is_ok());
        assert!(eval_str("1e-9").is_ok());
        assert!(eval_str("42e0").is_ok());
        assert!(eval_str("8.e2").is_ok());

        assert!(eval_str("2.3.4").is_err());
        assert!(eval_str("..").is_err());
        assert!(eval_str("..1").is_err());
        assert!(eval_str("1..").is_err());
        assert!(eval_str(".1.").is_err());
        assert!(eval_str(".e9").is_err());
        assert!(eval_str(".e").is_err());
        assert!(eval_str("1e9e4").is_err());
        assert!(eval_str("1e42.1").is_err());
    }

    #[test]
    fn test_unary_minus() {
        assert_eq_num!(eval_str("-2").unwrap(), -2.0);
        assert_eq_num!(eval_str("2--2").unwrap(), 4.0);
        assert_eq_num!(eval_str("2+-2").unwrap(), 0.0);
        assert_eq_num!(eval_str("-2+-2").unwrap(), -4.0);
        assert_eq_num!(eval_str("2---2").unwrap(), 0.0);
        assert!(eval_str("2*+-2").is_err());

        assert!(eval_str("-true").is_err());
    }

    #[test]
    fn test_brackets() {
        assert_eq_num!(eval_str("4 * (5 - 1)").unwrap(), 16.0);
        assert_eq_num!(eval_str("(2 + 2) * (3 + 3)").unwrap(), 24.0);
        assert_eq_num!(eval_str("(2 + 2)").unwrap(), 4.0);
        assert_eq_num!(eval_str("-(2 + 2)").unwrap(), -4.0);
        assert_eq_num!(eval_str("-((2 + 3) * 4)").unwrap(), -20.0);
        assert_eq_num!(eval_str("-((2 + -4) * 5) / 2").unwrap(), 5.0);
        assert_eq_num!(eval_str("(1 + 2) + 3").unwrap(), 6.0);
        assert!(eval_str("-2 + 2)").is_err());
        assert!(eval_str("-(2 + 2").is_err());
        assert!(eval_str("()").is_err());
    }

    #[test]
    fn test_power() {
        assert!(eval_str("4 ^").is_err());
        assert!(eval_str("^ 3").is_err());
        assert_eq_num!(eval_str("1 ^ -3").unwrap(), 1.0);
        assert_eq_num!(eval_str("(-1) ^ -3").unwrap(), -1.0);
        assert_eq_num!(eval_str("(-1) ^ -4").unwrap(), 1.0);
        assert_eq_num!(eval_str("2 ^ -3").unwrap(), 0.125);
        assert_eq_num!(eval_str("2 ^ 0").unwrap(), 1.0);
        assert_eq_num!(eval_str("3 ^ 5").unwrap(), 243.0);
        assert_eq_num!(eval_str("-1 ^ 4").unwrap(), 1.0);
        assert_eq_num!(eval_str("-1 ^ 5").unwrap(), -1.0);
        assert_eq_num!(eval_str("-1 ^ -5").unwrap(), -1.0);
        assert_eq_num!(eval_str("(1 + 1) ^ (4 * 2)").unwrap(), 256.0);

        assert!(eval_str("true ^ true").is_err());
    }

    #[test]
    fn test_mod() {
        assert!(eval_str("2 %").is_err());
        assert!(eval_str("% 3").is_err());
        assert_eq_bool!(eval_str("is_nan(100 % 0)").unwrap(), true);
        assert_eq_num!(eval_str("7 % 3").unwrap(), 1.0);
        assert_eq_num!(eval_str("7 % -3").unwrap(), 1.0);
        assert_eq_num!(eval_str("-7 % 3").unwrap(), -1.0);
        assert_eq_num!(eval_str("-9 % -3").unwrap(), 0.0);
        assert_eq_num!(eval_str("42 % 1337").unwrap(), 42.0);
        assert_eq_num!(eval_str("2 + 3 * 4 % 5").unwrap(), 4.0);

        assert!(eval_str("true % false").is_err());
    }

    #[test]
    fn test_variables() {
        let mut ctx = Context::new();
        assert_eq_num!(eval_str_ctx("a = 2", &mut ctx).unwrap(), 2.0);
        assert_eq_num!(eval_str_ctx("b = a + 1", &mut ctx).unwrap(), 3.0);
        assert_eq_num!(eval_str_ctx("c = a + b", &mut ctx).unwrap(), 5.0);
        assert_eq_num!(ctx.get_var("a").unwrap(), 2.0);
        assert_eq_num!(ctx.get_var("b").unwrap(), 3.0);
        assert_eq_num!(ctx.get_var("c").unwrap(), 5.0);

        assert!(eval_str("not_defined").is_err());

        let mut ctx = Context::new();
        assert_eq_num!(eval_str_ctx("some_longer_name = 2", &mut ctx).unwrap(), 2.0);
        assert_eq_num!(ctx.get_var("some_longer_name").unwrap(), 2.0);

        assert!(eval_str("a b = 2").is_err());
        assert!(eval_str("2 = 2").is_err());
        assert!(eval_str("* = 2").is_err());
        assert!(eval_str("() = 2").is_err());
    }

    #[test]
    fn test_builtin_functions() {
        use std::f64::consts;

        let eps = 1e-10;
        assert_eq_num!(eval_str("sin(pi/2)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("cos(pi/2)").unwrap(), 0.0, eps);
        assert_eq_num!(eval_str("tan(pi/4)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("asin(1)").unwrap(), consts::FRAC_PI_2, eps);
        assert_eq_num!(eval_str("acos(1)").unwrap(), 0.0, eps);
        assert_eq_num!(eval_str("atan(1)").unwrap(), consts::FRAC_PI_4, eps);
        assert_eq_num!(eval_str("sinh(1)").unwrap(), 1_f64.sinh(), eps);
        assert_eq_num!(eval_str("cosh(1)").unwrap(), 1_f64.cosh(), eps);
        assert_eq_num!(eval_str("tanh(1)").unwrap(), 1_f64.tanh(), eps);

        assert_eq_num!(eval_str("ln(e)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("log2(1024)").unwrap(), 10.0, eps);
        assert_eq_num!(eval_str("log10(1000)").unwrap(), 3.0, eps);
        assert_eq_num!(eval_str("log(27, 3)").unwrap(), 3.0, eps);
        assert_eq_bool!(eval_str("is_nan(log(42, -21))").unwrap(), true);

        assert_eq_num!(eval_str("abs(-1)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("abs(1)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("min(1, 5)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("max(1, 5)").unwrap(), 5.0, eps);
        assert_eq_num!(eval_str("floor(1.5)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("ceil(1.5)").unwrap(), 2.0, eps);
        assert_eq_num!(eval_str("round(1.5)").unwrap(), 2.0, eps);
        assert_eq_num!(eval_str("round(1.4)").unwrap(), 1.0, eps);
        assert_eq_num!(eval_str("round(1.6)").unwrap(), 2.0, eps);

        assert_eq_num!(eval_str("sqrt(4)").unwrap(), 2.0, eps);
        assert_eq_bool!(eval_str("is_nan(sqrt(-1))").unwrap(), true);
        assert_eq_num!(eval_str("exp(2)").unwrap(), 7.389056099, eps);

        assert!(eval_str("exp(true)").is_err());

        assert_eq_bool!(eval_str("is_nan(nan)").unwrap(), true);
        assert_eq_bool!(eval_str("is_nan(inf)").unwrap(), false);
        assert_eq_bool!(eval_str("is_nan(42)").unwrap(), false);
        assert_eq_bool!(eval_str("is_infinite(nan)").unwrap(), false);
        assert_eq_bool!(eval_str("is_infinite(inf)").unwrap(), true);
        assert_eq_bool!(eval_str("is_infinite(42)").unwrap(), false);
        assert_eq_bool!(eval_str("is_infinite(1/0)").unwrap(), true);
        assert_eq_bool!(eval_str("is_finite(nan)").unwrap(), false);
        assert_eq_bool!(eval_str("is_finite(inf)").unwrap(), false);
        assert_eq_bool!(eval_str("is_finite(42)").unwrap(), true);
    }

    #[test]
    fn test_functions() {
        use crate::eval::Function;

        let mut ctx = Context::new();
        ctx.add_function(
            "add",
            Function::new_builtin(2, |_ctx, args| {
                let Value::Number(arg1) = args[0] else {
                    unreachable!()
                };
                let Value::Number(arg2) = args[1] else {
                    unreachable!()
                };
                Ok(Value::Number(arg1 + arg2))
            }),
        )
        .unwrap();

        assert!(eval_str_ctx("add()", &mut ctx).is_err());
        assert!(eval_str_ctx("add(1)", &mut ctx).is_err());
        assert!(eval_str_ctx("add(1,)", &mut ctx).is_err());
        assert!(eval_str_ctx("add(,1)", &mut ctx).is_err());
        assert!(eval_str_ctx("add(1 1)", &mut ctx).is_err());
        assert_eq_num!(eval_str_ctx("add(1, 2)", &mut ctx).unwrap(), 3.0);
        assert!(eval_str_ctx("add(1, 2, 3)", &mut ctx).is_err());
        assert_eq_num!(eval_str_ctx("add(1, add(2, 3))", &mut ctx).unwrap(), 6.0);
    }

    #[test]
    fn test_multiple_lines() {
        let mut ctx = Context::new();

        let result = eval_str_ctx(
            r"a = 2
            b = 3
            c = a + b",
            &mut ctx,
        )
        .unwrap();

        assert_eq_num!(result, 5.0);
        assert_eq_num!(ctx.get_var("a").unwrap(), 2.0);
        assert_eq_num!(ctx.get_var("b").unwrap(), 3.0);
        assert_eq_num!(ctx.get_var("c").unwrap(), 5.0);

        assert_eq_num!(eval_str("\n42\n").unwrap(), 42.0);
        assert_eq_num!(eval_str("42\n").unwrap(), 42.0);
        assert_eq_num!(eval_str("\n42").unwrap(), 42.0);
        assert_eq_num!(eval_str("\n\n\n").unwrap(), 0.0);
    }

    #[test]
    fn test_newlines_not_allowed() {
        assert!(eval_str("1 + \n 2").is_err());
        assert!(eval_str("sin(pi\n/2)").is_err());
        assert!(eval_str("sin(\npi/2)").is_err());
        assert!(eval_str("1 * (2 + \n 3)").is_err());
        assert!(eval_str("a = \n2").is_err());
    }

    #[test]
    fn test_user_functions() {
        let code = "\
            fn add(a, b, c) {\n\
                a + b + c\n\
            }\n\
            \n\
            fn sub(a, b) {\n\
                a - b\n\
            }\n\
            \n\
            sub(42, add(1, 2, 3))";
        assert_eq_num!(eval_str(code).unwrap(), 36.0);

        assert!(eval_str("fn add(a, {b) a + b }").is_err());
        assert!(eval_str("fn empty_body() {}").is_ok());
        assert!(eval_str("fn no_args() {\n inspect(1)\n }").is_ok());
        assert!(eval_str("fn one_liner(a, b) { a + b }").is_ok());
        assert!(eval_str("fn trailing_comma(a, b,) { a + b }").is_err());
        assert!(eval_str("fn leading_comma(, a, b) { a + b }").is_err());
        assert!(eval_str("fn no_comma(a b) { a + b }").is_err());
        assert!(eval_str("fn contains_expression(a, b, 1 + 1) { a + b }").is_err());
        assert!(eval_str("fn duplicate_arg_name(a, a) { a + a }").is_err());
    }

    #[test]
    fn test_if_statements() {
        let code = "\
            a = 0
            if (false) {
                a = 2
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 0.0);

        let code = "\
            a = 0
            if (true) {
                a = 2
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 2.0);

        let code = "\
            a = 0
            if (false) {
                a = 2
            } else {
                a = 3
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let code = "\
            a = 0
            if (true) {
                a = 2
            } else {
                a = 3
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 2.0);

        let code = "\
            a = 0
            if (false) {
                a = 2
            } else if (false) {
                a = 3
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 0.0);

        let code = "\
            a = 0
            if (false) {
                a = 2
            } else if (false) {
                a = 3
            } else {
                a = 4
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 4.0);

        let code = "\
            a = 0
            if (true) {
                a = 2
            } else if (false) {
                a = 3
            } else {
                a = 4
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 2.0);

        let code = "\
            a = 0
            if (true) {
                a = 2
            } else if (true) {
                a = 3
            } else {
                a = 4
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 2.0);

        let code = "\
            a = 0
            if (false) {
                a = 2
            } else if (true) {
                a = 3
            } else {
                a = 4
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let code = "\
            a = 0
            if (false) {
                a = 2
            } else if (false) {
                a = 3
            } else if (true) {
                a = 4
            } else {
                a = 5
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 4.0);
    }

    #[test]
    fn test_boolean_negate_operator() {
        assert_eq_bool!(eval_str("!1").unwrap(), false);
        assert_eq_bool!(eval_str("!0.01").unwrap(), false);
        assert_eq_bool!(eval_str("!0.00001").unwrap(), false);
        assert_eq_bool!(eval_str("!42").unwrap(), false);
        assert_eq_bool!(eval_str("!-42").unwrap(), false);
        assert_eq_bool!(eval_str("!-1").unwrap(), false);
        assert_eq_bool!(eval_str("!!42").unwrap(), true);

        assert_eq_bool!(eval_str("!true").unwrap(), false);
        assert_eq_bool!(eval_str("!!!true").unwrap(), false);
        assert_eq_bool!(eval_str("!false").unwrap(), true);
    }

    #[test]
    fn test_equality_operator() {
        assert_eq_bool!(eval_str("1 == 0").unwrap(), false);
        assert_eq_bool!(eval_str("-1 == -2").unwrap(), false);
        assert_eq_bool!(eval_str("42 == 21").unwrap(), false);
        assert_eq_bool!(eval_str("0 == 0").unwrap(), true);
        assert_eq_bool!(eval_str("42 == 42").unwrap(), true);
        assert_eq_bool!(eval_str("-1 == -1").unwrap(), true);
        assert_eq_bool!(eval_str("-0 == 0").unwrap(), true);

        assert_eq_bool!(eval_str("true == true").unwrap(), true);
        assert_eq_bool!(eval_str("false == true").unwrap(), false);
        assert_eq_bool!(eval_str("1.0 == true").unwrap(), false);
        assert_eq_bool!(eval_str("false == 0.0").unwrap(), false);
    }

    #[test]
    fn test_inequality_operator() {
        assert_eq_bool!(eval_str("1 != 0").unwrap(), true);
        assert_eq_bool!(eval_str("-1 != -2").unwrap(), true);
        assert_eq_bool!(eval_str("42 != 21").unwrap(), true);
        assert_eq_bool!(eval_str("-42 != 42").unwrap(), true);
        assert_eq_bool!(eval_str("0 != 0").unwrap(), false);
        assert_eq_bool!(eval_str("42 != 42").unwrap(), false);
        assert_eq_bool!(eval_str("-1 != -1").unwrap(), false);
        assert_eq_bool!(eval_str("-0 != 0").unwrap(), false);

        assert_eq_bool!(eval_str("true != true").unwrap(), false);
        assert_eq_bool!(eval_str("false != true").unwrap(), true);
        assert_eq_bool!(eval_str("1.0 != true").unwrap(), true);
        assert_eq_bool!(eval_str("false != 0.0").unwrap(), true);
    }

    #[test]
    fn test_less_than_operator() {
        assert_eq_bool!(eval_str("0 < 1").unwrap(), true);
        assert_eq_bool!(eval_str("-2 < -1").unwrap(), true);
        assert_eq_bool!(eval_str("42 < 21").unwrap(), false);
        assert_eq_bool!(eval_str("-42 < 42").unwrap(), true);
        assert_eq_bool!(eval_str("0 < 0").unwrap(), false);
        assert_eq_bool!(eval_str("42 < 42").unwrap(), false);
        assert_eq_bool!(eval_str("-1 < -1").unwrap(), false);
        assert_eq_bool!(eval_str("-0 < 0").unwrap(), false);

        assert!(eval_str("true < 2").is_err());
        assert!(eval_str("true < false").is_err());
        assert!(eval_str("7 < false").is_err());
    }

    #[test]
    fn test_greater_than_operator() {
        assert_eq_bool!(eval_str("0 > 1").unwrap(), false);
        assert_eq_bool!(eval_str("-2 > -1").unwrap(), false);
        assert_eq_bool!(eval_str("42 > 21").unwrap(), true);
        assert_eq_bool!(eval_str("-42 > 42").unwrap(), false);
        assert_eq_bool!(eval_str("0 > 0").unwrap(), false);
        assert_eq_bool!(eval_str("42 > 42").unwrap(), false);
        assert_eq_bool!(eval_str("-1 > -1").unwrap(), false);
        assert_eq_bool!(eval_str("-0 > 0").unwrap(), false);

        assert!(eval_str("true > 2").is_err());
        assert!(eval_str("true > false").is_err());
        assert!(eval_str("7 > false").is_err());
    }

    #[test]
    fn test_less_than_or_equal_operator() {
        assert_eq_bool!(eval_str("0 <= 1").unwrap(), true);
        assert_eq_bool!(eval_str("-2 <= -1").unwrap(), true);
        assert_eq_bool!(eval_str("42 <= 21").unwrap(), false);
        assert_eq_bool!(eval_str("-42 <= 42").unwrap(), true);
        assert_eq_bool!(eval_str("0 <= 0").unwrap(), true);
        assert_eq_bool!(eval_str("42 <= 42").unwrap(), true);
        assert_eq_bool!(eval_str("-1 <= -1").unwrap(), true);
        assert_eq_bool!(eval_str("-0 <= 0").unwrap(), true);

        assert!(eval_str("true <= 2").is_err());
        assert!(eval_str("true <= false").is_err());
        assert!(eval_str("7 <= false").is_err());
    }

    #[test]
    fn test_greater_than_or_equal_operator() {
        assert_eq_bool!(eval_str("0 >= 1").unwrap(), false);
        assert_eq_bool!(eval_str("-2 >= -1").unwrap(), false);
        assert_eq_bool!(eval_str("42 >= 21").unwrap(), true);
        assert_eq_bool!(eval_str("-42 >= 42").unwrap(), false);
        assert_eq_bool!(eval_str("0 >= 0").unwrap(), true);
        assert_eq_bool!(eval_str("42 >= 42").unwrap(), true);
        assert_eq_bool!(eval_str("-1 >= -1").unwrap(), true);
        assert_eq_bool!(eval_str("-0 >= 0").unwrap(), true);

        assert!(eval_str("true >= 2").is_err());
        assert!(eval_str("true >= false").is_err());
        assert!(eval_str("7 >= false").is_err());
    }

    #[test]
    fn test_and_operator() {
        assert_eq_bool!(eval_str("0 && 0").unwrap(), false);
        assert_eq_bool!(eval_str("8 && 0").unwrap(), false);
        assert_eq_bool!(eval_str("-7 && 0").unwrap(), false);
        assert_eq_bool!(eval_str("0 && 15").unwrap(), false);
        assert_eq_bool!(eval_str("0 && -27").unwrap(), false);
        assert_eq_bool!(eval_str("18 && 1").unwrap(), true);
        assert_eq_bool!(eval_str("42 && -42").unwrap(), true);

        let code = "\
            a = 2
            a > 0 && a > 1";
        assert_eq_bool!(eval_str(code).unwrap(), true);

        assert_eq_bool!(eval_str("1 + 1 > 1 && 2 + 2 > 2").unwrap(), true);

        assert_eq_bool!(eval_str("false && true").unwrap(), false);
        assert_eq_bool!(eval_str("false && false").unwrap(), false);
        assert_eq_bool!(eval_str("true && true").unwrap(), true);
        assert_eq_bool!(eval_str("1 && true").unwrap(), true);
    }

    #[test]
    fn test_and_operator_short_circuit() {
        let mut ctx = Context::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            false && set_a()
        ";
        eval_str_ctx(code, &mut ctx).unwrap();
        assert!(!ctx._internal_side_effect_flag);

        let mut ctx = Context::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            true && set_a()
        ";
        eval_str_ctx(code, &mut ctx).unwrap();
        assert!(ctx._internal_side_effect_flag);
    }

    #[test]
    fn test_or_operator() {
        assert_eq_bool!(eval_str("0 || 0").unwrap(), false);
        assert_eq_bool!(eval_str("8 || 0").unwrap(), true);
        assert_eq_bool!(eval_str("-7 || 0").unwrap(), true);
        assert_eq_bool!(eval_str("0 || 15").unwrap(), true);
        assert_eq_bool!(eval_str("0 || -27").unwrap(), true);
        assert_eq_bool!(eval_str("18 || 1").unwrap(), true);
        assert_eq_bool!(eval_str("42 || -42").unwrap(), true);

        assert_eq_bool!(eval_str("false || true").unwrap(), true);
        assert_eq_bool!(eval_str("false || false").unwrap(), false);
        assert_eq_bool!(eval_str("true || true").unwrap(), true);
        assert_eq_bool!(eval_str("1 || true").unwrap(), true);
    }

    #[test]
    fn test_or_operator_short_circuit() {
        let mut ctx = Context::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            false || set_a()
        ";
        eval_str_ctx(code, &mut ctx).unwrap();
        assert!(ctx._internal_side_effect_flag);

        let mut ctx = Context::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            true || set_a()
        ";
        eval_str_ctx(code, &mut ctx).unwrap();
        assert!(!ctx._internal_side_effect_flag);
    }

    #[test]
    fn test_while_loop() {
        let code = "\
            a = 0
            while (0) {
                a = 1
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 0.0);

        let code = "\
            a = 0
            while (2 * a < 10) {
                a = a + 1
            }
            a";
        assert_eq_num!(eval_str(code).unwrap(), 5.0);
    }

    #[test]
    fn test_continue_statement() {
        assert!(eval_str("continue").is_err());
        assert!(eval_str("while (true) { continue 42 }").is_err());

        let code = "\
            fn my_func() {
                continue
            }

            a = 0
            b = 0
            while (a == 0) {
                a = 1
                my_func()
                b = 1
            }
            b
        ";
        assert!(eval_str(code).is_err());

        let code = "\
            a = 0
            b = 0
            while (a < 10) {
                a = a + 1
                if (a % 2 == 0) {
                    continue
                }
                b = b + 1
            }
            b";
        assert_eq_num!(eval_str(code).unwrap(), 5.0);
    }

    #[test]
    fn test_break_statement() {
        assert!(eval_str("break").is_err());
        assert!(eval_str("while (true) { break 42 }").is_err());

        let code = "\
            fn my_func() {
                break
            }

            a = 0
            b = 0
            while (a == 0) {
                a = 1
                my_func()
                b = 1
            }
            b
        ";
        assert!(eval_str(code).is_err());

        let code = "\
            a = 0
            b = 0
            while (a < 10) {
                a = a + 1
                if (a % 3 == 0) {
                    break
                }
                b = b + 1
            }
            b";
        assert_eq_num!(eval_str(code).unwrap(), 2.0);
    }

    #[test]
    fn test_return_statement() {
        assert!(eval_str("return").is_err());
        assert!(eval_str("return 1").is_err());
        assert!(eval_str("return 1 + 1").is_err());

        let code = "\
            fn do_nothing() {
                return
                1
            }
            do_nothing()";
        assert_eq_num!(eval_str(code).unwrap(), 0.0);

        let code = "\
            fn do_nothing() { return }
            do_nothing()";
        assert_eq_num!(eval_str(code).unwrap(), 0.0);

        let code = "\
            fn add(a, b) {
                return a + b
                a - b
            }
            add(1, 2)";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let mut ctx = Context::new();
        let code = "\
            fn my_abs(num) {
                if (num < 0) {
                    return -num
                }
                num
            }";
        assert!(eval_str_ctx(code, &mut ctx).is_ok());
        assert_eq_num!(eval_str_ctx("my_abs(-1)", &mut ctx).unwrap(), 1.0);
        assert_eq_num!(eval_str_ctx("my_abs(42)", &mut ctx).unwrap(), 42.0);
        assert_eq_num!(eval_str_ctx("my_abs(-0.23)", &mut ctx).unwrap(), 0.23);

        let code = "\
            fn a() {
                b()
                return 1
            }

            fn b() {
                c()
                return 2
            }
            
            fn c() {
                return 3
            }

            a()";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);
    }

    #[test]
    fn test_errors_on_missing_newline() {
        assert!(eval_str("1 + 1 2 + 2").is_err());
        assert!(eval_str("1 2").is_err());
        assert!(eval_str("(1 * 3) 2").is_err());

        assert!(eval_str("fn add(a, b) { a + b } fn sub(a, b) { a - b }").is_err());
        assert!(eval_str("if (1){ 1 } if (2){ 2 }").is_err());
    }

    #[test]
    fn test_comments() {
        let code = "\
            // This function adds to numbers
            fn add(a, b) {
                a + b  // <- this is where the magic happens
            }

            sum = add(1, 2) // <- assignment that requires a newline after it
            sum// no space before comment
        ";

        assert_eq_num!(eval_str(code).unwrap(), 3.0);
    }

    #[test]
    fn test_boolean_literals() {
        assert_eq_bool!(eval_str("true").unwrap(), true);
        assert_eq_bool!(eval_str("false").unwrap(), false);
    }
}
