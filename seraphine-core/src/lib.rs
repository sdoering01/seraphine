pub mod bytecode;
pub mod error;
pub mod eval;
pub mod io;
pub mod parser;
pub mod stdlib;
pub mod tokenizer;
pub mod vm;

mod common;
mod macros;

#[cfg(test)]
mod tests {
    use std::io::Write;

    use crate::{
        error::SeraphineError,
        eval::{Evaluator, Function, Value},
        io,
        macros::{
            assert_eq_bool, assert_eq_num, assert_eq_num_list, assert_eq_num_object, assert_eq_str,
            assert_null,
        },
    };

    fn eval_str(s: &str) -> Result<Value, SeraphineError> {
        Evaluator::new().eval_str(s)
    }

    fn eval_str_with(s: &str, eval: &mut Evaluator) -> Result<Value, SeraphineError> {
        eval.eval_str(s)
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
        let mut eval = Evaluator::new();
        assert!(eval_str_with("a = 2", &mut eval).is_ok());
        assert!(eval_str_with("b = a + 1", &mut eval).is_ok());
        assert!(eval_str_with("c = a + b", &mut eval).is_ok());
        assert_eq_num!(eval.get_var("a").unwrap(), 2.0);
        assert_eq_num!(eval.get_var("b").unwrap(), 3.0);
        assert_eq_num!(eval.get_var("c").unwrap(), 5.0);

        assert!(eval_str("not_defined").is_err());

        let mut eval = Evaluator::new();
        assert!(eval_str_with("some_longer_name = 2", &mut eval).is_ok());
        assert_eq_num!(eval.get_var("some_longer_name").unwrap(), 2.0);

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
        let mut eval = Evaluator::new();
        let func = Function::new_builtin("add", None, Some(2), |_ctx, _this, args| {
            let Value::Number(arg1) = args[0] else {
                unreachable!()
            };
            let Value::Number(arg2) = args[1] else {
                unreachable!()
            };
            Ok(Value::Number(arg1 + arg2))
        });
        eval.set_var("add", Value::Function(func));

        assert!(eval_str_with("add()", &mut eval).is_err());
        assert!(eval_str_with("add(1)", &mut eval).is_err());
        assert!(eval_str_with("add(1,)", &mut eval).is_err());
        assert!(eval_str_with("add(,1)", &mut eval).is_err());
        assert!(eval_str_with("add(1 1)", &mut eval).is_err());
        assert_eq_num!(eval_str_with("add(1, 2)", &mut eval).unwrap(), 3.0);
        assert!(eval_str_with("add(1, 2, 3)", &mut eval).is_err());
        assert_eq_num!(eval_str_with("add(1, add(2, 3))", &mut eval).unwrap(), 6.0);
    }

    #[test]
    fn test_multiple_lines() {
        let mut eval = Evaluator::new();

        eval_str_with(
            r"a = 2
            b = 3
            c = a + b",
            &mut eval,
        )
        .unwrap();

        assert_eq_num!(eval.get_var("a").unwrap(), 2.0);
        assert_eq_num!(eval.get_var("b").unwrap(), 3.0);
        assert_eq_num!(eval.get_var("c").unwrap(), 5.0);

        assert_eq_num!(eval_str("\n42\n").unwrap(), 42.0);
        assert_eq_num!(eval_str("42\n").unwrap(), 42.0);
        assert_eq_num!(eval_str("\n42").unwrap(), 42.0);
        assert_null!(eval_str("\n\n\n").unwrap());
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
        assert!(eval_str("fn trailing_comma(a, b,) { a + b }").is_ok());
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
        let mut eval = Evaluator::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            false && set_a()
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert!(!eval._internal_side_effect_flag);

        let mut eval = Evaluator::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            true && set_a()
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert!(eval._internal_side_effect_flag);
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
        let mut eval = Evaluator::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            false || set_a()
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert!(eval._internal_side_effect_flag);

        let mut eval = Evaluator::new();
        let code = "\
            fn set_a() {
                _set_internal_side_effect_flag()
                true
            }
            true || set_a()
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert!(!eval._internal_side_effect_flag);
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
        assert_null!(eval_str(code).unwrap());

        let code = "\
            fn do_nothing() { return }
            do_nothing()";
        assert_null!(eval_str(code).unwrap());

        let code = "\
            fn add(a, b) {
                return a + b
                a - b
            }
            add(1, 2)";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let mut eval = Evaluator::new();
        let code = "\
            fn my_abs(num) {
                if (num < 0) {
                    return -num
                }
                num
            }";
        assert!(eval_str_with(code, &mut eval).is_ok());
        assert_eq_num!(eval_str_with("my_abs(-1)", &mut eval).unwrap(), 1.0);
        assert_eq_num!(eval_str_with("my_abs(42)", &mut eval).unwrap(), 42.0);
        assert_eq_num!(eval_str_with("my_abs(-0.23)", &mut eval).unwrap(), 0.23);

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

    #[test]
    fn test_string_literals() {
        assert_eq_str!(eval_str(r#""abc""#).unwrap(), "abc");
        assert_eq_str!(eval_str(r#""123""#).unwrap(), "123");
        assert_eq_str!(
            eval_str(r#""This is a sentence.""#).unwrap(),
            "This is a sentence."
        );
        assert_eq_str!(eval_str(r#""""#).unwrap(), "");
        assert_eq_str!(eval_str(r#""\"\n\t\r\0\\""#).unwrap(), "\"\n\t\r\0\\");

        assert!(eval_str(r#"""#).is_err());
        assert!(eval_str(r#""abc"#).is_err());
        assert!(eval_str(r#"abc""#).is_err());

        let code = r#"\
            s = "abc
            "
        "#;
        assert!(eval_str(code).is_err());

        let code = r#"\
            s = "abc

        "#;
        assert!(eval_str(code).is_err());
    }

    #[test]
    fn test_string_boolean_coercion() {
        assert_eq_bool!(eval_str(r#"!!"abc""#).unwrap(), true);
        assert_eq_bool!(eval_str(r#"!!"\0""#).unwrap(), true);
        assert_eq_bool!(eval_str(r#"!!"""#).unwrap(), false);
    }

    #[test]
    fn test_string_concatenation() {
        assert_eq_str!(eval_str(r#""abc" + "def""#).unwrap(), "abcdef");

        let mut eval = Evaluator::new();
        let code = r#"
            a = "abc"
            b = "def"
            c = a + b
        "#;
        eval_str_with(code, &mut eval).unwrap();
        assert_eq_str!(eval.get_var("a").unwrap(), "abc");
        assert_eq_str!(eval.get_var("b").unwrap(), "def");
        assert_eq_str!(eval.get_var("c").unwrap(), "abcdef");
        assert_eq_str!(eval_str_with(r#"a + "def""#, &mut eval).unwrap(), "abcdef");
        assert_eq_str!(eval_str_with(r#""abc" + b"#, &mut eval).unwrap(), "abcdef");
    }

    #[test]
    fn test_parse_number() {
        assert_eq_num!(eval_str(r#"parse_number("1e9")"#).unwrap(), 1e9);
        assert_eq_num!(eval_str(r#"parse_number("-42.1")"#).unwrap(), -42.1);
        assert_eq_bool!(eval_str(r#"is_nan(parse_number(""))"#).unwrap(), true);
        assert_eq_bool!(eval_str(r#"is_nan(parse_number("abc"))"#).unwrap(), true);

        assert!(eval_str("is_nan(parse_number())").is_err());
        assert!(eval_str("is_nan(parse_number(42))").is_err());
        assert!(eval_str("is_nan(parse_number(true))").is_err());
    }

    #[test]
    fn test_to_string() {
        assert_eq_str!(eval_str("to_string(123)").unwrap(), "123");
        assert_eq_str!(eval_str("to_string(true)").unwrap(), "true");
        assert_eq_str!(eval_str("to_string(123)").unwrap(), "123");
        assert_eq_str!(eval_str("to_string(nan)").unwrap(), "nan");
        assert_eq_str!(eval_str("to_string(inf)").unwrap(), "inf");
        assert_eq_str!(eval_str(r#"to_string("")"#).unwrap(), "");
        assert_eq_str!(eval_str(r#"to_string("abc")"#).unwrap(), "abc");
    }

    #[test]
    fn test_print() {
        let (mut stdout_reader, stdout_writer) = io::create_channel_reader_writer();
        let (mut stderr_reader, stderr_writer) = io::create_channel_reader_writer();
        let mut eval = Evaluator::builder()
            .stdout(stdout_writer)
            .stderr(stderr_writer)
            .build();

        eval_str_with(r#"println()"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "\n");
        assert_eq!(stderr_reader.read_available_to_string(), "");

        eval_str_with(r#"println("Hello, world!")"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "Hello, world!\n");
        assert_eq!(stderr_reader.read_available_to_string(), "");

        eval_str_with(r#"println("Hello", ",", "world", "!")"#, &mut eval).unwrap();
        assert_eq!(
            stdout_reader.read_available_to_string(),
            "Hello , world !\n"
        );
        assert_eq!(stderr_reader.read_available_to_string(), "");

        eval_str_with(r#"eprintln()"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "");
        assert_eq!(stderr_reader.read_available_to_string(), "\n");

        eval_str_with(r#"eprintln("Goodbye, world!")"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "");
        assert_eq!(
            stderr_reader.read_available_to_string(),
            "Goodbye, world!\n"
        );

        eval_str_with(r#"eprintln("Goodbye", ",", "world", "!")"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "");
        assert_eq!(
            stderr_reader.read_available_to_string(),
            "Goodbye , world !\n"
        );

        eval_str_with(r#"print()"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "");
        assert_eq!(stderr_reader.read_available_to_string(), "");

        eval_str_with(r#"print(42)"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "42");
        assert_eq!(stderr_reader.read_available_to_string(), "");

        eval_str_with(r#"print(1, 1, 2, 3, 5)"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "1 1 2 3 5");
        assert_eq!(stderr_reader.read_available_to_string(), "");

        eval_str_with(r#"eprint()"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "");
        assert_eq!(stderr_reader.read_available_to_string(), "");

        eval_str_with(r#"eprint(false)"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "");
        assert_eq!(stderr_reader.read_available_to_string(), "false");

        eval_str_with(r#"eprint(2, 3, 5, 7, 11, true)"#, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "");
        assert_eq!(stderr_reader.read_available_to_string(), "2 3 5 7 11 true");
    }

    #[test]
    fn test_read_line() {
        let (reader, mut writer) = io::create_channel_reader_writer();
        let mut eval = Evaluator::builder().stdin(reader).build();

        writer.write_all(b"This is some input\n").unwrap();
        assert_eq_str!(
            eval_str_with("read_line()", &mut eval).unwrap(),
            "This is some input"
        );

        writer.write_all(b"\n").unwrap();
        assert_eq_str!(eval_str_with("read_line()", &mut eval).unwrap(), "");

        writer.write_all(b"first line\nsecond line\n").unwrap();
        assert_eq_str!(
            eval_str_with("read_line()", &mut eval).unwrap(),
            "first line"
        );
        assert_eq_str!(
            eval_str_with("read_line()", &mut eval).unwrap(),
            "second line"
        );
    }

    #[test]
    fn test_named_functions_are_not_expressions() {
        assert!(eval_str("my_func = fn my_func() { }").is_err());
        assert!(eval_str("print(fn my_func() { })").is_err());
    }

    #[test]
    fn test_unnamed_functions() {
        let code = "\
            fn apply_twice(func, val) {
                func(func(val))
            }

            apply_twice(fn (num) { 2 * num }, 7)
        ";
        assert_eq_num!(eval_str(code).unwrap(), 28.0);

        let code = r#"
            str_concat = fn (s) {
                s + s
            }
            str_concat("abc")
        "#;
        assert_eq_str!(eval_str(code).unwrap(), "abcabc");

        assert_eq_bool!(
            eval_str("fn (a, b) { (a && !b) || (!a && b) }(true, true)").unwrap(),
            false
        );
        assert_eq_bool!(
            eval_str("(fn (a, b) { (a && !b) || (!a && b) })(true, true)").unwrap(),
            false
        );
    }

    #[test]
    fn test_function_values() {
        let code = "\
            fn add(a, b) {
                a + b
            }

            my_add = add
            my_add(2, 3)
        ";
        assert_eq_num!(eval_str(code).unwrap(), 5.0);

        let code = "\
            fn apply_both(f1, f2, v) {
                f2(f1(v))
            }

            fn double (num) {
                2 * num
            }

            apply_both(double, fn (num) { num ^ 2 }, 4)
        ";

        assert_eq_num!(eval_str(code).unwrap(), 64.0);

        let code = "\
            fn (a, b) {
                a + b
            }
        ";
        assert!(eval_str(code).is_ok());
    }

    #[test]
    fn test_function_equality() {
        assert_eq_bool!(eval_str("fn () { } == fn () { }").unwrap(), false);

        let mut eval = Evaluator::new();
        let code = "\
            fn add1(a, b) {
                a + b
            }

            fn add2(a, b) {
                a + b
            }

            add1_2 = add1
            add1_3 = add1_2
            add2_2 = add2
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert_eq_bool!(eval_str_with("add1 == add2", &mut eval).unwrap(), false);
        assert_eq_bool!(eval_str_with("add1 == add1", &mut eval).unwrap(), true);
        assert_eq_bool!(eval_str_with("add1 == add1_2", &mut eval).unwrap(), true);
        assert_eq_bool!(eval_str_with("add1 == add1_3", &mut eval).unwrap(), true);
        assert_eq_bool!(eval_str_with("add1_3 == add1_3", &mut eval).unwrap(), true);
        assert_eq_bool!(eval_str_with("add1_2 == add1_3", &mut eval).unwrap(), true);
        assert_eq_bool!(eval_str_with("add1_2 == add2_2", &mut eval).unwrap(), false);

        let code = "\
            o = { answer() { 42 } }
            o.answer == o.answer
        ";
        assert_eq_bool!(eval_str(code).unwrap(), true);

        let code = "\
            fn answer() { 42 }
            o = { answer }
            
            o.answer == answer
        ";
        assert_eq_bool!(eval_str(code).unwrap(), false);

        let code = "\
            fn answer() { 42 }
            o1 = { answer }
            o2 = { answer }
            
            o1.answer == o2.answer
        ";
        assert_eq_bool!(eval_str(code).unwrap(), false);
    }

    #[test]
    fn test_member_access() {
        let code = r#"
            "Hello, world!".length
        "#;
        assert_eq_num!(eval_str(code).unwrap(), 13.0);

        let code = r#"
            s = "Hello, world!"
            s.length
        "#;
        assert_eq_num!(eval_str(code).unwrap(), 13.0);

        let code = r#"
            "  abc ".trim()
        "#;
        assert_eq_str!(eval_str(code).unwrap(), "abc");

        let code = r#"
            s = "  abc "
            s.trim()
        "#;
        assert_eq_str!(eval_str(code).unwrap(), "abc");

        let code = r#"
            s = "  abc "
            t = s.trim
            t()
        "#;
        assert_eq_str!(eval_str(code).unwrap(), "abc");

        let code = r#"
            "  abc ".trim().trim()
        "#;
        assert_eq_str!(eval_str(code).unwrap(), "abc");

        let code = r#"
            "abc".this_does_not_exist()
        "#;
        assert!(eval_str(code).is_err());
    }

    #[test]
    fn test_list_literals() {
        let code = "[]";
        assert_eq_num_list!(eval_str(code).unwrap(), []);

        let code = "[3.7]";
        assert_eq_num_list!(eval_str(code).unwrap(), [3.7]);

        let code = "[1, 2, 3]";
        assert_eq_num_list!(eval_str(code).unwrap(), [1.0, 2.0, 3.0]);

        let code = "\
            num = 42
            [1, 2, 3, num]
        ";
        assert_eq_num_list!(eval_str(code).unwrap(), [1.0, 2.0, 3.0, 42.0]);

        let code = r#"
            ["abc", fn() { }, 42]
        "#;
        let Ok(Value::List(list)) = eval_str(code) else {
            panic!("Expected list");
        };
        let list = list.borrow();
        assert_eq!(list.len(), 3);
        assert_eq_str!(&list[0], "abc");
        assert!(matches!(list[1], Value::Function(_)));
        assert_eq_num!(&list[2], &42.0);
    }

    #[test]
    fn test_list_indexing() {
        let code = "[1, 2, 3][0]";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            list = [1, 2, 3]
            list[0]
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            list = [42, -1, 18.9]
            list[2]
        ";
        assert_eq_num!(eval_str(code).unwrap(), 18.9);

        let code = "\
            list = [42, -1, 18.9]
            list[3]
        ";
        assert!(eval_str(code).is_err());

        // TODO: Add this once negative indexing is either supported or properly rejected
        // let code = "\
        //     list = [42, -1, 18.9]
        //     list[-1]
        // ";
        // assert!(eval_str(code).is_err());

        let code = "\
            list = []
            list[0]
        ";
        assert!(eval_str(code).is_err());
    }

    #[test]
    fn test_indexing_assignment() {
        let code = "\
            list = [1, 2, 3]
            list[0] = 42
            list
        ";
        assert_eq_num_list!(eval_str(code).unwrap(), [42.0, 2.0, 3.0]);

        let code = "\
            list = [1, 2, 3]
            list[5] = 42
            list
        ";
        assert!(eval_str(code).is_err());

        let code = "\
            matrix = [[1, 2], [3, 4]]
            matrix[0][0] = 42
        ";
        let mut eval = Evaluator::new();
        assert!(eval_str_with(code, &mut eval).is_ok());
        assert_eq_num_list!(eval_str_with("matrix[0]", &mut eval).unwrap(), [42.0, 2.0]);
        assert_eq_num_list!(eval_str_with("matrix[1]", &mut eval).unwrap(), [3.0, 4.0]);

        let code = "\
            x = 1
            y = 1
            matrix = [[1, 2], [3, 4]]
            matrix[y][x] = 42
        ";
        let mut eval = Evaluator::new();
        assert!(eval_str_with(code, &mut eval).is_ok());
        assert_eq_num_list!(eval_str_with("matrix[0]", &mut eval).unwrap(), [1.0, 2.0]);
        assert_eq_num_list!(eval_str_with("matrix[1]", &mut eval).unwrap(), [3.0, 42.0]);

        let code = "\
            fn get_x() { 0 }
            fn get_y() { 1 }
            matrix = [[1, 2], [3, 4]]
            matrix[get_y()][get_x()] = 42
        ";
        let mut eval = Evaluator::new();
        assert!(eval_str_with(code, &mut eval).is_ok());
        assert_eq_num_list!(eval_str_with("matrix[0]", &mut eval).unwrap(), [1.0, 2.0]);
        assert_eq_num_list!(eval_str_with("matrix[1]", &mut eval).unwrap(), [42.0, 4.0]);

        let code = "\
            fn get_list() { [1, 2, 3] }
            get_list()[0] = 42
        ";
        assert!(eval_str(code).is_ok());
    }

    #[test]
    fn test_object_literals() {
        let code = "{}";
        assert_eq_num_object!(eval_str(code).unwrap(), {});

        let code = "{ a: 1 }";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 1.0 });

        let code = "{ a: 1, b: 2 }";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 1.0, "b" => 2.0 });

        let code = "{ a: 1, a: 2 }";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 2.0 });

        let code = "\
            a = 1
            b = 2
            { a, b }
        ";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 1.0, "b" => 2.0 });

        let code = "\
            a = 1
            { a, a: 2 }
        ";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 2.0 });

        let code = "{ 1: 1 }";
        assert!(eval_str(code).is_err());

        let code = "{ a: }";
        assert!(eval_str(code).is_err());

        let code = "{ a: }";
        assert!(eval_str(code).is_err());
    }

    #[test]
    fn test_multiline_obeject_literals() {
        let code = "\
            {
                a: 1,
                b: 2,
            }
        ";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 1.0, "b" => 2.0 });

        let code = "\
            {
                a
                    :
            1
            }
        ";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 1.0 });

        let code = "\
            o = {
                a() {
                    1
                }
            }
            o.a()
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            {
                a() { 1 }
                b() { 2 }
            }
        ";
        assert!(eval_str(code).is_err());

        let code = "\
            o = {
                a
                    (

                    )
                    {
                        1
                    }
            }
            o.a()
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);
    }

    #[test]
    fn test_object_get() {
        let code = "\
            obj = { a: 1, b: 2 }
            obj.a
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            { a: 1, b: 2 }.a
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            obj = { a: 1, b: 2 }
            obj.c
        ";
        assert!(eval_str(code).is_err());

        let code = r#"
            obj = { a: 1, b: 2 }
            obj["a"]
        "#;
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = r#"
            obj = { a: 1, b: 2 }
            obj["c"]
        "#;
        assert!(eval_str(code).is_err());

        let code = "\
            obj = { a: 1, b: 2 }
            obj[1]
        ";
        assert!(eval_str(code).is_err());

        let code = r#"
            key = "a"
            obj = { a: 1, b: 2 }
            obj[key]
        "#;
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = r#"
            fn get_key() { "a" }
            obj = { a: 1, b: 2 }
            obj[get_key()]
        "#;
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            obj = { inner: { answer: 42 } }
            obj.inner.answer
        ";
        assert_eq_num!(eval_str(code).unwrap(), 42.0);

        let code = r#"
            obj = { inner: { answer: 42 } }
            obj["inner"].answer
        "#;
        assert_eq_num!(eval_str(code).unwrap(), 42.0);
    }

    #[test]
    fn test_object_set() {
        let code = "\
            obj = {}
            obj.a = 42
            obj
        ";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 42.0 });

        let code = "\
            obj = { a: 1, b: 2 }
            obj.a = 42
            obj
        ";
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 42.0, "b" => 2.0 });

        let code = r#"
            obj = { a: 1, b: 2 }
            obj["a"] = 42
            obj
        "#;
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 42.0, "b" => 2.0 });

        let code = r#"
            key = "a"
            obj = { a: 1, b: 2 }
            obj[key] = 42
            obj
        "#;
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 42.0, "b" => 2.0 });

        let code = r#"
            key = "a"
            obj = { a: 1, b: 2 }
            obj[key] = 42
            obj
        "#;
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 42.0, "b" => 2.0 });

        let code = r#"
            fn get_key() { "a" }
            obj = { a: 1, b: 2 }
            obj[get_key()] = 42
            obj
        "#;
        assert_eq_num_object!(eval_str(code).unwrap(), { "a" => 42.0, "b" => 2.0 });

        let code = r#"
            obj = { inner: { answer: 21 } }
            obj.inner.answer = 42
            obj.inner
        "#;
        assert_eq_num_object!(eval_str(code).unwrap(), { "answer" => 42.0 });
    }

    #[test]
    fn test_object_methods() {
        let code = "
            obj = { add: fn (a, b) { a + b } }
            obj.add(1, 2)
        ";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let code = "
            obj = { add(a, b) { a + b } }
            obj.add(1, 2)
        ";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let code = "
            obj = { num: 0, increment() { this.num = this.num + 1 } }
            obj.increment()
            obj.num
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "
            fn increment() {
                this.num = this.num + 1
            }

            obj = { num: 0, increment }
            obj.increment()
            obj.num
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "
            o1 = { num: 1, get_num() { this.num } }
            o2 = { num: 2, get_num: o1.get_num }
            o2.get_num()
        ";
        assert_eq_num!(eval_str(code).unwrap(), 2.0);
    }

    #[test]
    fn test_self_referential_print() {
        let (mut stdout_reader, stdout_writer) = io::create_channel_reader_writer();
        let mut eval = Evaluator::builder().stdout(stdout_writer).build();

        let code = "\
            obj = {}
            obj.a = obj
            print(obj)
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "{ a: { ... } }");

        let code = "\
            lst = []
            lst.push(lst)
            print(lst)
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "[[...]]");

        let code = "\
            lst = []
            lst.push({lst})
            print(lst)
        ";
        eval_str_with(code, &mut eval).unwrap();
        assert_eq!(stdout_reader.read_available_to_string(), "[{ lst: [...] }]");
    }

    #[test]
    fn test_list_equality() {
        assert_eq_bool!(eval_str("[] == []").unwrap(), false);
        assert_eq_bool!(eval_str("[1, 2, 3] == [1, 2, 3]").unwrap(), false);

        let code = "\
            l = []
            l == l
        ";
        assert_eq_bool!(eval_str(code).unwrap(), true);

        let code = "\
            l1 = []
            l2 = []
            l1 == l2
        ";
        assert_eq_bool!(eval_str(code).unwrap(), false);

        let code = "\
            l = [1, 2, 3]
            l == l
        ";
        assert_eq_bool!(eval_str(code).unwrap(), true);
    }

    #[test]
    fn test_object_equality() {
        assert_eq_bool!(eval_str("{} == {}").unwrap(), false);
        assert_eq_bool!(eval_str("{a: 1, b: 2} == {a: 1, b: 2}").unwrap(), false);

        let code = "\
            o = {}
            o == o
        ";
        assert_eq_bool!(eval_str(code).unwrap(), true);

        let code = "\
            o1 = {}
            o2 = {}
            o1 == o2
        ";
        assert_eq_bool!(eval_str(code).unwrap(), false);

        let code = "\
            o = {a: 1, b: 2}
            o == o
        ";
        assert_eq_bool!(eval_str(code).unwrap(), true);
    }

    #[test]
    fn test_null() {
        assert_null!(eval_str("null").unwrap());
        assert_eq_bool!(eval_str("null == null").unwrap(), true);
    }

    #[test]
    fn test_multiline_function_definition() {
        let code = "\
            fn
                add
                (
                a
                ,
                b,
            )
                {
                a + b
            }
            add(1, 2)
        ";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);
    }

    #[test]
    fn test_multiline_function_call() {
        let code = "\
            fn add(a, b) {
                a + b
            }
            add(
                1
                ,
                2
            )
        ";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let code = "\
            fn add(a, b) {
                a + b
            }
            add
            (
                1
                ,
                2
            )
        ";
        assert!(eval_str(code).is_err());
    }

    #[test]
    fn test_multiline_list_literal() {
        let code = "\
            [
                1
                ,
                2
                ,
            ]
        ";
        assert_eq_num_list!(eval_str(code).unwrap(), [1.0, 2.0]);
    }

    #[test]
    fn test_newline_in_assignment() {
        let code = "\
            a
            =
            1
            a
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            o = {}
            o.a
            =
            1
            o.a
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            l = [0]
            l[0]
            =
            1
            l[0]
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);
    }

    #[test]
    fn test_newline_in_member_access() {
        let code = r#"
            "  abc "
                .trim()
                .slice(1, 2)
        "#;
        assert_eq_str!(eval_str(code).unwrap(), "b");

        let code = "\
            o = {
                a: 1
            }
            o
            .
            a
            =
            2
            o
            .
            a
        ";
        assert_eq_num!(eval_str(code).unwrap(), 2.0);
    }

    #[test]
    fn test_newline_in_indexing() {
        let code = "\
            l = [1, 2]
            l[
                0
            ]
            =
            3
            l[
                0
            ]
        ";
        assert_eq_num!(eval_str(code).unwrap(), 3.0);

        let code = "\
            l = [1, 2]
            l
            [
                0
            ]
            =
            3
            l
            [
                0
            ]
        ";
        assert!(eval_str(code).is_err());
    }

    #[test]
    fn test_newlines_in_expressions() {
        let code = "\
            a = 1

            a
            +
            (
            -
            2
            +
            3
            *
            4
            ^
            5
            +
            6
            )
        ";
        assert_eq_num!(eval_str(code).unwrap(), 3077.0);
    }

    #[test]
    fn test_newlines_in_while_loop() {
        let code = "\
            i = 0
            while
                (
                    i < 10
                )
            {
                i = i + 1

                if (i == 5) {
                    break
                }
            }
            i
        ";
        assert_eq_num!(eval_str(code).unwrap(), 5.0);
    }

    #[test]
    fn test_newlines_in_if_statement() {
        let code = "\
            a = 0
            if
                (
                    true
                )
            {
                a = 1
            }
            else if
                (
                    false
                )
            {
                a = 2
            }
            else
            {
                a = 3
            }
            a
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        let code = "\
            if (true) { }
            else
            if (false) { }
        ";
        assert!(eval_str(code).is_err());
    }

    #[test]
    fn test_for_loops() {
        let code = "\
            sum = 0
            for (i in [1, 2, 3]) {
                sum = sum + i
            }
            sum
        ";
        assert_eq_num!(eval_str(code).unwrap(), 6.0);

        let code = "\
            fn get_numbers() {
                [1, 2, 3]
            }
            sum = 0
            for (i in get_numbers()) {
                sum = sum + i
            }
            sum
        ";
        assert_eq_num!(eval_str(code).unwrap(), 6.0);

        let code = "\
            sum = 0
            for (i in [1, 2, 3]) {
                if (i == 2) {
                    continue
                }
                sum = sum + i
            }
            sum
        ";
        assert_eq_num!(eval_str(code).unwrap(), 4.0);

        let code = "\
            sum = 0
            for (i in [1, 2, 3]) {
                if (i == 2) {
                    break
                }
                sum = sum + i
            }
            sum
        ";
        assert_eq_num!(eval_str(code).unwrap(), 1.0);

        assert!(eval_str("for (i in 1) { }").is_err());

        assert!(eval_str("for (i [1, 2, 3]) { }").is_err());
        assert!(eval_str("for (in [1, 2, 3]) { }").is_err());
        assert!(eval_str("for (i in) { }").is_err());
        assert!(eval_str("for (in) { }").is_err());
    }

    #[test]
    fn test_newlines_in_for_loop() {
        let code = "\
            sum = 0
            for
                (
                    i
                    in
                    [
                        1
                        ,
                        2
                        ,
                        3
                    ]
                )
            {
                sum = sum + i
            }
            sum
        ";
        assert_eq_num!(eval_str(code).unwrap(), 6.0);
    }

    #[test]
    fn test_builtin_range() {
        fn eval_range_to_list(range: &str) -> Result<Value, SeraphineError> {
            let code = format!(
                "\
                list = []
                for (i in {range}) {{
                    list.push(i)
                }}
                list
            "
            );
            eval_str(&code)
        }

        assert_eq_num_list!(eval_range_to_list("range(3)").unwrap(), [0.0, 1.0, 2.0]);
        assert_eq_num_list!(eval_range_to_list("range(1, 3)").unwrap(), [1.0, 2.0]);
        assert_eq_num_list!(eval_range_to_list("range(3, 3)").unwrap(), []);
        assert_eq_num_list!(eval_range_to_list("range(10, 3)").unwrap(), []);
        assert_eq_num_list!(
            eval_range_to_list("range(1, 10, 3)").unwrap(),
            [1.0, 4.0, 7.0]
        );

        assert_eq_num_list!(eval_range_to_list("range(2, 3, -1)").unwrap(), []);
        assert_eq_num_list!(eval_range_to_list("range(3, 3, -1)").unwrap(), []);
        assert_eq_num_list!(
            eval_range_to_list("range(3, 0, -1)").unwrap(),
            [3.0, 2.0, 1.0]
        );
        assert_eq_num_list!(
            eval_range_to_list("range(10, 3, -2)").unwrap(),
            [10.0, 8.0, 6.0, 4.0]
        );
        assert_eq_num_list!(
            eval_range_to_list("range(10, 4, -2)").unwrap(),
            [10.0, 8.0, 6.0]
        );

        assert!(eval_range_to_list("range()").is_err());
        assert!(eval_range_to_list("range(1, 2, 0)").is_err());
        assert!(eval_range_to_list("range(1, 2, 3, 4)").is_err());
    }
}
