use seraphine_core::{assert_eq_bool, assert_eq_num, assert_null, eval::Evaluator, io};

use crate::common::{eval_str, eval_str_with};

#[test]
fn test_basic_arithmetic() {
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
    let (mut stdout_reader, stdout_writer) = io::create_channel_reader_writer();
    let mut eval = Evaluator::builder().stdout(stdout_writer).build();
    let code = "\
        fn set_a() {
            print(\"side effect\")
            true
        }
        false && set_a()
    ";
    eval_str_with(code, &mut eval).unwrap();
    assert_eq!(stdout_reader.read_available_to_string(), "");

    let code = "\
        fn set_a() {
            print(\"side effect\")
            true
        }
        true && set_a()
    ";
    eval_str_with(code, &mut eval).unwrap();
    assert_eq!(stdout_reader.read_available_to_string(), "side effect");
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
    let (mut stdout_reader, stdout_writer) = io::create_channel_reader_writer();
    let mut eval = Evaluator::builder().stdout(stdout_writer).build();
    let code = "\
        fn set_a() {
            print(\"side effect\")
            true
        }
        false || set_a()
    ";
    eval_str_with(code, &mut eval).unwrap();
    assert_eq!(stdout_reader.read_available_to_string(), "side effect");

    let code = "\
        fn set_a() {
            print(\"side effect\")
            true
        }
        true || set_a()
    ";
    eval_str_with(code, &mut eval).unwrap();
    assert_eq!(stdout_reader.read_available_to_string(), "");
}

#[test]
fn test_boolean_literals() {
    assert_eq_bool!(eval_str("true").unwrap(), true);
    assert_eq_bool!(eval_str("false").unwrap(), false);
}

#[test]
fn test_null() {
    assert_null!(eval_str("null").unwrap());
    assert_eq_bool!(eval_str("null == null").unwrap(), true);
}
