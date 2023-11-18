use seraphine_core::{assert_eq_num, eval::Evaluator};

use crate::common::{eval_str, eval_str_with};

mod arithmetic;
mod control_flow;
mod functions;
mod list;
mod multiline;
mod object;
mod stdlib;
mod string;

#[test]
fn test_comments() {
    let code = "\
        // This function adds two numbers
        fn add(a, b) {
            a + b  // <- this is where the magic happens
        }

        sum = add(1, 2) // <- assignment that requires a newline after it
        sum// no space before comment
    ";

    assert_eq_num!(eval_str(code).unwrap(), 3.0);
}

#[test]
fn test_variables() {
    let mut eval = &mut Evaluator::new();
    assert!(eval_str_with("a = 2", &mut eval).is_ok());
    assert!(eval_str_with("b = a + 1", &mut eval).is_ok());
    assert!(eval_str_with("c = a + b", &mut eval).is_ok());
    assert_eq_num!(eval_str_with("a", &mut eval).unwrap(), 2.0);
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
