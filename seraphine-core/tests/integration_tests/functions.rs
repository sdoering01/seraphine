use seraphine_core::{
    assert_eq_bool, assert_eq_num, assert_eq_str,
    eval::Evaluator,
    value::{Function, Value},
};

use crate::common::{eval_str, eval_str_with};

#[test]
fn test_rust_functions() {
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
fn test_named_functions_are_not_expressions() {
    assert!(eval_str("my_func = fn my_func() { }").is_err());
    assert!(eval_str("print(fn my_func() { })").is_err());
}

#[test]
fn test_closures() {
    let code = "\
    fn getter_factory() {
        a = 42
        fn () { a }
    }

    getter = getter_factory()
    getter()
    ";
    assert_eq_num!(eval_str(code).unwrap(), 42.0);
}
