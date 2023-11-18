use seraphine_core::{assert_eq_num, assert_null, eval::Evaluator};

use crate::common::{eval_str, eval_str_with};

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
