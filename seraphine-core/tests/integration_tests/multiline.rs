use seraphine_core::{assert_eq_num, assert_eq_num_list, assert_eq_num_object, assert_eq_str};

use crate::common::eval_str;

#[test]
fn test_errors_on_missing_newline() {
    assert!(eval_str("1 + 1 2 + 2").is_err());
    assert!(eval_str("1 2").is_err());
    assert!(eval_str("(1 * 3) 2").is_err());

    assert!(eval_str("fn add(a, b) { a + b } fn sub(a, b) { a - b }").is_err());
    assert!(eval_str("if (1){ 1 } if (2){ 2 }").is_err());
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
fn test_multiline_assignment() {
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
fn test_multiline_member_access() {
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
fn test_multiline_indexing() {
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
fn test_multiline_expressions() {
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
fn test_multiline_while_loop() {
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
fn test_multiline_if_statement() {
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
fn test_multiline_for_loop() {
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
