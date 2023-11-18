use seraphine_core::{assert_eq_bool, assert_eq_num, assert_eq_str, eval::Evaluator};

use crate::common::{eval_str, eval_str_with};

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
