use seraphine_core::{assert_eq_bool, assert_eq_num, assert_eq_num_object};

use crate::common::eval_str;

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
