use seraphine_core::{
    assert_eq_bool, assert_eq_num, assert_eq_num_list, assert_eq_str, eval::Evaluator, value::Value,
};

use crate::common::{eval_str, eval_str_with};

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
