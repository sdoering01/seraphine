use std::io::Write;

use seraphine_core::{
    assert_eq_bool, assert_eq_num, assert_eq_num_list, assert_eq_str, error::SeraphineError,
    eval::Evaluator, io, value::Value,
};

use crate::common::{eval_str, eval_str_with};

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
