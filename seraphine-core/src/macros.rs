#![allow(unused_macros)]
#![allow(unused_imports)]

macro_rules! assert_null {
    ( $value:expr ) => {
        match $value {
            $crate::value::Value::Null => {}
            value => {
                ::std::panic!("value is not null: {:?}", value);
            }
        }
    };
}
pub(crate) use assert_null;

macro_rules! assert_eq_num {
    ( $left:expr, $right:expr ) => {
        match ($left, $right) {
            (value, expected) => {
                let $crate::value::Value::Number(got) = value else {
                    ::std::panic!("value is not a number");
                };
                ::std::assert_eq!(got, expected);
            }
        }
    };
    ( $left:expr, $right:expr, $eps:expr ) => {
        match ($left, $right) {
            (value, expected) => {
                let $crate::value::Value::Number(got) = value else {
                    ::std::panic!("value is not a number");
                };
                ::std::assert!((got - expected).abs() < $eps);
            }
        }
    };
}
pub(crate) use assert_eq_num;

macro_rules! assert_eq_bool {
    ( $left:expr, $right:expr ) => {
        match ($left, $right) {
            (value, expected) => {
                let $crate::value::Value::Bool(got) = value else {
                    ::std::panic!("value is not a bool");
                };
                ::std::assert_eq!(got, expected);
            }
        }
    };
}
pub(crate) use assert_eq_bool;

macro_rules! assert_eq_str {
    ( $left:expr, $right:expr ) => {
        match ($left, $right) {
            (value, expected) => {
                let $crate::value::Value::String(got) = value else {
                    ::std::panic!("value is not a string");
                };
                ::std::assert_eq!(got, expected);
            }
        }
    };
}
pub(crate) use assert_eq_str;

macro_rules! assert_eq_num_list {
    ( $left:expr, $right:expr ) => {
        match ($left, $right) {
            (value, expected) => {
                let $crate::value::Value::List(got) = value else {
                    ::std::panic!("value is not a list");
                };
                let got = got.borrow();
                ::std::assert_eq!(got.len(), expected.len(), "length mismatch");
                for (i, (got, expected)) in got.iter().zip(expected.iter()).enumerate() {
                    let $crate::value::Value::Number(got) = got else {
                        ::std::panic!("value is not a number");
                    };
                    ::std::assert_eq!(*got, *expected, "at index {}", i);
                }
            }
        }
    };
}
pub(crate) use assert_eq_num_list;

/// Asserts that the given object has the given keys and values.
///
/// Note that the object syntax is different from the one used in the language, since
/// `macro_rules` cannot parse it.
///
/// Example:
/// ```ignore
/// assert_eq_num_object!(eval_str("{ a: 1, b: 2 }"), { "a" => 1.0, "b" => 2.0 });
/// ```
macro_rules! assert_eq_num_object {
    ( $obj:expr, { $( $key:expr => $val:expr ),* } ) => {
        {
            let $crate::value::Value::Object(obj) = $obj else {
                ::std::panic!("value is not an object");
            };
            #[allow(unused_variables)]
            let obj = obj.borrow();
            #[allow(unused_mut)]
            let mut keys = 0;
            $(
                match obj.get($key) {
                    Some($crate::value::Value::Number(got)) => {
                        ::std::assert_eq!(*got, $val, "at key {}", $key);
                    }
                    Some(_) => {
                        ::std::panic!("value is not a number");
                    }
                    None => {
                        ::std::panic!(r#"key "{}" not found in object"#, $key);
                    }
                }
                keys += 1;
            )*
            ::std::assert_eq!(keys, obj.len(), "length mismatch");
        }
    };
}
pub(crate) use assert_eq_num_object;
