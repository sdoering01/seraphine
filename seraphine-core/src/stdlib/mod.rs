mod functions;

use crate::{
    error::StdlibError,
    eval::{Function, Value},
    runtime::common::RuntimeContext,
};

pub(crate) fn get_standard_variables() -> Vec<(&'static str, Value)> {
    use std::f64::{
        consts::{E, PI},
        INFINITY, NAN,
    };

    vec![
        ("pi", Value::Number(PI)),
        ("e", Value::Number(E)),
        ("nan", Value::Number(NAN)),
        ("inf", Value::Number(INFINITY)),
    ]
}

pub(crate) fn get_standard_functions() -> Vec<(&'static str, Value)> {
    fn make_value_tuple<F>(
        name: &'static str,
        n_args: Option<usize>,
        func: F,
    ) -> (&'static str, Value)
    where
        F: Fn(&mut RuntimeContext, Option<Value>, Vec<Value>) -> Result<Value, StdlibError>
            + 'static,
    {
        (
            name,
            Value::Function(Function::new_builtin(name, None, n_args, func)),
        )
    }

    vec![
        make_value_tuple(
            "_set_internal_side_effect_flag",
            Some(0),
            functions::_set_internal_side_effect_flag,
        ),
        make_value_tuple("print", None, functions::print),
        make_value_tuple("println", None, functions::println),
        make_value_tuple("eprint", None, functions::eprint),
        make_value_tuple("eprintln", None, functions::eprintln),
        make_value_tuple("read_line", Some(0), functions::read_line),
        make_value_tuple("to_string", Some(1), functions::to_string),
        make_value_tuple("parse_number", Some(1), functions::parse_number),
        make_value_tuple("range", None, functions::range),
        make_value_tuple("is_nan", Some(1), functions::is_nan),
        make_value_tuple("is_infinite", Some(1), functions::is_infinite),
        make_value_tuple("is_finite", Some(1), functions::is_finite),
        make_value_tuple("sin", Some(1), functions::sin),
        make_value_tuple("cos", Some(1), functions::cos),
        make_value_tuple("tan", Some(1), functions::tan),
        make_value_tuple("asin", Some(1), functions::asin),
        make_value_tuple("acos", Some(1), functions::acos),
        make_value_tuple("atan", Some(1), functions::atan),
        make_value_tuple("atan2", Some(1), functions::atan2),
        make_value_tuple("tanh", Some(1), functions::tanh),
        make_value_tuple("sinh", Some(1), functions::sinh),
        make_value_tuple("cosh", Some(1), functions::cosh),
        make_value_tuple("ln", Some(1), functions::ln),
        make_value_tuple("log2", Some(1), functions::log2),
        make_value_tuple("log10", Some(1), functions::log10),
        make_value_tuple("log", Some(2), functions::log),
        make_value_tuple("abs", Some(1), functions::abs),
        make_value_tuple("min", Some(2), functions::min),
        make_value_tuple("max", Some(2), functions::max),
        make_value_tuple("floor", Some(1), functions::floor),
        make_value_tuple("ceil", Some(1), functions::ceil),
        make_value_tuple("round", Some(1), functions::round),
        make_value_tuple("sqrt", Some(1), functions::sqrt),
        make_value_tuple("exp", Some(1), functions::exp),
        make_value_tuple("inspect", Some(1), functions::inspect),
    ]
}
