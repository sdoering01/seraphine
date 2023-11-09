use std::{cell::RefCell, io::BufRead, rc::Rc};

use crate::{
    error::EvalError,
    eval::{print_values, SeraphineIterator, Type, Value, NULL_VALUE},
    runtime::common::RuntimeContext,
};

pub(super) fn _set_internal_side_effect_flag(
    ctx: &mut RuntimeContext,
    _this: Option<Value>,
    _args: Vec<Value>,
) -> Result<Value, EvalError> {
    ctx._internal_side_effect_flag = true;
    Ok(NULL_VALUE)
}

pub(super) fn print(
    ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    print_values(&mut ctx.stdout, &args)?;
    ctx.stdout.flush()?;
    Ok(NULL_VALUE)
}

pub(super) fn println(
    ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    print_values(&mut ctx.stdout, &args)?;
    writeln!(ctx.stdout)?;
    Ok(NULL_VALUE)
}

pub(super) fn eprint(
    ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    print_values(&mut ctx.stderr, &args)?;
    ctx.stderr.flush()?;
    Ok(NULL_VALUE)
}

pub(super) fn eprintln(
    ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    print_values(&mut ctx.stderr, &args)?;
    writeln!(ctx.stderr)?;
    Ok(NULL_VALUE)
}

pub(super) fn read_line(
    ctx: &mut RuntimeContext,
    _this: Option<Value>,
    _args: Vec<Value>,
) -> Result<Value, EvalError> {
    let mut str = String::new();
    ctx.stdin.read_line(&mut str)?;
    str.pop();
    Ok(Value::String(str))
}

pub(super) fn to_string(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    Ok(Value::String(args[0].convert_to_string()))
}

pub(super) fn parse_number(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::String)?;
    let Value::String(ref arg) = args[0] else {
        unreachable!()
    };
    let num = arg.parse().unwrap_or(f64::NAN);
    Ok(Value::Number(num))
}

pub(super) fn range(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    fn make_range(
        start: i64,
        end: i64,
        step: i64,
    ) -> Result<Rc<RefCell<dyn SeraphineIterator>>, EvalError> {
        if step == 0 {
            return Err(EvalError::GenericError(
                "range: step cannot be 0".to_string(),
            ));
        }

        if step > 0 {
            Ok(Rc::new(RefCell::new(
                (start..end)
                    .step_by(step as usize)
                    .map(|n| Value::Number(n as f64)),
            )))
        } else {
            // TODO: This could panic for i64::MIN
            let step = -step;

            // `end` is not inclusive but `start` is; `+ 1` since end is the smaller number
            // in this case
            Ok(Rc::new(RefCell::new(
                ((end + 1)..=start)
                    .rev()
                    .step_by(step as usize)
                    .map(|n| Value::Number(n as f64)),
            )))
        }
    }

    // TODO: Limit arguments to range 1..=3 in a better way
    if args.is_empty() {
        return Err(EvalError::FunctionWrongArgAmount {
            name: Some("range".to_string()),
            expected: 1,
            got: args.len(),
        });
    } else if args.len() > 3 {
        return Err(EvalError::FunctionWrongArgAmount {
            name: Some("range".to_string()),
            expected: 3,
            got: args.len(),
        });
    }

    let range = if args.len() == 1 {
        args[0].assert_type(Type::Number)?;
        let Value::Number(end) = args[0] else {
            unreachable!()
        };

        // TODO: Fix this
        let end = end as i64;

        make_range(0, end, 1)?
    } else {
        args[0].assert_type(Type::Number)?;
        args[1].assert_type(Type::Number)?;
        if args.len() == 3 {
            args[2].assert_type(Type::Number)?;
        }

        let Value::Number(start) = args[0] else {
            unreachable!()
        };
        let Value::Number(end) = args[1] else {
            unreachable!()
        };

        let step = if args.len() == 3 {
            let Value::Number(step) = args[2] else {
                unreachable!()
            };
            step
        } else {
            1.0
        };

        // TODO: Fix this
        let start = start as i64;
        let end = end as i64;
        let step = step as i64;

        make_range(start, end, step)?
    };

    Ok(Value::Iterator(range))
}

pub(super) fn is_nan(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Bool(arg.is_nan()))
}

pub(super) fn is_infinite(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Bool(arg.is_infinite()))
}

pub(super) fn is_finite(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Bool(arg.is_finite()))
}

pub(super) fn sin(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.sin()))
}

pub(super) fn cos(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.cos()))
}

pub(super) fn tan(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.tan()))
}

pub(super) fn asin(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.asin()))
}

pub(super) fn acos(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.acos()))
}

pub(super) fn atan(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.atan()))
}

pub(super) fn atan2(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    args[1].assert_type(Type::Number)?;
    let Value::Number(arg1) = args[0] else {
        unreachable!()
    };
    let Value::Number(arg2) = args[1] else {
        unreachable!()
    };
    Ok(Value::Number(arg1.atan2(arg2)))
}

pub(super) fn tanh(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.tanh()))
}

pub(super) fn sinh(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.sinh()))
}

pub(super) fn cosh(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.cosh()))
}

pub(super) fn ln(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.ln()))
}

pub(super) fn log2(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.log2()))
}

pub(super) fn log10(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.log10()))
}

pub(super) fn log(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    args[1].assert_type(Type::Number)?;
    let Value::Number(arg1) = args[0] else {
        unreachable!()
    };
    let Value::Number(arg2) = args[1] else {
        unreachable!()
    };
    Ok(Value::Number(arg1.log(arg2)))
}

pub(super) fn abs(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.abs()))
}

pub(super) fn min(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    args[1].assert_type(Type::Number)?;
    let Value::Number(arg1) = args[0] else {
        unreachable!()
    };
    let Value::Number(arg2) = args[1] else {
        unreachable!()
    };
    Ok(Value::Number(arg1.min(arg2)))
}

pub(super) fn max(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    args[1].assert_type(Type::Number)?;
    let Value::Number(arg1) = args[0] else {
        unreachable!()
    };
    let Value::Number(arg2) = args[1] else {
        unreachable!()
    };
    Ok(Value::Number(arg1.max(arg2)))
}

pub(super) fn floor(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.floor()))
}

pub(super) fn ceil(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.ceil()))
}

pub(super) fn round(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.round()))
}

pub(super) fn sqrt(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.sqrt()))
}

pub(super) fn exp(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    args[0].assert_type(Type::Number)?;
    let Value::Number(arg) = args[0] else {
        unreachable!()
    };
    Ok(Value::Number(arg.exp()))
}

pub(super) fn inspect(
    _ctx: &mut RuntimeContext,
    _this: Option<Value>,
    args: Vec<Value>,
) -> Result<Value, EvalError> {
    println!("{}", args[0]);
    Ok(args[0].clone())
}
