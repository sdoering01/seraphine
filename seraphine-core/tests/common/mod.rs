use seraphine_core::{error::SeraphineError, eval::Evaluator, value::Value};

pub fn eval_str(s: &str) -> Result<Value, SeraphineError> {
    Evaluator::new().eval_str(s)
}

pub fn eval_str_with(s: &str, eval: &mut Evaluator) -> Result<Value, SeraphineError> {
    eval.eval_str(s)
}
