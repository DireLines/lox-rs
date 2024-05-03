use super::*;
use crate::parser::parse_str_with;
use crate::parser::Expression;

#[test]
fn test_eval_expr() {
    let sample = "3 == \"3\"";
    let x = parse_str_with(sample, Expression::new);
    let result = eval(x, ());
    assert!(result == Value::Bool(false));
}
