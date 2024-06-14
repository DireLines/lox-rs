use super::*;
use crate::parser::parse_str_with;
use crate::parser::Expression;

#[test]
fn test_eval_expr_mismatch_types() {
    let sample = "3 == \"3\"";
    let x = parse_str_with(sample, Expression::new);
    let mut env = EnvStack::default();
    let result = eval_expr(x, &mut env);
    assert!(result == Value::Bool(false));
}

#[test]
fn test_eval_expr_trivial() {
    let sample = "3 == 3";
    let x = parse_str_with(sample, Expression::new);
    let mut env = EnvStack::default();
    let result = eval_expr(x, &mut env);
    assert!(result == Value::Bool(true));
}

#[test]
fn test_eval_expr_nums() {
    let sample = "3 == (5+5)*(3-4)";
    let x = parse_str_with(sample, Expression::new);
    let mut env = EnvStack::default();
    let result = eval_expr(x, &mut env);
    assert!(result == Value::Bool(false));
}

#[test]
fn test_eval_expr_declare() {
    let declare = "var b = 5;";
    let use_var = "b == 5";
    let declare_ast = parse_str_with(declare, Declaration::new);
    let use_var_ast = parse_str_with(use_var, Expression::new);
    let mut env = EnvStack::default();
    let _ = interpret(vec![declare_ast], &mut env);
    let result = eval_expr(use_var_ast, &mut env);
    assert!(result == Value::Bool(true));
}

#[test]
fn test_eval_expr_declare_assign() {
    let declare = "var b = 5;";
    let assign = "b = 6;";
    let use_var = "b == 6";
    let declare_ast = parse_str_with(declare, Declaration::new);
    let assign_ast = parse_str_with(assign, Declaration::new);
    let use_var_ast = parse_str_with(use_var, Expression::new);
    let mut env = EnvStack::default();
    let _ = interpret(vec![declare_ast, assign_ast], &mut env);
    let result = eval_expr(use_var_ast, &mut env);
    assert!(result == Value::Bool(true));
}
