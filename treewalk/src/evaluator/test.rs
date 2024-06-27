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

#[test]
fn test_interpret_statement_if() {
    let program = "var a = 1; if (3 == 4) { a = 2; } else { a = 3; }";
    let ast = parse_str_with(program, Program::new);
    let mut env = EnvStack::default();
    let _ = interpret(ast.body, &mut env);
    assert_eq!(*env.get("a".to_string()).unwrap(), Value::Number(3.0));
}

#[test]
fn test_interpret_statement_while() {
    let program = "var a = 1; while (a < 6) { a = a + 1; }";
    let ast = parse_str_with(program, Program::new);
    let mut env = EnvStack::default();
    let _ = interpret(ast.body, &mut env);
    assert_eq!(*env.get("a".to_string()).unwrap(), Value::Number(6.0));
}

#[test]
fn test_eval_expr_short_circuit_or() {
    //c is not defined, would crash if evaluated
    let program = "var a = 1; var b = a or c;";
    let ast = parse_str_with(program, Program::new);
    let mut env = EnvStack::default();
    let _ = interpret(ast.body, &mut env);
    assert_eq!(*env.get("b".to_string()).unwrap(), Value::Number(1.0));
}

#[test]
fn test_eval_expr_short_circuit_and() {
    //c is not defined, would crash if evaluated
    let program = "var a = false; var b = a and c;";
    let ast = parse_str_with(program, Program::new);
    let mut env = EnvStack::default();
    let _ = interpret(ast.body, &mut env);
    assert_eq!(*env.get("b".to_string()).unwrap(), Value::Bool(false));
}
