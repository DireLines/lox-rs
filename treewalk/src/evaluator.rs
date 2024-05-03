use crate::parser::{BinaryOperator, Expression};
use std::error::Error;
type Result<T> = std::result::Result<T, Box<dyn Error>>;
//lox runtime value
#[derive(Debug, PartialEq, Clone)]
enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

fn truth_value(v: &Value) -> bool {
    match v {
        Value::Nil => false,
        Value::Bool(b) => *b,
        _ => true,
    }
}
fn is_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}
fn pair_of_numbers(a: &Value, b: &Value) -> Option<(f64, f64)> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Some((*a, *b)),
        _ => None,
    }
}
fn pair_of_strings(a: &Value, b: &Value) -> Option<(String, String)> {
    match (a, b) {
        (Value::String(a), Value::String(b)) => Some((a.to_owned(), b.to_owned())),
        _ => None,
    }
}
fn eval(expr: Expression, state: ()) -> Value {
    match expr {
        Expression::Number(n) => Value::Number(n),
        Expression::String(s) => Value::String(s),
        Expression::Identifier(name) => panic!("need to make environment"),
        Expression::Bool(b) => Value::Bool(b),
        Expression::Nil => Value::Nil,
        Expression::This => panic!("need to know how classes work"),
        Expression::Grouping { expression } => eval(*expression, state),
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval(*left, state);
            let right = eval(*right, state);
            match operator {
                BinaryOperator::Minus => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left - right);
                    }
                    panic!("Operands must be two numbers.")
                } //nums
                BinaryOperator::Plus => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left + right);
                    } else if let Some((left, right)) = pair_of_strings(&left, &right) {
                        return Value::String(left + &right);
                    }
                    panic!("Operands must be two numbers or two strings.")
                }
                BinaryOperator::Div => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left / right);
                    }
                    panic!("Operands must be two numbers.")
                } //nums
                BinaryOperator::Mul => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left * right);
                    }
                    panic!("Operands must be two numbers.")
                } //nums
                BinaryOperator::Equal => todo!(), //anything
                BinaryOperator::EqualEqual => Value::Bool(is_equal(&left, &right)),
                BinaryOperator::NotEqual => Value::Bool(!is_equal(&left, &right)),
                BinaryOperator::Greater => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left > right);
                    }
                    panic!("Operands must be two numbers.")
                } //nums
                BinaryOperator::GreaterEqual => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left >= right);
                    }
                    panic!("Operands must be two numbers.")
                } //nums
                BinaryOperator::Less => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left < right);
                    }
                    panic!("Operands must be two numbers.")
                } //nums
                BinaryOperator::LessEqual => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left <= right);
                    }
                    panic!("Operands must be two numbers.")
                } //nums
                BinaryOperator::Or => Value::Bool(truth_value(&left) || truth_value(&right)),
                BinaryOperator::And => Value::Bool(truth_value(&left) && truth_value(&right)),
            }
        }
        _ => Value::Nil,
    }
}

#[cfg(test)]
mod test;
