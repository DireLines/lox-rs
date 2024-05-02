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

fn binary_op_evaluator(op: BinaryOperator) -> fn(Expression, Expression) -> Result<Value> {
    match op {
        BinaryOperator::Plus => todo!(),
        BinaryOperator::Minus => todo!(),
        BinaryOperator::Div => todo!(),
        BinaryOperator::Mul => todo!(),
        BinaryOperator::Equal => todo!(),
        BinaryOperator::EqualEqual => todo!(),
        BinaryOperator::NotEqual => todo!(),
        BinaryOperator::Greater => todo!(),
        BinaryOperator::GreaterEqual => todo!(),
        BinaryOperator::Less => todo!(),
        BinaryOperator::LessEqual => todo!(),
        BinaryOperator::Or => todo!(),
        BinaryOperator::And => todo!(),
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
                BinaryOperator::Minus => todo!(),
                BinaryOperator::Plus => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left + right);
                    } else if let Some((left, right)) = pair_of_strings(&left, &right) {
                        return Value::String(left + &right);
                    }
                    panic!("Operands must be two numbers or two strings.")
                }
                BinaryOperator::Div => todo!(),
                BinaryOperator::Mul => todo!(),
                BinaryOperator::Equal => todo!(),
                BinaryOperator::EqualEqual => todo!(),
                BinaryOperator::NotEqual => todo!(),
                BinaryOperator::Greater => todo!(),
                BinaryOperator::GreaterEqual => todo!(),
                BinaryOperator::Less => todo!(),
                BinaryOperator::LessEqual => todo!(),
                BinaryOperator::Or => todo!(),
                BinaryOperator::And => todo!(),
            }
        }
        _ => Value::Nil,
    }
}

#[cfg(test)]
mod test;
