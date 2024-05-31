use crate::parser::{BinaryOperator, Declaration, Expression, Program, Statement, UnaryOperator};
use std::{collections::HashMap, error::Error, fmt::Display};
type Result<T> = std::result::Result<T, Box<dyn Error>>;
//lox runtime value
#[derive(Debug, PartialEq, Clone)]
enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Default)]
struct Environment<'a> {
    environment: HashMap<String, Value>,
    enclosing: Option<&'a mut Environment<'a>>,
}
impl<'a> Environment<'a> {
    fn new(enclosing: &'a mut Environment<'a>) -> Self {
        Self {
            environment: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }
    fn define(&mut self, name: String, value: Value) {
        self.environment.insert(name, value);
    }
    fn declare(&mut self, name: String) {
        self.environment.insert(name, Value::Nil);
    }
    fn assign(&mut self, name: String, value: Value) {
        println!("assigning {name} to {:?}", value.clone());
        if let Some(v) = self.environment.get_mut(&name) {
            *v = value;
        } else {
            if let Some(enclosing) = &mut self.enclosing {
                enclosing.assign(name, value);
                return;
            }
            panic!();
            // throw new RuntimeError(name,
            //     "Undefined variable '" + name.lexeme + "'.");
        }
    }
    fn get(&self, name: String) -> Option<&Value> {
        if let Some(v) = self.environment.get(&name) {
            Some(v)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(name)
        } else {
            panic!();
            // throw new RuntimeError(name,
            //     "Undefined variable '" + name.lexeme + "'.");
        }
    }
    fn get_mut(&mut self, name: String) -> Option<&mut Value> {
        if let Some(v) = self.environment.get_mut(&name) {
            Some(v)
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.get_mut(name)
        } else {
            panic!();
            // throw new RuntimeError(name,
            //     "Undefined variable '" + name.lexeme + "'.");
        }
    }
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

fn evaluate(program: Program) -> Result<()> {
    //make the environment
    //parse the program
    //interpret each thing in the program
    Ok(())
}

fn interpret<'a, 'b>(declarations: Vec<Declaration>, state: &'a mut Environment<'a>) {
    for declaration in declarations {
        match declaration {
            Declaration::ClassDecl {
                name,
                parent_name,
                body,
            } => todo!(),
            Declaration::FunDecl(_) => todo!(),
            Declaration::VarDecl { name, definition } => {
                let v = eval_expr(definition.unwrap_or(Expression::Nil), state);
                state.define(name, v);
            }
            Declaration::Statement(statement) => interpret_statement(*statement, state),
        }
    }
}

fn interpret_statement<'a, 'b>(statement: Statement, state: &'b mut Environment<'a>) {
    match statement {
        Statement::ExprStmt(expr) => {
            eval_expr(expr, state);
        }
        Statement::PrintStmt(expr) => {
            let v = eval_expr(expr, state);
            println!("{v}");
        }
        Statement::Block { body } => {
            //make new scope
            let mut inner = Environment::new(state);
            interpret(body, &mut inner);
        }
        Statement::VarDecl(var_decl) => match var_decl {
            Declaration::VarDecl { name, definition } => {
                let v = eval_expr(definition.unwrap_or(Expression::Nil), state);
                state.define(name, v);
            }
            _ => todo!(),
        },
        _ => todo!(),
    }
}
fn eval_expr(expr: Expression, state: &mut Environment) -> Value {
    match expr {
        Expression::Number(n) => Value::Number(n),
        Expression::String(s) => Value::String(s),
        Expression::Identifier(name) => state.get(name).expect("variable not defined").clone(),
        Expression::Bool(b) => Value::Bool(b),
        Expression::Nil => Value::Nil,
        Expression::This => panic!("need to know how classes work"),
        Expression::Grouping { expression } => eval_expr(*expression, state),
        Expression::Unary { operator, right } => {
            let right = eval_expr(*right, state);
            match operator {
                UnaryOperator::Neg => match right {
                    Value::Number(n) => Value::Number(-n),
                    _ => panic!("Operand must be a number."),
                },
                UnaryOperator::Not => Value::Bool(!truth_value(&right)),
            }
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval_expr(*left, state);
            let right = eval_expr(*right, state);
            match operator {
                BinaryOperator::Minus => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left - right);
                    }
                    panic!("Operands must be two numbers.")
                }
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
                }
                BinaryOperator::Mul => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left * right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Equal => todo!(), //anything
                BinaryOperator::EqualEqual => Value::Bool(is_equal(&left, &right)),
                BinaryOperator::NotEqual => Value::Bool(!is_equal(&left, &right)),
                BinaryOperator::Greater => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left > right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::GreaterEqual => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left >= right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Less => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left < right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::LessEqual => {
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left <= right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Or => Value::Bool(truth_value(&left) || truth_value(&right)),
                BinaryOperator::And => Value::Bool(truth_value(&left) && truth_value(&right)),
            }
        }
        Expression::MemberAssign { path, field, value } => {
            //TODO: replace with fully parsing the path
            let v = eval_expr(*value, state);
            let full_path = field;
            state.assign(full_path, v.clone());
            v
        }
        Expression::Call { base, path } => todo!(),
        Expression::SuperFieldAccess { field } => todo!(),
        _ => Value::Nil,
    }
}

#[cfg(test)]
mod test;
