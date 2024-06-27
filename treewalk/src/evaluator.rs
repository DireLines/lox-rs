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

type Environment = HashMap<String, Value>;
#[derive(Debug)]
struct EnvStack(Vec<Environment>);
impl Default for EnvStack {
    fn default() -> Self {
        Self(vec![Default::default()])
    }
}
impl EnvStack {
    fn push_env(&mut self) {
        self.0.push(HashMap::new())
    }
    fn current(&self) -> &Environment {
        self.0.last().expect("we do not expect empty list of environments, there should always at least be global scope")
    }
    fn current_mut(&mut self) -> &mut Environment {
        self.0.last_mut().expect("we do not expect empty list of environments, there should always at least be global scope")
    }
    fn define(&mut self, name: String, value: Value) {
        self.current_mut().insert(name, value);
    }
    fn declare(&mut self, name: String) {
        self.current_mut().insert(name, Value::Nil);
    }
    fn assign(&mut self, name: String, value: Value) {
        for env in self.0.iter_mut().rev() {
            if let Some(v) = env.get_mut(&name) {
                *v = value;
                return;
            }
        }
        panic!("bad assign");
        // throw new RuntimeError(name,
        //     "Undefined variable '" + name.lexeme + "'.");
    }
    fn get(&self, name: String) -> Option<&Value> {
        for env in self.0.iter().rev() {
            if let Some(v) = env.get(&name) {
                return Some(v);
            }
        }
        panic!("bad get");
        // throw new RuntimeError(name,
        //     "Undefined variable '" + name.lexeme + "'.");
    }
    fn get_mut(&mut self, name: String) -> Option<&mut Value> {
        for env in self.0.iter_mut().rev() {
            if let Some(v) = env.get_mut(&name) {
                return Some(v);
            }
        }
        panic!("bad get_mut");
        // throw new RuntimeError(name,
        //     "Undefined variable '" + name.lexeme + "'.");
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

fn interpret(declarations: Vec<Declaration>, state: &mut EnvStack) {
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

fn interpret_statement(statement: Statement, state: &mut EnvStack) {
    match statement {
        Statement::ExprStmt(expr) => {
            eval_expr(expr, state);
        }
        Statement::PrintStmt(expr) => {
            let v = eval_expr(expr, state);
            println!("{v}");
        }
        Statement::Block { body } => {
            state.push_env();
            interpret(body, state);
        }
        Statement::VarDecl(var_decl) => match var_decl {
            Declaration::VarDecl { name, definition } => {
                let v = eval_expr(definition.unwrap_or(Expression::Nil), state);
                state.define(name, v);
            }
            _ => todo!(),
        },
        Statement::IfStmt {
            condition,
            if_case,
            else_case,
        } => {
            let cond = eval_expr(*condition, state);
            if truth_value(&cond) {
                interpret_statement(*if_case, state);
            } else if let Some(else_case) = *else_case {
                interpret_statement(else_case, state);
            }
        }
        Statement::WhileStmt { condition, body } => {
            //TODO: maybe eval_expr doesn't need to consume the expression?
            while (truth_value(&eval_expr(*condition.clone(), state))) {
                interpret_statement(*body.clone(), state);
            }
        }
        Statement::ForStmt {
            stmt,
            condition,
            increment,
            body,
        } => {}
        _ => todo!(),
    }
}
fn eval_expr(expr: Expression, state: &mut EnvStack) -> Value {
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
            match operator {
                BinaryOperator::Minus => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left - right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Plus => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left + right);
                    } else if let Some((left, right)) = pair_of_strings(&left, &right) {
                        return Value::String(left + &right);
                    }
                    panic!("Operands must be two numbers or two strings.")
                }
                BinaryOperator::Div => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left / right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Mul => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Number(left * right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Equal => todo!(), //anything
                BinaryOperator::EqualEqual => {
                    let right = eval_expr(*right, state);
                    Value::Bool(is_equal(&left, &right))
                }
                BinaryOperator::NotEqual => {
                    let right = eval_expr(*right, state);
                    Value::Bool(!is_equal(&left, &right))
                }
                BinaryOperator::Greater => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left > right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::GreaterEqual => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left >= right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Less => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left < right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::LessEqual => {
                    let right = eval_expr(*right, state);
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Value::Bool(left <= right);
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Or => {
                    if truth_value(&left) {
                        return left;
                    }
                    let right = eval_expr(*right, state);
                    right
                }
                BinaryOperator::And => {
                    if !truth_value(&left) {
                        return left;
                    }
                    let right = eval_expr(*right, state);
                    right
                }
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
