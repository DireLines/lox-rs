use crate::parser::{
    BinaryOperator, Declaration, Expression, Function, Program, Statement, UnaryOperator,
};
use std::{collections::HashMap, error::Error, fmt::Display};

//TODO: maybe use this for error and early return handling instead of panics
enum LoxEvaluatorError {
    LoxRuntimeError(Box<dyn Error>),
    EarlyReturn(Value),
}
type Result<T> = std::result::Result<T, Box<dyn Error>>;
//lox runtime value
#[derive(Debug, PartialEq, Clone)]
enum Value {
    Function(Function),
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

const RETURN_VALUE_SPECIAL_KEY: &str = "return value special key";

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Function(fun) => write!(f, "<fun {}>", fun.name),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

type Environment = HashMap<String, Value>;
#[derive(Debug)]
pub struct EnvStack(Vec<Environment>);
impl Default for EnvStack {
    fn default() -> Self {
        Self(vec![Default::default()])
    }
}
impl EnvStack {
    fn push_env(&mut self) {
        self.0.push(HashMap::new())
    }
    fn pop_env(&mut self) {
        //TODO: handle pop error
        self.0.pop();
    }
    fn current(&self) -> &Environment {
        self.0.last().expect("we do not expect empty list of environments, there should always at least be global scope")
    }
    fn current_mut(&mut self) -> &mut Environment {
        self.0.last_mut().expect("we do not expect empty list of environments, there should always at least be global scope")
    }
    fn global_scope(&mut self) -> &mut Environment {
        self.0.first_mut().expect("we do not expect empty list of environments, there should always at least be global scope")
    }
    fn define(&mut self, name: &str, value: Value) {
        self.current_mut().insert(name.to_string(), value);
    }
    fn define_global(&mut self, name: &str, value: Value) {
        self.global_scope().insert(name.to_string(), value);
    }
    fn declare(&mut self, name: &str) {
        self.current_mut().insert(name.to_string(), Value::Nil);
    }
    fn assign(&mut self, name: &str, value: Value) {
        for env in self.0.iter_mut().rev() {
            if let Some(v) = env.get_mut(name) {
                *v = value;
                return;
            }
        }
        panic!("bad assign");
        // throw new RuntimeError(name,
        //     "Undefined variable '" + name.lexeme + "'.");
    }
    /// intended for retrieving an owned return value without cloning when a function call completes
    fn remove(&mut self, name: &str) -> Option<Value> {
        self.0.first_mut()?.remove(name)
    }
    fn get(&self, name: &str, resolution_depth: usize) -> Option<&Value> {
        let env = self.0.get(resolution_depth).expect("there were not enough environments in the stack, even after semantic analysis to determine resolution distance");
        if let Some(v) = env.get(name) {
            return Some(v);
        }
        panic!("bad get");
        // throw new RuntimeError(name,
        //     "Undefined variable '" + name.lexeme + "'.");
    }
    fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        for env in self.0.iter_mut().rev() {
            if let Some(v) = env.get_mut(name) {
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

fn evaluate(program: &Program) -> Result<EnvStack> {
    //make the environment
    let mut env = EnvStack::default();
    //interpret each thing in the program
    interpret(&program.body, &mut env)?;
    Ok(env)
}

pub fn interpret(declarations: &[Declaration], state: &mut EnvStack) -> Result<()> {
    for declaration in declarations {
        match declaration {
            Declaration::ClassDecl {
                name,
                parent_name,
                body,
            } => todo!(),
            Declaration::FunDecl(fun) => state.define(&fun.name, Value::Function(fun.clone())),
            Declaration::VarDecl { name, definition } => {
                let v = eval_expr(definition.as_ref().unwrap_or(&Expression::Nil), state)?;
                state.define(&name, v);
            }
            Declaration::Statement(statement) => interpret_statement(&statement, state)?,
        }
    }
    Ok(())
}

fn interpret_statement(statement: &Statement, state: &mut EnvStack) -> Result<()> {
    match statement {
        Statement::ExprStmt(expr) => {
            eval_expr(&expr, state)?;
        }
        Statement::PrintStmt(expr) => {
            let v = eval_expr(&expr, state)?;
            println!("{v}");
        }
        Statement::Block { body } => {
            state.push_env();
            interpret(body, state)?;
            state.pop_env();
        }
        Statement::VarDecl(var_decl) => match var_decl {
            Declaration::VarDecl { name, definition } => {
                let v = eval_expr(definition.as_ref().unwrap_or(&Expression::Nil), state)?;
                state.define(&name, v);
            }
            Declaration::ClassDecl { .. } => todo!(),
            Declaration::FunDecl(_) => todo!(),
            Declaration::Statement(statement) => interpret_statement(&statement, state)?,
        },
        Statement::IfStmt {
            condition,
            if_case,
            else_case,
        } => {
            let cond = eval_expr(&condition, state)?;
            if truth_value(&cond) {
                interpret_statement(if_case, state)?;
            } else if let Some(else_case) = else_case.as_ref() {
                interpret_statement(else_case, state)?;
            }
        }
        Statement::WhileStmt { condition, body } => {
            //TODO: maybe eval_expr doesn't need to consume the expression?
            while (truth_value(&eval_expr(&condition, state)?)) {
                interpret_statement(body, state)?;
            }
        }
        Statement::ReturnStmt(value) => {
            if let Some(value) = value {
                let return_value = eval_expr(value, state)?;
                state.define_global(RETURN_VALUE_SPECIAL_KEY, return_value);
                return Err("early return".into());
            }
        }
        _ => todo!(),
    }
    Ok(())
}
fn eval_expr(expr: &Expression, state: &mut EnvStack) -> Result<Value> {
    match expr {
        Expression::Number(n) => Ok(Value::Number(*n)),
        Expression::String(s) => Ok(Value::String(s.to_string())),
        Expression::Identifier {
            name,
            resolution_depth,
        } => Ok(state
            .get(name, resolution_depth.expect("variable not defined"))
            .expect("variable not defined")
            .clone()),
        Expression::Bool(b) => Ok(Value::Bool(*b)),
        Expression::Nil => Ok(Value::Nil),
        Expression::This => panic!("need to know how classes work"),
        Expression::Grouping { expression } => eval_expr(expression, state),
        Expression::Unary { operator, right } => {
            let right = eval_expr(right, state)?;
            match operator {
                UnaryOperator::Neg => match right {
                    Value::Number(n) => Ok(Value::Number(-n)),
                    _ => panic!("Operand must be a number."),
                },
                UnaryOperator::Not => Ok(Value::Bool(!truth_value(&right))),
            }
        }
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left = eval_expr(left, state)?;
            match operator {
                BinaryOperator::Minus => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left - right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Plus => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left + right));
                    } else if let Some((left, right)) = pair_of_strings(&left, &right) {
                        return Ok(Value::String(left + &right));
                    }
                    panic!("Operands must be two numbers or two strings.")
                }
                BinaryOperator::Div => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left / right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Mul => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left * right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Equal => todo!(), //anything
                BinaryOperator::EqualEqual => {
                    let right = eval_expr(right, state)?;
                    Ok(Value::Bool(is_equal(&left, &right)))
                }
                BinaryOperator::NotEqual => {
                    let right = eval_expr(right, state)?;
                    Ok(Value::Bool(!is_equal(&left, &right)))
                }
                BinaryOperator::Greater => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left > right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::GreaterEqual => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left >= right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Less => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left < right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::LessEqual => {
                    let right = eval_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left <= right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Or => {
                    if truth_value(&left) {
                        return Ok(left);
                    }
                    let right = eval_expr(right, state);
                    right
                }
                BinaryOperator::And => {
                    if !truth_value(&left) {
                        return Ok(left);
                    }
                    let right = eval_expr(right, state);
                    right
                }
            }
        }
        Expression::MemberAssign {
            path,
            field,
            value,
            resolution_depth,
        } => {
            //TODO: replace with fully parsing the path
            let v = eval_expr(value, state)?;
            let full_path = field;
            state.assign(full_path, v.clone());
            Ok(v)
        }
        Expression::Call { base, path } => match base.as_ref() {
            Expression::Identifier {
                name: fun_name,
                resolution_depth,
            } => {
                let fun = state.get(
                    &fun_name,
                    resolution_depth
                        .expect("this happens after semantic pass to populate resolution distance"),
                );
                let member_access = &path[0];
                match member_access {
                    crate::parser::MemberAccess::Field(_) => todo!(),
                    crate::parser::MemberAccess::Args { args } => {
                        if let Some(fun) = fun {
                            match fun.clone() {
                                Value::Function(fun) => {
                                    //evaluate arguments
                                    let evaluated_args = args
                                        .iter()
                                        .map(|arg| eval_expr(arg, state).expect("we don't think you can hit a return statement, which is the only reason you can get an err, when evaluating args"))
                                        .collect::<Vec<_>>();
                                    //make new env for function call
                                    let mut function_env = EnvStack::default();
                                    //define values of the arguments in this new env
                                    for (name, value) in
                                        fun.parameters.iter().zip(evaluated_args.into_iter())
                                    {
                                        function_env.define(name, value);
                                    }
                                    //interpret the body of the function with that env
                                    if let Some(body) = *fun.body {
                                        interpret_statement(&body, &mut function_env);
                                    }
                                    Ok(function_env
                                        .remove(RETURN_VALUE_SPECIAL_KEY)
                                        .unwrap_or(Value::Nil))
                                }
                                _ => panic!("this variable is not a function"),
                            }
                        } else {
                            panic!("no such function");
                        }
                    }
                }
            }
            _ => todo!(),
        },
        Expression::SuperFieldAccess { field } => todo!(),
        _ => Ok(Value::Nil),
    }
}

#[cfg(test)]
mod test;
