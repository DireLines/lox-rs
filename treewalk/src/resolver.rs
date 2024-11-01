use crate::parser::{
    BinaryOperator, Declaration, Expression, Function, Program, Statement, UnaryOperator,
};
use std::collections::HashMap;
type Environment = HashMap<String, bool>;
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
    fn define(&mut self, name: &str) {
        self.current_mut().insert(name.to_string(), true);
    }
    fn define_global(&mut self, name: &str, value: bool) {
        self.global_scope().insert(name.to_string(), value);
    }
    fn declare(&mut self, name: &str) {
        self.current_mut().insert(name.to_string(), false);
    }
    fn assign(&mut self, name: &str, value: bool) {
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
}

//fills in the resolution_depth for all Expression::Identifier nodes in the program
fn resolve_variables(program: &mut Program) {
    //make the environment
    let mut env = EnvStack::default();
    resolve_decls(&mut program.body, &mut env);
}

fn resolve_decls(declarations: &mut [Declaration], state: &mut EnvStack) {
    for declaration in declarations {
        match declaration {
            Declaration::ClassDecl {
                name,
                parent_name,
                body,
            } => todo!(),
            Declaration::FunDecl(fun) => {
                // TODO introduce a new scope for function body and bind its parameters in that scope.
                state.define(&fun.name)
            }
            Declaration::VarDecl { name, definition } => {
                state.define(&name);
            }
            Declaration::Statement(statement) => {
                resolve_statement(statement.as_mut(), state);
            }
        }
    }
}

fn resolve_statement(statement: &mut Statement, state: &mut EnvStack) {
    match statement {
        Statement::ExprStmt(expr) => {
            resolve_expr(&expr, state)?;
        }
        Statement::PrintStmt(expr) => {
            let v = resolve_expr(&expr, state)?;
            println!("{v}");
        }
        Statement::Block { body } => {
            state.push_env();
            resolve_decls(body, state)?;
            state.pop_env();
        }
        Statement::VarDecl(var_decl) => match var_decl {
            Declaration::VarDecl { name, definition } => {
                let v = resolve_expr(definition.as_ref().unwrap_or(&Expression::Nil), state)?;
                state.define(&name);
            }
            Declaration::ClassDecl { .. } => todo!(),
            Declaration::FunDecl(_) => todo!(),
            Declaration::Statement(statement) => resolve_statement(&statement, state)?,
        },
        Statement::IfStmt {
            condition,
            if_case,
            else_case,
        } => {
            resolve_expr(&condition, state);
            resolve_statement(if_case, state);
            resolve_statement(else_case, state);
        }
        Statement::WhileStmt { condition, body } => {
            resolve_expr(&condition, state);
            resolve_statement(body, state);
        }
        Statement::ForStmt {
            stmt,
            condition,
            increment,
            body,
        } => {
            resolve_statement(
                &Statement::VarDecl(Declaration::Statement(stmt.clone())),
                state,
            )?;
            let mut body_decls = vec![Declaration::Statement(body.clone())];
            if let Some(increment) = increment.as_ref() {
                body_decls.push(Declaration::Statement(Box::new(Statement::ExprStmt(
                    increment.clone(),
                ))));
            }
            let condition = condition.clone().unwrap_or(Expression::Bool(true));
            let while_stmt = Statement::WhileStmt {
                condition: Box::new(condition),
                body: Box::new(Statement::Block { body: body_decls }),
            };
            resolve_statement(&while_stmt, state);
        }
        Statement::ReturnStmt(value) => {
            if let Some(value) = value {
                let return_value = resolve_expr(value, state)?;
                return Err("early return".into());
            }
        }
        _ => todo!(),
    }
}

fn resolve_expr(expr: &mut Expression, state: &mut EnvStack) {
    match expr {
        Expression::Identifier {
            name,
            resolution_depth,
        } => Ok(state
            .get(
                name,
                resolution_depth
                    .expect("this happens after semantic pass to populate resolution distance"),
            )
            .expect("variable not defined")
            .clone()),
        Expression::Grouping { expression } => resolve_expr(expression, state),
        Expression::Unary { operator, right } => {
            let right = resolve_expr(right, state)?;
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
            resolve_expr(left, state);
            resolve_expr(right, state);
                BinaryOperator::Minus => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left - right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Plus => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left + right));
                    } else if let Some((left, right)) = pair_of_strings(&left, &right) {
                        return Ok(Value::String(left + &right));
                    }
                    panic!("Operands must be two numbers or two strings.")
                }
                BinaryOperator::Div => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left / right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Mul => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Number(left * right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Equal => todo!(), //anything
                BinaryOperator::EqualEqual => {
                    let right = resolve_expr(right, state)?;
                    Ok(Value::Bool(is_equal(&left, &right)))
                }
                BinaryOperator::NotEqual => {
                    let right = resolve_expr(right, state)?;
                    Ok(Value::Bool(!is_equal(&left, &right)))
                }
                BinaryOperator::Greater => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left > right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::GreaterEqual => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left >= right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Less => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left < right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::LessEqual => {
                    let right = resolve_expr(right, state)?;
                    if let Some((left, right)) = pair_of_numbers(&left, &right) {
                        return Ok(Value::Bool(left <= right));
                    }
                    panic!("Operands must be two numbers.")
                }
                BinaryOperator::Or => {
                    if truth_value(&left) {
                        return Ok(left);
                    }
                    let right = resolve_expr(right, state);
                    right
                }
                BinaryOperator::And => {
                    if !truth_value(&left) {
                        return Ok(left);
                    }
                    let right = resolve_expr(right, state);
                    right
                }
            }
        }
        Expression::MemberAssign { path, field, value } => {
            //TODO: replace with fully parsing the path
            let v = resolve_expr(value, state)?;
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
                                            .map(|arg| resolve_expr(arg, state).expect("we don't think you can hit a return statement, which is the only reason you can get an err, when evaluating args"))
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
