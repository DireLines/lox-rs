use crate::parser::{
    BinaryOperator, Declaration, Expression, Function, Program, Statement, UnaryOperator,
};
use std::{collections::HashMap, error::Error};

type Result<T> = std::result::Result<T, Box<dyn Error>>;
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
}

//fills in the resolution_depth for all Expression::Identifier nodes in the program
fn resolve_variables(program: &mut Program) {
    //make the environment
    let mut env = EnvStack::default();
    resolve_decls(&mut program.body, &mut env);
}

fn resolve_decls(declarations: &mut [Declaration], state: &mut EnvStack) {
    for declaration in declarations {
        resolve_decl(declaration, state);
    }
}
fn resolve_decl(declaration: &mut Declaration, state: &mut EnvStack) {
    match declaration {
        Declaration::ClassDecl {
            name,
            parent_name,
            body,
        } => todo!(),
        Declaration::FunDecl(fun) => {
            state.define(&fun.name);
            state.push_env();
            for param in fun.parameters.iter() {
                state.define(&param);
            }
            if let Some(body) = fun.body.as_mut() {
                resolve_statement(body, state);
            }
            state.pop_env();
        }
        Declaration::VarDecl { name, definition } => {
            state.declare(name);
            if let Some(defn) = definition {
                resolve_expr(defn, state);
            }
            state.define(name);
        }
        Declaration::Statement(statement) => {
            resolve_statement(statement.as_mut(), state);
        }
    }
}

fn resolve_statement(statement: &mut Statement, state: &mut EnvStack) -> Result<()> {
    match statement {
        Statement::ExprStmt(expr) => {
            resolve_expr(expr, state);
        }
        Statement::PrintStmt(expr) => {
            resolve_expr(expr, state);
        }
        Statement::Block { body } => {
            state.push_env();
            resolve_decls(body, state);
            state.pop_env();
        }
        Statement::VarDecl(var_decl) => {
            resolve_decl(var_decl, state);
        }
        Statement::IfStmt {
            condition,
            if_case,
            else_case,
        } => {
            resolve_expr(condition, state);
            resolve_statement(if_case, state);
            if let Some(else_case) = else_case.as_mut() {
                resolve_statement(else_case, state)?;
            }
        }
        Statement::WhileStmt { condition, body } => {
            resolve_expr(condition, state);
            resolve_statement(body, state);
        }
        Statement::ReturnStmt(value) => {
            if let Some(value) = value {
                resolve_expr(value, state);
            }
        }
        _ => todo!(),
    }
    Ok(())
}

fn resolve_expr(expr: &mut Expression, state: &mut EnvStack) {
    match expr {
        Expression::Identifier {
            name,
            resolution_depth,
        } => {
            //do a normal variable resolution to find resolution depth for this name
            //write resolution depth into the AST for this identifier
            for (i, scope) in state.0.iter().rev().enumerate() {
                if scope.contains_key(name) {
                    *resolution_depth = Some(state.0.len() - i);
                }
            }
        }
        Expression::Grouping { expression } => resolve_expr(expression, state),
        Expression::Unary { operator, right } => resolve_expr(right, state),
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            resolve_expr(left, state);
            resolve_expr(right, state);
        }
        Expression::MemberAssign {
            path,
            field,
            value,
            resolution_depth,
        } => {
            //TODO: replace with fully parsing the path
            let v = resolve_expr(value, state);
            let full_path = field;
            for (i, scope) in state.0.iter().rev().enumerate() {
                if scope.contains_key(full_path) {
                    *resolution_depth = Some(state.0.len() - i);
                }
            }
            state.assign(full_path, true);
        }
        Expression::Call { base, path } => {
            resolve_expr(base, state);
            match base.as_ref() {
                Expression::Identifier {
                    name: _,
                    resolution_depth: _,
                } => {
                    let member_access = &mut path[0];
                    match member_access {
                        crate::parser::MemberAccess::Field(_) => todo!(),
                        crate::parser::MemberAccess::Args { args } => {
                            args.iter_mut().for_each(|arg| resolve_expr(arg, state))
                        }
                    }
                }
                _ => todo!(),
            }
        }
        Expression::SuperFieldAccess { field } => todo!(),
        _ => {}
    }
}
