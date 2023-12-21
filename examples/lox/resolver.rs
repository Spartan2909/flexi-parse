use super::error_codes;
use super::Ast;
use super::Expr;
use super::Function;
use super::Stmt;

use std::cell::Cell;
use std::collections::HashMap;

use flexi_parse::error::Error;
use flexi_parse::new_error;
use flexi_parse::token::Ident;
use flexi_parse::Result;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Method,
    Initialiser,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
    SubClass,
}

struct State {
    scopes: Vec<HashMap<String, bool>>,
    error: Option<Error>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl State {
    fn declare(&mut self, name: &Ident) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name.string()) {
                self.error(new_error(
                    "Already a variable with this name in scope".to_string(),
                    name,
                    error_codes::SHADOW,
                ));
            } else {
                scope.insert(name.string().to_owned(), false);
            }
        }
    }

    fn define(&mut self, name: &Ident) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.string().to_owned(), true);
        }
    }

    fn resolve_local(&self, name: &Ident, distance: &Cell<Option<usize>>) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(name.string()) {
                distance.set(Some(self.scopes.len() - 1 - i));
                return;
            }
        }
    }

    fn error(&mut self, error: Error) {
        if let Some(existing_error) = &mut self.error {
            existing_error.add(error);
        } else {
            self.error = Some(error);
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

impl Expr {
    fn resolve(&self, state: &mut State) {
        match self {
            Expr::Assign {
                name,
                value,
                distance,
            } => {
                value.resolve(state);
                state.resolve_local(name, distance);
            }
            Expr::Binary(binary) => {
                binary.left().resolve(state);
                binary.right().resolve(state);
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                callee.resolve(state);
                for argument in arguments {
                    argument.resolve(state);
                }
            }
            Expr::Get { object, name: _ } => object.resolve(state),
            Expr::Group(expr) => expr.resolve(state),
            Expr::Literal(_) => {}
            Expr::Logical(logical) => {
                logical.left().resolve(state);
                logical.right().resolve(state);
            }
            Expr::Set {
                object,
                name: _,
                value,
            } => {
                object.resolve(state);
                value.resolve(state);
            }
            Expr::Super {
                keyword,
                distance,
                dot: _,
                method: _,
            } => {
                if state.current_class == ClassType::None {
                    state.error(new_error(
                        "Can't use 'super' outside of a class".to_string(),
                        keyword,
                        error_codes::INVALID_SUPER,
                    ));
                } else if state.current_class == ClassType::Class {
                    state.error(new_error(
                        "Can't use 'super' in a class with no superclass".to_string(),
                        keyword,
                        error_codes::INVALID_SUPER,
                    ));
                }
                state.resolve_local(keyword.ident(), distance);
            }
            Expr::This { keyword, distance } => {
                if state.current_class == ClassType::None {
                    state.error(new_error(
                        "Can't use 'this' outside of a class".to_string(),
                        keyword,
                        error_codes::THIS_OUTSIDE_CLASS,
                    ));
                }

                state.resolve_local(keyword.ident(), distance);
            }
            Expr::Unary(unary) => unary.right().resolve(state),
            Expr::Variable { name, distance } => {
                if let Some(scope) = state.scopes.last() {
                    if scope.get(name.string()) == Some(&false) {
                        state.error(new_error(
                            "Can't read a variable in its own intialiser".to_string(),
                            name,
                            error_codes::INVALID_INITIALISER,
                        ));
                    }
                }
                state.resolve_local(name, distance);
            }
        }
    }
}

impl Function {
    fn resolve(&self, state: &mut State, kind: FunctionType) {
        let enclosing = state.current_function;
        state.current_function = kind;
        state.begin_scope();
        for param in &self.params {
            state.declare(param);
            state.define(param);
        }
        for stmt in &self.body {
            stmt.resolve(state);
        }
        state.end_scope();
        state.current_function = enclosing;
    }
}

impl Stmt {
    fn resolve(&self, state: &mut State) {
        match self {
            Stmt::Block(stmts) => {
                state.begin_scope();
                for stmt in stmts {
                    stmt.resolve(state);
                }
                state.end_scope();
            }
            Stmt::Class {
                name,
                superclass,
                superclass_distance,
                methods,
            } => {
                let enclosing = state.current_class;
                state.current_class = ClassType::Class;

                state.declare(name);
                state.define(name);

                if let Some(superclass) = superclass {
                    if name.string() == superclass.string() {
                        state.error(new_error(
                            "A class can't inherit from itself".to_string(),
                            superclass,
                            error_codes::CYCLICAL_INHERITANCE,
                        ));
                    }

                    state.current_class = ClassType::SubClass;

                    state.resolve_local(superclass, superclass_distance);

                    state.begin_scope();
                    state
                        .scopes
                        .last_mut()
                        .unwrap()
                        .insert("super".to_string(), true);
                }

                state.begin_scope();
                state
                    .scopes
                    .last_mut()
                    .unwrap()
                    .insert("this".to_string(), true);

                for method in methods {
                    let kind = if method.name.string() == "init" {
                        FunctionType::Initialiser
                    } else {
                        FunctionType::Method
                    };
                    method.resolve(state, kind);
                }

                state.end_scope();

                if superclass.is_some() {
                    state.end_scope();
                }

                state.current_class = enclosing;
            }
            Stmt::Expr(expr) | Stmt::Print(expr) => expr.resolve(state),
            Stmt::Function(function) => {
                state.declare(&function.name);
                state.declare(&function.name);
                function.resolve(state, FunctionType::Function);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.resolve(state);
                then_branch.resolve(state);
                if let Some(else_branch) = else_branch {
                    else_branch.resolve(state);
                }
            }
            Stmt::Return { keyword, value } => {
                if state.current_function == FunctionType::None {
                    state.error(new_error(
                        "Can't return from top-level code".to_string(),
                        keyword,
                        error_codes::RETURN_OUTSIDE_FUNCTION,
                    ));
                }
                if let Some(value) = value {
                    value.resolve(state);
                }
            }
            Stmt::Variable { name, initialiser } => {
                state.declare(name);
                if let Some(initialiser) = initialiser {
                    initialiser.resolve(state);
                }
                state.define(name);
            }
            Stmt::While { condition, body } => {
                condition.resolve(state);
                body.resolve(state);
            }
        }
    }
}

pub(super) fn resolve(ast: &Ast) -> Result<()> {
    let mut state = State {
        scopes: vec![],
        error: None,
        current_function: FunctionType::None,
        current_class: ClassType::None,
    };
    for stmt in &ast.0 {
        stmt.resolve(&mut state);
    }
    state.error.map_or(Ok(()), Err)
}
