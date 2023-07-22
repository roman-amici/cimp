use crate::{ast::*, token::Token};
use std::{collections::HashMap, fmt::format};

pub struct Resolver {
    scopes: Vec<Scope>,
}

#[derive(Debug)]
pub struct ResolverError {
    pub token: Token,
    pub message: String,
}

struct Scope {
    next_slot: usize,
    slots: HashMap<String, usize>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut resolver = Resolver { scopes: vec![] };
        // Global scope
        resolver.begin_scope();
        resolver
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            next_slot: 0,
            slots: HashMap::new(),
        });
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn resolve_expr(&mut self, expr: &mut Expr) -> Result<(), ResolverError> {
        match expr {
            Expr::Unary(unary) => self.resolve_expr(&mut unary.expr),
            Expr::Binary(binary) => {
                self.resolve_expr(&mut binary.left)?;
                self.resolve_expr(&mut binary.right)
            }
            Expr::Assign(assign) => {
                self.resolve_expr(&mut assign.l_value)?;
                self.resolve_expr(&mut assign.expr)
            }
            Expr::Call(call) => {
                for arg in call.args.iter_mut() {
                    self.resolve_expr(arg)?;
                }
                self.resolve_expr(&mut call.call_site)
            }
            Expr::ArrayDeref(array) => {
                self.resolve_expr(&mut array.call_site)?;
                self.resolve_expr(&mut array.index)
            }
            Expr::Identifier(identifier) => self.resolve_identifier(identifier),
            _ => Ok(()),
        }
    }

    fn resolve_identifier(&mut self, identifier: &mut Identifier) -> Result<(), ResolverError> {
        for (scope_index, scope) in self.scopes.iter().rev().enumerate() {
            let name = identifier.token.literal.as_ref().unwrap();
            if let Some(slot_index) = scope.slots.get(name) {
                identifier.resolution = Some(Resolution {
                    scope_index,
                    slot_index: *slot_index,
                });

                return Ok(());
            }
        }
        let token = identifier.token.clone();
        let name = token.literal.as_ref().unwrap().clone();
        Err(ResolverError {
            message: String::from(format!("Unable to find variable '{}' in this scope", name)),
            token,
        })
    }

    pub fn resolve_statement(&mut self, statement: &mut Statement) -> Result<(), ResolverError> {
        match statement {
            Statement::ExprStatement(expr_statement) => self.resolve_expr(&mut expr_statement.expr),
            Statement::BlockStatement(block_statement) => {
                self.begin_scope();

                for statement in block_statement.statements.iter_mut() {
                    self.resolve_statement(statement)?;
                }

                self.end_scope();
                Ok(())
            }
            Statement::IfStatement(if_statement) => {
                self.resolve_expr(&mut if_statement.condition)?;
                self.resolve_statement(&mut if_statement.block)
            }
            Statement::ForStatement(for_statement) => {
                self.resolve_expr(&mut for_statement.initializer)?;
                self.resolve_expr(&mut for_statement.condition)?;
                self.resolve_expr(&mut for_statement.update)?;
                self.resolve_statement(&mut for_statement.block)
            }
            Statement::WhileStatement(while_statement) => {
                self.resolve_expr(&mut while_statement.condition)?;
                self.resolve_statement(&mut while_statement.block)
            }
            Statement::ReturnStatement(return_statement) => {
                if let Some(expr) = return_statement.return_value.as_mut() {
                    self.resolve_expr(expr)?;
                }
                Ok(())
            }
            Statement::LetStatement(let_statement) => {
                if let Some(expr) = let_statement.initializer.as_mut() {
                    self.resolve_expr(expr)?;
                }

                self.add_variable(let_statement.variable_name.clone())
            }

            _ => Ok(()),
        }
    }

    fn add_variable(&mut self, variable_name: Token) -> Result<(), ResolverError> {
        let name = variable_name.literal.as_ref().unwrap().clone();

        if let Some(scope) = self.scopes.last_mut() {
            if scope.slots.get(&name).is_some() {
                return Err(ResolverError {
                    token: variable_name,
                    message: format!(
                        "A variable with name '{}' already exists in this scope",
                        name
                    ),
                });
            } else {
                scope.slots.insert(name, scope.next_slot);
                scope.next_slot += 1;
            }
        }

        Ok(())
    }

    fn resolve_declaration(&mut self, declaration: &mut Declaration) -> Result<(), ResolverError> {
        match declaration {
            Declaration::FunctionDeclaration(fn_declaration) => {
                self.resolve_fn_declaration(fn_declaration)
            }
            Declaration::GlobalDeclaration(global_declaration) => {
                assert!(self.scopes.len() == 1);
                self.resolve_statement(global_declaration)
            }
        }
    }

    fn resolve_fn_declaration(
        &mut self,
        fn_declaration: &mut FunctionDeclaration,
    ) -> Result<(), ResolverError> {
        self.add_variable(fn_declaration.function_name.clone())?;

        self.begin_scope();

        for (arg_name, _) in fn_declaration.args.iter() {
            self.add_variable(arg_name.clone())?;
        }

        self.resolve_statement(&mut fn_declaration.block)?;

        self.end_scope();

        Ok(())
    }
}
