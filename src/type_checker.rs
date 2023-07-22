use crate::{
    ast::*,
    token::{self, TokenType},
};
use std::{collections::HashMap, iter::zip};

use crate::token::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Float,
    Int,
    Void,
    Fn(Option<Box<Type>>, Vec<Type>),
    Struct, // TODO: structs
}

struct Scope {
    next: usize,
    map: HashMap<usize, Type>,
}

pub struct TypeChecker {
    type_names: HashMap<String, Type>,

    scopes: Vec<Scope>,

    function_context: Option<Type>,
}

#[derive(Debug)]
pub struct TypeError {
    token: Token,
    message: String,
}

fn type_error(token: Token, expected: Type, actual: Type) -> TypeError {
    TypeError {
        token,
        message: format!("Expected type {:?} but found type {:?}", expected, actual),
    }
}

fn check_equal(token: &Token, expected: &Type, actual: &Type) -> Result<(), TypeError> {
    if *expected != *actual {
        Err(type_error(token.clone(), expected.clone(), actual.clone()))
    } else {
        Ok(())
    }
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        match *self {
            Type::Int | Type::Float => true,
            _ => false,
        }
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut tc = TypeChecker {
            type_names: HashMap::new(),
            scopes: vec![],
            function_context: None,
        };

        tc.begin_scope();

        tc.type_names.insert(String::from("i64"), Type::Int);
        tc.type_names.insert(String::from("f64"), Type::Float);

        tc
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            next: 0,
            map: HashMap::new(),
        });
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn check_statement(&mut self, statement: &mut Statement) -> Result<(), TypeError> {
        match statement {
            Statement::BlockStatement(block) => {
                self.begin_scope();
                for statement in block.statements.iter_mut() {
                    self.check_statement(statement)?;
                }
                self.end_scope();
            }
            Statement::ReturnStatement(ret) => self.check_return_statement(ret)?,
            Statement::ExprStatement(expr) => {
                self.check_expr(&mut expr.expr)?;
            }
            Statement::IfStatement(if_statement) => {
                let condition_type = self.check_expr(&mut if_statement.condition)?;
                check_equal(&if_statement.token, &Type::Int, &condition_type)?;
                self.check_statement(&mut if_statement.block)?;
            }
            Statement::ForStatement(for_statement) => {
                self.check_expr(&mut for_statement.initializer)?;
                let condition_type = self.check_expr(&mut for_statement.condition)?;
                check_equal(&for_statement.token, &Type::Int, &condition_type)?;
                self.check_expr(&mut for_statement.update)?;
                self.check_statement(&mut for_statement.block)?;
            }
            Statement::WhileStatement(while_statement) => {
                let condition_type = self.check_expr(&mut while_statement.condition)?;
                check_equal(&while_statement.token, &Type::Int, &condition_type)?;
                self.check_statement(&mut while_statement.block)?;
            }
            Statement::LetStatement(let_statement) => self.check_let_statement(let_statement)?,
            _ => (),
        }

        Ok(())
    }

    fn check_let_statement(&mut self, let_statement: &mut LetStatement) -> Result<(), TypeError> {
        let initializer_type = if let Some(expr) = let_statement.initializer.as_mut() {
            Some(self.check_expr(expr)?)
        } else {
            None
        };

        if let_statement.type_name.is_none() && initializer_type.is_none() {
            return Err(TypeError {
                token: let_statement.token.clone(),
                message: String::from("Let statement must have type or initializer."),
            });
        }

        let variable_type = if initializer_type.is_none() {
            self.get_type_from_name(let_statement.type_name.as_ref().unwrap())?
        } else {
            initializer_type.unwrap()
        };

        let_statement.type_info = Some(Box::new(variable_type.clone()));
        self.add_type(variable_type);
        Ok(())
    }

    fn check_return_statement(&mut self, ret: &mut ReturnStatement) -> Result<(), TypeError> {
        let expr_type = if let Some(expr) = ret.return_value.as_mut() {
            self.check_expr(expr)?
        } else {
            Type::Void
        };

        let fn_type = self.function_context.as_ref().unwrap();
        if let Type::Fn(Some(return_type), _) = fn_type {
            check_equal(&ret.token, &return_type, &expr_type)?
        } else {
            panic!("Expected function type");
        }

        Ok(())
    }

    pub fn check_expr(&mut self, expr: &mut Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::Identifier(identifier) => {
                let id_type = self.get_type(identifier.resolution.as_ref().unwrap());
                identifier.type_info = Some(id_type.clone());

                Ok(id_type)
            }
            Expr::Number(number) => {
                if number.token.literal.as_ref().unwrap().contains(".") {
                    number.type_info = Some(Type::Float);
                    Ok(Type::Float)
                } else {
                    number.type_info = Some(Type::Int);
                    Ok(Type::Int)
                }
            }
            Expr::Unary(unary) => self.check_unary(unary),
            Expr::Binary(binary) => self.check_binary(binary),
            Expr::Assign(assign) => self.check_assign(assign),
            Expr::Call(call) => self.check_call(call),
            Expr::ArrayDeref(array_deref) => self.check_array_deref(array_deref),
        }
    }

    fn check_unary(&mut self, unary: &mut Unary) -> Result<Type, TypeError> {
        let type_info = self.check_expr(&mut unary.expr)?;

        match unary.op.token_type {
            TokenType::Amp => Ok(Type::Int),
            TokenType::Minus => Ok(type_info),
            TokenType::At => {
                if type_info == Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(type_error(unary.token.clone(), Type::Int, type_info))
                }
            }
            TokenType::Bang => {
                if type_info == Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(type_error(unary.token.clone(), Type::Int, type_info))
                }
            }
            _ => unreachable!(),
        }
    }

    fn check_binary(&mut self, binary: &mut Binary) -> Result<Type, TypeError> {
        let left = self.check_expr(&mut binary.left)?;
        let right = self.check_expr(&mut binary.right)?;

        match binary.op.token_type {
            TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash => {
                if left.is_numeric() && right.is_numeric() && left == right {
                    Ok(left)
                } else {
                    Err(type_error(binary.op.clone(), left, right))
                }
            }
            TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => {
                if left != right {
                    Err(type_error(binary.op.clone(), left, right))
                } else {
                    Ok(Type::Int)
                }
            }
            TokenType::AmpAmp | TokenType::BarBar => {
                if left == Type::Int && right == Type::Int {
                    Ok(left)
                } else {
                    Err(type_error(
                        binary.op.clone(),
                        Type::Int,
                        if left != Type::Int { left } else { right },
                    ))
                }
            }
            _ => unreachable!(),
        }
    }

    fn check_call(&mut self, call: &mut Call) -> Result<Type, TypeError> {
        let fn_type = self.check_expr(&mut call.call_site)?;

        if let Type::Fn(return_type, arg_types) = fn_type {
            if arg_types.len() != call.args.len() {
                return Err(TypeError {
                    token: call.token.clone(),
                    message: String::from("Argument counts don't match"),
                });
            }

            for (arg, arg_type) in zip(&mut call.args, &arg_types) {
                let arg_type_actual = self.check_expr(arg)?;
                if arg_type_actual != *arg_type {
                    return Err(type_error(
                        call.token.clone(),
                        arg_type.clone(),
                        arg_type_actual,
                    ));
                }
            }

            if let Some(return_type) = return_type {
                Ok(*return_type.clone())
            } else {
                Ok(Type::Void)
            }
        } else {
            Err(TypeError {
                token: call.token.clone(),
                message: format!("Expected function type but found {:?}", fn_type),
            })
        }
    }

    fn check_array_deref(&mut self, array_deref: &mut ArrayDeref) -> Result<Type, TypeError> {
        let index_type = self.check_expr(&mut array_deref.index)?;
        if index_type != Type::Int {
            return Err(type_error(array_deref.token.clone(), Type::Int, index_type));
        }

        let call_site_type = self.check_expr(&mut array_deref.call_site)?;
        if call_site_type != Type::Int {
            return Err(type_error(
                array_deref.token.clone(),
                Type::Int,
                call_site_type,
            ));
        }

        Ok(Type::Int)
    }

    fn check_assign(&mut self, assign: &mut Assign) -> Result<Type, TypeError> {
        let expr_type = self.check_expr(&mut assign.expr)?;
        let l_value_type = self.check_expr(&mut assign.l_value)?;

        if expr_type != l_value_type {
            Err(type_error(assign.token.clone(), l_value_type, expr_type))
        } else {
            Ok(expr_type)
        }
    }

    fn get_type_from_name(&self, type_name: &Token) -> Result<Type, TypeError> {
        let literal = type_name.literal.as_ref().unwrap().clone();
        if let Some(t) = self.type_names.get(&literal) {
            Ok(t.clone())
        } else {
            Err(TypeError {
                token: type_name.clone(),
                message: format!("Could not find type with name '{}'", literal),
            })
        }
    }

    fn get_type(&self, resolution: &Resolution) -> Type {
        let scope = &self.scopes[self.scopes.len() - resolution.scope_index - 1];

        scope.map[&resolution.slot_index].clone()
    }

    fn add_type(&mut self, type_to_add: Type) {
        let scope = self.scopes.iter_mut().last().unwrap();
        let index = scope.next;
        scope.next += 1;
        scope.map.insert(index, type_to_add);
    }

    fn check_declaration(&mut self, declaration: &mut Declaration) -> Result<(), TypeError> {
        match declaration {
            Declaration::GlobalDeclaration(global) => self.check_statement(global),
            Declaration::FunctionDeclaration(fn_declaration) => {
                let return_type = if let Some(return_type) = fn_declaration.return_type.as_ref() {
                    Some(Box::new(self.get_type_from_name(return_type)?))
                } else {
                    None
                };

                let mut arg_types = vec![];
                for (_, arg_type_name) in fn_declaration.args.iter_mut() {
                    let arg_type = self.get_type_from_name(arg_type_name)?;
                    self.add_type(arg_type.clone());
                    arg_types.push(arg_type);
                }

                self.add_type(Type::Fn(return_type, arg_types));
                Ok(())
            }
        }
    }
}
