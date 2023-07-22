use crate::{ast::*, token::TokenType, type_checker::Type};

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Void,
}

pub struct TreeWalker {
    pub scopes: Vec<Vec<Value>>,
}

impl TreeWalker {
    pub fn new() -> Self {
        TreeWalker {
            scopes: vec![vec![]],
        }
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Number(number) => self.eval_number(number),
            Expr::Identifier(identifier) => self.eval_identifier(identifier),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            _ => todo!(),
        }
    }

    fn eval_number(&mut self, number: &Number) -> Value {
        match number.type_info.as_ref().unwrap() {
            Type::Float => Value::Float(number.token.literal.as_ref().unwrap().parse().unwrap()),
            Type::Int => Value::Int(number.token.literal.as_ref().unwrap().parse().unwrap()),
            _ => unreachable!(),
        }
    }

    fn eval_identifier(&mut self, identifier: &Identifier) -> Value {
        let resolution = identifier.resolution.as_ref().unwrap();
        let scope = &self.scopes[self.scopes.len() - resolution.scope_index - 1];
        scope[resolution.slot_index].clone()
    }

    fn eval_unary(&mut self, unary: &Unary) -> Value {
        let value = self.eval_expr(&unary.expr);
        match unary.op.token_type {
            TokenType::Minus => match value {
                Value::Int(i) => Value::Int(-i),
                Value::Float(f) => Value::Float(-f),
                _ => unreachable!(),
            },
            TokenType::Bang => match value {
                Value::Int(i) => {
                    let b = !(i != 0);
                    Value::Int(if b { 1 } else { 0 })
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn eval_binary(&mut self, binary: &Binary) -> Value {
        let left = self.eval_expr(&binary.left);
        let right = self.eval_expr(&binary.right);

        match binary.op.token_type {
            TokenType::Plus => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 + i2),
                (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 + f2),
                _ => unreachable!(),
            },
            TokenType::Minus => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 - i2),
                (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 - f2),
                _ => unreachable!(),
            },
            TokenType::Star => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 * i2),
                (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 * f2),
                _ => unreachable!(),
            },
            TokenType::Slash => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 / i2),
                (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 / f2),
                _ => unreachable!(),
            },
            TokenType::EqualEqual => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 == i2 { 1 } else { 0 }),
                (Value::Float(f1), Value::Float(f2)) => Value::Int(if f1 == f2 { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::BangEqual => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 != i2 { 1 } else { 0 }),
                (Value::Float(f1), Value::Float(f2)) => Value::Int(if f1 != f2 { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::Less => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 < i2 { 1 } else { 0 }),
                (Value::Float(f1), Value::Float(f2)) => Value::Int(if f1 < f2 { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::LessEqual => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 <= i2 { 1 } else { 0 }),
                (Value::Float(f1), Value::Float(f2)) => Value::Int(if f1 <= f2 { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::Greater => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 > i2 { 1 } else { 0 }),
                (Value::Float(f1), Value::Float(f2)) => Value::Int(if f1 > f2 { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::GreaterEqual => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => Value::Int(if i1 >= i2 { 1 } else { 0 }),
                (Value::Float(f1), Value::Float(f2)) => Value::Int(if f1 >= f2 { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::AmpAmp => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => {
                    let b = (i1 != 0) && (i2 != 0);
                    Value::Int(if b { 1 } else { 0 })
                }
                _ => unreachable!(),
            },
            TokenType::BarBar => match (left, right) {
                (Value::Int(i1), Value::Int(i2)) => {
                    let b = (i1 != 0) || (i2 != 0);
                    Value::Int(if b { 1 } else { 0 })
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn default_value(&mut self, type_info: &Type) -> Value {
        match type_info {
            Type::Int => Value::Int(0),
            Type::Float => Value::Float(0.0),
            _ => todo!(),
        }
    }

    pub fn eval_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::ExprStatement(expr) => {
                let val = self.eval_expr(&expr.expr);
                println!("{:?}", val)
            }
            Statement::LetStatement(let_statement) => self.eval_let_statement(let_statement),
            Statement::BlockStatement(block_statement) => {
                self.scopes.push(vec![]);
                for statement in block_statement.statements.iter() {
                    self.eval_statement(&statement);
                }
                self.scopes.pop().unwrap();
            }
            _ => unreachable!(),
        }
    }

    fn eval_let_statement(&mut self, let_statement: &LetStatement) {
        let initializer = if let Some(expr) = &let_statement.initializer {
            self.eval_expr(expr)
        } else {
            self.default_value(let_statement.type_info.as_ref().unwrap())
        };

        let scope = self.scopes.last_mut().unwrap();
        scope.push(initializer);
    }
}
