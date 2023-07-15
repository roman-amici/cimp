use std::rc::Rc;

use crate::token::{Token, TokenType};
use crate::{ast::*, token};

pub struct Parser {
    tokens: Vec<Token>,

    index: usize,
}

#[derive(Debug)]
pub struct ParserError {
    token: Token,
    error_message: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, index: 0 }
    }

    fn peek_type(&self) -> TokenType {
        if self.index < self.tokens.len() {
            self.tokens[self.index].token_type
        } else {
            TokenType::EOF
        }
    }

    fn peek(&self) -> Token {
        if self.index < self.tokens.len() {
            self.tokens[self.index].clone()
        } else {
            self.eof_token()
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.peek_type() == token_type
    }

    fn eof_token(&self) -> Token {
        if let Some(t) = self.tokens.last() {
            let mut t = t.clone();
            t.token_type = TokenType::EOF;
            t
        } else {
            Token {
                column: 0,
                file_name: Rc::new(String::from("No file")),
                line: 0,
                token_type: TokenType::EOF,
                literal: None,
            }
        }
    }

    fn next(&mut self) -> Token {
        if self.index < self.tokens.len() {
            self.index += 1;
            self.tokens[self.index - 1].clone()
        } else {
            self.eof_token()
        }
    }

    fn consume(&mut self, token_type: TokenType) -> Result<Token, ParserError> {
        let t = self.next();
        if t.token_type == token_type {
            Ok(t)
        } else {
            Err(ParserError {
                error_message: format!("Expected {:?} but got {:?}.", token_type, t.token_type),
                token: t,
            })
        }
    }

    fn try_consume(&mut self, token_type: TokenType) -> Option<Token> {
        if self.peek_type() == token_type {
            Some(self.consume(token_type).unwrap())
        } else {
            None
        }
    }

    fn try_consume_any(&mut self, token_types: &[TokenType]) -> Option<Token> {
        for token_type in token_types {
            let t = self.try_consume(*token_type);
            if t.is_some() {
                return t;
            }
        }

        None
    }

    fn consume_any(&mut self, token_types: &[TokenType]) -> Result<Token, ParserError> {
        let t = self.next();
        for token_type in token_types {
            if *token_type == t.token_type {
                return Ok(t);
            }
        }

        Err(ParserError {
            error_message: format!(
                "Expected one of {:?} but got {:?}.",
                token_types, t.token_type
            ),
            token: t,
        })
    }

    fn parse_primary_expression(&mut self) -> Result<Expr, ParserError> {
        match self.peek_type() {
            TokenType::Identifier => Ok(Expr::Identifier(Identifier {
                token: self.consume(TokenType::Identifier)?,
            })),
            TokenType::Number => Ok(Expr::Number(Number {
                token: self.consume(TokenType::Number)?,
            })),
            TokenType::LeftParen => {
                let expr = self.parse_expression()?;
                self.consume(TokenType::RightParen)?;
                Ok(expr)
            }
            _ => {
                let token = self.next();
                Err(ParserError {
                    error_message: format!("Expected token {:?}", token.token_type),
                    token,
                })
            }
        }
    }

    fn parse_call_expression(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_primary_expression()?;

        // Match one call after another at the same precedence
        while let Some(token) = self.try_consume_any(&[TokenType::LeftParen, TokenType::LeftSquare])
        {
            match token.token_type {
                TokenType::LeftParen => {
                    let mut args = vec![];
                    while !self.check(TokenType::EOF) && !self.check(TokenType::RightParen) {
                        let arg = self.parse_expression()?;
                        args.push(arg);

                        if !self.try_consume(TokenType::Comma).is_some() {
                            break;
                        }
                    }
                    self.consume(TokenType::RightParen)?;

                    return Ok(Expr::Call(Call {
                        token: token,
                        call_site: Box::new(expr),
                        args: args,
                    }));
                }
                TokenType::LeftSquare => {
                    let inner = self.parse_expression()?;
                    self.consume(TokenType::RightSquare)?;
                    return Ok(Expr::ArrayDeref(ArrayDeref {
                        token: token,
                        call_site: Box::new(expr),
                        index: Box::new(inner),
                    }));
                }
                _ => unreachable!(),
            }
        }

        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expr, ParserError> {
        if let Some(operator) = self.try_consume_any(&[
            TokenType::Amp,
            TokenType::Minus,
            TokenType::Bang,
            TokenType::At,
        ]) {
            let expr = self.parse_call_expression()?;
            Ok(Expr::Unary(Unary {
                token: operator.clone(),
                op: operator,
                expr: Box::new(expr),
            }))
        } else {
            self.parse_call_expression()
        }
    }

    fn parse_multiplicatoin_division(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_unary_expression()?;

        while let Some(operator) = self.try_consume_any(&[TokenType::Star, TokenType::Slash]) {
            let right = self.parse_unary_expression()?;
            left = Expr::Binary(Binary {
                token: operator.clone(),
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        Ok(left)
    }

    fn parse_addition_subtraction_expression(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_multiplicatoin_division()?;

        while let Some(operator) = self.try_consume_any(&[TokenType::Plus, TokenType::Minus]) {
            let right = self.parse_multiplicatoin_division()?;
            left = Expr::Binary(Binary {
                token: operator.clone(),
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        Ok(left)
    }

    fn parse_binary_comparison_expression(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_addition_subtraction_expression()?;

        while let Some(operator) = self.try_consume_any(&[
            TokenType::Less,
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::LessEqual,
            TokenType::EqualEqual,
        ]) {
            let right = self.parse_addition_subtraction_expression()?;
            left = Expr::Binary(Binary {
                token: operator.clone(),
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        Ok(left)
    }

    fn parse_binary_and_expression(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_binary_comparison_expression()?;

        while let Some(operator) = self.try_consume(TokenType::AmpAmp) {
            let right = self.parse_binary_comparison_expression()?;
            left = Expr::Binary(Binary {
                token: operator.clone(),
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        Ok(left)
    }

    fn parse_binary_or_expression(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_binary_and_expression()?;

        while let Some(operator) = self.try_consume(TokenType::BarBar) {
            let right = self.parse_binary_and_expression()?;
            left = Expr::Binary(Binary {
                token: operator.clone(),
                op: operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        Ok(left)
    }

    fn parse_assign_expression(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_binary_or_expression()?;

        if let Some(equal) = self.try_consume(TokenType::Equal) {
            let rhs = self.parse_expression()?;
            return Ok(Expr::Assign(Assign {
                token: equal,
                l_value: Box::new(expr),
                expr: Box::new(rhs),
            }));
        }

        Ok(expr)
    }

    pub fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_assign_expression()
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.peek_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::LeftCurly => self.parse_block_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::For => self.parse_for_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Break => self.parse_break_statement(),
            TokenType::Continue => self.parse_continue_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.consume(TokenType::If)?;

        self.consume(TokenType::LeftParen)?;

        let condition = self.parse_expression()?;
        self.consume(TokenType::RightParen)?;

        let block = self.parse_block_statement()?;

        let else_block = if self.try_consume(TokenType::Else).is_some() {
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };

        Ok(Statement::IfStatement(IfStatement {
            token: start,
            condition: Box::new(condition),
            block: Box::new(block),
            else_block,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.consume(TokenType::While)?;

        self.consume(TokenType::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::RightParen)?;
        let block = self.parse_block_statement()?;

        Ok(Statement::WhileStatement(WhileStatement {
            token: start,
            condition: Box::new(condition),
            block: Box::new(block),
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.consume(TokenType::For)?;

        self.consume(TokenType::LeftParen)?;
        let initializer = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::Semicolon)?;
        let update = self.parse_expression()?;
        self.consume(TokenType::RightParen)?;
        let block = self.parse_block_statement()?;

        Ok(Statement::ForStatement(ForStatement {
            token: start,
            initializer: Box::new(initializer),
            condition: Box::new(condition),
            update: Box::new(update),
            block: Box::new(block),
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.consume(TokenType::Return)?;

        let return_value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };
        self.consume(TokenType::Semicolon)?;

        Ok(Statement::ReturnStatement(ReturnStatement {
            token: start,
            return_value,
        }))
    }

    fn parse_break_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.consume(TokenType::Break)?;

        Ok(Statement::BreakStatement(BreakStatement { token }))
    }

    fn parse_continue_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.consume(TokenType::Continue)?;

        Ok(Statement::BreakStatement(BreakStatement { token }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression()?;
        let token = self.consume(TokenType::Semicolon)?;

        Ok(Statement::ExprStatement(ExprStatement {
            token,
            expr: Box::new(expr),
        }))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.consume(TokenType::Let)?;
        let variable_name: Token = self.consume(TokenType::Identifier)?;

        let type_name = if self.try_consume(TokenType::Colon).is_some() {
            Some(self.consume(TokenType::Identifier)?)
        } else {
            None
        };

        let initializer = if self.try_consume(TokenType::Equal).is_some() {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        Ok(Statement::LetStatement(LetStatement {
            token,
            variable_name,
            type_name,
            initializer,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.consume(TokenType::LeftCurly)?;

        let mut statements = vec![];
        while !self.check(TokenType::EOF) && !self.check(TokenType::RightCurly) {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        self.consume(TokenType::RightCurly)?;

        Ok(Statement::BlockStatement(BlockStatement {
            token: start,
            statements: statements,
        }))
    }

    fn parse_declarations(&mut self) -> Result<Vec<Declaration>, ParserError> {
        let mut declaraitons = vec![];

        while !self.check(TokenType::EOF) {
            let declaration = match self.peek_type() {
                TokenType::Fn => self.parse_function_declaration()?,
                TokenType::Let => Declaration::GlobalDeclaration(self.parse_let_statement()?),
                _ => {
                    let token = self.peek();
                    return Err(ParserError {
                        error_message: format!("Unexpected token {:?}", token.token_type),
                        token,
                    });
                }
            };

            declaraitons.push(declaration);
        }

        Ok(declaraitons)
    }

    fn parse_function_declaration(&mut self) -> Result<Declaration, ParserError> {
        let start_token = self.consume(TokenType::Fn)?;

        let function_name = self.consume(TokenType::Identifier)?;
        self.consume(TokenType::LeftParen)?;

        let mut args = vec![];
        while !self.check(TokenType::RightParen) && !self.check(TokenType::EOF) {
            let variable = self.consume(TokenType::Identifier)?;
            self.consume(TokenType::Colon)?;
            let type_name = self.consume(TokenType::Identifier)?;

            args.push((variable, type_name));

            if self.try_consume(TokenType::Comma).is_none() {
                break;
            }
        }
        self.consume(TokenType::RightParen)?;

        let return_type = if self.try_consume(TokenType::Colon).is_some() {
            Some(self.consume(TokenType::Identifier)?)
        } else {
            None
        };

        let block = Box::new(self.parse_block_statement()?);

        Ok(Declaration::FunctionDeclaration(FunctionDeclaration {
            token: start_token,
            function_name,
            args,
            return_type,
            block,
        }))
    }
}
