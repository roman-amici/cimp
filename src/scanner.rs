use std::{collections::HashMap, rc::Rc};

use crate::token::{Token, TokenType};

struct Scanner {
    file_name: Rc<String>,
    line: usize,
    column: usize,
    chars: Vec<char>,
    tokens: Vec<Token>,
    index: usize,
    keywords: HashMap<String, TokenType>,
}

pub fn scan(input: &str, file_name: &str) -> Vec<Token> {
    let mut scanner = Scanner {
        file_name: Rc::new(String::from(file_name)),
        line: 0,
        column: 0,
        index: 0,
        chars: input.chars().collect(),
        tokens: vec![],
        keywords: Scanner::make_keywords(),
    };

    scanner.scan();
    scanner.tokens
}

impl Scanner {
    // TODO: lazy static
    fn make_keywords() -> HashMap<String, TokenType> {
        let mut h = HashMap::new();
        h.insert(String::from("if"), TokenType::If);
        h.insert(String::from("else"), TokenType::Else);
        h.insert(String::from("break"), TokenType::Break);
        h.insert(String::from("continue"), TokenType::Continue);
        h.insert(String::from("return"), TokenType::Return);
        h.insert(String::from("let"), TokenType::Let);
        h.insert(String::from("for"), TokenType::For);
        h.insert(String::from("while"), TokenType::While);
        h.insert(String::from("struct"), TokenType::Struct);
        h.insert(String::from("fn"), TokenType::Fn);

        h
    }

    fn scan(&mut self) {
        loop {
            let c = self.next_char();
            let t = match c {
                '\0' => return,
                '(' => Some(self.make_token(TokenType::LeftParen)),
                ')' => Some(self.make_token(TokenType::RightParen)),
                '{' => Some(self.make_token(TokenType::LeftCurly)),
                '}' => Some(self.make_token(TokenType::RightCurly)),
                '[' => Some(self.make_token(TokenType::LeftSquare)),
                ']' => Some(self.make_token(TokenType::RightSquare)),
                ',' => Some(self.make_token(TokenType::Comma)),
                ':' => Some(self.make_token(TokenType::Colon)),
                '.' => Some(self.make_token(TokenType::Dot)), // TODO: leading decimal
                ';' => Some(self.make_token(TokenType::Semicolon)),
                '@' => Some(self.make_token(TokenType::At)),
                '*' => Some(self.make_token(TokenType::Star)),
                '+' => Some(self.make_token(TokenType::Plus)),
                '-' => Some(self.make_token(TokenType::Minus)),
                '/' => {
                    if self.peek() == '/' {
                        self.next_char();
                        self.consume_comment()
                    } else {
                        Some(self.make_token(TokenType::Slash))
                    }
                }
                '=' => {
                    if self.peek() == '=' {
                        self.next_char();
                        Some(self.make_token(TokenType::EqualEqual))
                    } else {
                        Some(self.make_token(TokenType::Equal))
                    }
                }
                '>' => {
                    if self.peek() == '=' {
                        self.next_char();
                        Some(self.make_token(TokenType::GreaterEqual))
                    } else {
                        Some(self.make_token(TokenType::Greater))
                    }
                }
                '<' => {
                    if self.peek() == '=' {
                        self.next_char();
                        Some(self.make_token(TokenType::LessEqual))
                    } else {
                        Some(self.make_token(TokenType::Less))
                    }
                }
                '!' => {
                    if self.peek() == '=' {
                        self.next_char();
                        Some(self.make_token(TokenType::Bang))
                    } else {
                        Some(self.make_token(TokenType::BangEqual))
                    }
                }
                '&' => {
                    if self.peek() == '&' {
                        self.next_char();
                        Some(self.make_token(TokenType::AmpAmp))
                    } else {
                        Some(self.make_token(TokenType::Amp))
                    }
                }
                '|' => {
                    if self.peek() == '|' {
                        self.next_char();
                        Some(self.make_token(TokenType::BarBar))
                    } else {
                        Some(self.make_token(TokenType::Bar))
                    }
                }
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                    None
                }
                ' ' => None,
                '\t' => None,
                _ => {
                    if c.is_alphabetic() {
                        self.consume_identifier(c)
                    } else if c.is_ascii_digit() {
                        self.consume_number(c)
                    } else {
                        None
                    }
                }
            };

            if let Some(t) = t {
                self.tokens.push(t);
            }
        }
    }

    fn consume_identifier(&mut self, start_char: char) -> Option<Token> {
        let start_column = self.column - 1;

        let mut chars = vec![start_char];

        while self.peek().is_alphanumeric() {
            chars.push(self.next_char());
        }

        let s: String = chars.iter().collect();

        let token_type = if let Some(token_type) = self.keywords.get(&s) {
            *token_type
        } else {
            TokenType::Identifier
        };

        Some(Token {
            file_name: self.file_name.clone(),
            column: start_column,
            line: self.line,
            token_type,
            literal: Some(s),
        })
    }

    fn consume_number(&mut self, start_char: char) -> Option<Token> {
        let start_column = self.column - 1;
        let mut chars = vec![start_char];

        while self.peek().is_ascii_digit() {
            chars.push(self.next_char());
        }

        if self.peek() == '.' {
            chars.push(self.next_char());
            while self.peek().is_ascii_digit() {
                chars.push(self.next_char());
            }
        }

        let s: String = chars.iter().collect();

        Some(Token {
            file_name: self.file_name.clone(),
            column: start_column,
            line: self.line,
            token_type: TokenType::Number,
            literal: Some(s),
        })
    }

    fn consume_comment(&mut self) -> Option<Token> {
        while self.peek() != '\0' && self.peek() != '\n' {
            self.next_char();
        }
        None
    }

    fn next_char(&mut self) -> char {
        if self.index < self.chars.len() {
            self.column += 1;
            self.index += 1;
            self.chars[self.index - 1]
        } else {
            '\0'
        }
    }

    fn peek(&self) -> char {
        if self.index < self.chars.len() {
            self.chars[self.index]
        } else {
            '\0'
        }
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        Token {
            file_name: self.file_name.clone(),
            token_type,
            column: self.column - 1,
            line: self.line,
            literal: None,
        }
    }
}
