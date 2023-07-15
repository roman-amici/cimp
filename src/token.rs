use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    LeftSquare,
    RightSquare,
    Comma,
    Colon,
    Dot,
    Minus,
    Plus,
    Star,
    Slash,
    At,
    Semicolon,
    Equal,
    EqualEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Bang,
    BangEqual,
    Amp,
    AmpAmp,
    Bar,
    BarBar,

    If,
    Else,
    Break,
    Continue,
    Return,
    Let,
    For,
    While,

    Struct,
    Fn,

    Number,
    Identifier,

    EOF, // TODO: strings?
}

#[derive(Clone, Debug)]
pub struct Token {
    pub file_name: Rc<String>,
    pub line: usize,
    pub column: usize,
    pub token_type: TokenType,
    pub literal: Option<String>,
}
