use crate::{token::Token, type_checker::Type};

type ExprPtr = Box<Expr>;

pub enum Expr {
    Number(Number),
    Identifier(Identifier),
    Unary(Unary),
    Binary(Binary),
    Assign(Assign),
    Call(Call),
    ArrayDeref(ArrayDeref),
}

pub struct Number {
    pub token: Token,

    pub type_info: Option<Type>,
}

pub struct Resolution {
    pub scope_index: usize,
    pub slot_index: usize,
}

pub struct Identifier {
    pub token: Token,

    pub resolution: Option<Resolution>,
    pub type_info: Option<Type>,
}

pub struct Unary {
    pub token: Token,
    pub op: Token,
    pub expr: ExprPtr,
}

pub struct Binary {
    pub token: Token,
    pub op: Token,
    pub left: ExprPtr,
    pub right: ExprPtr,
}

pub struct Assign {
    pub token: Token,
    pub l_value: ExprPtr,
    pub expr: ExprPtr,
}

pub struct Call {
    pub token: Token,
    pub call_site: ExprPtr,
    pub args: Vec<Expr>,
}

pub struct ArrayDeref {
    pub token: Token,
    pub call_site: ExprPtr,
    pub index: ExprPtr,
}

pub struct StructMember {
    pub token: Token,
    pub call_site: ExprPtr,
    pub name: Token,
}

pub type StatementPtr = Box<Statement>;

pub enum Statement {
    ExprStatement(ExprStatement),
    BlockStatement(BlockStatement),
    IfStatement(IfStatement),
    ForStatement(ForStatement),
    WhileStatement(WhileStatement),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ContinueStatement(ContinueStatement),
    BreakStatement(BreakStatement),
}

pub struct ExprStatement {
    pub token: Token,
    pub expr: ExprPtr,
}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

pub struct IfStatement {
    pub token: Token,
    pub condition: ExprPtr,
    pub block: StatementPtr,
    pub else_block: Option<StatementPtr>,
}

pub struct ForStatement {
    pub token: Token,
    pub initializer: ExprPtr,
    pub condition: ExprPtr,
    pub update: ExprPtr,
    pub block: StatementPtr,
}

pub struct WhileStatement {
    pub token: Token,
    pub condition: ExprPtr,
    pub block: StatementPtr,
}

pub struct LetStatement {
    pub token: Token,
    pub variable_name: Token,
    pub type_name: Option<Token>,
    pub initializer: Option<ExprPtr>,

    pub type_info: Option<Box<Type>>,
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<ExprPtr>,
}

pub struct ContinueStatement {
    pub token: Token,
}

pub struct BreakStatement {
    pub token: Token,
}

pub enum Declaration {
    GlobalDeclaration(Statement),
    FunctionDeclaration(FunctionDeclaration),
}

pub struct FunctionDeclaration {
    pub token: Token,
    pub function_name: Token,
    pub args: Vec<(Token, Token)>,
    pub return_type: Option<Token>,
    pub block: StatementPtr,
}
