pub mod symbols;

pub use symbols::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BasicType {
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Unit,
    Basic(BasicType),
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub r#type: Option<Type>,
}

#[derive(Debug)]
pub enum LiteralKind {
    Bool(bool),
    Char(char),
    String(String),
    Integer(i32),
    Float(f32),
}

// #[derive(Debug)]
// pub struct Literal {
//     pub kind: LiteralKind,
// }

#[derive(Debug)]
pub enum Expression {
    Return(Option<Box<Expression>>),
    // assign `x = a`
    Assign(Box<Expression>, Box<Expression>),
    // object/array
    // continue/break
    // AssignOp(BinOp, Box<Expression>, Box<Expression>),
    // Call(Box<Expression>, Vec<Box<Expression>>),
    // for/for n/for in
    // if
    // binary op `x + 3`, etc.
    BinOp(Box<BinaryOperation>),
    // unary op `-x`, `!x`, etc.
    // block
    Block(Block),
    // literal
    Literal(LiteralKind),
    // variable
    Variable(Variable),
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub type_str: String,
    pub r#type: Option<Type>,
    // pub mutable: boolean?
}

#[derive(Debug)]
pub enum Export {
    None,
    Implicit,         // export
    Explicit(String), // export("whatever")
}

#[derive(Debug)]
pub struct FunctionAttributes {
    pub export: Export,
}

#[derive(Debug)]
pub struct Function {
    pub attrs: FunctionAttributes,
    pub name: String, // todo: don't use String?
    pub params: Vec<Param>,
    pub return_type_str: Option<String>,
    pub return_type: Option<Type>,
    pub block: Option<Block>,
}

#[derive(Debug)]
pub struct Block {
    pub expressions: Vec<Expression>,
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

impl BinaryOperator {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Or => 1,
            Self::And => 2,
            Self::Equal | Self::NotEqual => 3,
            Self::Less | Self::LessOrEqual | Self::Greater | Self::GreaterOrEqual => 4,
            Self::Add | Self::Sub => 5,
            Self::Mul | Self::Div => 6,
        }
    }
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub left: Expression,
    pub right: Expression,
    pub operator: BinaryOperator,
    pub r#type: Option<Type>,
}

#[derive(Debug)]
pub struct Assignment {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug)]
pub enum ItemKind {
    Function(Box<Function>),
}

#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug)]
pub struct Ast {
    pub items: Vec<Box<Item>>,
}
