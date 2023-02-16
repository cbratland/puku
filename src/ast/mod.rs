// todo: honestly not sure how to do types yet exactly
// im gonna need both built in and user-defined
#[derive(Debug, Clone, Copy)]
pub enum Type {
    I32,
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
}

#[derive(Debug)]
pub enum LiteralKind {
    Bool,
    Char,
    String,
    Integer,
    Float,
}

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
    // variable
    Variable(Variable),
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub r#type: Type,
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

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub left: Expression,
    pub right: Expression,
    pub operator: BinaryOperator,
    pub r#type: Type,
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
