pub mod symbol;

pub use symbol::*;

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
    // span in source to type that hasn't been typechecked
    Unchecked(Span),

    Unit,
    Basic(BasicType),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Variable {
    pub name: String,
    pub r#type: Option<Type>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub loc: u32,
    pub len: u16,
}

impl Span {
    // beginning of self to end of end
    pub fn to(&self, end: &Span) -> Self {
        Self {
            loc: self.loc,
            len: ((end.loc + end.len as u32) - self.loc) as u16,
        }
    }

    // beginning of self to beginning of end
    pub fn until(&self, end: &Span) -> Self {
        Self {
            loc: self.loc,
            len: (end.loc - self.loc) as u16,
        }
    }

    // end of self to beginning of end
    pub fn between(&self, end: &Span) -> Self {
        Self {
            loc: self.loc + self.len as u32,
            len: (end.loc - self.loc) as u16,
        }
    }

    pub fn in_src<'a>(&self, src: &'a str) -> &'a str {
        let loc = self.loc as usize;
        &src[loc..loc + self.len as usize]
    }
}

#[derive(Debug, PartialEq)]
pub enum StatementKind {
    // let lhs = rhs
    Let(Box<Local>),
    // return expr?
    Return(Option<Box<Expression>>),
    Expr(Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

impl Statement {
    pub fn declaration(ident: String, init: Expression, span: Span) -> Self {
        Self {
            kind: StatementKind::Let(Box::new(Local {
                mutable: false,
                ident,
                r#type: None,
                init,
            })),
            span,
        }
    }

    pub fn ret(expr: Option<Expression>, span: Span) -> Self {
        Self {
            kind: StatementKind::Return(expr.map(Box::new)),
            span,
        }
    }

    pub fn expr(expr: Expression) -> Self {
        let span = expr.span;
        Self {
            kind: StatementKind::Expr(Box::new(expr)),
            span,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Local {
    pub mutable: bool,
    pub ident: String,
    pub r#type: Option<Type>,
    pub init: Expression,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    // assign `x = a`
    // Assign(Box<Expression>, Box<Expression>),
    // assign with an operator e.g. `x += a`
    // AssignOp(BinOp, Box<Expression>, Box<Expression>),
    // function call with params
    // Call(Box<Expression>, Vec<Box<Expression>>),
    // if
    If(Box<If>),
    // (expr)
    Group(Box<Expression>),
    // binary op `x + 3`, etc.
    BinOp(Box<BinaryOperation>),
    // unary op `-x`, `!x`, etc.
    UnaryOp(Box<UnaryOperation>),
    // block
    Block(Block),
    // literal
    Literal(LiteralKind),
    // variable
    Variable(Variable),
    // TODO:
    // object/array
    // continue/break
    // for/for n/for in/loop
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub fn binop(left: Expression, right: Expression, operator: BinaryOperator) -> Self {
        let span = left.span.to(&right.span);
        Self {
            kind: ExpressionKind::BinOp(Box::new(BinaryOperation {
                left,
                right,
                operator,
                r#type: None,
            })),
            span,
        }
    }

    pub fn uop(expr: Expression, operator: UnaryOperator) -> Self {
        let span = Span {
            loc: expr.span.loc - 1,
            len: expr.span.len + 1,
        };
        Self {
            kind: ExpressionKind::UnaryOp(Box::new(UnaryOperation {
                expr,
                operator,
                r#type: None,
            })),
            span,
        }
    }

    pub fn var(name: String, span: Span) -> Self {
        Self {
            kind: ExpressionKind::Variable(Variable { name, r#type: None }),
            span,
        }
    }

    pub fn literal(kind: LiteralKind, span: Span) -> Self {
        Self {
            kind: ExpressionKind::Literal(kind),
            span,
        }
    }

    pub fn group(expr: Expression, span: Span) -> Self {
        Self {
            kind: ExpressionKind::Group(Box::new(expr)),
            span,
        }
    }

    pub fn if_expr(
        cond: Expression,
        then_branch: Block,
        else_branch: Option<Expression>,
        span: Span,
    ) -> Self {
        Self {
            kind: ExpressionKind::If(Box::new(If {
                cond,
                then_branch,
                else_branch,
                span,
            })),
            span,
        }
    }

    pub fn block(block: Block) -> Self {
        let span = block.span;
        Self {
            kind: ExpressionKind::Block(block),
            span,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub r#type: Option<Type>,
    pub span: Span,
    // pub mutable: boolean?
}

#[derive(Debug, PartialEq, Eq)]
pub enum Export {
    None,
    Implicit,         // export
    Explicit(String), // export("whatever")
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionAttributes {
    pub export: Export,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub attrs: FunctionAttributes,
    pub name: String, // todo: don't use String?
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub block: Option<Block>,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    EqualEqual,
    NotEqual,
    And,
    Or,
    Caret,
    AndAnd,
    OrOr,
    ShiftLeft,
    ShiftRight,
}

impl BinaryOperator {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::OrOr => 1,
            Self::AndAnd => 2,
            Self::Or => 3,
            Self::Caret => 4,
            Self::And => 5,
            Self::EqualEqual | Self::NotEqual => 6,
            Self::Less | Self::LessOrEqual | Self::Greater | Self::GreaterOrEqual => 7,
            Self::ShiftLeft | Self::ShiftRight => 8,
            Self::Add | Self::Sub => 9,
            Self::Mul | Self::Div | Self::Mod => 10,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BinaryOperation {
    pub left: Expression,
    pub right: Expression,
    pub operator: BinaryOperator,
    pub r#type: Option<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct UnaryOperation {
    pub expr: Expression,
    pub operator: UnaryOperator,
    pub r#type: Option<Type>,
}

#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub cond: Expression,
    pub then_branch: Block,
    pub else_branch: Option<Expression>, // todo: prob make this only if or block
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ItemKind {
    Function(Box<Function>),
}

#[derive(Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub items: Vec<Item>,
}
