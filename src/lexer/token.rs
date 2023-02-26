use crate::ast::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOpToken {
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Caret,      // ^
    And,        // &
    Pipe,       // |
    ShiftLeft,  // <<
    ShiftRight, // >>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Delimiter {
    Parenthesis, // ()
    Brace,       // []
    Bracket,     // {}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Equal,       // =
    EqualEqual,  // ==
    NotEqual,    // !=
    LThan,       // <
    LThanEqual,  // <=
    GThan,       // >
    GThanEqual,  // >=
    AndAnd,      // &&
    PipePipe,    // ||
    Exclamation, // !
    BinOp(BinOpToken),
    BinOpEqual(BinOpToken),
    Comma,    // ,
    Arrow,    // ->
    Question, // ?
    Colon,    // :
    OpenDelimiter(Delimiter),
    CloseDelimiter(Delimiter),
    LineComment,  // //
    BlockComment, // /* */
    DocComment,   // ///
    Literal,
    Identifier,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn is_op(&self) -> bool {
        use TokenKind::*;
        match self.kind {
            Equal | LThan | LThanEqual | EqualEqual | NotEqual | GThanEqual | GThan | AndAnd
            | PipePipe | Exclamation | BinOp(_) | BinOpEqual(_) | Comma | Colon => true,
            _ => false,
        }
    }

    pub fn is_keyword(&self, src: &str, keyword: &str) -> bool {
        self.kind == TokenKind::Identifier && self.as_str(src) == keyword
    }

    pub fn as_str<'a>(&self, src: &'a str) -> &'a str {
        let loc = self.span.loc as usize;
        &src[loc..loc + self.span.len as usize]
    }

    pub fn identifier<'a>(&self, src: &'a str) -> Option<&'a str> {
        if self.kind == TokenKind::Identifier {
            Some(self.as_str(src))
        } else {
            None
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, loc: u32, len: u16) -> Self {
        Self {
            kind,
            span: Span { loc, len },
        }
    }
}
