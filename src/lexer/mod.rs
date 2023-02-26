#[cfg(test)]
mod tests;
mod token;

use std::str::Chars;
pub use token::*;

pub struct Lexer<'a> {
    // pub src: &'a str,
    chars: Chars<'a>,
    pub loc: u32,
    len_remaining: usize,
    // pub keywords: Vec<String>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            // src,
            chars: src.chars(),
            loc: 0,
            len_remaining: src.len(),
            // keywords: vec![String::from("return")],
        }
    }

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn update_len_remaining(&mut self) {
        let new_len = self.chars.as_str().len();
        self.loc += (self.len_remaining - new_len) as u32;
        self.len_remaining = new_len;
    }

    fn moved_len(&self) -> u32 {
        (self.len_remaining - self.chars.as_str().len()) as u32
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.next();
        }
    }

    fn is_whitespace(c: char) -> bool {
        matches!(
            c,
            '\u{0009}'   // \t
            | '\u{000A}' // \n
            | '\u{000B}' // vertical tab
            | '\u{000C}' // form feed
            | '\u{000D}' // \r
            | '\u{0020}' // space
            | '\u{0085}'
            // bidi
            | '\u{200E}' // LEFT-TO-RIGHT MARK
            | '\u{200F}' // RIGHT-TO-LEFT MARK
            // unicode
            | '\u{2028}' // LINE SEPARATOR
            | '\u{2029}' // PARAGRAPH SEPARATOR
        )
    }

    pub fn next_token(&mut self) -> (Token, /* preceeded by whitespace */ bool) {
        let mut preceeded_by_whitespace = false;
        loop {
            let first_char = match self.next() {
                Some(c) => c,
                None => {
                    return (
                        Token::new(TokenKind::Eof, self.loc, 0),
                        preceeded_by_whitespace,
                    )
                }
            };
            let kind = match first_char {
                // comments
                '/' => match self.peek() {
                    '/' => {
                        self.next();
                        let kind = if self.peek() == '/' {
                            TokenKind::DocComment
                        } else {
                            TokenKind::LineComment
                        };
                        self.eat_while(|c| c != '\n');
                        kind
                    }
                    // '*' => {} // TODO: block comments
                    _ => TokenKind::BinOp(BinOpToken::Slash),
                },

                // whitespace
                c if Self::is_whitespace(c) => {
                    preceeded_by_whitespace = true;
                    self.update_len_remaining();
                    continue;
                }

                ',' => TokenKind::Comma,
                ':' => TokenKind::Colon,
                '(' => TokenKind::OpenDelimiter(Delimiter::Parenthesis),
                ')' => TokenKind::CloseDelimiter(Delimiter::Parenthesis),
                '{' => TokenKind::OpenDelimiter(Delimiter::Bracket),
                '}' => TokenKind::CloseDelimiter(Delimiter::Bracket),

                '-' => match self.peek() {
                    '>' => {
                        self.next();
                        TokenKind::Arrow
                    }
                    _ => TokenKind::BinOp(BinOpToken::Minus),
                },
                '+' => TokenKind::BinOp(BinOpToken::Plus),
                '*' => TokenKind::BinOp(BinOpToken::Star),
                '%' => TokenKind::BinOp(BinOpToken::Percent),
                '^' => TokenKind::BinOp(BinOpToken::Caret),
                '&' => match self.peek() {
                    '|' => {
                        self.next();
                        TokenKind::AndAnd
                    }
                    _ => TokenKind::BinOp(BinOpToken::And),
                },
                '|' => match self.peek() {
                    '|' => {
                        self.next();
                        TokenKind::PipePipe
                    }
                    _ => TokenKind::BinOp(BinOpToken::Pipe),
                },

                '<' => match self.peek() {
                    '<' => {
                        self.next();
                        TokenKind::BinOp(BinOpToken::ShiftLeft)
                    }
                    '=' => {
                        self.next();
                        TokenKind::LThanEqual
                    }
                    _ => TokenKind::LThan,
                },
                '>' => match self.peek() {
                    '>' => {
                        self.next();
                        TokenKind::BinOp(BinOpToken::ShiftRight)
                    }
                    '=' => {
                        self.next();
                        TokenKind::GThanEqual
                    }
                    _ => TokenKind::GThan,
                },

                c if c == '_' || unicode_xid::UnicodeXID::is_xid_start(c) => {
                    self.eat_while(unicode_xid::UnicodeXID::is_xid_continue);
                    TokenKind::Identifier
                }

                c if c.is_numeric() => TokenKind::Literal,

                _ => panic!("unknown token"),
            };
            let token_len = self.moved_len();
            let loc = self.loc;
            self.update_len_remaining();
            return (
                Token::new(kind, loc, token_len as u16),
                preceeded_by_whitespace,
            );
        }
    }
}

pub fn tokenize<'a>(src: &'a str) -> Vec<Token> {
    let mut lexer = Lexer::new(src);
    let mut tokens: Vec<Token> = vec![];
    loop {
        let token = lexer.next_token().0;
        let done = TokenKind::Eof == token.kind;
        tokens.push(token);
        if done {
            break;
        }
    }
    tokens

    // let tokens = vec![
    //     Token::new(TokenKind::Keyword(String::from("export")), 0),
    //     Token::new(TokenKind::Keyword(String::from("func")), 0),
    //     Token::new(TokenKind::Identifier(String::from("add")), 0),
    //     Token::new(TokenKind::LParen, 0),
    //     Token::new(TokenKind::Identifier(String::from("a")), 0),
    //     Token::new(TokenKind::Colon, 0),
    //     Token::new(TokenKind::Identifier(String::from("i32")), 0),
    //     Token::new(TokenKind::Comma, 0),
    //     Token::new(TokenKind::Identifier(String::from("b")), 0),
    //     Token::new(TokenKind::Colon, 0),
    //     Token::new(TokenKind::Identifier(String::from("i32")), 0),
    //     Token::new(TokenKind::RParen, 0),
    //     Token::new(TokenKind::Arrow, 0),
    //     Token::new(TokenKind::Identifier(String::from("i32")), 0),
    //     Token::new(TokenKind::LBracket, 0),
    //     Token::new(TokenKind::Keyword(String::from("return")), 0),
    //     Token::new(TokenKind::Identifier(String::from("a")), 0),
    //     Token::new(TokenKind::Operator(OperatorKind::Plus), 0),
    //     Token::new(TokenKind::Identifier(String::from("b")), 0),
    //     Token::new(TokenKind::RBracket, 0),
    // ];

    // tokens
}
