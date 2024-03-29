#[cfg(test)]
mod tests;
mod token;

use std::str::Chars;
pub use token::*;

pub struct Lexer<'a> {
    chars: Chars<'a>,
    pub loc: u32,
    len_remaining: usize,
    // pub keywords: Vec<String>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars(),
            loc: 0,
            len_remaining: src.len(),
            // keywords: vec![String::from("return")],
        }
    }

    // move to next character
    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    // return next character without consuming
    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    // check if at end of file
    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    // update remaining char count
    fn update_len_remaining(&mut self) {
        let new_len = self.chars.as_str().len();
        self.loc += (self.len_remaining - new_len) as u32;
        self.len_remaining = new_len;
    }

    // return number of characters since last update_len_remaining
    fn moved_len(&self) -> u32 {
        (self.len_remaining - self.chars.as_str().len()) as u32
    }

    // eat characters while predicate is true
    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.next();
        }
    }

    // check if character is a whitespace
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

    // lex the next token
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
                    '*' => {
                        self.next();
                        while let Some(c) = self.next() {
                            match c {
                                '*' => {
                                    if self.peek() == '/' {
                                        self.next();
                                        break;
                                    }
                                }
                                _ => continue,
                            }
                        }
                        TokenKind::BlockComment
                    }
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
                '?' => TokenKind::Question,
                '(' => TokenKind::OpenDelimiter(Delimiter::Parenthesis),
                ')' => TokenKind::CloseDelimiter(Delimiter::Parenthesis),
                '{' => TokenKind::OpenDelimiter(Delimiter::Brace),
                '}' => TokenKind::CloseDelimiter(Delimiter::Brace),
                '[' => TokenKind::OpenDelimiter(Delimiter::Bracket),
                ']' => TokenKind::CloseDelimiter(Delimiter::Bracket),

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
                    '&' => {
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
                '=' => match self.peek() {
                    '=' => {
                        self.next();
                        TokenKind::EqualEqual
                    }
                    _ => TokenKind::Equal,
                },
                '!' => match self.peek() {
                    '=' => {
                        self.next();
                        TokenKind::NotEqual
                    }
                    _ => TokenKind::Exclamation,
                },

                c if c == '"' || c == '\'' => {
                    let kind = match c {
                        '"' => LiteralKind::String,
                        '\'' => LiteralKind::Char,
                        _ => panic!(),
                    };
                    self.eat_while(|c2| c2 != '\n' && c2 != c);
                    if self.next().unwrap_or(' ') != c {
                        panic!("expected closing `{c}`")
                    }
                    TokenKind::Literal(kind)
                }

                c if c == '_' || unicode_xid::UnicodeXID::is_xid_start(c) => {
                    self.eat_while(unicode_xid::UnicodeXID::is_xid_continue);
                    TokenKind::Identifier
                }

                c if c.is_numeric() => {
                    let mut kind = LiteralKind::Integer;
                    loop {
                        match self.peek() {
                            c if c.is_numeric() => (),
                            '.' => kind = LiteralKind::Float,
                            _ => break,
                        };
                        self.next();
                    }
                    TokenKind::Literal(kind)
                }

                _ => panic!("unknown token {}", first_char),
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

pub fn tokenize(src: &str) -> Vec<Token> {
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
}
