#[allow(non_upper_case_globals)]
pub mod keyword;
mod tests;

use crate::lexer::BinOpToken;

use super::ast::*;
// use std::collections::HashMap;
use super::lexer::{Delimiter, Token, TokenKind};

type Result<T> = std::result::Result<T, ()>;

pub struct Parser<'a> {
    pub src: &'a str,
    pub token: Token, // current token
    pub prev_token: Token,
    pub stream: TokenStream,
}

#[derive(Clone)]
pub struct TokenStream {
    tokens: Vec<Token>,
    loc: usize,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, loc: 0 }
    }

    pub fn next(&mut self) -> Token {
        let next = self.tokens.get(self.loc);
        self.loc += 1;
        match next {
            Some(token) => token.clone(),
            None => Token::new(TokenKind::Eof, 0, 0),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, tokens: Vec<Token>) -> Self {
        let mut stream = TokenStream::new(tokens);
        Self {
            src,
            token: stream.next(),
            prev_token: Token::new(TokenKind::Eof, 0, 0),
            stream,
        }
    }

    pub fn next(&mut self) {
        let next = self.stream.next();
        self.prev_token = std::mem::replace(&mut self.token, next);
    }

    pub fn check(&self, token: &TokenKind) -> bool {
        self.token.kind == *token
    }

    pub fn eat(&mut self, token: &TokenKind) -> bool {
        let present = self.check(token);
        if present {
            self.next();
        }
        present
    }

    pub fn check_keyword(&self, keyword: &str) -> bool {
        self.token.kind == TokenKind::Identifier && self.token.as_str(self.src) == keyword
    }

    pub fn eat_keyword(&mut self, keyword: &str) -> bool {
        let present = self.check_keyword(keyword);
        if present {
            self.next();
        }
        present
    }

    pub fn peek(&self, n: usize) -> Token {
        let mut stream = self.stream.clone();
        let mut token = Token::new(TokenKind::Eof, 0, 0);
        let mut i = 0;
        while i < n {
            token = stream.next();
            i += 1;
        }
        token
    }

    pub fn expect(&mut self, token: &TokenKind) -> Result<()> {
        if self.eat(token) {
            Ok(())
        } else {
            Err(())
        }
    }
}

// item parsing
impl<'a> Parser<'a> {
    pub fn parse_items(&mut self) -> Vec<Box<Item>> {
        let mut items = vec![];

        while let Some(item) = self.parse_item() {
            items.push(item);
        }

        // if !self.eat(&TokenKind::Eof) {}

        items
    }

    pub fn parse_item(&mut self) -> Option<Box<Item>> {
        if let Some(kind) = self.parse_item_kind() {
            Some(Box::new(Item { kind }))
        } else {
            None
        }
    }

    fn parse_item_kind(&mut self) -> Option<ItemKind> {
        if self.check_func() {
            let func = self.parse_func().ok().unwrap();
            println!("parsed func: {:?}", func);
            Some(ItemKind::Function(Box::new(func)))
        } else {
            None
        }
    }

    fn parse_func(&mut self) -> Result<Function> {
        // parse qualifiers
        let export = if self.eat_keyword(keyword::Export) {
            Export::Implicit
        } else {
            Export::None
        };
        // todo: import

        if !self.eat_keyword(keyword::Func) {
            return Err(());
        }

        // parse name
        let name = self.token.identifier(self.src).ok_or(())?;
        self.next();

        // todo: generics

        // parse params
        self.expect(&TokenKind::OpenDelimiter(Delimiter::Parenthesis))?;
        let mut params: Vec<Param> = vec![];
        loop {
            if self.eat(&TokenKind::CloseDelimiter(Delimiter::Parenthesis)) {
                break;
            }

            let param_name = self.token.identifier(self.src).ok_or(())?;
            self.next();
            self.expect(&TokenKind::Colon)?;
            let type_name = self.token.identifier(self.src).ok_or(())?;
            self.next();
            params.push(Param {
                name: param_name.to_string(),
                r#type: match type_name {
                    "i32" => Type::I32,
                    _ => panic!("invalid param type"),
                },
            });

            _ = self.eat(&TokenKind::Comma);
        }

        // parse return type
        let return_type = if self.eat(&TokenKind::Arrow) {
            let rtype = self.token.identifier(self.src).ok_or(())?;
            self.next();
            Some(match rtype {
                "i32" => Type::I32,
                _ => panic!("invalid ret type"),
            })
        } else {
            None
        };

        // parse block
        let block = if self.check(&TokenKind::OpenDelimiter(Delimiter::Bracket)) {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Function {
            attrs: FunctionAttributes { export },
            name: name.to_string(),
            params,
            return_type,
            block,
        })
    }

    fn parse_block(&mut self) -> Result<Block> {
        self.expect(&TokenKind::OpenDelimiter(Delimiter::Bracket))?;
        let mut expressions: Vec<Expression> = vec![];
        loop {
            if self.eat(&TokenKind::CloseDelimiter(Delimiter::Bracket)) {
                break;
            }
            if let Some(expr) = self.parse_expr() {
                expressions.push(*expr);
            }
        }
        Ok(Block { expressions })
    }
}

// expression parsing
impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Option<Box<Expression>> {
        if self.check_keyword(keyword::Return) {
            // println!("return");
            self.parse_return()
        } else if TokenKind::Identifier == self.token.kind {
            // println!("identifier");
            let name = self.token.identifier(self.src)?;
            self.next();
            let variable = Expression::Variable(Variable {
                name: name.to_string(),
            });
            if let TokenKind::BinOp(_) = &self.token.kind {
                // println!("binop");
                self.parse_binary_expr(variable)
            } else {
                Some(Box::new(variable))
            }
        } else {
            None
        }
    }

    pub fn parse_return(&mut self) -> Option<Box<Expression>> {
        if !self.eat_keyword(keyword::Return) {
            return None;
        }
        Some(Box::new(Expression::Return(self.parse_expr())))
    }

    // left recursive (not done)
    // e := a + e | a - e
    // a := n | (e)
    pub fn parse_binary_expr(&mut self, left: Expression) -> Option<Box<Expression>> {
        // let left = *self.parse_expr()?;
        let operator = if let TokenKind::BinOp(op) = &self.token.kind {
            match op {
                BinOpToken::Plus => BinaryOperator::Add,
                _ => todo!(),
            }
        } else {
            return None;
        };
        self.next();
        let right = *self.parse_expr()?;
        Some(Box::new(Expression::BinOp(Box::new(BinaryOperation {
            left,
            right,
            operator,
            r#type: Type::I32,
        }))))
    }
}

impl<'a> Parser<'a> {
    // check if token is "func" or a qualifier + "func" next
    fn check_func(&self) -> bool {
        let qualifiers = &[keyword::Export, keyword::Import];
        self.check_keyword(keyword::Func)
            || qualifiers.iter().any(|&kw| self.check_keyword(kw)) && {
                let token = self.peek(1);
                token.is_keyword(self.src, keyword::Func)
            }
    }
}

pub fn parse(src: &str, tokens: Vec<Token>) -> Result<Ast> {
    let mut parser = Parser::new(src, tokens);
    let items = parser.parse_items();
    Ok(Ast { items })
}
