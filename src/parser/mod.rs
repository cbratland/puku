#[allow(non_upper_case_globals)]
pub mod keyword;

#[cfg(test)]
mod tests;

mod error;

use crate::ast::{self, *};
use crate::lexer::{BinOpToken, Delimiter, LiteralKind, Token, TokenKind};
pub use error::*;
use std::cmp::Ordering;

type Result<T> = std::result::Result<T, ParseError>;

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

pub struct Parser<'a> {
    pub src: &'a str,
    pub token: Token, // current token
    pub prev_token: Token,
    pub stream: TokenStream,
}

impl<'a> Parser<'a> {
    // create parser
    pub fn new(src: &'a str, tokens: Vec<Token>) -> Self {
        let mut stream = TokenStream::new(tokens);
        Self {
            src,
            token: stream.next(),
            prev_token: Token::new(TokenKind::Eof, 0, 0),
            stream,
        }
    }

    // move to the next token
    pub fn next(&mut self) {
        let next = self.stream.next();
        self.prev_token = std::mem::replace(&mut self.token, next);
    }

    // check if current token is kind
    pub fn check(&self, token: &TokenKind) -> bool {
        self.token.kind == *token
    }

    // consume current token if it's kind, otherwise return false
    pub fn eat(&mut self, token: &TokenKind) -> bool {
        let present = self.check(token);
        if present {
            self.next();
        }
        present
    }

    // check if current token is keyword
    pub fn check_keyword(&self, keyword: &str) -> bool {
        self.token.kind == TokenKind::Identifier && self.token.as_str(self.src) == keyword
    }

    // consume current token if it's specified keyword
    pub fn eat_keyword(&mut self, keyword: &str) -> bool {
        let present = self.check_keyword(keyword);
        if present {
            self.next();
        }
        present
    }

    // get token n spaces ahead
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

    // eat a token and error if it fails
    pub fn expect(&mut self, token: &TokenKind) -> Result<()> {
        if self.eat(token) {
            Ok(())
        } else {
            Err(ParseError::expected_token(token, self.token.span))
        }
    }
}

// item parsing
impl<'a> Parser<'a> {
    // parse all items in the token stream
    pub fn parse_items(&mut self) -> Result<Vec<Item>> {
        let mut items = vec![];

        while let Some(item) = self.parse_item()? {
            items.push(item);
        }

        if !self.eat(&TokenKind::Eof) {
            panic!("expected eof")
        }

        Ok(items)
    }

    // parse an item
    pub fn parse_item(&mut self) -> Result<Option<Item>> {
        // skip comments
        // todo: comments need to be skipped everywhere
        while self.check(&TokenKind::LineComment) {
            self.next();
        }
        // check item kind
        let start = self.token.span;
        if let Some(kind) = self.parse_item_kind()? {
            Ok(Some(Item {
                kind,
                span: start.until(&self.token.span),
            }))
        } else {
            Ok(None)
        }
    }

    // check for item kind
    fn parse_item_kind(&mut self) -> Result<Option<ItemKind>> {
        if self.check_func() {
            let func = self.parse_func()?;
            println!("parsed func: {:?}", func);
            Ok(Some(ItemKind::Function(Box::new(func))))
        } else {
            Ok(None)
        }
    }

    // parse a function definition
    // [qualifiers] func [name]([params]) -> [ret_type] [block]
    fn parse_func(&mut self) -> Result<Function> {
        // parse qualifiers
        let export = if self.eat_keyword(keyword::Export) {
            Export::Implicit
        } else {
            Export::None
        };
        // todo: import

        if !self.eat_keyword(keyword::Func) {
            return Err(ParseError::unhandled());
        }

        // parse name
        let name = self
            .token
            .identifier(self.src)
            .ok_or_else(ParseError::unhandled)?;
        self.next();

        // todo: generics

        // parse params
        self.expect(&TokenKind::OpenDelimiter(Delimiter::Parenthesis))?;
        let mut params: Vec<Param> = vec![];
        loop {
            if self.eat(&TokenKind::CloseDelimiter(Delimiter::Parenthesis)) {
                break;
            }
            let start = self.token.span;
            let param_name = self
                .token
                .identifier(self.src)
                .ok_or_else(ParseError::unhandled)?;
            self.next();
            self.expect(&TokenKind::Colon)?;
            let type_name = self
                .token
                .identifier(self.src)
                .ok_or_else(ParseError::unhandled)?;
            params.push(Param {
                name: param_name.to_string(),
                type_str: type_name.to_string(),
                r#type: None,
                span: start.to(&self.token.span),
            });
            self.next();

            if !self.check(&TokenKind::CloseDelimiter(Delimiter::Parenthesis)) {
                self.expect(&TokenKind::Comma)?;
            }
        }

        // parse return type
        let return_type_str = if self.eat(&TokenKind::Arrow) {
            let rtype = self
                .token
                .identifier(self.src)
                .ok_or_else(ParseError::unhandled)?;
            self.next();
            // TODO: change
            Some(rtype.to_string())
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
            return_type_str,
            return_type: None,
            block,
        })
    }

    // parse a block of expressions
    // { [expr]* }
    fn parse_block(&mut self) -> Result<Block> {
        let start = self.token.span;
        self.expect(&TokenKind::OpenDelimiter(Delimiter::Bracket))?;
        let mut expressions: Vec<Expression> = vec![];
        while !self.eat(&TokenKind::CloseDelimiter(Delimiter::Bracket)) {
            if let Some(expr) = self.parse_expr()? {
                expressions.push(expr);
            }
        }
        Ok(Block {
            expressions,
            span: start.until(&self.token.span),
        })
    }
}

// expression parsing
impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<Option<Expression>> {
        if self.check_keyword(keyword::Return) {
            // return [expr]
            self.parse_return()
        } else {
            let left = self.parse_expr_unit()?;
            if let TokenKind::BinOp(_) = self.token.kind {
                self.parse_binary_expr(left)
            } else {
                Ok(Some(left))
            }
        }
    }

    // parse stuff that could be added together in a binary expression
    pub fn parse_expr_unit(&mut self) -> Result<Expression> {
        if self.check(&TokenKind::Identifier) {
            // [variable]
            // println!("identifier");
            let name = self
                .token
                .identifier(self.src)
                .ok_or_else(ParseError::unhandled)?;
            self.next();
            let variable = Expression::var(name.to_string(), self.token.span);
            Ok(variable)
        } else if let TokenKind::Literal(kind) = &self.token.kind {
            let lit = self.token.as_str(self.src);
            let lit = match kind {
                LiteralKind::String => panic!("todo: parse string literal"),
                LiteralKind::Integer => {
                    // parse integer literal
                    if let Ok(int) = lit.parse::<i32>() {
                        Expression::literal(ast::LiteralKind::Integer(int), self.token.span)
                    } else {
                        panic!("invalid")
                    }
                }
                _ => panic!("unknown literal: {}", lit),
            };
            self.next();
            Ok(lit)
        } else {
            Err(ParseError::expected_expression(self.token.span))
        }
    }

    // parse return expression
    pub fn parse_return(&mut self) -> Result<Option<Expression>> {
        let start = self.token.span;
        if !self.eat_keyword(keyword::Return) {
            return Err(ParseError::expected_keyword(keyword::Return, start));
        }
        let expr = self.parse_expr()?;
        Ok(Some(Expression::ret(expr, start.to(&self.token.span))))
    }

    // parse binary expression
    // expr [op] expr
    pub fn parse_binary_expr(&mut self, left: Expression) -> Result<Option<Expression>> {
        let mut op_stack: Vec<(BinaryOperator, u8)> = vec![];
        let mut expr_stack = vec![left];
        while let TokenKind::BinOp(op) = &self.token.kind {
            let operator = match op {
                BinOpToken::Plus => BinaryOperator::Add,
                BinOpToken::Minus => BinaryOperator::Sub,
                BinOpToken::Star => BinaryOperator::Mul,
                BinOpToken::Slash => BinaryOperator::Div,
                _ => todo!(),
            };
            self.next();
            let precendence = operator.precedence();
            _ = Self::parse_precedence(
                Some((operator, precendence)),
                &mut op_stack,
                &mut expr_stack,
            );

            let right = self.parse_expr_unit()?;
            expr_stack.push(right);
        }

        Ok(Self::parse_precedence(None, &mut op_stack, &mut expr_stack))
    }

    // https://en.wikipedia.org/wiki/Simple_precedence_parser
    fn parse_precedence(
        next: Option<(BinaryOperator, u8)>,
        op_stack: &mut Vec<(BinaryOperator, u8)>,
        expr_stack: &mut Vec<Expression>,
    ) -> Option<Expression> {
        loop {
            match (op_stack.pop(), next) {
                // fully reduced
                (None, None) => {
                    return if let Some(final_expr) = expr_stack.pop() {
                        if expr_stack.is_empty() {
                            Some(final_expr)
                        } else {
                            panic!("not reduced")
                        }
                    } else {
                        None
                    };
                }

                // add next op to stack
                (None, Some(next)) => {
                    op_stack.push(next);
                    break;
                }

                // do final reduce (no more ops to add)
                (Some((op, _)), None) => Self::reduce_op(op, expr_stack),

                // do shift or reduce
                (Some((op_top, p_top)), Some((op_next, p_next))) => match p_top.cmp(&p_next) {
                    // shift
                    Ordering::Less | Ordering::Equal => {
                        op_stack.push((op_top, p_top));
                        op_stack.push((op_next, p_next));
                        break;
                    }
                    // reduce
                    Ordering::Greater => {
                        Self::reduce_op(op_top, expr_stack);
                    }
                },
            }
        }

        None
    }

    // reduce top two exepressions on stack into binop expr with operator
    fn reduce_op(operator: BinaryOperator, expr_stack: &mut Vec<Expression>) {
        match (expr_stack.pop(), expr_stack.pop()) {
            (Some(right), Some(left)) => {
                expr_stack.push(Expression::binop(left, right, operator));
            }
            _ => panic!("need 2 exprs to reduce"),
        }
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

// parses a list of tokens into an ast struct
pub fn parse(src: &str, tokens: Vec<Token>) -> Result<Ast> {
    let mut parser = Parser::new(src, tokens);
    let items = parser.parse_items()?;
    Ok(Ast { items })
}
