#[allow(non_upper_case_globals)]
pub mod keyword;

#[cfg(test)]
pub mod tests;

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
        if self.token.is_comment() {
            self.next();
        }
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

    // eat a keyword and error if it fails
    pub fn expect_keyword(&mut self, keyword: &str) -> Result<()> {
        if self.eat_keyword(keyword) {
            Ok(())
        } else {
            Err(ParseError::expected_keyword(keyword, self.token.span))
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
            Err(ParseError::expected_token(&TokenKind::Eof, self.token.span))
        } else {
            Ok(items)
        }
    }

    // parse an item
    pub fn parse_item(&mut self) -> Result<Option<Item>> {
        // check item kind
        let start = self.token.span;
        if let Some(kind) = self.parse_item_kind()? {
            Ok(Some(Item {
                kind,
                span: start.to(&self.prev_token.span),
            }))
        } else {
            Ok(None)
        }
    }

    // check for item kind
    fn parse_item_kind(&mut self) -> Result<Option<ItemKind>> {
        if self.check_func() {
            let func = self.parse_func()?;
            //println!("parsed func: {:#?}", func);
            Ok(Some(ItemKind::Function(Box::new(func))))
        } else {
            Ok(None)
        }
    }

    fn parse_type_ascription(&mut self) -> Result<TypeAscription> {
        let start = self.token.span;
        let name = self
            .token
            .identifier(self.src)
            .ok_or_else(|| ParseError::expected_token(&TokenKind::Identifier, self.token.span))?;
        self.next();
        self.expect(&TokenKind::Colon)?;
        // parameter type
        if self.token.kind != TokenKind::Identifier {
            return Err(ParseError::expected_token(
                &TokenKind::Identifier,
                self.token.span,
            ));
        }
        self.next();

        Ok(TypeAscription {
            name: name.to_string(),
            r#type: Some(Type::Unchecked(self.prev_token.span)),
            span: start.to(&self.prev_token.span),
        })
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
        let import = if self.eat_keyword(keyword::Import) {
            Import::Implicit
        } else {
            Import::None
        };

        if export != Export::None && import != Import::None {
            panic!("can't have both export and import qualifiers");
        }

        if !self.eat_keyword(keyword::Func) {
            return Err(ParseError::unhandled());
        }

        // parse name
        let name = self
            .token
            .identifier(self.src)
            .ok_or_else(|| ParseError::expected_token(&TokenKind::Identifier, self.token.span))?;
        self.next();

        // todo: generics

        // parse params
        self.expect(&TokenKind::OpenDelimiter(Delimiter::Parenthesis))?;
        let mut params: Vec<TypeAscription> = vec![];
        loop {
            if self.eat(&TokenKind::CloseDelimiter(Delimiter::Parenthesis)) {
                break;
            }
            params.push(self.parse_type_ascription()?);

            if !self.check(&TokenKind::CloseDelimiter(Delimiter::Parenthesis)) {
                self.expect(&TokenKind::Comma)?;
            }
        }

        // parse return type
        let return_type = if self.eat(&TokenKind::Arrow) {
            match self.token.kind {
                TokenKind::Identifier => {
                    let span = self.token.span;
                    self.next();
                    Some(span)
                }
                _ => None,
            }
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
            attrs: FunctionAttributes { export, import },
            name: name.to_string(),
            params,
            return_type: return_type.map(Type::Unchecked),
            block,
        })
    }

    // parse a block of expressions
    // { [expr]* }
    fn parse_block(&mut self) -> Result<Block> {
        let start = self.token.span;
        self.expect(&TokenKind::OpenDelimiter(Delimiter::Bracket))?;
        let mut statements: Vec<Statement> = vec![];
        while !self.eat(&TokenKind::CloseDelimiter(Delimiter::Bracket)) {
            statements.push(self.parse_stmt()?);
        }
        Ok(Block {
            statements,
            span: start.to(&self.prev_token.span),
        })
    }
}

// statement parsing
impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Result<Statement> {
        if self.check_keyword(keyword::Let) {
            // let [expr] = [expr]
            self.parse_declaration()
        } else if self.check_keyword(keyword::Return) {
            // return [expr]
            self.parse_return()
        } else if self.eat_keyword(keyword::Break) {
            // break
            Ok(Statement::br(self.prev_token.span))
        } else if self.eat_keyword(keyword::Continue) {
            // continue
            Ok(Statement::cont(self.prev_token.span))
        } else {
            let expr = self.parse_expr()?;
            Ok(Statement::expr(expr))
        }
    }

    // parse return expression
    pub fn parse_return(&mut self) -> Result<Statement> {
        let start = self.token.span;
        if !self.eat_keyword(keyword::Return) {
            return Err(ParseError::expected_keyword(keyword::Return, start));
        }
        let expr = self.parse_expr()?;
        let span = start.to(&expr.span);
        Ok(Statement::ret(Some(expr), span))
    }

    pub fn parse_declaration(&mut self) -> Result<Statement> {
        if !self.eat_keyword(keyword::Let) {
            return Err(ParseError::expected_keyword(keyword::Let, self.token.span));
        }
        let assign: Expression = self.parse_expr()?;
        if let ExpressionKind::Assign(lhs, rhs) = assign.kind {
            if let AssignmentVariable::Variable(ident) = *lhs {
                // TODO: include let in span
                Ok(Statement::declaration(ident.name, None, *rhs, assign.span))
            } else if let AssignmentVariable::TypeAscription(ident) = *lhs {
                Ok(Statement::declaration(
                    ident.name,
                    ident.r#type,
                    *rhs,
                    assign.span,
                ))
            } else {
                // expected variable for let identifier
                panic!("expected variable after let identifier");
            }
        } else {
            panic!("expected assignment after let keyword");
            // Err(ParseError::unhandled())
        }
    }
}

// expression parsing
impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<Expression> {
        let left = self.parse_expr_unit()?;
        if self.token.is_bin_op() {
            // TODO: this can be improved
            if self.peek(1).kind == TokenKind::Equal {
                let operator = self.token_to_binop(&self.token.kind)?;
                self.next(); // operator
                self.next(); // equal
                let right = self.parse_expr()?;
                let span = left.span.to(&right.span);
                if let ExpressionKind::Variable(var) = left.kind {
                    let name = var.name.clone();
                    let var_span = var.span;
                    Ok(Expression::assign(
                        AssignmentVariable::Variable(var),
                        Expression::binop(Expression::var(name, var_span), right, operator),
                        span,
                    ))
                } else {
                    Err(ParseError::unhandled())
                }
            } else {
                let result = self.parse_binary_expr(left)?;
                result.ok_or_else(ParseError::unhandled)
            }
        } else {
            Ok(left)
        }
    }

    // parse stuff that could be added together in a binary expression
    pub fn parse_expr_unit(&mut self) -> Result<Expression> {
        if self.check_keyword(keyword::If) {
            self.parse_if_expr()
        } else if self.check_keyword(keyword::While) {
            self.parse_while_expr()
        } else if matches!(
            self.token.kind,
            TokenKind::BinOp(BinOpToken::Minus)
                | TokenKind::BinOp(BinOpToken::Plus)
                | TokenKind::Exclamation
        ) {
            // unary
            let operator = match self.token.kind {
                TokenKind::BinOp(BinOpToken::Minus) => UnaryOperator::Minus,
                TokenKind::BinOp(BinOpToken::Plus) => UnaryOperator::Plus,
                TokenKind::Exclamation => UnaryOperator::Not,
                _ => panic!(),
            };
            self.next();
            let expr = self.parse_expr_unit()?;
            Ok(Expression::uop(expr, operator))
        } else if self.check(&TokenKind::OpenDelimiter(Delimiter::Parenthesis)) {
            // group
            let start = self.token.span;
            self.eat(&TokenKind::OpenDelimiter(Delimiter::Parenthesis));
            let expr = self.parse_expr()?;
            let span = start.to(&self.token.span);
            self.expect(&TokenKind::CloseDelimiter(Delimiter::Parenthesis))?;
            Ok(Expression::group(expr, span))
        } else if self.check(&TokenKind::Identifier) {
            // variable
            let name = self.token.identifier(self.src).ok_or_else(|| {
                ParseError::expected_token(&TokenKind::Identifier, self.token.span)
            })?;
            self.next();
            let variable = match name {
                "true" => {
                    return Ok(Expression::literal(
                        ast::LiteralKind::Bool(true),
                        self.prev_token.span,
                    ))
                }
                "false" => {
                    return Ok(Expression::literal(
                        ast::LiteralKind::Bool(false),
                        self.prev_token.span,
                    ))
                }
                _ => Variable {
                    name: name.to_string(),
                    r#type: None,
                    span: self.prev_token.span,
                },
            };

            if self.eat(&TokenKind::Colon) {
                // type ascription
                if self.token.kind != TokenKind::Identifier {
                    return Err(ParseError::expected_token(
                        &TokenKind::Identifier,
                        self.token.span,
                    ));
                }
                self.next();
                let ty = Type::Unchecked(self.prev_token.span);
                let ascription = AssignmentVariable::TypeAscription(TypeAscription {
                    name: variable.name,
                    r#type: Some(ty),
                    span: variable.span.to(&self.prev_token.span),
                });

                // assign
                self.expect(&TokenKind::Equal)?;
                let rhs = self.parse_expr()?;
                let span = variable.span.to(&rhs.span);
                Ok(Expression::assign(ascription, rhs, span))
            } else if self.eat(&TokenKind::Equal) {
                // assign
                let rhs = self.parse_expr()?;
                let span = variable.span.to(&rhs.span);
                Ok(Expression::assign(
                    AssignmentVariable::Variable(variable),
                    rhs,
                    span,
                ))
            } else {
                let span = variable.span;
                let expr = Expression {
                    kind: ExpressionKind::Variable(variable),
                    span,
                };
                if self.eat(&TokenKind::OpenDelimiter(Delimiter::Parenthesis)) {
                    // function call
                    let mut args = vec![];
                    while !self.eat(&TokenKind::CloseDelimiter(Delimiter::Parenthesis)) {
                        args.push(self.parse_expr()?);
                        if !self.check(&TokenKind::CloseDelimiter(Delimiter::Parenthesis)) {
                            self.expect(&TokenKind::Comma)?;
                        }
                    }
                    return Ok(Expression::call(expr, args, self.token.span));
                } else {
                    // variable
                    Ok(expr)
                }
            }
        } else if let TokenKind::Literal(kind) = &self.token.kind {
            // literal
            let lit = self.token.as_str(self.src);
            let lit = match kind {
                LiteralKind::String => panic!("todo: parse string literal"),
                LiteralKind::Integer => {
                    // parse integer literal
                    if let Ok(int) = lit.parse::<i32>() {
                        Expression::literal(ast::LiteralKind::Integer(int), self.token.span)
                    } else {
                        panic!("invalid int {lit}")
                    }
                }
                LiteralKind::Float => {
                    // parse integer literal
                    if let Ok(float) = lit.parse::<f32>() {
                        Expression::literal(ast::LiteralKind::Float(float), self.token.span)
                    } else {
                        panic!("invalid float {lit}")
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

    pub fn token_to_binop(&self, token: &TokenKind) -> Result<BinaryOperator> {
        match &token {
            TokenKind::BinOp(op) => match op {
                BinOpToken::Plus => Ok(BinaryOperator::Add),
                BinOpToken::Minus => Ok(BinaryOperator::Sub),
                BinOpToken::Star => Ok(BinaryOperator::Mul),
                BinOpToken::Slash => Ok(BinaryOperator::Div),
                BinOpToken::Percent => Ok(BinaryOperator::Mod),
                BinOpToken::Caret => Ok(BinaryOperator::Caret),
                BinOpToken::And => Ok(BinaryOperator::And),
                BinOpToken::Pipe => Ok(BinaryOperator::Or),
                BinOpToken::ShiftLeft => Ok(BinaryOperator::ShiftLeft),
                BinOpToken::ShiftRight => Ok(BinaryOperator::ShiftRight),
            },
            TokenKind::LThan => Ok(BinaryOperator::Less),
            TokenKind::LThanEqual => Ok(BinaryOperator::LessOrEqual),
            TokenKind::GThan => Ok(BinaryOperator::Greater),
            TokenKind::GThanEqual => Ok(BinaryOperator::GreaterOrEqual),
            TokenKind::EqualEqual => Ok(BinaryOperator::EqualEqual),
            TokenKind::NotEqual => Ok(BinaryOperator::NotEqual),
            TokenKind::AndAnd => Ok(BinaryOperator::AndAnd),
            TokenKind::PipePipe => Ok(BinaryOperator::OrOr),
            _ => Err(ParseError::unhandled()),
        }
    }

    // parse binary expression
    // expr [op] expr
    pub fn parse_binary_expr(&mut self, left: Expression) -> Result<Option<Expression>> {
        let mut op_stack: Vec<(BinaryOperator, u8)> = vec![];
        let mut expr_stack = vec![left];
        while self.token.is_bin_op() {
            let operator = self.token_to_binop(&self.token.kind)?;
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

    pub fn parse_if_expr(&mut self) -> Result<Expression> {
        let start = self.token.span;
        self.expect_keyword(keyword::If)?;
        let cond = self.parse_expr()?;
        let then_branch = self.parse_block()?;
        let else_branch = if self.eat_keyword(keyword::Else) {
            Some(if self.check_keyword(keyword::If) {
                self.parse_if_expr()?
            } else {
                Expression::block(self.parse_block()?)
            })
        } else {
            None
        };
        Ok(Expression::if_expr(
            cond,
            then_branch,
            else_branch,
            start.to(&self.prev_token.span),
        ))
    }

    pub fn parse_while_expr(&mut self) -> Result<Expression> {
        let start = self.token.span;
        self.expect_keyword(keyword::While)?;
        let cond = self.parse_expr()?;
        let block = self.parse_block()?;
        Ok(Expression::while_expr(
            cond,
            block,
            start.to(&self.prev_token.span),
        ))
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
