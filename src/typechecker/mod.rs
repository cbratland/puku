#[cfg(test)]
mod tests;

use crate::ast::*;

use crate::parser::ErrorLevel;
use crate::parser::ParseError;

type Result<T> = std::result::Result<T, ParseError>;

impl ParseError {
    pub fn mismatched(expected: Type, span: Span) -> Self {
        Self {
            level: ErrorLevel::Error,
            message: format!("mismatched types (expected {:?})", expected),
            span: Some(span),
        }
    }

    pub fn notfound(typ: &str, span: Span) -> Self {
        Self {
            level: ErrorLevel::Error,
            message: format!("cannot find type `{}` in this scope", typ),
            span: Some(span),
        }
    }
}

struct TypeChecker<'a> {
    src: &'a str,
    symbol_table: SymbolTable<TypeSymbol>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            symbol_table: SymbolTable::default(),
        }
    }

    pub fn check(&mut self, ast: &mut Ast) -> Result<()> {
        for item in &mut ast.items {
            let ItemKind::Function(func) = &item.kind;

            // parse function return type
            let return_type = if let Some(Type::Unchecked(span)) = &func.return_type {
                self.get_type(*span)?
            } else if let Some(ret) = func.return_type {
                ret
            } else {
                Type::Unit
            };

            // add function as symbol
            self.symbol_table
                .insert(&func.name, TypeSymbol::func(func.name.clone(), return_type));
        }
        for item in &mut ast.items {
            self.check_item(item)?;
        }
        Ok(())
    }

    fn get_type(&self, span: Span) -> Result<Type> {
        let type_str = span.in_src(self.src);
        self.get_type_from_name(type_str, span)
    }

    fn get_type_from_name(&self, name: &str, span: Span) -> Result<Type> {
        match self.symbol_table.get(name) {
            Some(sym) => Ok(sym.r#type),
            None => Err(ParseError::notfound(name, span)),
        }
    }

    fn check_item(&mut self, item: &mut Item) -> Result<()> {
        match &mut item.kind {
            ItemKind::Function(func) => {
                // parse function return type
                if let Some(Type::Unchecked(span)) = &func.return_type {
                    func.return_type = Some(self.get_type(*span)?);
                }
                // parse param types
                for param in &mut func.params {
                    if let Some(Type::Unchecked(span)) = &param.r#type {
                        param.r#type = Some(self.get_type(*span)?);
                    }
                }

                // typecheck block expressions
                if let Some(block) = &mut func.block {
                    self.symbol_table.push_scope();
                    // push param variables to symbol table
                    for param in &mut func.params {
                        self.symbol_table.insert(
                            &param.name,
                            TypeSymbol::var(
                                param.name.clone(),
                                param.r#type.expect("param type not filled in"),
                            ),
                        );
                    }
                    for stmt in &mut block.statements {
                        self.check_stmt(stmt)?;
                    }
                    self.symbol_table.pop_scope();
                }
            }
        };
        Ok(())
    }

    fn check_block(&mut self, block: &mut Block) -> Result<()> {
        self.symbol_table.push_scope();
        for stmt in &mut block.statements {
            self.check_stmt(stmt)?;
        }
        self.symbol_table.pop_scope();
        Ok(())
    }

    pub fn check_stmt(&mut self, stmt: &mut Statement) -> Result<Type> {
        match &mut stmt.kind {
            StatementKind::Return(ret) => {
                if let Some(expr) = ret {
                    self.check_expr(expr)?;
                }
                Ok(Type::Unit)
            }
            StatementKind::Let(local) => {
                let var_type = self.check_expr(&mut local.init)?;

                // check unchecked types
                if let Some(Type::Unchecked(span)) = &local.r#type {
                    let var_type = self.get_type(*span)?;
                    local.r#type = Some(var_type);
                }

                // compare static type with inferred type
                if let Some(static_type) = &local.r#type {
                    if var_type != *static_type {
                        return Err(ParseError::mismatched(*static_type, local.init.span));
                    }
                } else {
                    // infer type
                    local.r#type = Some(var_type);
                }
                self.symbol_table.insert(
                    &local.ident,
                    TypeSymbol::var(
                        local.ident.clone(),
                        local.r#type.expect("variable type not filled in"),
                    ),
                );
                Ok(Type::Unit)
            }
            StatementKind::Expr(expr) => self.check_expr(expr),
            StatementKind::Break | StatementKind::Continue => Ok(Type::Unit),
        }
    }

    pub fn check_expr(&mut self, expr: &mut Expression) -> Result<Type> {
        Ok(match &mut expr.kind {
            ExpressionKind::BinOp(op) => {
                let left = self.check_expr(&mut op.left)?;
                let right = self.check_expr(&mut op.right)?;
                if left == right {
                    op.r#type = Some(left);
                } else {
                    return Err(ParseError::mismatched(left, op.right.span));
                }
                match op.operator {
                    BinaryOperator::EqualEqual
                    | BinaryOperator::NotEqual
                    | BinaryOperator::AndAnd
                    | BinaryOperator::OrOr
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterOrEqual
                    | BinaryOperator::Less
                    | BinaryOperator::LessOrEqual => {
                        // op.r#type = Some(Type::Basic(BasicType::Bool));
                        Type::Basic(BasicType::Bool)
                    }
                    _ => left,
                }
            }
            ExpressionKind::Variable(var) => {
                let var_type = self.get_type_from_name(&var.name, expr.span)?;
                var.r#type = Some(var_type);
                var_type
            }
            ExpressionKind::Call(callee, args) => {
                if let ExpressionKind::Variable(var) = &mut callee.kind {
                    for arg in args {
                        self.check_expr(arg)?;
                    }
                    let var_type = self.get_type_from_name(&var.name, expr.span)?;
                    // todo: check arg types
                    var.r#type = Some(var_type);
                    var_type
                } else {
                    panic!("unhandled callee kind {:?}", callee.kind);
                }
            }
            ExpressionKind::Index(expr, index) => {
                let expr_type = self.check_expr(expr)?;
                let index_type = self.check_expr(index)?;
                if index_type != Type::Basic(BasicType::Int32) {
                    return Err(ParseError::mismatched(
                        Type::Basic(BasicType::Int32),
                        index.span,
                    ));
                }
                if let Type::Array(arr_type) = expr_type {
                    Type::Basic(arr_type)
                } else {
                    panic!("expected array type")
                }
            }
            ExpressionKind::Assign(lhs, rhs) => {
                let lhs_type = match &mut **lhs {
                    AssignmentVariable::Variable(var) => {
                        let var_type = self.get_type_from_name(&var.name, var.span)?;
                        var.r#type = Some(var_type);
                        var_type
                    }
                    AssignmentVariable::TypeAscription(ascription) => {
                        let var_type =
                            self.get_type_from_name(&ascription.name, ascription.span)?;
                        ascription.r#type = Some(var_type);
                        var_type
                    }
                };
                let rhs_type = self.check_expr(rhs)?;
                if lhs_type != rhs_type {
                    return Err(ParseError::mismatched(lhs_type, rhs.span));
                }
                lhs_type
            }
            ExpressionKind::Literal(lit) => match &mut **lit {
                LiteralKind::Integer(_) => Type::Basic(BasicType::Int32),
                LiteralKind::Float(_) => Type::Basic(BasicType::Float32),
                LiteralKind::Bool(_) => Type::Basic(BasicType::Bool),
                LiteralKind::Array(lit) => {
                    let mut array_type = None;
                    for expr in &mut lit.elems {
                        let expr_type = self.check_expr(expr)?;
                        if let Some(array_type) = &mut array_type {
                            if *array_type != expr_type {
                                return Err(ParseError::mismatched(expr_type, expr.span));
                            }
                        } else {
                            array_type = Some(expr_type);
                        }
                    }
                    lit.r#type = array_type;
                    if let Some(Type::Basic(array_type)) = array_type {
                        Type::Array(array_type)
                    } else {
                        panic!("empty array")
                    }
                }
                _ => panic!("unhandled literal kind {:?}", lit),
            },
            ExpressionKind::Group(expr) => self.check_expr(expr)?,
            ExpressionKind::UnaryOp(op) => {
                let utype = self.check_expr(&mut op.expr)?;
                op.r#type = Some(utype);
                if op.operator == UnaryOperator::Not && utype != Type::Basic(BasicType::Bool) {
                    return Err(ParseError::mismatched(
                        Type::Basic(BasicType::Bool),
                        op.expr.span,
                    ));
                }
                utype
            }
            ExpressionKind::If(if_expr) => {
                let cond_type = self.check_expr(&mut if_expr.cond)?;
                if cond_type != Type::Basic(BasicType::Bool) {
                    return Err(ParseError::mismatched(
                        Type::Basic(BasicType::Bool),
                        if_expr.cond.span,
                    ));
                }
                self.check_block(&mut if_expr.then_branch)?;
                if let Some(else_branch) = &mut if_expr.else_branch {
                    self.check_expr(else_branch)?;
                }
                // todo: allow ifs to evaluate to a type
                Type::Unit
            }
            ExpressionKind::Loop(body) => self.check_expr(body)?,
            ExpressionKind::While(cond, body) => {
                let cond_type = self.check_expr(cond)?;
                if cond_type != Type::Basic(BasicType::Bool) {
                    return Err(ParseError::mismatched(
                        Type::Basic(BasicType::Bool),
                        cond.span,
                    ));
                }
                self.check_expr(body)?
            }
            ExpressionKind::Block(block) => {
                self.check_block(block)?;
                Type::Unit
            } // _ => panic!("unhandled expression {:?}", expr),
        })
    }
}

pub fn check(src: &str, ast: &mut Ast) -> Result<()> {
    let mut checker = TypeChecker::new(src);
    checker.check(ast)
}
