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

pub fn check(src: &str, ast: &mut Ast) -> Result<()> {
    let mut symbol_table = SymbolTable::new();
    for item in &mut ast.items {
        check_item(src, item, &mut symbol_table)?;
    }
    Ok(())
}

fn get_type(symbol_table: &SymbolTable, src: &str, span: Span) -> Result<Type> {
    let type_str = span.in_src(src);
    get_type_from_name(symbol_table, type_str, span)
}

fn get_type_from_name(symbol_table: &SymbolTable, name: &str, span: Span) -> Result<Type> {
    match symbol_table.get(name) {
        Some(sym) => Ok(sym.r#type),
        None => Err(ParseError::notfound(name, span)),
    }
}

fn check_item(src: &str, item: &mut Item, symbol_table: &mut SymbolTable) -> Result<()> {
    match &mut item.kind {
        ItemKind::Function(func) => {
            // parse function return type
            if let Some(Type::Unchecked(span)) = &func.return_type {
                func.return_type = Some(get_type(symbol_table, src, *span)?);
            }
            // parse param types
            for param in &mut func.params {
                if let Some(Type::Unchecked(span)) = &param.r#type {
                    param.r#type = Some(get_type(symbol_table, src, *span)?);
                }
            }
            // typecheck block expressions
            if let Some(block) = &mut func.block {
                symbol_table.push_scope();
                // push param variables to symbol table
                for param in &mut func.params {
                    symbol_table.insert(
                        &param.name,
                        Symbol::var(
                            param.name.clone(),
                            param.r#type.expect("param type not filled in"),
                        ),
                    );
                }
                for stmt in &mut block.statements {
                    check_stmt(stmt, symbol_table)?;
                }
                symbol_table.pop_scope();
            }
        }
    };
    Ok(())
}

fn check_block(block: &mut Block, symbol_table: &mut SymbolTable) -> Result<()> {
    symbol_table.push_scope();
    for stmt in &mut block.statements {
        check_stmt(stmt, symbol_table)?;
    }
    symbol_table.pop_scope();
    Ok(())
}

pub fn check_stmt(stmt: &mut Statement, symbol_table: &mut SymbolTable) -> Result<Type> {
    match &mut stmt.kind {
        StatementKind::Return(ret) => {
            if let Some(expr) = ret {
                check_expr(expr, symbol_table)?;
            }
            Ok(Type::Unit)
        }
        StatementKind::Expr(expr) => check_expr(expr, symbol_table),
    }
}

pub fn check_expr(expr: &mut Expression, symbol_table: &mut SymbolTable) -> Result<Type> {
    Ok(match &mut expr.kind {
        ExpressionKind::BinOp(op) => {
            let left = check_expr(&mut op.left, symbol_table)?;
            let right = check_expr(&mut op.right, symbol_table)?;
            if left == right {
                op.r#type = Some(left);
            } else {
                return Err(ParseError::mismatched(left, op.right.span));
            }
            left
        }
        ExpressionKind::Variable(var) => {
            let var_type = get_type_from_name(symbol_table, &var.name, expr.span)?;
            var.r#type = Some(var_type);
            var_type
        }
        ExpressionKind::Literal(lit) => match lit {
            LiteralKind::Integer(_) => Type::Basic(BasicType::Int32),
            LiteralKind::Float(_) => Type::Basic(BasicType::Float32),
            LiteralKind::Bool(_) => Type::Basic(BasicType::Bool),
            _ => panic!("unhandled literal kind {:?}", lit),
        },
        ExpressionKind::Group(expr) => check_expr(expr, symbol_table)?,
        ExpressionKind::UnaryOp(op) => {
            let utype = check_expr(&mut op.expr, symbol_table)?;
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
            let cond_type = check_expr(&mut if_expr.cond, symbol_table)?;
            if cond_type != Type::Basic(BasicType::Bool) {
                return Err(ParseError::mismatched(
                    Type::Basic(BasicType::Bool),
                    if_expr.cond.span,
                ));
            }
            check_block(&mut if_expr.then_branch, symbol_table)?;
            if let Some(else_branch) = &mut if_expr.else_branch {
                check_expr(else_branch, symbol_table)?;
            }
            // todo: allow ifs to evaluate to a type
            Type::Unit
        }
        ExpressionKind::Block(block) => {
            check_block(block, symbol_table)?;
            Type::Unit
        }
        // _ => panic!("unhandled expression {:?}", expr),
    })
}
