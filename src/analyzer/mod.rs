use crate::ast::*;

use crate::parser::ErrorLevel;
use crate::parser::ParseError;

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone)]
struct VariableData {
    mutable: bool,
}

struct Analyzer {
    symbol_table: SymbolTable<VariableData>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn analyze(&mut self, ast: &mut Ast) -> Result<()> {
        for item in &mut ast.items {
            // let ItemKind::Function(func) = &item.kind;
            self.check_item(item)?;
        }
        Ok(())
    }

    fn check_item(&mut self, item: &mut Item) -> Result<()> {
        match &mut item.kind {
            ItemKind::Function(func) => {
                if let Some(block) = &mut func.block {
                    if func.attrs.import != Import::None {
                        return Err(ParseError {
                            level: ErrorLevel::Error,
                            message: String::from("imported functions cannot have a body"),
                            span: Some(block.span),
                        });
                    }
                    self.check_block(block)?;
                } else {
                    if func.attrs.import == Import::None {
                        return Err(ParseError {
                            level: ErrorLevel::Error,
                            message: String::from("function must have a body"),
                            span: Some(item.span),
                        });
                    }
                }
            }
        };
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &mut Statement) -> Result<()> {
        match &mut stmt.kind {
            StatementKind::Return(Some(expr)) => {
                self.check_expr(expr)?;
            }
            StatementKind::Let(local) => {
                let name = &local.ident;
                let data = VariableData {
                    mutable: local.mutable,
                };
                self.symbol_table.insert(name, data);
                self.check_expr(&mut local.init)?;
            }
            StatementKind::Expr(expr) => self.check_expr(expr)?,
            _ => (),
        };
        Ok(())
    }

    pub fn check_expr(&mut self, expr: &mut Expression) -> Result<()> {
        match &mut expr.kind {
            ExpressionKind::Assign(lhs, rhs) => {
                // self.check_expr(var)?;
                let name = match &**lhs {
                    AssignmentVariable::Variable(var) => &var.name,
                    AssignmentVariable::TypeAscription(ascription) => &ascription.name,
                };
                if let Some(data) = self.symbol_table.get(name) {
                    if !data.mutable {
                        return Err(ParseError {
                            level: ErrorLevel::Error,
                            message: format!("cannot assign to immutable variable `{}`", name),
                            span: Some(expr.span),
                        });
                    }
                    self.check_expr(rhs)?;
                } else {
                    panic!("variable `{}` not found", name)
                }
            }
            ExpressionKind::BinOp(binop) => {
                self.check_expr(&mut binop.left)?;
                self.check_expr(&mut binop.right)?;
                if binop.operator == BinaryOperator::Div {
                    if let ExpressionKind::Literal(lit) = &binop.right.kind {
                        if *lit == LiteralKind::Integer(0) || *lit == LiteralKind::Float(0.0) {
                            return Err(ParseError {
                                level: ErrorLevel::Error,
                                message: String::from("division by zero"),
                                span: Some(expr.span),
                            });
                        }
                    }
                }
            }
            ExpressionKind::Block(block) => self.check_block(block)?,
            ExpressionKind::Group(expr) => self.check_expr(expr)?,
            ExpressionKind::Loop(block) => self.check_expr(block)?,
            ExpressionKind::While(cond, block) => {
                self.check_expr(cond)?;
                self.check_expr(block)?;
            }
            ExpressionKind::If(if_expr) => {
                self.check_expr(&mut if_expr.cond)?;
                self.check_block(&mut if_expr.then_branch)?;
                if let Some(else_expr) = &mut if_expr.else_branch {
                    self.check_expr(else_expr)?;
                }
            }
            _ => (),
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
}

pub fn analyze(_src: &str, ast: &mut Ast) -> Result<()> {
    let mut analyzer = Analyzer::new();
    analyzer.analyze(ast)
}
