use crate::ast::*;

pub fn check(src: &str, ast: &mut Ast) {
    let mut symbol_table = SymbolTable::new();
    for item in &mut ast.items {
        check_item(src, item, &mut symbol_table);
    }
}

fn check_item(src: &str, item: &mut Item, symbol_table: &mut SymbolTable) {
    match &mut item.kind {
        ItemKind::Function(func) => {
            // parse function return type
            if let Some(Type::Unchecked(span)) = &func.return_type {
                let type_str = span.in_src(src);
                let type_val = symbol_table
                    .get(type_str)
                    .expect("function return type is undefined")
                    .r#type;
                func.return_type = Some(type_val);
            }
            // parse param types
            for param in &mut func.params {
                if let Some(Type::Unchecked(span)) = &param.r#type {
                    let type_str = span.in_src(src);
                    let type_val = symbol_table
                        .get(type_str)
                        .expect("function param type is undefined")
                        .r#type;
                    param.r#type = Some(type_val);
                }
            }
            // typecheck block expressions
            if let Some(block) = &mut func.block {
                symbol_table.push_scope();
                // push param variables to symbol table
                for param in &mut func.params {
                    symbol_table.insert(
                        &param.name,
                        Symbol::var(param.name.clone(), param.r#type.unwrap()),
                    );
                }
                for expr in &mut block.expressions {
                    check_expr(expr, symbol_table);
                }
                symbol_table.pop_scope();
            }
        }
    };
}

fn check_expr(expr: &mut Expression, symbol_table: &mut SymbolTable) -> Type {
    match &mut expr.kind {
        ExpressionKind::Return(ret) => {
            if let Some(expr) = ret {
                check_expr(expr, symbol_table);
            }
            Type::Unit
        }
        ExpressionKind::BinOp(op) => {
            let left = check_expr(&mut op.left, symbol_table);
            let right = check_expr(&mut op.right, symbol_table);
            if left == right {
                op.r#type = Some(left);
            } else {
                panic!("bin op types don't match")
            }
            left
        }
        ExpressionKind::Variable(var) => {
            let var_type = symbol_table
                .get(&var.name)
                .expect("variable not defined")
                .r#type;
            var.r#type = Some(var_type);
            var_type
        }
        ExpressionKind::Literal(lit) => match lit {
            LiteralKind::Integer(_) => Type::Basic(BasicType::Int32),
            LiteralKind::Float(_) => Type::Basic(BasicType::Float32),
            LiteralKind::Bool(_) => Type::Basic(BasicType::Bool),
            _ => panic!("unhandled literal kind {:?}", lit),
        },
        _ => panic!("unhandled expression {:?}", expr),
    }
}
