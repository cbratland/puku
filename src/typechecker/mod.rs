use super::ast::*;

pub fn check(ast: &mut Ast) {
    let mut symbol_table = SymbolTable::new();
    for item in &mut ast.items {
        check_item(item, &mut symbol_table);
    }
}

fn check_item(item: &mut Item, symbol_table: &mut SymbolTable) {
    match &mut item.kind {
        ItemKind::Function(func) => {
            // parse function return type
            if let Some(ret_type) = &func.return_type_str {
                let ptype = symbol_table
                    .get(ret_type)
                    .expect("function return type is undefined")
                    .r#type;
                func.return_type = Some(ptype);
            }
            // parse param types
            for param in &mut func.params {
                let ptype = symbol_table
                    .get(&param.type_str)
                    .expect("param has undefined type")
                    .r#type;
                param.r#type = Some(ptype);
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
        _ => panic!("unhandled expression {:?}", expr),
    }
}
