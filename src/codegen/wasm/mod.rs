mod emit;
#[allow(dead_code)]
mod ir;
mod leb;

pub use emit::emit;

use crate::ast::{self, BinaryOperator, Expression, ItemKind};
use ir::*;
use std::collections::HashMap;

// todo: make this better
pub fn gen_ir<'a>(ast: ast::Ast) -> Module {
    let mut types: Vec<ir::Type> = vec![];
    let mut functions: Vec<ir::Function> = vec![];
    let mut exports: Vec<ir::Export> = vec![];

    for function in ast.items {
        let function = match (*function).kind {
            ItemKind::Function(func) => func,
            // _ => continue,
        };
        let func_type = ir::Type {
            params: function
                .params
                .iter()
                .map(|e| type_to_wasm(&e.r#type))
                .collect::<Vec<Valtype>>(),
            returns: if let Some(return_type) = function.return_type {
                vec![type_to_wasm(&return_type)]
            } else {
                vec![]
            },
        };
        let mut code: Vec<u8> = vec![];
        gen_function_code(&mut code, &function);

        let ir_function = Function {
            type_index: types.len() as u8,
            code: Code {
                locals: vec![],
                body: code,
            },
        };
        let export = Export {
            name: function.name,
            kind: ExternalKind::Function,
            index: functions.len() as u32,
        };

        types.push(func_type);
        functions.push(ir_function);
        exports.push(export);
    }

    Module {
        types: Some(types),
        functions: Some(functions),
        exports: Some(exports),
        ..Default::default()
    }
}

fn type_to_wasm(ast_type: &ast::Type) -> Valtype {
    match ast_type {
        ast::Type::I32 => Valtype::I32,
    }
}

fn gen_function_code<W: std::io::Write>(buffer: &mut W, function: &ast::Function) {
    let mut locals: HashMap<String, u8> = HashMap::new();
    for (i, arg) in function.params.iter().enumerate() {
        locals.insert(arg.name.clone(), i as u8);
    }
    if let Some(block) = &function.block {
        for expression in &block.expressions {
            gen_expr_code(buffer, expression, &mut locals);
        }
    }
}

// generates wasm bytecode for expression and writes to buffer
fn gen_expr_code<W: std::io::Write>(
    buffer: &mut W,
    expression: &Expression,
    locals: &mut HashMap<String, u8>,
) {
    // todo: support more expressions
    match expression {
        Expression::Variable(var) => {
            let index = match locals.get(&var.name) {
                Some(idx) => *idx,
                None => {
                    let new_idx = locals.len() as u8;
                    locals.insert(var.name.clone(), new_idx);
                    new_idx
                }
            };
            buffer.write_all(&[Opcode::LocalGet as u8, index]).unwrap();
        }
        Expression::Return(expr) => {
            if let Some(expr) = expr {
                gen_expr_code(buffer, expr, locals);
                buffer.write_all(&[Opcode::Return as u8]).unwrap();
            }
        }
        Expression::BinOp(op) => {
            gen_expr_code(buffer, &op.left, locals);
            gen_expr_code(buffer, &op.right, locals);
            buffer
                .write_all(&[match op.operator {
                    BinaryOperator::Add => match op.r#type {
                        ast::Type::I32 => Opcode::I32Add,
                    },
                    BinaryOperator::Sub => match op.r#type {
                        ast::Type::I32 => Opcode::I32Sub,
                    },
                    BinaryOperator::Mul => match op.r#type {
                        ast::Type::I32 => Opcode::I32Mul,
                    },
                    _ => todo!(),
                } as u8])
                .unwrap();
        }
        Expression::Literal(lit) => match lit {
            ast::LiteralKind::Integer(int) => {
                buffer.write_all(&[Opcode::I32Const as u8]).unwrap();
                leb128::write::signed(buffer, *int as i64).unwrap();
            }
            _ => todo!(),
        },
        _ => todo!(),
    }
}

// hardcoded defaults for testing:
// let types = vec![ir::Type {
//     params: vec![Valtype::I32, Valtype::I32],
//     returns: vec![Valtype::I32],
// }];

// let functions = vec![ir::Function {
//     type_index: 0,
//     code: Code {
//         locals: vec![],
//         body: vec![
//             Opcode::LocalGet as u8,
//             0,
//             Opcode::LocalGet as u8,
//             1,
//             Opcode::I32Add as u8,
//         ],
//     },
// }];

// let exports = vec![Export {
//     name: String::from("add"),
//     kind: ExternalKind::Function,
//     index: 0,
// }];