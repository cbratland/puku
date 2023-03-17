mod emit;
#[allow(dead_code)]
mod ir;
mod leb;

pub use emit::emit;

use crate::ast::{
    self, BinaryOperator, Expression, ExpressionKind, ItemKind, Statement, StatementKind,
    UnaryOperator,
};
use ir::*;
use std::collections::HashMap;

// todo: make this better
pub fn gen_ir(ast: ast::Ast) -> Module {
    let mut types: Vec<ir::Type> = vec![];
    let mut functions: Vec<ir::Function> = vec![];
    let mut exports: Vec<ir::Export> = vec![];

    for function in ast.items {
        let ItemKind::Function(function) = function.kind;
        let func_type = ir::Type {
            params: function
                .params
                .iter()
                .map(|e| type_to_wasm(&e.r#type.expect("function param has no type")))
                .collect::<Vec<Valtype>>(),
            returns: if let Some(return_type) = function.return_type {
                vec![type_to_wasm(&return_type)]
            } else {
                vec![]
            },
        };
        let mut code: Vec<u8> = vec![];
        let locals = gen_function_code(&mut code, &function);

        // bytes: [00, 61, 73, 6D, 01, 00, 00, 00, 01, 87, 80, 80, 80, 00, 01, 60, 02, 7F, 7F, 01, 7F, 03, 82, 80, 80, 80, 00, 01, 00, 05, 83, 80, 80, 80, 00, 01, 00, 00, 07, 87, 80, 80, 80, 00, 01, 03, 61, 64, 64, 00, 00, 0A, 92, 80, 80, 80, 00, 01, 10, 01, 7F, 41, 01, 21, 02, 20, 00, 20, 01, 41, 01, 6A, 6A, 0F, 0B]
        // bytes: [00, 61, 73, 6D, 01, 00, 00, 00, 01, 87, 80, 80, 80, 00, 01, 60, 02, 7F, 7F, 01, 7F, 03, 82, 80, 80, 80, 00, 01, 00, 05, 83, 80, 80, 80, 00, 01, 00, 00, 07, 87, 80, 80, 80, 00, 01, 03, 61, 64, 64, 00, 00, 0A, 91, 80, 80, 80, 00, 01, 0F, 00, 41, 01, 21, 02, 20, 00, 20, 01, 41, 01, 6A, 6A, 0F, 0B]

        let ir_function = Function {
            type_index: types.len() as u8,
            code: Code { locals, body: code },
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
        ast::Type::Basic(basic) => match basic {
            ast::BasicType::Int32 => Valtype::I32,
            ast::BasicType::Int64 => Valtype::I64,
            ast::BasicType::Float32 => Valtype::F32,
            ast::BasicType::Float64 => Valtype::F64,

            ast::BasicType::Bool => Valtype::I32,

            _ => panic!("unknown basic type {:?}", basic),
        },
        _ => panic!("unknown type {:?}", ast_type),
    }
}

fn gen_function_code<W: std::io::Write>(
    buffer: &mut W,
    function: &ast::Function,
) -> Vec<(Valtype, u32)> {
    // locals, key: name, value: (index, type)
    let mut locals: HashMap<String, (u8, Valtype)> = HashMap::new();
    for (i, arg) in function.params.iter().enumerate() {
        locals.insert(
            arg.name.clone(),
            (
                i as u8,
                type_to_wasm(&arg.r#type.expect("param has no type")),
            ),
        );
    }
    if let Some(block) = &function.block {
        for statement in &block.statements {
            gen_stmt_code(buffer, statement, &mut locals);
        }
    }

    // todo: there has got to be a better way of writing this
    let mut locals_vec: Vec<(Valtype, u32)> = vec![];
    let mut i32_count = 0;
    let mut i64_count = 0;
    let mut f32_count = 0;
    let mut f64_count = 0;
    for (_, (_, valtype)) in locals.iter() {
        match valtype {
            Valtype::I32 => i32_count += 1,
            Valtype::I64 => i64_count += 1,
            Valtype::F32 => f32_count += 1,
            Valtype::F64 => f64_count += 1,
            Valtype::V128 => panic!("unsupported type"),
        }
    }
    if i32_count > 0 {
        locals_vec.push((Valtype::I32, i32_count));
    }
    if i64_count > 0 {
        locals_vec.push((Valtype::I64, i64_count));
    }
    if f32_count > 0 {
        locals_vec.push((Valtype::F32, f32_count));
    }
    if f64_count > 0 {
        locals_vec.push((Valtype::F64, f64_count));
    }
    return locals_vec;
}

fn gen_stmt_code<W: std::io::Write>(
    buffer: &mut W,
    statement: &Statement,
    locals: &mut HashMap<String, (u8, Valtype)>,
) {
    match &statement.kind {
        StatementKind::Return(expr) => {
            if let Some(expr) = expr {
                gen_expr_code(buffer, expr, locals);
                buffer.write_all(&[Opcode::Return as u8]).unwrap();
            }
        }
        StatementKind::Let(local) => {
            gen_expr_code(buffer, &local.init, locals);
            let idx = locals.len() as u8;
            buffer.write_all(&[Opcode::LocalSet as u8, idx]).unwrap();
            locals.insert(
                local.ident.clone(),
                (idx, type_to_wasm(&local.r#type.expect("missing var type"))),
            );
        }
        StatementKind::Expr(expr) => gen_expr_code(buffer, expr, locals),
    }
}

// generates wasm bytecode for expression and writes to buffer
fn gen_expr_code<W: std::io::Write>(
    buffer: &mut W,
    expression: &Expression,
    locals: &mut HashMap<String, (u8, Valtype)>,
) {
    // todo: support more expressions
    match &expression.kind {
        ExpressionKind::Variable(var) => {
            let index = match locals.get(&var.name) {
                Some(idx) => idx.0,
                None => {
                    let new_idx = locals.len() as u8;
                    locals.insert(
                        var.name.clone(),
                        (
                            new_idx,
                            type_to_wasm(&var.r#type.expect("missing var type")),
                        ),
                    );
                    new_idx
                }
            };
            buffer.write_all(&[Opcode::LocalGet as u8, index]).unwrap();
        }
        ExpressionKind::BinOp(op) => {
            gen_expr_code(buffer, &op.left, locals);
            gen_expr_code(buffer, &op.right, locals);
            let wasm_type = type_to_wasm(&op.r#type.expect("missing type"));
            buffer
                .write_all(&[match op.operator {
                    BinaryOperator::Add => match wasm_type {
                        Valtype::I32 => Opcode::I32Add,
                        Valtype::I64 => Opcode::I64Add,
                        Valtype::F32 => Opcode::F32Add,
                        Valtype::F64 => Opcode::F64Add,
                        _ => panic!("unknown type {:?}", op.r#type),
                    },
                    BinaryOperator::Sub => match wasm_type {
                        Valtype::I32 => Opcode::I32Sub,
                        Valtype::I64 => Opcode::I64Sub,
                        Valtype::F32 => Opcode::F32Sub,
                        _ => panic!("unknown type {:?}", op.r#type),
                    },
                    BinaryOperator::Mul => match wasm_type {
                        Valtype::I32 => Opcode::I32Mul,
                        Valtype::I64 => Opcode::I64Mul,
                        Valtype::F32 => Opcode::F32Mul,
                        _ => panic!("unknown type {:?}", op.r#type),
                    },
                    BinaryOperator::And | BinaryOperator::AndAnd => match wasm_type {
                        Valtype::I32 => Opcode::I32And,
                        _ => panic!("unknown type {:?}", op.r#type),
                    },
                    BinaryOperator::Or | BinaryOperator::OrOr => match wasm_type {
                        Valtype::I32 => Opcode::I32Or,
                        _ => panic!("unknown type {:?}", op.r#type),
                    },
                    _ => todo!(),
                } as u8])
                .unwrap();
        }
        ExpressionKind::Literal(lit) => match lit {
            ast::LiteralKind::Integer(int) => {
                buffer.write_all(&[Opcode::I32Const as u8]).unwrap();
                leb128::write::signed(buffer, *int as i64).unwrap();
            }
            ast::LiteralKind::Float(float) => {
                buffer.write_all(&[Opcode::F32Const as u8]).unwrap();
                buffer.write_all(&float.to_le_bytes()).unwrap();
            }
            ast::LiteralKind::Bool(bool) => {
                buffer.write_all(&[Opcode::I32Const as u8]).unwrap();
                leb128::write::signed(buffer, i64::from(*bool)).unwrap();
            }
            _ => panic!("unhandled literal"),
        },
        ExpressionKind::Group(expr) => gen_expr_code(buffer, expr, locals),
        ExpressionKind::UnaryOp(op) => match op.operator {
            UnaryOperator::Minus => {
                let wasm_type = type_to_wasm(&op.r#type.expect("missing type"));
                buffer
                    .write_all(&[
                        match wasm_type {
                            Valtype::I32 => Opcode::I32Const,
                            Valtype::I64 => Opcode::I64Const,
                            Valtype::F32 => Opcode::F32Const,
                            Valtype::F64 => Opcode::F64Const,
                            Valtype::V128 => todo!(),
                        } as u8,
                        0,
                    ])
                    .unwrap();
                gen_expr_code(buffer, &op.expr, locals);
                buffer
                    .write_all(&[match wasm_type {
                        Valtype::I32 => Opcode::I32Sub,
                        Valtype::I64 => Opcode::I64Sub,
                        Valtype::F32 => Opcode::F32Sub,
                        Valtype::F64 => Opcode::F64Sub,
                        _ => panic!("unknown type {:?}", op.r#type),
                    } as u8])
                    .unwrap();
            }
            UnaryOperator::Plus => gen_expr_code(buffer, &op.expr, locals), // useless
            UnaryOperator::Not => {
                gen_expr_code(buffer, &op.expr, locals);
                let wasm_type = type_to_wasm(&op.r#type.expect("missing type"));
                buffer
                    .write_all(&[match wasm_type {
                        Valtype::I32 => Opcode::I32Eqz,
                        Valtype::I64 => Opcode::I64Eqz,
                        _ => panic!("unknown type {:?}", op.r#type),
                    } as u8])
                    .unwrap();
            }
        },
        ExpressionKind::If(if_expr) => {
            gen_expr_code(buffer, &if_expr.cond, locals);
            buffer.write_all(&[Opcode::If as u8, 0x40]).unwrap();
            // buffer
            //     .write_all(&[Opcode::I32Eqz as u8, Opcode::BrIf as u8])
            //     .unwrap();
            // leb128::write::signed(buffer, 0).unwrap();
            // gen_expr_code(buffer, &if_expr.cond, locals);
            for stmt in &if_expr.then_branch.statements {
                gen_stmt_code(buffer, stmt, locals);
            }
            if let Some(else_branch) = &if_expr.else_branch {
                buffer.write_all(&[Opcode::Else as u8]).unwrap();
                gen_expr_code(buffer, &else_branch, locals);
            }
            buffer.write_all(&[Opcode::End as u8]).unwrap();
        }
        ExpressionKind::Block(block) => {
            for stmt in &block.statements {
                gen_stmt_code(buffer, stmt, locals);
            }
        } // _ => panic!("unhandled expression {:?}", expression.kind),
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
