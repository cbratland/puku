use super::*;
use crate::lexer::tokenize;

pub fn parse_expression(src: &str) -> Result<Expression> {
    let tokens = tokenize(src);
    let mut parser = Parser::new(src, tokens);
    parser.parse_expr()
}

#[test]
fn empty_source() {
    let src = "";
    let tokens = tokenize(src);
    let ast = parse(src, tokens).expect("parse failed");
    assert!(ast.items.is_empty());
}

#[test]
fn empty_function() {
    let src = "func test() {}";
    let tokens = tokenize(src);
    let ast = parse(src, tokens).expect("parse failed");
    assert_eq!(
        ast,
        Ast {
            items: vec![Item {
                kind: ItemKind::Function(Box::new(Function {
                    attrs: FunctionAttributes {
                        export: Export::None,
                        import: Import::None
                    },
                    name: String::from("test"),
                    return_type: None,
                    params: vec![],
                    block: Some(Block {
                        statements: vec![],
                        span: Span { loc: 12, len: 2 }
                    })
                })),
                span: Span { loc: 0, len: 14 }
            }]
        }
    );
}

#[test]
fn literals() {
    let int_expr = parse_expression("1").unwrap();
    assert_eq!(
        int_expr,
        Expression::literal(ast::LiteralKind::Integer(1), Span { loc: 0, len: 1 })
    );

    let float_expr = parse_expression("1.0").unwrap();
    assert_eq!(
        float_expr,
        Expression::literal(ast::LiteralKind::Float(1.0), Span { loc: 0, len: 3 })
    );
}

#[test]
fn binary_expr() {
    for (op, operator) in vec![
        ("+", BinaryOperator::Add),
        ("-", BinaryOperator::Sub),
        ("*", BinaryOperator::Mul),
        ("/", BinaryOperator::Div),
        ("%", BinaryOperator::Mod),
        ("^", BinaryOperator::Caret),
        ("&", BinaryOperator::And),
        ("|", BinaryOperator::Or),
        ("<<", BinaryOperator::ShiftLeft),
        (">>", BinaryOperator::ShiftRight),
    ] {
        let src = &format!("1 {op} 2");
        let expr = parse_expression(src).unwrap();
        assert_eq!(
            expr,
            Expression::binop(
                Expression::literal(ast::LiteralKind::Integer(1), Span { loc: 0, len: 1 }),
                Expression::literal(
                    ast::LiteralKind::Integer(2),
                    Span {
                        loc: 3 + op.len() as u32,
                        len: 1
                    }
                ),
                operator
            )
        )
    }
}

#[test]
fn unary_expr() {
    for (op, operator) in vec![
        ("+", UnaryOperator::Plus),
        ("-", UnaryOperator::Minus),
        ("!", UnaryOperator::Not),
    ] {
        let src = &format!("{op}x");
        let expr = parse_expression(src).unwrap();
        assert_eq!(
            expr,
            Expression::uop(
                Expression::var(
                    String::from("x"),
                    Span {
                        loc: op.len() as u32,
                        len: 1
                    }
                ),
                operator
            )
        )
    }
}

#[test]
fn precedence() {
    let src = "(2 + 3) * 2 + 3 / 3";
    let expr = parse_expression(src).unwrap();
    assert_eq!(
        expr,
        Expression::binop(
            Expression::binop(
                Expression::group(
                    Expression::binop(
                        Expression::literal(ast::LiteralKind::Integer(2), Span { loc: 1, len: 1 }),
                        Expression::literal(ast::LiteralKind::Integer(3), Span { loc: 5, len: 1 }),
                        BinaryOperator::Add
                    ),
                    Span { loc: 0, len: 7 }
                ),
                Expression::literal(ast::LiteralKind::Integer(2), Span { loc: 10, len: 1 }),
                BinaryOperator::Mul
            ),
            Expression::binop(
                Expression::literal(ast::LiteralKind::Integer(3), Span { loc: 14, len: 1 }),
                Expression::literal(ast::LiteralKind::Integer(3), Span { loc: 18, len: 1 }),
                BinaryOperator::Div
            ),
            BinaryOperator::Add
        )
    )
}

#[test]
fn if_expr() {
    let src = "if !true { 1 } else if true { 2 } else { 3 }";
    let expr = parse_expression(src).unwrap();
    assert_eq!(
        expr,
        Expression::if_expr(
            Expression::uop(
                Expression::literal(ast::LiteralKind::Bool(true), Span { loc: 4, len: 4 }),
                UnaryOperator::Not
            ),
            Block {
                statements: vec![Statement::expr(Expression::literal(
                    ast::LiteralKind::Integer(1),
                    Span { loc: 11, len: 1 }
                ))],
                span: Span { loc: 9, len: 5 }
            },
            Some(Expression::if_expr(
                Expression::literal(ast::LiteralKind::Bool(true), Span { loc: 23, len: 4 }),
                Block {
                    statements: vec![Statement::expr(Expression::literal(
                        ast::LiteralKind::Integer(2),
                        Span { loc: 30, len: 1 }
                    ))],
                    span: Span { loc: 28, len: 5 }
                },
                Some(Expression::block(Block {
                    statements: vec![Statement::expr(Expression::literal(
                        ast::LiteralKind::Integer(3),
                        Span { loc: 41, len: 1 }
                    ))],
                    span: Span { loc: 39, len: 5 }
                })),
                Span { loc: 20, len: 24 }
            )),
            Span { loc: 0, len: 44 }
        )
    )
}
