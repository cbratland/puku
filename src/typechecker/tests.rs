use super::*;
// use crate::parser::tests::parse_expression;
use crate::lexer::tokenize;
use crate::parser::parse;
use crate::parser::tests::parse_expression;

pub fn check_expression(expr: &mut Expression) -> Result<Type> {
    let mut checker = TypeChecker::new("");
    checker.check_expr(expr)
}

#[test]
fn empty_source() {
    let src = "";
    let tokens = tokenize(src);
    let mut ast = parse(src, tokens).expect("parse failed");
    check(src, &mut ast).expect("typecheck failed")
}

#[test]
fn literals() {
    let mut int_expr = parse_expression("1").unwrap();
    assert_eq!(
        check_expression(&mut int_expr).expect("check failed"),
        Type::Basic(BasicType::Int32)
    );
    let mut float_expr = parse_expression("1.0").unwrap();
    assert_eq!(
        check_expression(&mut float_expr).expect("check failed"),
        Type::Basic(BasicType::Float32)
    );
}

#[test]
fn binary_expr() {
    for op in vec!["+", "-", "*", "/", "%", "^", "&", "|", "<<", ">>"] {
        let int_src = &format!("1 {op} 2");
        let mut int_expr = parse_expression(int_src).unwrap();
        assert_eq!(
            check_expression(&mut int_expr).expect("check failed"),
            Type::Basic(BasicType::Int32)
        );

        // TODO: bitwise shouldn't work on floats

        let float_src = &format!("1.0 {op} 2.0");
        let mut float_expr = parse_expression(float_src).unwrap();
        assert_eq!(
            check_expression(&mut float_expr).expect("check failed"),
            Type::Basic(BasicType::Float32)
        );
    }
}
