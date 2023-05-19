use super::*;

use crate::lexer::tokenize;
use crate::parser::parse;
use crate::parser::tests::parse_expression;

pub fn check_expression(_src: &str, expr: &mut Expression) -> Result<()> {
    let mut analyzer = Analyzer::new();
    analyzer.check_expr(expr)
}

#[test]
fn empty_source() {
    let src = "";
    let tokens = tokenize(src);
    let mut ast = parse(src, tokens).expect("parse failed");
    analyze(src, &mut ast).expect("analyze failed")
}

#[test]
fn import_with_body() {
    let src = "import func test() {}";
    let tokens = tokenize(src);
    let mut ast = parse(src, tokens).expect("parse failed");
    let err = analyze(src, &mut ast);
    if let Err(err) = err {
        // ensure span is function name
        assert_eq!(err.span, Some(Span { loc: 12, len: 4 }));
    } else {
        panic!("expected error")
    }
}

#[test]
fn func_without_body() {
    let src = "func test()";
    let tokens = tokenize(src);
    let mut ast = parse(src, tokens).expect("parse failed");
    let err = analyze(src, &mut ast);
    if let Err(err) = err {
        // ensure span is function name
        assert_eq!(err.span, Some(Span { loc: 5, len: 4 }));
    } else {
        panic!("expected error")
    }
}

#[test]
fn div_by_zero() {
    let src = "10.0 / 0.0";
    // let tokens = tokenize(src);
    let mut ast = parse_expression(src).expect("parse failed");
    let err = check_expression(src, &mut ast);
    if let Err(err) = err {
        // ensure span is full expression
        assert_eq!(err.span, Some(Span { loc: 0, len: 10 }));
    } else {
        panic!("expected error")
    }
}

#[test]
fn immutable_assign() {
    let src = "func test() {
        let x = 1
        x = 2
    }";
    let tokens = tokenize(src);
    let mut ast = parse(src, tokens).expect("parse failed");
    let err = analyze(src, &mut ast);
    if let Err(err) = err {
        // ensure span is variable assignment
        assert_eq!(err.span, Some(Span { loc: 40, len: 5 }));
    } else {
        panic!("expected error")
    }
}
