use super::*;
use crate::lexer::tokenize;

#[test]
fn empty_string() {
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
    let ItemKind::Function(func) = &ast.items.first().unwrap().kind;
    assert_eq!(func.name.as_str(), "test");
    assert!(func.params.is_empty());
    assert!(func.return_type.is_none());
    assert!(func
        .block
        .as_ref()
        .expect("function has no block")
        .expressions
        .is_empty());
    // println!("{:#?}", ast);
}
